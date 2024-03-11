library(rstudioapi)
library(sf)
library(tidycensus)
library(tidytransit)
library(tidyverse)
library(tigris)
library(reshape2)
library(hms)




create_route_period_metrics <- function(gtfs_type, gtfs_path, service_change_start_date){
  #Read selected GTFS file
  if(gtfs_type == "current"){
    print("Running preformance metrics for CURRENT network")
    # If GTFS file is updated, make sure it is saved in the 1_inputs folder
    # and that the name is changed in the path below


    kcm <- read_gtfs(gtfs_path)
  } else if(gtfs_type == "future"){
    print("Running preformance metrics for FUTURE network")
    # If GTFS file is updated, make sure it is saved in the 1_inputs folder
    # and that the name is changed in the path below
    kcm <- read_gtfs(gtfs_path)
    kcm$routes <- kcm$routes %>%
      mutate(route_short_name = ifelse(is.na(route_short_name), route_long_name,
                                       paste0(route_short_name,
                                              ' ',
                                              route_long_name)))
  } else {
    print("Error on selected network. Review selection in 'gtfs_type' object")
  }

  # # # # # # # # # # # # # # # # # # # # # # # #
  # Step 1 - Process Main Bulk Metrics          ####
  # # # # # # # # # # # # # # # # # # # # # # # #

  # Identify service ID by type of day (weekday, sat or sun)

  if(gtfs_type == "current"){
    service_days <- kcm$calendar %>%
      # Make sure service IDs are actually in use in the trip tables
      filter(service_id %in% unique(kcm$trips$service_id)) %>%
      filter(end_date > lubridate::ymd(service_change_start_date) ) %>%
      # Define daytype of service ID to filter in next steps
      mutate(daytype = ifelse(saturday == 1, 'sat',
                              ifelse(sunday == 1, 'sun', 'wkd')))
  } else {
    #don't need to filter by calendar date for the MC gtfs
    service_days <- kcm$calendar %>%
      # Make sure service IDs are actually in use in the trip tables
      filter(service_id %in% unique(kcm$trips$service_id)) %>%
      # Define daytype of service ID to filter in next steps
      mutate(daytype = ifelse(saturday == 1, 'sat',
                              ifelse(sunday == 1, 'sun', 'wkd')))

  }


  # Weekdays
  # Filter only trips in the routes in the corridors selected
  trips <- kcm$trips %>%
    # Keep only relevant variables
    filter(service_id %in% service_days$service_id) %>%
    select(route_id, service_id, trip_id, direction_id)


  if (is.character(kcm$stop_times$arrival_time) == TRUE){
    print("character stop times")
    stop_times <- kcm$stop_times %>%
      # Filter stop-trips of the relevant routes
      filter(trip_id %in% trips$trip_id) %>%
      select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)

    stop_times[c("hour", "min", "sec")] <- str_split_fixed(stop_times$arrival_time, ":", 3)

    stop_times_2 <- stop_times %>%
      mutate( hour_sec = as.numeric(hour) *3600,
              min_sec = as.numeric(min) * 60) %>%
      mutate(start_time_sec = hour_sec+min_sec+as.numeric(sec)) %>%
      # Sort the dataframe
      arrange(trip_id, start_time_sec, stop_sequence) %>%
      # Group by unique trip (Recall this dataframe is already sorted by time)
      group_by(trip_id) %>%
      # Create two new columns for each trip (group)
      # One with the start time (string type) of every trip
      # The other with the end time (string type) of every trip
      mutate(start_time_str = as.numeric(min(start_time_sec)),
             end_time_str = as.numeric(max(start_time_sec))) %>%
      mutate(start_time_hr = trunc(start_time_str/3600, 0),
             start_time_min = ((start_time_str - (start_time_hr*3600))/60),
             start_time_seconds = start_time_str - ((start_time_hr*3600) + (start_time_min*60) )) %>%
      mutate(end_time_hr = trunc(end_time_str/3600, 0),
             end_time_min = ((end_time_str - (end_time_hr*3600))/60),
             end_time_seconds = end_time_str - ((end_time_hr*3600) + (end_time_min*60) ))


    stop_times <- stop_times_2


  }else{
    print("check data structure of stop_times$arrival_times.")
    break
  }
  # This snippet process the time fields from character type into more manageable type and values
  # The raw time in the GTFS contained midnight and past-midnight trips hours as
  # 24, 25, 26, 27 etc.
  # First, join the trip dataframe with the stop times computed previously
  trips_stops <- left_join(trips, stop_times) %>%
    left_join(service_days %>%  select(c(service_id, daytype))) %>%
    select(-service_id, -trip_id) %>%
    # Remove potential duplicates to avoid errors in the calculations
    distinct(., route_id, direction_id, start_time_str, .keep_all = TRUE) %>%
    # Extract only the hour integer from the character time and set it as numeric

    # Convert midnight and past-midnight hours to equivalent hours
    # e.g. 24 to 00, 25 to 01, 26 to 02, etc.
    mutate(start_time_hr = ifelse(start_time_hr > 23, start_time_hr - 24, start_time_hr),
           end_time_hr = ifelse(end_time_hr > 23, end_time_hr - 24, end_time_hr)) %>%
    # Add back the minutes and seconds to the cleaned numeric trip hours
    # This will reverse back the type od the data to character
    mutate(start_time = ((start_time_hr*3600) + (start_time_min*60) + start_time_seconds) ,
           end_time = ((end_time_hr*3600) + (end_time_min*60) + end_time_seconds)) %>%
    # Change start and end time type from character to difftime type (hms)
    # This will allow to perform calculations such as sums and subtractions

    # Calculate service hours for every trip
    # A conditional statement is used for those trips that start before midnight and end after it
    mutate(service_hr = ifelse(start_time <  end_time,
                               (end_time - start_time)/3600,
                               ( (end_time -  start_time + (24*3600)))/3600),
           # Define period of day based on the integer hour the trip starts
           period = ifelse((start_time_hr %in% c(8:17) & daytype == "sat"), "5.sat_main",
                           ifelse((!(start_time_hr %in% c(8:17)) & daytype %in% c("sat", "sun")), "7.other",
                                  ifelse((start_time_hr %in% c(8:17) & daytype == "sun"), "6.sun_main",
                                         ifelse( ( start_time_hr %in% c(5:8) & daytype == "wkd"), '1.am_peak',
                                                 ifelse((start_time_hr %in% c(15:18) & daytype == "wkd"), '3.pm_peak',
                                                        ifelse((start_time_hr %in% c(9:14) & daytype == "wkd"), '2.midday', '4.night'))))))) %>%
    # Sort dataframe, the relevant variable is start time of the trip
    # Note that we use the string start time, to ensure that a trip starting at 25:00:00
    # is sorted at the end of the day an not earlier as a 01:00:00 trip
    arrange(route_id, daytype, direction_id, start_time_str) %>%
    group_by(route_id, daytype, direction_id) %>%
    # For every corridor calculate the start time of the first trip and the
    # end time of the last trip
    mutate(start_first_trip = first(start_time),
           end_last_trip = last(start_time)) %>%
    # Calculate the start of first trip and end of last trip in numeric hours
    # (this has the hours as integer and the minutes and seconds in decimals)
    mutate(first = hms::hms(seconds = start_first_trip),
           last = hms::hms(seconds = end_last_trip)) %>%
    # Compute the span of service for all day for every corridor
    # This all day span of service was not used for any further calculation
    # It can be removed in final version
    mutate(span_gtfs_hrs = ifelse(first <  last,
                                  last - first,
                                  last - first + 24*3600)) %>%
    # Calculate how many hours before 5am a trip starts (Upper)
    # Calulate how many hours after 7pm (19:00) a trip starts (Lower)
    mutate(upper = 5*3600 - as.numeric(first),
           lower = ifelse(last > 14*3600 & last < 19*3600, 0, last - 19*3600)) %>%
    # Calculate actual span of service at night period
    # If trips in a corridor starts before 5am and ends after 7pm that means there
    # is some span service during the night. Add upper and lower (hours in night period)
    # and multiply by 60 minutes
    mutate(span_night_mins = ifelse(upper >= 0 & lower >= 0, (upper + lower) / 60,
                                    # If trips in a corridor starts after 5am and ends past midnight
                                    # then calculate net hours including those past midnight
                                    ifelse(upper <= 0 & lower < 0, (24*3600 + lower) / 60,
                                           # If trips in a corridor starts after 5am and end before midnght but past 19:00
                                           # then calculate hours using hours past 19 pm (lower) and multiply by 60
                                           ifelse(upper <= 0 & lower >= 0, lower / 60,
                                                  # If trips in a corridor starts before 5am and ends past midnight
                                                  # then calculate net hours before 5am
                                                  ifelse(upper >= 0 & lower < 0, (upper + 24*3600 + lower) / 60, 999999)))),
           # Calculate adjustment in minutes for trips in corridors that do not cover
           # the complete am or pm period
           span_pm_peak_adj_mins = ifelse(last > 14*3600 & last < 19*3600, (19*3600 - last)/60, 0),
           span_am_peak_adj_mins = ifelse(first > 5*3600 & first < 11*3600, (first - 5*3600)/60, 0)) %>%
    mutate(span_night_mins = ifelse(span_night_mins >= 600, 600, span_night_mins)) %>%
    select(-first, -last, -lower, -upper)

  # Clean up
  rm(stop_times)


  # Aggregate dataframe by corridor and period
  route_trips <- trips_stops %>%
    group_by(route_id,daytype, period, direction_id,
             span_night_mins, span_pm_peak_adj_mins, span_am_peak_adj_mins) %>%
    # Calculate trips in period and service hours in period
    summarise(num_trips = n(),
              serv_hours = sum(service_hr)) %>%
    # Calculate service hour per trip and adjust minutes of operation in period based on the
    # adjustment values calculated earlier
    mutate(srv_hr_trip = serv_hours/num_trips,
           mins_period = ifelse(period == '1.am_peak', (4*60) - span_am_peak_adj_mins,
                                ifelse(period == '3.pm_peak', (4*60) - span_pm_peak_adj_mins,
                                       ifelse(period == '2.midday', 6*60,
                                              ifelse(period == '5.sat_main', (10*60) - span_pm_peak_adj_mins - span_am_peak_adj_mins,
                                                     ifelse(period == '6.sun_main', (10*60) - span_pm_peak_adj_mins - span_am_peak_adj_mins,
                                                            span_night_mins)))))) %>%
    # Calculate average headways based on GTFS processed data
    mutate(avg_headway_mins = round(mins_period/num_trips, 0)) %>%
    ungroup()

  out <- left_join(route_trips, kcm$routes %>%
                     select(route_id, route_short_name, route_type)) %>%
    select(route_short_name,daytype, period, num_trips,
           serv_hours, srv_hr_trip, mins_period, avg_headway_mins, route_type) %>%
    group_by(route_short_name,  daytype, period, route_type) %>%
    summarise_all(sum) %>%
    mutate(srv_hr_trip = serv_hours/num_trips,
           avg_headway_mins = mins_period/num_trips) %>%
    mutate(network_type = gtfs_type)

}
