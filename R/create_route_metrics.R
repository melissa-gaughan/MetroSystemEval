
#' Generate system evaluation performance metrics
#'
#' @param gtfs_type A string, indicating whether the GTFS represents current service or future (Metro Connects) service. Should be either "current" or "future".
#' @param gtfs_path The full file path of where your GTFS lives.
#' @param service_change_start_date The date ("YYYY-MM-DD") on which the service change starts. Will be used to filter GTFS to relevant service.
#'
#' @return A dataframe of route-level service metrics by period and day.
#' @export
#'
#' @examples NA
create_route_metrics <- function(gtfs_type, gtfs_path, service_change_start_date){
  #Read selected GTFS file
  if(gtfs_type == "current"){
    base::print("Running preformance metrics for CURRENT network")
    # If GTFS file is updated, make sure it is saved in the 1_inputs folder
    # and that the name is changed in the path below


    kcm <- tidytransit::read_gtfs(gtfs_path)
  } else if(gtfs_type == "future"){
    print("Running preformance metrics for FUTURE network")
    # If GTFS file is updated, make sure it is saved in the 1_inputs folder
    # and that the name is changed in the path below
    kcm <- tidytransit::read_gtfs(gtfs_path)
    kcm$routes <- kcm$routes %>%
      dplyr::mutate(route_short_name = base::ifelse(base::is.na(.data$route_short_name), .data$route_long_name,
                                       base::paste0(.data$route_short_name,
                                              ' ',
                                              .data$route_long_name)))
  } else {
    base::print("Error on selected network. Review selection in 'gtfs_type' object")
  }

  # # # # # # # # # # # # # # # # # # # # # # # #
  # Step 1 - Process Main Bulk Metrics          ####
  # # # # # # # # # # # # # # # # # # # # # # # #

  # Identify service ID by type of day (weekday, sat or sun)


    service_days <- kcm$calendar %>%
      # Make sure service IDs are actually in use in the trip tables
      dplyr::filter(.data$service_id %in% base::unique(kcm$trips$service_id)) %>%
      dplyr::filter(.data$end_date > lubridate::ymd(service_change_start_date) ) %>%
      # Define daytype of service ID to filter in next steps
      dplyr::mutate(daytype = base::ifelse(.data$saturday == 1, 'sat',
                                           base::ifelse(.data$sunday == 1, 'sun', 'wkd')))

  # Filter only trips in the routes in the corridors selected
  trips <- kcm$trips %>%
    # Keep only relevant variables
    dplyr:: filter(.data$service_id %in% service_days$service_id) %>%
    dplyr::select(.data$route_id, .data$service_id, .data$trip_id, .data$direction_id)


  if (is.character(kcm$stop_times$arrival_time) == TRUE){
    base::print("character stop times")
    stop_times <- kcm$stop_times %>%
      # Filter stop-trips of the relevant routes
      dplyr::filter(.data$trip_id %in% trips$trip_id) %>%
      dplyr::select(.data$trip_id, .data$arrival_time, .data$departure_time, .data$stop_id, .data$stop_sequence)

    stop_times[c("hour", "min", "sec")] <- stringr::str_split_fixed(stop_times$arrival_time, ":", 3)

    stop_times_2 <- stop_times %>%
      dplyr::mutate( hour_sec = base::as.numeric(.data$hour) *3600,
                     min_sec = base::as.numeric(.data$min) * 60) %>%
      dplyr::mutate(start_time_sec = .data$hour_sec+.data$min_sec+base::as.numeric(.data$sec)) %>%
      # Sort the dataframe
      dplyr::arrange(.data$trip_id, .data$start_time_sec, .data$stop_sequence) %>%
      # Group by unique trip (Recall this dataframe is already sorted by time)
      dplyr::group_by(.data$trip_id) %>%
      # Create two new columns for each trip (group)
      # One with the start time (string type) of every trip
      # The other with the end time (string type) of every trip
      dplyr::mutate(start_time_str = base::as.numeric(min(.data$start_time_sec)),
                    end_time_str = base::as.numeric(max(.data$start_time_sec))) %>%
      dplyr::mutate(start_time_hr = base::trunc(.data$start_time_str/3600, 0),
                    start_time_min = ((.data$start_time_str - (.data$start_time_hr*3600))/60),
                    start_time_seconds = .data$start_time_str - ((.data$start_time_hr*3600) + (.data$start_time_min*60) )) %>%
      dplyr::mutate(end_time_hr = base::trunc(.data$end_time_str/3600, 0),
                    end_time_min = ((.data$end_time_str - (.data$end_time_hr*3600))/60),
                    end_time_seconds = .data$end_time_str - ((.data$end_time_hr*3600) + (.data$end_time_min*60) ))


    stop_times <- stop_times_2


  }else{
    base::print(base::paste0("check data structure of stop_times$arrival_times for ", gtfs_type, " at ", gtfs_path))
    stop()
  }
  # This snippet process the time fields from character type into more manageable type and values
  # The raw time in the GTFS contained midnight and past-midnight trips hours as
  # 24, 25, 26, 27 etc.
  # First, join the trip dataframe with the stop times computed previously
  trips_stops <- dplyr::left_join(trips, stop_times) %>%
    dplyr::left_join(service_days %>%  dplyr::select(c(.data$service_id, .data$daytype))) %>%
    dplyr::select(-.data$service_id, -.data$trip_id) %>%
    # Remove potential duplicates to avoid errors in the calculations
    dplyr::distinct( .data$route_id, .data$direction_id, .data$start_time_str, .keep_all = TRUE) %>%
    # Extract only the hour integer from the character time and set it as numeric

    # Convert midnight and past-midnight hours to equivalent hours
    # e.g. 24 to 00, 25 to 01, 26 to 02, etc.
    dplyr::mutate(start_time_hr = base::ifelse(.data$start_time_hr > 23, .data$start_time_hr - 24, .data$start_time_hr),
                  end_time_hr = base::ifelse(.data$end_time_hr > 23, .data$end_time_hr - 24, .data$end_time_hr)) %>%
    # Add back the minutes and seconds to the cleaned numeric trip hours
    # This will reverse back the type od the data to character
    dplyr::mutate(start_time = ((.data$start_time_hr*3600) + (.data$start_time_min*60) + .data$start_time_seconds) ,
                  end_time = ((.data$end_time_hr*3600) + (.data$end_time_min*60) + .data$end_time_seconds)) %>%
    # Change start and end time type from character to difftime type (hms)
    # This will allow to perform calculations such as sums and subtractions

    # Calculate service hours for every trip
    # A conditional statement is used for those trips that start before midnight and end after it
    dplyr::mutate(service_hr = base::ifelse(.data$start_time <  .data$end_time,
                               (.data$end_time - .data$start_time)/3600,
                               ( (.data$end_time -  .data$start_time + (24*3600)))/3600),
           # Define period of day based on the integer hour the trip starts
           period = base::ifelse((.data$start_time_hr %in% c(8:17) & .data$daytype == "sat"), "5.sat_main",
                                 base::ifelse((!(.data$start_time_hr %in% c(8:17)) & .data$daytype %in% c("sat", "sun")), "7.other",
                                              base::ifelse((.data$start_time_hr %in% c(8:17) & .data$daytype == "sun"), "6.sun_main",
                                                           base::ifelse( ( .data$start_time_hr %in% c(5:8) & .data$daytype == "wkd"), '1.am_peak',
                                                                         base::ifelse((.data$start_time_hr %in% c(15:18) & .data$daytype == "wkd"), '3.pm_peak',
                                                                                      base::ifelse((.data$start_time_hr %in% c(9:14) & .data$daytype == "wkd"), '2.midday', '4.night'))))))) %>%
    # Sort dataframe, the relevant variable is start time of the trip
    # Note that we use the string start time, to ensure that a trip starting at 25:00:00
    # is sorted at the end of the day an not earlier as a 01:00:00 trip
    dplyr::arrange(.data$route_id, .data$daytype, .data$direction_id, .data$start_time_str) %>%
    dplyr::group_by(.data$route_id, .data$daytype, .data$direction_id) %>%
    # For every corridor calculate the start time of the first trip and the
    # end time of the last trip
    dplyr::mutate(start_first_trip = dplyr::first(.data$start_time),
                  end_last_trip = dplyr::last(.data$start_time)) %>%
    # Calculate the start of first trip and end of last trip in numeric hours
    # (this has the hours as integer and the minutes and seconds in decimals)
    dplyr::mutate(first = hms::hms(seconds = .data$start_first_trip),
                  last = hms::hms(seconds = .data$end_last_trip)) %>%
    # Compute the span of service for all day for every corridor
    # This all day span of service was not used for any further calculation
    # It can be removed in final version
    dplyr::mutate(span_gtfs_hrs = base::ifelse(.data$first <  .data$last,
                                                     .data$last - .data$first,
                                                     .data$last - .data$first + 24*3600)) %>%
    # Calculate how many hours before 5am a trip starts (Upper)
    # Calulate how many hours after 7pm (19:00) a trip starts (Lower)
    dplyr::mutate(upper = 5*3600 - base::as.numeric(.data$first),
                  lower = ifelse(.data$last > 14*3600 & .data$last < 19*3600, 0, .data$last - 19*3600)) %>%
    # Calculate actual span of service at night period
    # If trips in a corridor starts before 5am and ends after 7pm that means there
    # is some span service during the night. Add upper and lower (hours in night period)
    # and multiply by 60 minutes
    dplyr::mutate(span_night_mins = base::ifelse(.data$upper >= 0 & .data$lower >= 0, (.data$upper + .data$lower) / 60,
                                    # If trips in a corridor starts after 5am and ends past midnight
                                    # then calculate net hours including those past midnight
                                    base::ifelse(.data$upper <= 0 & .data$lower < 0, (24*3600 + .data$lower) / 60,
                                           # If trips in a corridor starts after 5am and end before midnght but past 19:00
                                           # then calculate hours using hours past 19 pm (lower) and multiply by 60
                                           base::ifelse(.data$upper <= 0 & .data$lower >= 0, .data$lower / 60,
                                                  # If trips in a corridor starts before 5am and ends past midnight
                                                  # then calculate net hours before 5am
                                                  base::ifelse(.data$upper >= 0 & .data$lower < 0, (.data$upper + 24*3600 + .data$lower) / 60, 999999)))),
           # Calculate adjustment in minutes for trips in corridors that do not cover
           # the complete am or pm period
           span_pm_peak_adj_mins = base::ifelse(.data$last > 14*3600 & .data$last < 19*3600, (19*3600 - .data$last)/60, 0),
          span_am_peak_adj_mins = base::ifelse(.data$first > 5*3600 & .data$first < 11*3600, (.data$first - 5*3600)/60, 0)) %>%
    dplyr::mutate(span_night_mins = base::ifelse(.data$span_night_mins >= 600, 600, .data$span_night_mins)) %>%
    dplyr::select(-.data$first, -.data$last, -.data$lower, -.data$upper)


  # Aggregate dataframe by corridor and period
  route_trips <- trips_stops %>%
    dplyr::group_by(.data$route_id,.data$daytype, .data$period, .data$direction_id,
                    .data$span_night_mins, .data$span_pm_peak_adj_mins, .data$span_am_peak_adj_mins) %>%
    # Calculate trips in period and service hours in period
    dplyr::summarise(num_trips = dplyr::n(),
                     serv_hours = base::sum(.data$service_hr)) %>%
    # Calculate service hour per trip and adjust minutes of operation in period based on the
    # adjustment values calculated earlier
    dplyr::mutate(srv_hr_trip = .data$serv_hours/.data$num_trips,
                   mins_period = base::ifelse(.data$period == '1.am_peak', (4*60) - .data$span_am_peak_adj_mins,
                                      base::ifelse(.data$period == '3.pm_peak', (4*60) - .data$span_pm_peak_adj_mins,
                                                   base::ifelse(.data$period == '2.midday', 6*60,
                                                                base::ifelse(.data$period == '5.sat_main', (10*60) - .data$span_pm_peak_adj_mins - .data$span_am_peak_adj_mins,
                                                                             base::ifelse(.data$period == '6.sun_main', (10*60) - .data$span_pm_peak_adj_mins - .data$span_am_peak_adj_mins,
                                                                                          .data$span_night_mins)))))) %>%
    # Calculate average headways based on GTFS processed data
    dplyr::mutate(avg_headway_mins = base::round(.data$mins_period/.data$num_trips, 0)) %>%
    dplyr::ungroup()

  out <- dplyr::left_join(route_trips, kcm$routes %>%
                            dplyr::select(.data$route_id, .data$route_short_name, .data$route_type)) %>%
    dplyr::select(.data$route_short_name,.data$daytype, .data$period, .data$num_trips,
                  .data$serv_hours, .data$srv_hr_trip, .data$mins_period, .data$avg_headway_mins, .data$route_type) %>%
    dplyr::group_by(.data$route_short_name,  .data$daytype, .data$period, .data$route_type) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::mutate(srv_hr_trip = .data$serv_hours/.data$num_trips,
                  avg_headway_mins =.data$ mins_period/.data$num_trips) %>%
    dplyr::mutate(network_type = gtfs_type)

}
