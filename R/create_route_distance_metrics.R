#' create_route_distance_metrics
#'
#' @param gtfs_path_current String. Full file path to a zipped GTFS representing current transit service. Remember to switch back slashes to forward slashes.
#' @param gtfs_path_future String. Full file path to a zipped GTFS representing future transit service. Remember to switch back slashes to forward slashes.
#' @param coordinate_reference Numeric. The Coordinate Reference System code for the area. WA State Plane HARN FT US is 2926.
#'
#' @return A table of route distances for future and current transit service
#' @export
#'
#' @examples  example_distance_table <- create_route_distance_metrics(
#' gtfs_path_future = fs::path_package(
#' "extdata",  "2030_gtfs_highgrowth.zip", package= "MetroSystemEval"),
#' gtfs_path_current = fs::path_package(
#' "extdata", "233_gtfs.zip", package = "MetroSystemEval"),
#' coordinate_reference = 2926)

create_route_distance_metrics <- function( gtfs_path_current, gtfs_path_future, coordinate_reference= 2926){

  gtfs_current <- tidytransit::read_gtfs(gtfs_path_current) %>%
    tidytransit::gtfs_as_sf()


  current_distance <- gtfs_current$trips %>%
    dplyr::distinct(.data$route_id, .data$shape_id) %>%
    dplyr::left_join( gtfs_current$shapes) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = coordinate_reference) %>%
    dplyr::mutate(distance_miles = as.numeric(sf::st_length(.data$geometry))*0.000189394) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join( gtfs_current$routes %>%
                dplyr::select(.data$route_id, .data$route_short_name)) %>%
    dplyr::group_by(.data$route_short_name) %>%
    dplyr::summarise(distance_miles = max(.data$distance_miles))

  gtfs_future <- tidytransit::read_gtfs(gtfs_path_future) %>%
    tidytransit::gtfs_as_sf()


  future_distance <- gtfs_future$trips %>%
    dplyr::distinct(.data$route_id, .data$shape_id) %>%
    dplyr::left_join( gtfs_future$shapes) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = coordinate_reference) %>%
    dplyr::mutate(distance_miles = as.numeric(sf::st_length(.data$geometry))*0.000189394) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join( gtfs_future$routes %>%
                       dplyr::select(.data$route_id,  .data$route_long_name, .data$route_short_name)) %>%
    dplyr::mutate(route_short_name =
                    base::ifelse(base::is.na(.data$route_short_name),
                                 .data$route_long_name,
                                 base::paste0(.data$route_short_name,
                                   " ", .data$route_long_name))) %>%
    dplyr::group_by(.data$route_short_name) %>%
    dplyr::summarise(distance_miles = max(.data$distance_miles))

  route_distances_out <- dplyr::bind_rows(current_distance, future_distance)

 }
