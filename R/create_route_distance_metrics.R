#' create_route_distance_metrics
#'
#' @param gtfs_path_current String. Full file path to a zipped GTFS representing current transit service. Remember to switch back slashes to forward slashes.
#' @param gtfs_path_future String. Full file path to a zipped GTFS representing future transit service. Remember to switch back slashes to forward slashes.
#' @param coordinate_reference Numeric. The Coordinate Reference System code for the area. WA State Plane HARN FT US is 2926.
#'
#' @return A table of route distances for future and current transit service
#' @export
#'
#' @examples
create_route_distance_metrics <- function( gtfs_path_current, gtfs_path_future, coordinate_refence= 2926){

  gtfs_current <- tidytransit::read_gtfs(gtfs_path_current) %>%
    tidytransit::gtfs_as_sf()


  current_distance <- gtfs_current$trips %>%
    dplyr::distinct(.data$route_id, .data$shape_id) %>%
    dplyr::left_join(., gtfs_current$shapes) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = coordinate_reference) %>%
    dplyr::mutate(distance_miles = as.numeric(sf::st_length(.data$geometry))*0.000189394) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(., gtfs_current$routes %>%
                select(.data$route_id, .data$route_short_name)) %>%
    dplyr::group_by(.data$route_short_name) %>%
    dplyr::summarise(distance_miles = max(.data$distance_miles))

  gtfs_future <- tidytransit::read_gtfs(gtfs_path_future) %>%
    tidytransit::gtfs_as_sf()


  future_distance <- gtfs_future$trips %>%
    dplyr::distinct(.data$route_id, .data$shape_id) %>%
    dplyr::left_join(., gtfs_future$shapes) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = coordinate_reference) %>%
    dplyr::mutate(distance_miles = as.numeric(sf::st_length(.data$geometry))*0.000189394) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(., gtfs_future$routes %>%
                       select(.data$route_id, .data$route_short_name)) %>%
    dplyr::group_by(.data$route_short_name) %>%
    dplyr::summarise(distance_miles = max(.data$distance_miles))

  route_distances_out <- bind_rows(current_distance, future_distance)

 }
