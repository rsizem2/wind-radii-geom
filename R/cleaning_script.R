#' Reads in and cleans raw hurricane data for use with our new ggplot geom
#'
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer separate
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{katrina_data <- get_hurricane_data() %>%
#'                          filter(startsWith(storm_id, "KATRINA")}
#'
#' @export
get_hurricane_data <- function(){
    read.fwf(system.file("extdata","ebtrk_atlc_1988_2015.txt", package = "hurricane"),
             na.strings = "-99",
             widths = c(6,11,12,6,6,3,5,4,4,5,4,13,13,13,2,6),
             col.names = c("id_num", "storm_name", "datetime", "latitude",
                           "longitude", "max wind speed", "min central pressure",
                           "radius max wind speed", "eye diameter",
                           "pressure outer closed isobar","radius outer closed isobar",
                           "34 NE SE SW NW",
                           "50 NE SE SW NW",
                           "64 NE SE SW NW",
                           "storm type", "nearest landmass"),
             strip.white = TRUE) %>%
        tibble::as_tibble() %>%
        dplyr::select(!(max.wind.speed:radius.outer.closed.isobar)) %>%
        dplyr::select(!c(id_num,storm.type)) %>%
        tidyr::pivot_longer(ends_with("NE.SE.SW.NW"), names_to = "windspeed", values_to="NE.SE.SW.NW") %>%
        dplyr::mutate(windspeed = as.integer(substr(windspeed,2,3))) %>%
        tidyr::separate(col = "NE.SE.SW.NW",
                 into = c("NE","SE","SW","NW"),
                 sep = c(3,6,9)) %>%
        dplyr::mutate(NE = na_if(as.integer(trimws(NE)), -99),
               SE = na_if(as.integer(trimws(SE)), -99),
               SW = na_if(as.integer(trimws(SW)), -99),
               NW = na_if(as.integer(trimws(NW)), -99)) %>%
        dplyr::mutate(year = as.integer(substr(datetime,7,11))) %>%
        dplyr::mutate(datetime = as.POSIXct(datetime,
                                     tz = "GMT",
                                     format = "%m%d%H %Y")) %>%
        dplyr::mutate(storm_id = paste(storm_name, year, sep = "-"),
               .keep = "unused",
               .before = datetime) %>%
        dplyr::mutate(windspeed = as.factor(windspeed),
                      longitude = -1*as.numeric(longitude),
                      latitude = as.numeric(latitude))
}
