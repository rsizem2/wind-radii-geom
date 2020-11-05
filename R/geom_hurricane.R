#' Defines the GeomHurricane class via the ggproto function in ggplot2.
#'
#'
#' @importFrom ggplot2 ggproto aes Geom draw_key_polygon
#' @importFrom dplyr mutate select tibble rename
#' @importFrom geosphere destPoint
#' @importFrom grid polygonGrob gpar
#' @importFrom magrittr %>%
#'
#' @export
GeomHurricane <-
    ggplot2::ggproto("GeomHurricane", ggplot2::Geom,

                     required_aes = c("x","y","r_ne","r_se","r_sw","r_nw"),

                     default_aes = ggplot2::aes(colour = "grey20",
                                                fill = "grey20",
                                                linetype = 0,
                                                alpha = 0.65,
                                                scale_radii= 1.0),

                     draw_key = ggplot2::draw_key_polygon,

                     draw_group = function(data, panel_scales, coord) {
                         data <- dplyr::mutate(data, fill = fill, colour = colour)
                         centers <- dplyr::select(data, lon = x, lat = y)
                         radii <- dplyr::select(data, r_ne, r_se, r_sw, r_nw)
                         points <- dplyr::tibble()

                         # iterate over each quadrant
                         for (i in 1:4){
                             # iterate over rows
                             for (j in seq_len(nrow(data))){
                                 center <- dplyr::tibble(lon = centers[j,1], lat = centers[j,2])
                                 points <- geosphere::destPoint(p = centers,
                                                                b = ((i-1)*90):(90*i),
                                                                d = data$scale_radii * 1852 * radii[j,i]) %>%
                                     rbind(center, points)
                             }

                             # # Data Manipulation
                             # vertices <- points %>%
                             #     dplyr::rename(x = lon, y = lat) %>%
                             #     coord$transform(panel_scales)
                         }

                         # Data Manipulation
                         vertices <- points %>%
                             dplyr::rename(x = lon, y = lat) %>%
                             coord$transform(panel_scales)

                         # Plot the polygon
                         grid::polygonGrob(x = vertices$x,
                                           y = vertices$y,
                                           default.units = "native",
                                           gp = grid::gpar(col = data$colour,
                                                           fill = data$fill,
                                                           alpha = data$alpha,
                                                           lty = 1,
                                                           scale_radii = data$scale_radii))
                     }
    )

#' Function that builds a layer based on the specification of the GeomHurricane class.
#'
#' @param data A dataframe containing hurricane data. Should include latitude and longitude
#' data for the center of the hurricane as well as radii values indicating how far from the center
#' of the hurricane certain windspeeds extend in the NE, NW, SW and SE directions.
#'
#'
#' @examples
#'   \dontrun{ggplot(data = katrina) +
#'                geom_hurricane(aes(x = longitude, y = latitude,
#'                                r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                                fill = wind_speed, color = wind_speed)) +
#'                scale_color_manual(name = "Wind speed (kts)",
#'                                   values = c("red", "orange", "yellow")) +
#'                scale_fill_manual(name = "Wind speed (kts)",
#'                                  values = c("red", "orange", "yellow"))}
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_hurricane <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE, ...){

    ggplot2::layer(geom = GeomHurricane,
                   mapping = mapping,
                   data = data,
                   stat = stat,
                   position = position,
                   show.legend = show.legend,
                   inherit.aes = inherit.aes,
                   params = list(na.rm = na.rm,...)
    )
}
