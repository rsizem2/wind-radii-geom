#' Defines the GeomHurricane class via the ggproto function in ggplot2.
#'
#' The workhorse of this geom is the destPoint function from the `geosphere` package which takes a center point and radius and generates the vertices from which we can draw the polygonGrob. See \link{geom_hurricane} for specific details about parameters.
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
                                                alpha = 0.6,
                                                scale_radii= 1.0),

                     draw_key = ggplot2::draw_key_polygon,

                     draw_group = function(data, panel_scales, coord) {

                         # Generate auxiliary data to be used with destPoint or polygonGrob
                         data <- dplyr::mutate(data, fill = fill, colour = colour)
                         centers <- dplyr::select(data, lon = x, lat = y)
                         radii <- dplyr::select(data, r_ne, r_se, r_sw, r_nw)
                         points <- dplyr::tibble()

                         # Create circular quadrants using destPoint
                         for (i in 1:4){
                             for (j in seq_len(nrow(data))){
                                 center <- dplyr::tibble(lon = centers[j,1], lat = centers[j,2])
                                 arc_points <- geosphere::destPoint(p = centers,
                                                                    b = ((i-1)*90):(90*i),
                                                                    d = data$scale_radii * 1852 * radii[j,i])
                                 points <- rbind(arc_points, center, points)
                             }
                         }

                         # Transform the data
                         coords <- points %>%
                             dplyr::rename(x = lon, y = lat) %>%
                             coord$transform(panel_scales)

                         # Construct the polygonGrob
                         grid::polygonGrob(x = coords$x,
                                           y = coords$y,
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
#' @param data A dataframe containing hurricane data. See details for expected formatting.
#'
#' @details Required aesthetics:
#'   * x - longtitude of center.
#'   * y - latitude of center.
#'   * r_ne - radius from center point for northeast quadrant.
#'   * r_se - radius from center point for southeast quadrant.
#'   * r_nw - radius from center point for northwest quadrant.
#'   * r_sw - radius from center point for southwest quadrant.
#'   Optional aesthetics:
#'   * alpha - transparency of the drawn layer, default = 0.6.
#'   * fill - fill color, default = 'grey20'.
#'   * color - outline color, default = 'grey20'.
#'   * scale_radius - scaling factor for radius, default = 1.0 (no scaling).
#' @md
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
