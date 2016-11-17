#' Plot a choropleth map
#'
#' This function will attempt to plot a choropleth map using the data provided
#' to it.
#'
#' @param data A spatial polygons data frame. The map produced will be at the
#' county level if the column name \code{county} is present otherwise the map
#' will be at the level of the provided geography.
#' @param variable the name of the variable on the basis of which the map
#' should be themed
#' @param title the title to display on the map
#' @param qtiles the number of quantiles to split \code{variable} into. Note:
#' if \code{breaks} is specified, \code{qtiles} is ignored. By default
#' \code{variable} is split into quintiles.
#' @param breaks the (ordered) breaks on which to split \code{variable}, for
#' \code{n} splits \code{n+1} breaks must be specified. If \code{breaks} is
#' specified (other than \code{NA}), \code{qtiles} are ignored.
#' @param london_only boolean; for county level maps only (otherwise ignored),
#' should only the London counties be displayed?
#'
#' If \code{breaks} is specified, \code{qtiles} is ignored.
#'
#' @return A choropleth map at the county level if the supplied \code{data}
#' contained the column name \code{county}; otherwise a choropleth map
#' at the level of the provided geography.
#' @examples
#' \dontrun{
#' # example county-level data
#' county_data <- data.frame(county = county_boundary_data$county,
#'                           deaths = round(rnorm(length(county_boundary_data$county), 30, 8)))
#' # example ambulance-level data
#' service_data <- data.frame(service = ambulance_boundary_data$service,
#'                            hoaxes = round(rnorm(length(ambulance_boundary_data$service), 70, 8)))
#' service_data <- service_data[service_data$service != "IOW", ]
#'
#' # merge numeric data and spatial data
#' county_data_spatial <- merge(county_boundary_data, county_data, by = "county")
#' service_data_spatial <- merge(ambulance_boundary_data, service_data, by = "service")
#'
#' # make some nice maps
#' plotMap(county_data_spatial, "deaths", "My lovely map")
#' plotMap(county_data_spatial, "deaths", "My lovely map", london_only = TRUE)
#' plotMap(service_data_spatial, "hoaxes", "My lovely map", breaks = c(57, 65, 73, 83))
#' }
#' @export
plotMap <- function(data, variable, title, qtiles = 5, breaks = NA, london_only = FALSE) {

  # n-tile breaks
  if(any(is.na(breaks))) {
    n <- qtiles
    qbreaks <- stats::quantile(data@data[, variable], probs = 0:n/n, na.rm = TRUE, names = FALSE, type = 8)
  } else {
    qbreaks <- breaks
  }

  n_breaks <- length(qbreaks)

  if (length(qbreaks) > 6) {
    colours <- cartography::carto.pal(pal1 = "green.pal", n1 = floor(n_breaks / 2) , pal2 = "red.pal", n2 = ceiling(n_breaks / 2))
  } else {
    colours <- cartography::carto.pal(pal1 = "green.pal", n1 = length(qbreaks))
  }

  graphics::par(mar = c(0, 0, 1.5, 0))
  if("county" %in% colnames(data@data)) {
    # county level

    if(london_only == FALSE) {
      # England

      sp:::plot.SpatialPolygons(data, border = NA, col = NA, bg = "#A6CAE0")
      # Add cloro
      cartography::choroLayer(spdf = data, df = data@data, var = variable,
                 breaks = qbreaks, col = colours,
                 border = "#666666", lwd = 0.25, legend.pos = "topright",
                 legend.title.txt = variable,
                 legend.values.rnd = 2,
                 colNA = "#aaaaaa", add = TRUE)
    } else {
      # London
      sp:::plot.SpatialPolygons(data[data$in_london == TRUE, ], border = NA, col = NA, bg = "#A6CAE0")
      sp:::plot.SpatialPolygons(data, border = "#666666", lwd = 0.25, col = "#aaaaaa", bg = NA, add = TRUE)
      # Add cloro
      cartography::choroLayer(spdf = data[data$in_london == TRUE, ], df = data[data$in_london == TRUE, ]@data, var = variable,
                 breaks = qbreaks, col = colours,
                 border = "#666666", lwd = 0.25, legend.pos = "topright",
                 legend.title.txt = variable,
                 legend.values.rnd = 2,
                 colNA = "#aaaaaa", add = TRUE)

    }
    sp:::plot.SpatialPolygons(vanmaps::ambulance_boundary_data, border = "#ffffff", lwd = 2, col = NA, bg = NA, add = TRUE)

  } else {
    # ambulance service level

    sp:::plot.SpatialPolygons(data, border = NA, col = NA, bg = "#A6CAE0")
    # Add cloro
    cartography::choroLayer(spdf = data, df = data@data, var = variable,
               breaks = qbreaks, col = colours,
               border = "#ffffff", lwd = 2, legend.pos = "topright",
               legend.title.txt = variable,
               legend.values.rnd = 2,
               colNA = "#aaaaaa", add = TRUE)

  }
  cartography::layoutLayer(title = title, # title of the map
              author = "",  # no author text
              sources = paste("Contains National Statistics data: Crown copyright and database right 2016;",
                              "Contains OS data: Crown copyright and database right 2016", sep = "\n"),
              scale = NULL,
              col = NA,
              frame = FALSE,
              coltitle = "black") # color of the title
}

#' Save choropleth map(s)
#'
#' This function will attempt to save choropleth map(s) using the data provided
#' to it. If the data is at the County level it will save both England and
#' London only maps.
#'
#' @param fname file name to save the plots under (no directory paths or file
#' extensions)
#' @param fwidth the width (pixels) of the plots to be produced
#' @param data A spatial polygons data frame. The map produced will be at the
#' county level if the column name \code{county} is present otherwise the map
#' will be at the level of the provided geography.
#' @param variable the name of the variable on the basis of which the map
#' should be themed
#' @param title the title to display on the map
#' @param qtiles the number of quantiles to split \code{variable} into. Note:
#' if \code{breaks} is specified, \code{qtiles} is ignored. By default
#' \code{variable} is split into quintiles.
#' @param breaks the (ordered) breaks on which to split \code{variable}, for
#' \code{n} splits \code{n+1} breaks must be specified. If \code{breaks} is
#' specified (other than \code{NA}), \code{qtiles} are ignored.
#'
#' If \code{breaks} is specified, \code{qtiles} is ignored.
#'
#' @return \code{Save complete.} upon completion.  The saved files will appear
#' in the working directory.
#'
#' @examples
#' \dontrun{
#' # example county-level data
#' county_data <- data.frame(county = county_boundary_data$county,
#'                           deaths = round(rnorm(length(county_boundary_data$county), 30, 8)))
#' # example ambulance-level data
#' service_data <- data.frame(service = ambulance_boundary_data$service,
#'                            hoaxes = round(rnorm(length(ambulance_boundary_data$service), 70, 8)))
#' service_data <- service_data[service_data$service != "IOW", ]
#'
#' # merge numeric data and spatial data
#' county_data_spatial <- merge(county_boundary_data, county_data, by = "county")
#' service_data_spatial <- merge(ambulance_boundary_data, service_data, by = "service")
#'
#' # save the nice maps
#' saveMaps("lovely-map", 500, county_data, "deaths", "My lovely map")
#' }
#' @export
saveMaps <- function(fname, fwidth, data, variable, title, qtiles = 5, breaks = NA) {

  if("county" %in% colnames(data@data)) {
    # county level

    sizes <- cartography::getFigDim(data, width = fwidth, mar = c(0, 0, 1.5, 0))
    grDevices::png(filename = paste0(fname, "_counties_england.png"), width = sizes[1], height = sizes[2])
    plotMap(data, variable, title, qtiles, breaks, FALSE)
    grDevices::dev.off()

    sizes <- cartography::getFigDim(data[data$in_london == TRUE, ], width = fwidth, mar = c(0, 0, 1.5, 0))
    grDevices::png(filename = paste0(fname, "_counties_london.png"), width = sizes[1], height = sizes[2])
    plotMap(data, variable, title, qtiles, breaks, TRUE)
    grDevices::dev.off()
  } else {
    # ambulance service level

    sizes <- cartography::getFigDim(data, width = fwidth, mar = c(0, 0, 1.5, 0))
    grDevices::png(filename = paste0(fname, "_services_england.png"), width = sizes[1], height = sizes[2])
    plotMap(data, variable, title, qtiles, breaks, FALSE)
    grDevices::dev.off()
  }

  return("Save complete.")
}
