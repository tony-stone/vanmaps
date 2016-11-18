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
#' should only the London counties be displayed? Default is \code{FALSE}.
#' @param greyscale boolean; should the plot be in greyscale only?
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
plotMap <- function(data, variable, title, qtiles = 5, breaks = NA, london_only = FALSE, greyscale = FALSE) {

  # n-tile breaks
  if(any(is.na(breaks))) {
    n <- qtiles
    qbreaks <- stats::quantile(data@data[, variable], probs = 0:n/n, na.rm = TRUE, names = FALSE, type = 8)
  } else {
    qbreaks <- breaks
  }

  n_breaks <- length(qbreaks) - 1

  try(if (n_breaks < 2) stop("Too few breaks/quantiles: Min of 2."))
  try(if (greyscale == TRUE & n_breaks > 9) stop("Too many breaks/quantiles: Max of 9 with a greyscale palette. (Max of 11 with a colour palette.)"))
  try(if (greyscale == FALSE & n_breaks > 11) stop("Too many breaks/quantiles: Max of 11 with a colour palette."))

  if (n_breaks == 2) {
    colours <- RColorBrewer::brewer.pal(3, "Greys")[2:3]
    bgcolour <- "#ffffff"
  } else {
  if (greyscale == TRUE) {
    colours <- RColorBrewer::brewer.pal(n_breaks, "Greys")
    bgcolour <- "#ffffff"
  } else {
    colours <- RColorBrewer::brewer.pal(n_breaks, "PRGn")
    bgcolour <- "#A6CAE0"
  }
}
  graphics::par(mar = c(0, 0, 1.5, 0))
  if("county" %in% colnames(data@data)) {
    # county level

    if(london_only == FALSE) {
      # England

      plot(data, border = NA, col = NA, bg = bgcolour)
      # Add cloro
      cartography::choroLayer(spdf = data, df = data@data, var = variable,
                 breaks = qbreaks, col = colours,
                 border = "#666666", lwd = 0.25, legend.pos = "topright",
                 legend.title.txt = variable,
                 legend.values.rnd = 2,
                 colNA = "#aaaaaa", add = TRUE)
    } else {
      # London
      plot(data[data$in_london == TRUE, ], border = NA, col = NA, bg = bgcolour)
      plot(data, border = "#000000", lwd = 0.25, col = bgcolour, bg = NA, add = TRUE)
      # Add cloro
      cartography::choroLayer(spdf = data[data$in_london == TRUE, ], df = data[data$in_london == TRUE, ]@data, var = variable,
                 breaks = qbreaks, col = colours,
                 border = "#000000", lwd = 0.25, legend.pos = "topright",
                 legend.title.txt = variable,
                 legend.values.rnd = 2,
                 colNA = bgcolour, add = TRUE)

    }
    plot(vanmaps::ambulance_boundary_data, border = "#ffffff", lwd = 1, col = NA, bg = NA, add = TRUE)

  } else {
    # ambulance service level

    plot(data, border = NA, col = NA, bg = bgcolour)
    # Add cloro
    cartography::choroLayer(spdf = data, df = data@data, var = variable,
               breaks = qbreaks, col = colours,
               border = "#ffffff", lwd = 1, legend.pos = "topright",
               legend.title.txt = variable,
               legend.values.rnd = 2,
               colNA = bgcolour, add = TRUE)

  }
  cartography::layoutLayer(title = title, # title of the map
              author = "",  # no author text
              sources = "",
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
#' @param greyscale boolean; should the plot be in greyscale only? Default is
#' \code{FALSE}.
#'
#' If \code{breaks} is specified, \code{qtiles} is ignored. There is a
#' maximum of:
#' \itemize{
#'  \item 9 levels allowed for greyscale plots;
#'  \item 11 levels for colour plots.
#' }
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
saveMaps <- function(fname, fwidth, data, variable, title, qtiles = 5, breaks = NA, greyscale = FALSE) {

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
