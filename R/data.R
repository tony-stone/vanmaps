#' English ambulance services' boundaries data.
#'
#' A dataset containing the boundaries of English ambulance services derived
#' from:
#' \itemize{
#'   \item The ONS / OS's 2011 LSOA boundaries
#' (\url{http://geoportal.statistics.gov.uk/datasets/da831f80764346889837c72508f046fa_2});
#'   \item the ONS's 2011 LSOA to 2016 CCG lookup
#' (\url{http://ons.maps.arcgis.com/home/item.html?id=19e5c35c6a504a7b9e1b74bed1b6225f}); and
#'   \item NHS England's 2016 CCG to ambulance service lookup
#' (\url{https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/AmbSYS-TimeSeries-Interactive-Web-File-September-2016.xlsx}).
#' }
#'
#' Contains National Statistics data: Crown copyright and database right 2016;
#' Contains OS data: Crown copyright and database right 2016.
#'
#' @format A spatial polygons data frame with 11 rows and 1 data variables:
#' \describe{
#'   \item{service}{ambulance service, short code (e.g. EMAS)}
#' }
"ambulance_boundary_data"

#' English counties' (UTLA) boundaries data.
#'
#' A dataset containing the boundaries of English counties (Upper Tier Local
#' Authorities) derived from:
#' \itemize{
#'   \item The ONS / OS's 2011 LSOA boundaries
#' (\url{http://geoportal.statistics.gov.uk/datasets/da831f80764346889837c72508f046fa_2});
#'   \item 2011 LSOA to 2011 Local Authority District lookup from
#' \url{http://geoconvert.mimas.ac.uk} [retrieved 27/09/2016];
#'   \item the ONS's 2014 Non-Metropolitan District to 2014 County lookup;
#'   \item the ONS's 2014 Metropolitan District names;
#'   \item the ONS's 2014 London Borough lookup names; and
#'   \item the ONS's 2014 Unitary Authority names.
#'  }
#'  Items above with unspecified source from ONS's Code History Database
#' \url{http://ons.maps.arcgis.com/home/item.html?id=cb98d1fdaa3c462a94e61de1a7eeaf8b}.
#'
#' Minor alterations to 2011 LADs relating to:
#' \itemize{
#'   \item The St Albans and Welwyn Hatfield (Boundary Change) Order 2012;
#'   \item The East Hertfordshire and Stevenage (Boundary Change) Order 2013;
#'   \item The Gateshead and Northumberland (Boundary Change) Order 2013.
#'  }
#'
#' Within this data:
#' \itemize{
#'   \item the London Boroughss of City of London and Westminster have been merged; and
#'   \item the Unitary Authorities of Cornwall and The Isles of Scilly have been merged.
#'  }
#'
#' Contains National Statistics data: Crown copyright and database right 2016;
#' Contains OS data: Crown copyright and database right 2016.
#'
#' @format A spatial polygons data frame with 150 rows and 3 data variables:
#' \describe{
#'   \item{CNTY14}{ONS County/UTLA code}
#'   \item{county}{County/UTLA name}
#'   \item{in_london}{Boolean, is the county/UTLA a London Borough?}
#' }
"county_boundary_data"
