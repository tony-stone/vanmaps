library(data.table)
library(readxl)
library(rgdal)
library(maptools) # requires rgeos

# Create LSOA to County lookup --------------------------------------------

# LSOA2011 to District lookup from http://geoconvert.mimas.ac.uk [retrieved 27/09/2016]
# LTLA/UTLA data from ONS Code History Database (codes as at 01/11/2014)

## Attach County
# LSOA11 to LAD11
lsoa_to_lad_lu <- fread("data-raw/lookups/88022338_lut.csv", sep = ",", header = FALSE, skip = 1, colClasses = "character")
lsoa_to_lad_lu[, V2 := NULL]
lsoa_to_lad_lu <- lsoa_to_lad_lu[substr(V1, 1, 3) == "E01"]
setnames(lsoa_to_lad_lu, c("LSOA11", "LAD11"))

# Add the LAD14 codes
lsoa_to_lad_lu[, LAD14 := LAD11] # mainly the same
# Changes:
# The East Hertfordshire and Stevenage (Boundary Change) Order 2013
lsoa_to_lad_lu[LAD11 == "E06000048", LAD14 := "E06000057"]
lsoa_to_lad_lu[LAD11 == "E07000097", LAD14 := "E07000242"]
lsoa_to_lad_lu[LAD11 == "E07000101", LAD14 := "E07000243"]
# The St Albans and Welwyn Hatfield (Boundary Change) Order 2012
lsoa_to_lad_lu[LAD11 == "E07000100", LAD14 := "E07000240"]
lsoa_to_lad_lu[LAD11 == "E07000104", LAD14 := "E07000241"]
# Gateshead boundary changed, recoded
lsoa_to_lad_lu[LAD11 == "E08000020", LAD14 := "E08000037"]

## Most LADs are UTLAs
# LAD14 - LB
lad_lb <- data.table(read_excel("data-raw/lookups/CTRY_GOR_LB_EN.xlsx", col_names = FALSE, skip = 1L, col_types = rep("text", 6)))
lad_lb[, paste0("X", 2:5) := NULL]

# LAD14 - MD
lad_md <- data.table(read_excel("data-raw/lookups/CTRY_GOR_MD_EN.xlsx", col_names = FALSE, skip = 1L, col_types = rep("text", 6)))
lad_md[, paste0("X", 2:5) := NULL]

# LAD14 - UA
lad_ua <- data.table(read_excel("data-raw/lookups/CTRY_GOR_UA_EN.xlsx", col_names = FALSE, skip = 1L, col_types = rep("text", 6)))
lad_ua[, paste0("X", 2:5) := NULL]

#LAD14 - Special (pseudo-LADs)
lad_special <- data.table(X0 = c("E41000052", "E41000324"),
                          X1 = c("Cornwall and The Isles of Scilly", "Westminster and The City of London"))

# LAD14 - NMD/County (not UTLAs)
lad_nmd <- data.table(read_excel("data-raw/lookups/CTRY_GOR_CTY_NMD_EN.xlsx", col_names = FALSE, skip = 1L, col_types = rep("text", 8)))
lad_nmd[, paste0("X", c(1, 4:7)) := NULL]
setnames(lad_nmd, c("X0", "X2", "X3"), c("LAD14", "CNTY14", "county"))

## Combine LAD data
# Merge UTLAs
lad <- rbind(lad_md, lad_lb, lad_ua, lad_special)
setnames(lad, c("LAD14", "county"))
lad[, CNTY14 := LAD14]

# Add LTLAs
lad <- rbind(lad, lad_nmd)

county.data <- merge(lsoa_to_lad_lu, lad, by = "LAD14", all.x = TRUE)
county.data[, c("LAD11", "LAD14") := NULL]

# In London?
county.data[, in_london := (CNTY14 %in% c(lad_lb$X0, "E41000324"))]



# Prepare LSOA to Ambulance Service lookup (based on CCG) -----------------

lsoa.to.ccg <-  fread("data-raw/lookups/LSOA11_CCG16_LAD16_EN_LU.csv",
                   header = TRUE, sep = ",", colClasses = "character", na.strings = c("NA", ""), drop = c("LSOA11NM", "CCG16CD", "LAD16CD","LAD16NM"))
setnames(lsoa.to.ccg, c("LSOA11", "CCG.code", "CCG.name"))

ccg.to.amb <- data.table(read_excel("data-raw/lookups/AmbSYS-TimeSeries-Interactive-Web-File-September-2016.xlsx", sheet = "CCG to Ambulance Trust", col_names = FALSE, skip = 3L, col_types = rep("text", 4)))
setnames(ccg.to.amb, c("CCG.code.ons", "CCG.code", "CCG.name", "service"))

# Rename NHS England names to our names
ccg.to.amb[service == "EASTAmb", service := "EoE"]

lsoa.to.amb <- merge(lsoa.to.ccg[, .(LSOA11, CCG.code)], ccg.to.amb[, .(CCG.code, service)], by = "CCG.code", all = TRUE)
lsoa.to.amb[, CCG.code := NULL]


# Prepare spatial data ----------------------------------------------------

# Projection strings
WGS84_projection_str <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# ONS LSOA 2011 boundary data ---------------------------------------------
# download from http://geoportal.statistics.gov.uk/datasets/da831f80764346889837c72508f046fa_2.zip [retrieved 16/11/2016]
lsoa_boundary_data_raw <- readOGR("data-raw/spatial data/lsoa 2011 boundaries", "Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales")

# Keep only English LSOAs
lsoa_boundary_data_raw <- lsoa_boundary_data_raw[substr(lsoa_boundary_data_raw$lsoa11cd, 1, 1) == "E",]

# Transform to WGS84 coordinate system
lsoa_boundary_data_WGS84 <- spTransform(lsoa_boundary_data_raw, WGS84_projection_str)


# Merge with counties data ------------------------------------------------

lsoa_boundary_data <- merge(lsoa_boundary_data_WGS84, county.data, by.x = "lsoa11cd", by.y = "LSOA11")

# Union of LSOAs into Counties, returns SP (not SPDF)
county_boundaries <- unionSpatialPolygons(lsoa_boundary_data, lsoa_boundary_data$CNTY14)

# Prepare related data
counties_df <- data.frame(unique(county.data[, .(CNTY14, county, in_london)]))
row.names(counties_df) <- counties_df$CNTY14

# Promote back to SPDF
county_boundary_data <- SpatialPolygonsDataFrame(county_boundaries, counties_df, match.ID = TRUE)

# save
devtools::use_data(county_boundary_data, compress = "xz")

# Merge with ambulance service data ---------------------------------------

lsoa_boundary_data <- merge(lsoa_boundary_data_WGS84, lsoa.to.amb, by.x = "lsoa11cd", by.y = "LSOA11")

# Union of LSOAs into Counties, returns SP (not SPDF)
ambulance_boundaries <- unionSpatialPolygons(lsoa_boundary_data, lsoa_boundary_data$service)

# Prepare related data
ambulance_df <- data.frame(unique(lsoa.to.amb[, .(service)]))
row.names(ambulance_df) <- ambulance_df$service

# Promote back to SPDF
ambulance_boundary_data <- SpatialPolygonsDataFrame(ambulance_boundaries, ambulance_df, match.ID = TRUE)

# save
devtools::use_data(ambulance_boundary_data, compress = "xz")
