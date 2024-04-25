
library(tidyverse)
library(sf)

# Utility Service Territories
  # found here: https://atlas.eia.gov/datasets/f4cd55044b924fed9bc8b64022966097/explore?filters=eyJOQU1FIjpbIkRURSBFTEVDVFJJQyBDT01QQU5ZIl19&location=42.911225%2C-81.880048%2C6.78
service_territories <- st_read("../Data/Electric_Retail_Service_Territories/Electric_Retail_Service_Territories.shp")

# Isolating DTE service territory
dte_service_territory <- service_territories %>%
  filter(NAME == "DTE ELECTRIC COMPANY")

# Reading in MI census tracts as defined by the 2010 ACS
mi_tracts <- st_read("../Data/US Census/tl_2010_26_tract10/tl_2010_26_tract10.shp")

# Make sure MI census tract maps and DTE service territory have the same coordinate reference system
mi_tracts <- mi_tracts %>%
  st_transform(4326) %>%
  distinct()

dte_service_territory <- dte_service_territory %>%
  st_transform(4326) %>%
  # Correct for overlapping and duplicated vertices
  st_make_valid()

# Find all MI census tracts that are part of the DTE service territory
dte_tracts <- st_intersection(dte_service_territory, mi_tracts)

# Writing out MI census tracts as geojson
st_write(mi_tracts, dsn = "outputs/mi_tracts.geojson", layer = "mi_tracts.geojson",
         append = FALSE, delete_dsn = TRUE, delete_layer = TRUE)

# Writing out DTE service territory as geojson
st_write(dte_service_territory, dsn = "outputs/dte_service_territory.geojson", layer = "dte_service_territory.geojson",
         append = FALSE, delete_dsn = TRUE, delete_layer = TRUE)

# Write out all MI census tracts that intersect with DTE service territory
st_write(dte_tracts, dsn = "outputs/dte_tracts.geojson", layer = "dte_tracts.geojson",
         append = FALSE, delete_dsn = TRUE, delete_layer = TRUE)

# All MI census tracts in the DTE service territory
# unique(dte_tracts$GEOID10)




