rm(list=ls())
options(stringsAsFactors = FALSE)
options(scipen=999)
library(readxl)
library(tidyverse)

# data source
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2033.0.55.0012016?OpenDocument
# Postal Area, Indexes, SEIFA 2016 


# spreadsheet structure
# name        pos.  description

# contents    1     description of spreadsheet
# table 1     2     4 measures - index value & decile
# table 2     3     socio-economic disadvantage -- detail
# table 3     4     socio-economic advantage & disadvantage -- detail
# table 4     5     economic resources -- detail
# table 5     6     education and occupation - detail
# table 6     7     exluded areas

# short names 

# socio-economic disadvantage                 sed
# socio-economic advantage & disadvantage     sead
# economic resources                          er
# education and occupation                    eo

# type
# index             indx
# decile            dec
# post area code    poa

setwd("/Users/markhatcher/Documents/new_ways_of_working/seifa_postcode")

vct_names <- c("poa", "sed_indx", "sed_dec", "sead_indx", 
  "sead_dec", "er_indx", "er_dec", "eo_indx", "eo_dec", "population")

str_file_path <- "source_data/2033055001_poa_indexes.xls"

# read summary - converted 6 values to NA
df_seifa_summary <- readxl::read_excel(str_file_path, 
      sheet = 2, range = "A7:J2636", col_names = FALSE)

names(df_seifa_summary) <- vct_names

# add in exclusion indicator column
df_seifa_summary$excluded <- FALSE

# =======================================================================
# excluded areas 

# read excluded areas
df_exluded_areas <- readxl::read_excel(str_file_path, 
      sheet = 7, range = "A7:B47", col_names = FALSE)

# 1 column here [41 x 1]
names(df_exluded_areas) <- c("poa", "population")

# create a blank data.frame 
df_ex_area_blank <- data.frame(matrix(ncol = length(vct_names[2:9]), 
      nrow = nrow(df_exluded_areas )))

names(df_ex_area_blank) <- vct_names[2:9]

df_ex_area_all_cols <- cbind(df_exluded_areas, df_ex_area_blank) %>%
  select(poa, sed_indx:eo_dec, population)

df_ex_area_all_cols$excluded <- TRUE

# =======================================================================
# stack the two data.frames

# believe it or not there is an intersection between those
# exluded and those in the summmary table....

vct_over_lap <- df_seifa_summary$poa[df_seifa_summary$poa %in% 
            df_ex_area_all_cols$poa]

df_seifa_summary_filt <- df_seifa_summary %>% filter(!poa %in% vct_over_lap)

df_seifa_data <- bind_rows(df_seifa_summary_filt, df_ex_area_all_cols)

# ASSERT nothing got lost (sum of two data.frames less overlap)
nrow(df_seifa_data) == (2630 + 41 - 1)

# check that postcode is unique
(df_seifa_data$poa %>% unique() %>% length()) == nrow(df_seifa_data)

# print number of postal area codes [2670]
df_seifa_data$poa %>% unique() %>% length()

write.csv(df_seifa_data,'created_data/seifa.csv')





# END === save seifa
# =======================================================================
# /Users/markhatcher/Downloads/1270055003_asgs_2016_vol_3_aust_gpkg
setwd("/Users/markhatcher/Downloads/1270055003_asgs_2016_vol_3_aust_gpkg")

# data source:
#  https://data.gov.au/dataset/
#  ds-dga-32adc1ef-5bac-4eaa-9521-a116792f32a1/distribution/
#  dist-dga-b8d5f808-3294-4b8b-92b2-0e09e4fe1305/details?q=

# for coordinate reference system see the following source file:
# source file: POA_2016_AUST.xml
# xpath expression: /gmd:MD_Metadata/gmd:referenceSystemInfo[1]/
# gmd:MD_ReferenceSystem[1]/gmd:referenceSystemIdentifier[1]/gmd:RS_Identifier[1]/gmd:code[1]
# it was "4283"
library(sp)
library(rgdal)
library(sf)
poa_gpkg_spoly_df <- readOGR("ASGS 2016 Volume 3.gpkg", "POA_2016_AUST")
proj4string(poa_gpkg_spoly_df) <- sp::CRS("+init=epsg:4283")


sp_points_df_cent <- 
  sp::SpatialPointsDataFrame(
    coords = rgeos::gCentroid(poa_gpkg_spoly_df, byid = TRUE),  
    data = poa_gpkg_spoly_df@data, 
    proj4string = poa_gpkg_spoly_df@proj4string)

# woden 2606
# kingston 2604
# tuggers 2901
# mollymook 2539
# noosa heads 4566

vct_sub_set <- c("2606", "2604", "2901", "2539", "4566")
vct_geo_subset <- sp_points_df_cent@data$POA_NAME_2016 %in% vct_sub_set
vct_geo_subset %>% sum()
points_subset <- sp_points_df_cent[vct_geo_subset, ]

library(geosphere)
# 2539, 2604, 2606, 4566
# mollymook, kingston, woden, noosa
# distGeo

# mollymook ==> mollymook  0 km
# mollymook ==> kingston 103 kilometres
# mollymook ==>  woden 109 kilometres
# mollymook ==> noosa 1031

mat_distance <- distm(points_subset, fun = distHaversine)
colnames(mat_distance) <- points_subset@data$POA_NAME_2016
rownames(mat_distance) <- points_subset@data$POA_NAME_2016
mat_distance <- mat_distance / 1000

df_test_df <- otuSummary::matrixConvert(mat_distance, 
  c("from", "to", "distance_km"))
df_test_df


df_distance <- mat_distance %>% as.data.frame()
df_pc <- data.frame(poa_from = points_subset@data$POA_NAME_2016)
df_distance <- cbind(df_pc, df_distance)
df_distance
df_distance_long <- gather(df_distance, "poa_to", "distance_metres", -poa_from)
df_distance_long$distance_km <- df_distance_long$distance_metres / 1000
df_distance_long

mat_distance
library(otuSummary)
data(varespec) 
# 21 x 21 matrix
mat <- vegdist(varespec, method = "bray") %>% as.matrix()
xx <- matrixConvert(mat)

df_test_df <- otuSummary::matrixConvert(mat_distance)



# =================================================================

rm(list = ls())
library(sp)
library(rgdal)
library(sf)
poa_gpkg_spoly_df <- readOGR("ASGS 2016 Volume 3.gpkg", "POA_2016_AUST")

# set projection based on metadata
proj4string(poa_gpkg_spoly_df) <- sp::CRS("+init=epsg:4283")

# reproject to wgs84
poa_gpkg_spoly_df_wgs_84 <- sp::spTransform(poa_gpkg_spoly_df,
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

# get the centroids
sp_points_df_cent_wgs_84 <- 
  sp::SpatialPointsDataFrame(
    coords = rgeos::gCentroid(poa_gpkg_spoly_df_wgs_84 , byid = TRUE),  
    data = poa_gpkg_spoly_df_wgs_84@data, 
    proj4string = poa_gpkg_spoly_df_wgs_84@proj4string)

# 2668
sp_points_df_cent_wgs_84 %>% nrow()
# expected number of rows
(2668 * (2668 -1)) / 2

# 2668 x 2668
mat_distance <- geosphere::distm(sp_points_df_cent_wgs_84, fun = distHaversine)
# convert to km
mat_distance_km <- mat_distance / 1000

colnames(mat_distance_km) <- sp_points_df_cent_wgs_84@data$POA_CODE_2016
rownames(mat_distance_km) <- sp_points_df_cent_wgs_84@data$POA_CODE_2016
vct_col_names <- c("from", "to", "distance_km")

df_distance_km <- otuSummary::matrixConvert(mat_distance_km, vct_col_names)
df_distance_km %>% nrow()

# zipped up this is 50 mb
write.csv(df_distance_km,'centroid_distance_km.csv')














library(openxlsx)

wb <- createWorkbook("wb")
addWorksheet(wb, sheetName = "data")
writeData(wb, sheet = 1, df_distance_long)
## Save workbook to working directory
## Not run: 
saveWorkbook(wb, file = "centroid_distances.xlsx", overwrite = TRUE)





vct_test <- c("A", "B", "C")
df_test <- expand.grid(vct_test, vct_test)
names(df_test) <- c("from", "to")

df_test


test <- upper.tri(mat_distance)















