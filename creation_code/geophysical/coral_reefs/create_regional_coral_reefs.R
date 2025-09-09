##
##  Name:       create_ecoregion_coral_reefs.R
##
##  Objective:  Extract coral reefs from ecoregions in
##              region
##
##  Approach:   Point to regional ecoregions and global reef
##              spatial layers, filter for relevant ecoregions,
##              extract and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-02-26
##

##
## 1. Set up
##
 ## -- call to ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to ecoregions
    load(paste0(data_locale, data_file))


 ## -- call to reef layer -- ##
  # point to https locale
    https_locale <-
      paste0("https://datadownload-production.s3.us-east-1.amazonaws.com/",
             "WCMC008_CoralReefs2021_v4_1.zip")

  # create temporary file for downloading
    temp_file <- tempfile()

  # set temporary file directory
    # temp_directory <- tempfile()
    temp_directory <- "data_raw/geophysical/coral_reefs/"

  # download coral reefs
    https_locale %>% download.file(temp_file)

  # extract files
    temp_file %>%
        unzip(exdir = temp_directory)

  # point to data locale
    data_locale <-
      paste0(temp_directory,
             "14_001_WCMC008_CoralReefs2021_v4_1/01_Data/") %>%
      str_replace("\\//", "\\/")

 ## -- import coral reef polygons -- ##
  # point to data file
    data_file <- "WCMC008_CoralReef2021_Py_v4_1.shp"

  # import coral reef polygons
    coral_reefs_polygons <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- remove reef downloads -- ##
  # remove temp directory
    paste0(temp_directory, "14_001_WCMC008_CoralReefs2021_v4_1") %>%
      fs::dir_delete()

  # # remove temp directory
  #   paste0(temp_directory, "14_001_WCMC008_CoralReefs2021_v4_1") %>%
  #     fs::dir_delete()

##
## 2. Groom data
##
 ## -- extract coral reefs -- ##
  # set s2 to false
    sf_use_s2(FALSE)

  # extract
    regional_coral_reefs <-
      coral_reefs_polygons %>%
        st_intersection(regional_ecoregions %>% st_transform(4326))
# Simple feature collection with 1411 features and 19 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 32.86711 ymin: -27.01737 xmax: 63.51225 ymax: -1.593477
# Geodetic CRS:  WGS 84
# # A tibble: 1,411 × 20
#    LAYER_NAME METADATA_I ORIG_NAME    FAMILY       GENUS SPECIES
#  * <chr>           <dbl> <chr>        <chr>        <chr> <chr>
#  1 CRR                 1 Not Reported Not Reported Not … Not Re…
#  2 CRR                 1 Not Reported Not Reported Not … Not Re…
#  3 CRR                11 Not Reported Not Reported Not … Not Re…
#  4 CRR                11 Not Reported Not Reported Not … Not Re…
#  5 CRR                11 Not Reported Not Reported Not … Not Re…
#  6 CRR                80 Not Reported Not Reported Not … Not Re…
#  7 CRR                80 Not Reported Not Reported Not … Not Re…
#  8 CRR                80 Not Reported Not Reported Not … Not Re…
#  9 CRR                80 Not Reported Not Reported Not … Not Re…
# 10 CRR                80 Not Reported Not Reported Not … Not Re…
# # ℹ 1,401 more rows
# # ℹ 14 more variables: DATA_TYPE <chr>, START_DATE <chr>,
# #   END_DATE <chr>, DATE_TYPE <chr>, VERIF <chr>, NAME <chr>,
# #   LOC_DEF <chr>, SURVEY_MET <chr>, GIS_AREA_K <dbl>,
# #   Shape_Leng <dbl>, Shape_Area <dbl>, REP_AREA_K <chr>,
# #   Ecoregion <chr>, geometry <GEOMETRY [°]>
# # ℹ Use `print(n = ...)` to see more rows

   # get column names
     regional_coral_reefs %>% names()
#  [1] "LAYER_NAME" "METADATA_I" "ORIG_NAME"  "FAMILY"
#  [5] "GENUS"      "SPECIES"    "DATA_TYPE"  "START_DATE"
#  [9] "END_DATE"   "DATE_TYPE"  "VERIF"      "NAME"
# [13] "LOC_DEF"    "SURVEY_MET" "GIS_AREA_K" "Shape_Leng"
# [17] "Shape_Area" "REP_AREA_K" "Ecoregion"  "geometry"

  # simplify object
    regional_coral_reefs %<>%
      dplyr::select(Ecoregion,
                    # Eco_code,
                    # Province,
                    # Prov_code,
                    Shape_Leng,
                    Shape_Area,
                    geometry)

 # ## -- visualise -- ##
 #  # set palette
 #    c_palette <-
 #      wesanderson::wes_palette("Cavalcanti1", 15, "continuous")
 #
 #  # plot
 #    regional_coral_reefs %>%
 #      ggplot() +
 #      geom_sf(aes(colour = Ecoregion)) +
 #      theme_void() +
 #      scale_colour_manual(values = c_palette)


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coral_reefs/"

  # save ecoregions to file
    save(regional_coral_reefs,
      file = paste0(save_locale, "regional_coral_reefs.rda"))


# ## -- export to shapefile -- ##
#  # point to save locale
#    save_locale <- "data/geophysical/coral_reefs/wcmc_coral_reefs_2021_v4.1/"
#
#  # export geometry only
#    coral_reefs %>%
#      st_geometry() %>%
#      st_write(paste0(save_locale, "wcmc_coral_reefs_2021_v4.1.shp"))
# Writing layer `wcmc_coral_reefs_2021_v4.1' to data source
#   `data/geophysical/coral_reefs/wcmc_coral_reefs_2021_v4.1/wcmc_coral_reefs_2021_v4.1.shp'
#    using driver `ESRI Shapefile'
# Writing 17504 features with 0 fields and geometry type Multi Polygon.

##
## 4. Clean up workspace
##
  # clean up paths
    rm(https_locale,
       data_locale,
       data_file,
       save_locale)

  # remove original objects
    rm(coral_reefs_polygons)

  # remove intermediate objects
    rm(regional_ecoregions)

  # remove core objects
    rm(regional_coral_reefs)
