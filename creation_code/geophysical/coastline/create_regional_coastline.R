##
##  Name:       create_regional_coastline.R
##
##  Objective:  Create coastline for region of assessment
##              for Red List of Ecosystems evaluation
##
##  Approach:   Identify ecoregions of interest for
##              region and use univ. of hawaii coastline
##              data from global self-consistent,
##              hierarchical, high-resolution geography (gshhg)
##              data base to extract.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-11
##

##  Notes:      1. Coastline available through `rnaturalearth`
##                 lacks resolution for ETP. Currently
##                 downloading gshhg version 2.3.7 data
##                 direct from http.  R packages with
##                 gshhg download functions rely on
##                 `maptools` which is not available for
##                 R version 4.5.0 [ fs: 2025-05-01 ]
##              2. Should evaluate other sources and R
##                 package functions if available [ fs: 2025-05-01 ]
##

##
## 1. Set up
##
 ## -- call to global ecoregions -- ##
  # point to data locale
    data_locale <- "data_raw/spatial/shp/marine_ecoregions/"

  # point to data file
    data_file <- "meow_ecos.shp"

  # import ecoregions
    meow_ecos <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- call to ecoregion list -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # call to ecoregions
    load(paste0(data_locale, "ecoregion_list.rda"))


 # ## -- call to custom ecoregions -- ##
 #  # point to data locale
 #    data_locale <- "data_intermediate/spatial/ecoregions/"
 #
 #  # load data
 #    load(paste0(data_locale, "regional_ecoregions.rda"))


 ## -- call to gshhg coastline -- ##
  # point to https locale
    https_locale <-
      paste0("http://www.soest.hawaii.edu/pwessel/gshhg/",
             "gshhg-shp-2.3.7.zip")

  # create temporary file for downloading
    temp_file <- tempfile()

  # set temporary file directory
    # temp_directory <- tempfile()
    temp_directory <- "data_raw/geophysical/coastline/"

  # set timeout
    options(timeout = max(300, getOption("timeout")))

  # download coastline
    https_locale %>% download.file(temp_file)

  # extract files
    temp_file %>%
      unzip(exdir = temp_directory)

 ## -- import coastline polygons -- ##
  # point to data locale
    data_locale <-
      paste0(temp_directory, "/GSHHS_shp/f/")

  # point to data file
    data_file <- "GSHHS_f_L1.shp"

  # import coastline
    coastline_gshhs <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- remove coastline downloads -- ##
  # remove gshhs
    paste0(temp_directory, "GSHHS_shp") %>%
      fs::dir_delete()

  # remove wdbii
    paste0(temp_directory, "WDBII_shp") %>%
      fs::dir_delete()

  # set additional files to remove
    additional_files <-
      c("COPYING.LESSERv3",
        "LICENSE.TXT",
        "SHAPEFILES.TXT")

  # remove additional files
    paste0(temp_directory, additional_files) %>%
      fs::file_delete()


##
## 2. Groom data
##
  # have a look ----
    ecoregion_list
 # [1] "Central Somali Coast"
 # [2] "Northern Monsoon Current Coast"
 # [3] "East African Coral Coast"
 # [4] "Seychelles"
 # [5] "Cargados Carajos/Tromelin Island"
 # [6] "Mascarene Islands"
 # [7] "Southeast Madagascar"
 # [8] "Western and Northern Madagascar"
 # [9] "Bight of Sofala/Swamp Coast"
# [10] "Delagoa"

  # extract relevant ecoregions
    regional_ecoregions <-
      meow_ecos %>%
        dplyr::filter(ECOREGION %in% ecoregion_list)

  # # set regional ecoregions to wgs84
  #   regional_ecoregions %<>%
  #     st_transform(4326)

  # # set coastline to utm 37s
  #   coastline_gshhs %<>%
  #     st_transform(32737)

 ## -- clip to regional ecoregions -- ##
  # set s2 to false
    sf_use_s2(FALSE)

  # set ecoregional boundary
    regional_coastline <-
      coastline_gshhs %>%
        st_make_valid() %>%
        st_crop(regional_ecoregions) %>%
        st_geometry() %>%
        st_union()
# although coordinates are longitude/latitude, st_intersection
# assumes that they are planar
# although coordinates are longitude/latitude, st_union assumes
# that they are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries

 ## -- quick visual to validate -- ## ----
  # # open window
  #   quartz("coastline", 6, 6)
  #
  # # visualise
  #   regional_coastline %>% plot()

  # # transform back to wgs84
  #   regional_coastline %<>%
  #     st_transform(4326)

##
## 3. Generate outputs
##
 ## -- save coastline -- ##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coastline/"

  # save regional coastline to file
    save(regional_coastline,
      file = paste0(save_locale, "regional_coastline.rda"))


 # ## -- export for ecoregional planning -- ##
  # # point to save locale
    # save_locale <- "data/geophysical/regional_coastline/"

  # # save to shape
    # regional_coastline %>%
      # st_write(paste0(save_locale, "regional_coastline.shp"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(meow_ecos,
       ecoregion_list,
       coastline_gshhs,
       regional_ecoregions)

  # remove core objects
    rm(regional_coastline)

