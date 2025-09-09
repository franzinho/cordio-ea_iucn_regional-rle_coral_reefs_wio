##
##  Name:       create_regional_ecoregions_custom.R
##
##  Objective:  Extract relevant ecoregions and coral reefs for
##              regional analysis
##
##  Approach:   Point to raw ecoregions and global reef
##              spatial layers, filter for relevant ecoregions,
##              extract and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-11
##

##  Notes:      1. Need to fix projection errors with st_erase()
##                   Error in (function (msg)  :
##                   TopologyException: Input geom 0 is invalid:
##                   Self-intersection at 356522.31916756369 7023792.8360193577
##              2. Not using custom coastline (i.e. testing with
##                   common source gshhg coastline)  [ fs: 2025-06-29 ]
##

##
## 1. Set up
##
 ## -- call to wio regions -- ##
  # point to data locale
    data_locale <- "data_raw/spatial/shp/RLE_NCRA_Ecoregions/"

  # point to data file
    data_file <- "RLE_NCRA_Ecoregions.shp"

  # import ecoregions
    regional_ecoregions <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- call to africa continent -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # point to data file
    data_file <- "regional_coastline.rda"

  # load coastline
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # review custom wio ecoregions
    regional_ecoregions
# Simple feature collection with 19 features and 4 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: 30.55155 ymin: -29.69804 xmax: 66.57844 ymax: -0.855319
# CRS:           NA
# # A tibble: 19 × 5
   # ECOREGION        National_s Eco_ID Natnl_ID                  geometry
   # <chr>            <chr>       <int>    <int>            <MULTIPOLYGON>
 # 1 N Tanzania-Kenya Kilifi          1        3 (((38.95951 -2.732664, 4…
 # 2 East Madagascar  East Mada…      7        5 (((51.02612 -15.14415, 5…
 # 3 Mascarene Isl.   Mascarene…     11        6 (((52.80278 -18.88198, 5…
 # 4 Delagoa          Delagoa A       3        7 (((37.55524 -26.90026, 3…
 # 5 Seychelles Outer Seychelle…      9        8 (((52.05285 -6.109476, 5…
 # 6 South Madagascar South Mad…      8        9 (((47.4625 -20.59197, 47…
 # 7 Seychelles north Seychelle…     10       10 (((60.33726 -5.346552, 6…
 # 8 N Mozambique-S … N Mozmbiq…      2       11 (((38.34848 -10.5288, 42…
 # 9 Comoros          Comoros         4       12 (((45.68152 -10.86671, 4…
# 10 West Madagascar  West Mada…      5       13 (((44.60277 -14.07561, 4…
# 11 North Madagascar North Mad…      6       14 (((49.31628 -10.49592, 4…
# 12 N Tanzania-Kenya N Tanzani…      1       15 (((38.39076 -4.741862, 4…
# 13 N Tanzania-Kenya Kwale           1        4 (((43.79546 -4.158893, 4…
# 14 N Tanzania-Kenya Mombasa         1       16 (((38.60587 -3.922394, 4…
# 15 N Tanzania-Kenya Tana River      1        2 (((45.17483 -2.468939, 4…
# 16 N Tanzania-Kenya N Tanzani…      1       17 (((41.99165 -1.650009, 4…
# 17 N Tanzania-Kenya Lamu            1        1 (((41.01943 -1.650009, 4…
# 18 N Mozambique-S … S Tanzania      2       18 (((42.44633 -10.51187, 3…
# 19 Delagoa          Delagoa B       3       19 (((37.55524 -26.90026, 3…

  # set crs
    regional_ecoregions %<>%
      st_set_crs(4326)

  # set ecoregion to title
    regional_ecoregions %<>%
      rename(Ecoregion = ECOREGION)

 ## -- validate object -- ##
  # check validity
    regional_ecoregions %>% st_is_valid()
 # [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
# [12]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

  # fix object
    regional_ecoregions %<>% st_make_valid()

  # dissolve ecoregion boundaries
    regional_ecoregions <-
      regional_ecoregions %>%
        group_by(Ecoregion) %>%
        summarise(geometry = geometry %>% st_union())

 ## -- create test plot to guage overlap -- ##
  # # set number of ecoregions
    # n_ecoregions <-
      # regional_ecoregions %>% pull(Ecoregion) %>% unique() %>% length()

  # # set palette
    # c_palette <-
      # wes_palette("Cavalcanti1", n_ecoregions + 1, "continuous")

  # # open window
    # quartz("test overlay", 7, 7)

  # # create plot
    # ggplot() +
      # geom_sf(aes(colour = Ecoregion,
                  # fill   = Ecoregion),
              # alpha = 0.5,
              # data = regional_ecoregions) +
      # geom_sf(fill  = "grey50",
              # alpha = 0.7,
              # data  = regional_coastline) +
      # theme_void() +
      # scale_colour_manual(values = c_palette) +
      # scale_fill_manual(values = c_palette)

 ## -- extract coastline from ecoregions -- ##
  # set s2 to false
    sf_use_s2(FALSE)

  # define helper function that erases all of y from x
    st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

  # project ecoregions to utm
    regional_ecoregions %<>%
      st_transform(32737)

  # project coastline
    regional_coastline %<>%
      st_transform(32737)

  # mask with intersection
    regional_ecoregions %<>%
      st_erase(regional_coastline)
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries

##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/ecoregions/"

  # save custom wio ecoregions
    save(regional_ecoregions,
      file = paste0(save_locale, "regional_ecoregions.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coastline,
       st_erase)

  # remove core objects
    rm(regional_ecoregions)

