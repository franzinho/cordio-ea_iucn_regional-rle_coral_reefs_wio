##
##  Name:       create_reef_area_ecoregions.R
##
##  Objective:  Extract coral reefs from ecoregions for
##              regional analysis
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
 ## -- call to custom  ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # point to data file
    data_file <- "regional_coral_reefs.rda"

  # call to  ecoregions
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- get reef areas for ecoregions -- ##
  # get areas
    reef_area_ecoregions <-
      regional_coral_reefs %>%
        # rename(Ecoregion = ECOREGION) %>%
        st_transform(32737) %>%
        group_by(Ecoregion) %>%
        reframe(geometry = geometry %>% st_union()) %>%
      st_as_sf() %>%
        group_by(Ecoregion) %>%
        reframe(reef_area = geometry %>% st_area())
# # A tibble: 11 × 2
#    Ecoregion                 reef_area
#    <chr>                         [m^2]
#  1 Comoros                  611710576.
#  2 Delagoa                   30892764.
#  3 East Madagascar          334152068.
#  4 Mascarene Isl.           429833859.
#  5 N Mozambique-S Tanzania 2261521472.
#  6 N Tanzania-Kenya        1219303026.
#  7 North Madagascar         828190928.
#  8 Seychelles Outer         933464428.
#  9 Seychelles north         912089633.
# 10 South Madagascar          11821888.
# 11 West Madagascar         1107894614.

  # compare to original values
    reef_area_ecoregions %<>%
      mutate(reef_area = (reef_area %>% as.numeric()) / 1e6)
# # A tibble: 11 × 2
#    Ecoregion               reef_area
#    <chr>                       <dbl>
#  1 Comoros                     612.
#  2 Delagoa                      30.9
#  3 East Madagascar             334.
#  4 Mascarene Isl.              430.
#  5 N Mozambique-S Tanzania    2262.
#  6 N Tanzania-Kenya           1219.
#  7 North Madagascar            828.
#  8 Seychelles Outer            933.
#  9 Seychelles north            912.
# 10 South Madagascar             11.8
# 11 West Madagascar            1108.

 ## -- set proportional area -- ##
  # calculate proportion
    reef_area_ecoregions %<>%
      mutate(prop_area = (reef_area / sum(reef_area)) %>% round(4))
# # A tibble: 11 × 3
#    Ecoregion               reef_area prop_area
#    <chr>                       <dbl>     <dbl>
#  1 Comoros                     612.     0.0705
#  2 Delagoa                      30.9    0.0036
#  3 East Madagascar             334.     0.0385
#  4 Mascarene Isl.              430.     0.0495
#  5 N Mozambique-S Tanzania    2262.     0.260
#  6 N Tanzania-Kenya           1219.     0.140
#  7 North Madagascar            828.     0.0954
#  8 Seychelles Outer            933.     0.108
#  9 Seychelles north            912.     0.105
# 10 South Madagascar             11.8    0.0014
# 11 West Madagascar            1108.     0.128


  # get ecoregion list
    reef_area_ecoregions %>% pull(Ecoregion) %>% unique()
#  [1] "Comoros"                 "Delagoa"                
#  [3] "East Madagascar"         "Mascarene Isl."
#  [5] "N Mozambique-S Tanzania" "N Tanzania-Kenya"
#  [7] "North Madagascar"        "Seychelles Outer"
#  [9] "Seychelles north"        "South Madagascar"
# [11] "West Madagascar"


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coral_reefs/"

  # save reef area to file
    save(reef_area_ecoregions,
      file = paste0(save_locale, "reef_area_ecoregions.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coral_reefs)

  # remove core objects
    rm(reef_area_ecoregions)

