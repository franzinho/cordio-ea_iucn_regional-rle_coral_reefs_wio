##
##  Name:       create_criteria_b1_extent_of_occurrence.R
##
##  Objective:  Standardise & format data for analysing criterion B1:
##                Extent of occurrence (EOO)
##
##  Approach:   Call to coral reefs and ecoregions for wio,
##              calculate extent of occurrence and
##              classify to threat categories:
##              - Critically Endangered (CR) if number of EOO units <= 2
##              - Endangered (EN) if number of EOO units == 20
##              - Vulnerable (VU) if number of EOO units <= 50
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & James Mbugua
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. For this analysis, Criterion B1 will be assessed
##                 based on the *extent of occurrence* of coral reef
##                 ecosystem defined in the evaluation region
##                 (i.e. Ecoregions (or other Areas of Assessment)).
##              2. This is a two step process that entails generating
##                 the smallest convex polygon encompassing an ecosystem
##                 (coral reef) and calculation of area (Km sq) of
##                 the developed EOO geometry.

##
## 1. Set up
##
  # call to additional functionality
    # library(redlistr)

 ## -- import coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # set data file name
    data_file <- "regional_coral_reefs.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    regional_coral_reefs
# Simple feature collection with 1411 features and 3 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 32.86711 ymin: -27.01737 xmax: 63.51225 ymax: -1.593477
# Geodetic CRS:  WGS 84
# # A tibble: 1,411 × 4
   # Ecoregion Shape_Leng    Shape_Area                   geometry
   # <chr>          <dbl>         <dbl>             <GEOMETRY [°]>
 # 1 Comoros       0.153  0.000420      POLYGON ((46.47851 -11.97…
 # 2 Comoros       0.165  0.000453      POLYGON ((46.22181 -12.21…
 # 3 Comoros       0.0856 0.000118      POLYGON ((47.31098 -11.53…
 # 4 Comoros       1.06   0.00187       MULTIPOLYGON (((47.35986 …
 # 5 Comoros       0.665  0.00246       MULTIPOLYGON (((47.33667 …
 # 6 Comoros       0.0855 0.000118      POLYGON ((47.31125 -11.53…
 # 7 Comoros       0.0283 0.00000000707 MULTIPOLYGON (((47.35576 …
 # 8 Comoros       0.593  0.000000131   MULTIPOLYGON (((47.30239 …
 # 9 Comoros       0.190  0.0000000554  MULTIPOLYGON (((47.30719 …
# 10 Comoros       0.0132 0.00000652    POLYGON ((44.97759 -12.51…
# # ℹ 1,401 more rows
# # ℹ Use `print(n = ...)` to see more rows

##
## 3. Calculate extent of occurrence & threat categories
##
  # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                 "Delagoa"
 # [3] "East Madagascar"         "Mascarene Isl."
 # [5] "N Mozambique-S Tanzania" "N Tanzania-Kenya"
 # [7] "North Madagascar"        "Seychelles Outer"
 # [9] "Seychelles north"        "South Madagascar"
# [11] "West Madagascar"

  # create empty object to hold results
    criterion_b1_extent_of_occurrence <- tibble()

  # loop ecoregions
    for(i in 1:length(ecoregion_list)){

      # create eeo object
        eco_eoo <-
          regional_coral_reefs %>%
          filter(Ecoregion %in% ecoregion_list[i]) %>%
          st_transform(32737) %>%
          as_Spatial() %>%
          redlistr::makeEOO()

      # calculate area
        eco_eoo_area <-
           eco_eoo %>%
             redlistr::getAreaEOO()

      # set name
        dat <-
          tribble(       ~Ecoregion,  ~`EOO Area`,
                  ecoregion_list[i], eco_eoo_area)

      # harvest results
        criterion_b1_extent_of_occurrence %<>%
          bind_rows(dat)


    }

 ## -- set threat categories -- ##
  # classify
    criterion_b1_extent_of_occurrence %<>%
      mutate(Status = ifelse(`EOO Area` <= 2e3,                  "CR",     NA),
             Status = ifelse(`EOO Area` %>% between(2e3, 20e3),  "EN", Status),
             Status = ifelse(`EOO Area` %>% between(20e3, 50e3), "VU", Status),
             Status = ifelse(`EOO Area` %>% between(50e3, 55e3), "NT", Status), # Added 10% from VU as threshold for NT
             Status = ifelse(`EOO Area` > 55e3,                  "LC", Status))
# # A tibble: 11 × 3
   # Ecoregion               `EOO Area` Status
   # <chr>                        <dbl> <chr>
 # 1 Comoros                     56356. LC
 # 2 Delagoa                     63337. LC
 # 3 East Madagascar             32762. VU
 # 4 Mascarene Isl.              66238. LC
 # 5 N Mozambique-S Tanzania    186619. LC
 # 6 N Tanzania-Kenya            77683. LC
 # 7 North Madagascar            63096. LC
 # 8 Seychelles Outer           638837. LC
 # 9 Seychelles north           121239. LC
# 10 South Madagascar            38176. VU
# 11 West Madagascar            409601. LC


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/assessment/criteria/"

  # save to file
    save(criterion_b1_extent_of_occurrence,
      file = paste0(save_locale, "criterion_b1_extent_of_occurrence.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coral_reefs,
       ecoregion_list)

  # remove core data objects
    rm(criterion_b1_extent_of_occurrence)

