##
##  Name:       plot_regional_percent_cover_stations.R
##
##  Objective:  Standardise & format data for analysing criterion A:
##                Reduction in geographic distribution
##
##  Approach:   Call to clean data compilation, filter by time
##                period, geographic region, summarise and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Skipping regional reef visualisation until
##                 review of wcmc global reef layer [ FS: 2025-01-24 ]
##

##
## 1. Set up
##
 # -- call to regional ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/ecoregions/"

  # point to data file
    data_file <- "regional_ecoregions.rda"

  # call to ecoregions
    load(paste0(data_locale, data_file))


 ## -- point to regional coastline -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # point to data file
    data_file <- "regional_coastline.rda"

  # import coastline
    load(paste0(data_locale, data_file))


 ## -- point to regional coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # point to data file
    data_file <- "regional_coral_reefs.rda"

  # call to coral reefs
    load(paste0(data_locale, data_file))


 ## -- call to monitoring site positions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/monitoring_sites/"

  # point to data file
    data_file <- "regional_monitoring_sites.rda"
    # data_file <- "benthos_sites_locations.rda"
    # data_file <- "fish_site_locations.rda"

  # import site positions
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- review ecoregion object -- ##
  # have a look
    regional_ecoregions
# Simple feature collection with 11 features and 1 field
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: -151726.1 ymin: 6709593 xmax: 3485904 ymax: 9900473
# Projected CRS: WGS 84 / UTM zone 37S
# # A tibble: 11 × 2
#    Ecoregion                                                     geometry
#  * <chr>                                                   <GEOMETRY [m]>
#  1 Comoros                 POLYGON ((1386573 8784736, 1386643 8784691, 1…
#  2 Delagoa                 POLYGON ((352929.6 7019372, 352761.4 7019172,…
#  3 East Madagascar         POLYGON ((1812767 8290355, 1859169 8294619, 1…
#  4 Mascarene Isl.          MULTIPOLYGON (((2762101 7918440, 2843855 7949…
#  5 N Mozambique-S Tanzania POLYGON ((874419.7 8763185, 874370.3 8762590,…
#  6 N Tanzania-Kenya        MULTIPOLYGON (((940503.7 9294775, 949666.6 92…
#  7 North Madagascar        MULTIPOLYGON (((1634850 8820959, 1634864 8820…
#  8 Seychelles Outer        POLYGON ((1964547 9232621, 2008642 9177089, 2…
#  9 Seychelles north        POLYGON ((2853145 9145640, 2853321 9144484, 2…
# 10 South Madagascar        POLYGON ((1772612 7414567, 1773997 7413740, 1…
# 11 West Madagascar         POLYGON ((1147690 8433605, 1159340 8434628, 1…

  # combine national s segments
    regional_ecoregions %<>%
      group_by(Ecoregion) %>%
      summarise(geometry = geometry %>% st_combine())


 ## -- review coastline object -- ##
  # have a look
    regional_coastline
# Geometry set for 1 feature
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -118.1342 ymin: -5.979108 xmax: -73.69754 ymax: 35.81038
# Geodetic CRS:  WGS 84
# MULTIPOLYGON (((-109.228 10.29214, -109.2342 10...

  # set to utm
    regional_coastline %<>%
      st_transform(32737)


 ## -- review site location data object -- ##
  # have a look
    regional_monitoring_sites
# Simple feature collection with 605 features and 8 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: 32.6036 ymin: -27.9267 xmax: 59.62795 ymax: -1.704117
# Geodetic CRS:  WGS 84
# # A tibble: 605 × 9
#    Ecoregion first_year recent_year no_years year_gap recent_coral_cover
#  * <chr>          <dbl>       <dbl>    <int>    <dbl>              <dbl>
#  1 Comoros         2017        2017        1        1               73.7
#  2 Comoros         2017        2017        1        1               48.3
#  3 Comoros         2017        2017        1        1               31.9
#  4 Comoros         2017        2017        1        1               19.8
#  5 Comoros         2018        2018        1        1               77.0
#  6 Comoros         2018        2018        1        1               13.5
#  7 Comoros         2016        2016        1        1               39.1
#  8 Comoros         2016        2016        1        1               17.1
#  9 Comoros         2018        2018        1        1               41.6
# 10 Comoros         2009        2009        1        1               84
# # ℹ 595 more rows
# # ℹ 3 more variables: original_coral_cover <dbl>,
# #   mean_coral_cover <dbl>, geometry <POINT [°]>
# # ℹ Use `print(n = ...)` to see more rows

  # have a look
    # fish_site_locations
# # A tibble: 2,743 × 6
   # country site               locality longitude latitude source
   # <chr>   <chr>              <chr>        <dbl>    <dbl> <chr>
 # 1 mexico  punta pulpito      punta p…     -111.     26.5 (bent…
 # 2 mexico  el bajo            cabo pu…     -109.     23.5 (bent…
 # 3 mexico  casitas            cabo pu…     -109.     23.4 (bent…
 # 4 mexico  el pardito         isla sa…     -111.     24.8 (bent…
 # 5 mexico  san francisquito … san fra…     -113.     28.4 (bent…
 # 6 mexico  puerto lobos       puerto …     -113.     30.3 (bent…
 # 7 mexico  puerto libertad    puerto …     -113.     29.9 (bent…
 # 8 mexico  isla tortuga       unknown      -112.     27.4 (bent…
 # 9 mexico  isla san esteban   unknown      -113.     28.7 (bent…
# 10 mexico  isla patos         isla pa…     -112.     29.3 (bent…
# # ℹ 2,733 more rows
# # ℹ Use `print(n = ...)` to see more rows

  # # generate spatial object
  #   benthos_sites_locations_sf <-
  #     regional_monitoring_sites %>%
  #     # benthos_sites_locations %>%
  #     # fish_site_locations %>%
  #       dplyr::filter(!latitude  %>% is.na(),
  #                     !longitude %>% is.na()) %>%
  #       st_as_sf(coords = c("longitude", "latitude"),
  #                crs    = 4326)

  # set to utm
    # benthos_sites_locations_sf %<>%
    regional_monitoring_sites %<>%
      st_transform(32737)

  # add ecoregions
    # benthos_sites_locations_sf %<>%
    regional_monitoring_sites %<>%
      st_intersection(regional_ecoregions %>%
                        dplyr::select(Ecoregion,
                                      geometry))

##
## 3. Visualise
##
 ## -- check ecoregion taxonomy -- ##
  # check ecoregions from criterion a data table
      benthos_sites_locations_sf %>% pull(Ecoregion) %>% unique()
 # [1] "Chiapas-Nicaragua"          "Clipperton"
 # [3] "Cocos Islands"              "Cortezian"
 # [5] "Eastern Galapagos Islands"  "Guayaquil"
 # [7] "Mexican Tropical Pacific"   "Nicoya"
 # [9] "Northern Galapagos Islands" "Panama Bight"
# [11] "Revillagigedos"             "Western Galapagos Islands"

  # check reef ecoregions
    # regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # check list from ecoregions object
    regional_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Chiapas-Nicaragua"          "Clipperton"
 # [3] "Cocos Islands"              "Cortezian"
 # [5] "Eastern Galapagos Islands"  "Guayaquil"
 # [7] "Magdalena Transition"       "Mexican Tropical Pacific"
 # [9] "Nicoya"                     "Northern Galapagos Islands"
# [11] "Panama Bight"               "Revillagigedos"
# [13] "Western Galapagos Islands"

 ## -- set colour palette -- ##
  # get list of ecoregions from criterion a data table
    ecoregion_list <-
      regional_monitoring_sites %>% pull(Ecoregion) %>% unique()
# [1] "Comoros"                 "Delagoa"
# [3] "East Madagascar"         "Mascarene Isl."
# [5] "N Mozambique-S Tanzania" "N Tanzania-Kenya"
# [7] "North Madagascar"        "Seychelles Outer"
# [9] "Seychelles north"        "South Madagascar"
# [11] "West Madagascar"

  # # set order for ecoregions
  #   ecoregion_list <-
  #     c("Cortezian",
  #       "Mexican Tropical Pacific",
  #       "Revillagigedos",
  #       "Clipperton",
  #       "Chiapas-Nicaragua",
  #       "Nicoya",
  #       "Cocos Islands",
  #       "Panama Bight",
  #       "Guayaquil",
  #       "Northern Galapagos Islands",
  #       "Eastern Galapagos Islands",
  #       "Western Galapagos Islands")

  # set number of ecoregions
    n_units <-
      ecoregion_list %>% length()

  # set palette
    c_palette <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

  # set reef colour
    r_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[1]

  # set criteria b1 eoo colour
    c_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 2, "continuous")

 ## -- plot regional data -- ##
  # set save locale
    figure_locale <- "figures/biological/"

  # create pdf
    CairoPDF(paste0(figure_locale, "gcrmn_percent_cover_stations.pdf"), 7, 7)

  # open window
    # quartz("gcrmn monitoring stations", 7, 7)

  ## -- loop to generate figure -- ##
   # loop            # i=3  ## -- for testing -- ##
     for(i in 1:length(ecoregion_list)){
     # for(i in c(1, 3:length(ecoregion_list))){

       # print progress to screen
         cat(paste0("...processing:  ", ecoregion_list[i],
                    " [ ", i, " of ", length(ecoregion_list), " ]\n"))

       # set ecoregion bounding box
         e_zoom <-
           regional_ecoregions %>%
             dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
             st_bbox()

       # create figure
         p <-
         ggplot() +
           geom_sf(aes(colour = Ecoregion,
                       fill   = Ecoregion),
                   alpha = 0.2,
                   data   = regional_ecoregions %>%
                              st_crop(e_zoom)) +
           geom_sf(fill   = r_colour,
                   colour = r_colour,
                   size   = 0.1,
                   alpha  = 0.5,
                   data   = regional_coral_reefs %>%
                              dplyr::filter(Ecoregion %in% ecoregion_list[i])) +
           geom_sf(fill   = "grey75",
                   colour = "grey75",
                   size   = 0.1,
                   data   = regional_coastline %>%
                              st_crop(e_zoom)) +
           geom_sf(# aes(colour = c_colour[i]),
                   # colour = c_colour[i],
                   fill   = NA,
                   size   = 1.5,
                   alpha  = 0.4,
                   # data   = benthos_sites_locations_sf %>%
                   data   = regional_monitoring_sites %>%
                              dplyr::filter(Ecoregion %in% ecoregion_list[i])) +
           theme_void() +
           coord_sf(xlim = c(e_zoom[1], e_zoom[3]),
                    ylim = c(e_zoom[2], e_zoom[4]),
                    datum = st_crs(32737)) +
           ggspatial::annotation_scale() +
           scale_colour_manual(values = c_palette) +
           scale_fill_manual(values = c_palette) +
           labs(title    = ecoregion_list[i],
                subtitle = "Percent cover sites") +
           theme(legend.position = "none",
                 plot.title      = element_text(hjust = 0.5),
                 plot.subtitle   = element_text(hjust = 0.5,
                                                face  = "italic"))

    # plot image
      p %>% print()


   }

   # close device
     dev.off()


##
## 3. Clean up workspace
##
  # remove paths
    rm(data_locale,
       figure_locale)

  # remove plotting variables
    rm(ecoregion_of_interest,
       # crs_details,
       e_zoom,
       c_palette,
       r_colour,
       c_colour)

  # remove intermediate objects
    rm(regional_coral_reefs,
       regional_coastline,
       regional_ecoregions)

  # remove core objects
    rm(benthos_sites_locations_sf)
