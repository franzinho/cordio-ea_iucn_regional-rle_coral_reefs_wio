##
##  Name:       plot_criterion_b1_extent_of_occurrence.R
##
##  Objective:  Visualise extent of occurrence for  region
##
##  Approach:   Call to regional benthic taxa,  coasline and
##              visualise.
##
##              Outputs saved as *.png
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-13
##

##
## 1. Set up
##
 ## -- call to custom  ecoregions -- ##
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


 ## -- point to ecoregions + custom  coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coral_reefs/"

  # point to data file
    data_file <- "regional_coral_reefs.rda"

  # call to coral reefs
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    regional_coral_reefs
# Simple feature collection with 1368 features and 4 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 32.86711 ymin: -27.01737 xmax: 63.51225 ymax: 2.755531
# Geodetic CRS:  WGS 84
# # A tibble: 1,368 × 5
   # Ecoregion                        Eco_code Province  Prov_code
   # <chr>                               <dbl> <chr>         <dbl>
 # 1 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 2 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 3 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 4 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 5 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 6 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 7 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 8 Cargados Carajos/Tromelin Island    20097 Western …        20
 # 9 Cargados Carajos/Tromelin Island    20097 Western …        20
# 10 Cargados Carajos/Tromelin Island    20097 Western …        20
# # ℹ 1,358 more rows
# # ℹ 1 more variable: geometry <GEOMETRY [°]>
# # ℹ Use `print(n = ...)` to see more rows


 ## -- calculate criterion b1 exent of occurrence -- ##
  # get list of ecosystem units
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
 # [1] "Cargados Carajos/Tromelin Island"
 # [2] "Central Somali Coast"
 # [3] "Delagoa"
 # [4] "Mascarene Islands"
 # [5] "Seychelles"
 # [6] "Southeast Madagascar"
 # [7] "East African Coral Coast"
 # [8] "Northern Monsoon Current Coast"
 # [9] "Bight of Sofala/Swamp Coast"
# [10] "Western and Northern Madagascar"

 ## -- create empty object to hold results -- ##
  # extent of occurrence
    # eco_eoo <- tibble()
    eco_eoo <- list()

  # label centroids
    area_centroid <- tibble()

  # area
    eoo_area <- tibble()

  # loop to calculate eoo  # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

      # print progress to screen
        cat(paste0("...processing ", ecoregion_list[i], " [ ",
                   i, " of ", length(ecoregion_list), " ]\n"))

      # create eoo object
        eco_eoo[[i]] <-
          regional_coral_reefs %>%
            dplyr::filter(Ecoregion %in% ecoregion_list[i]) %>%
            st_transform(32737) %>%
            st_union() %>%
            # st_collection_extract("POLYGON") %>%
           as_Spatial() %>%
           redlistr::makeEOO()


     ## -- set area annotation -- ##
      # get centroid
        dat <-
          eco_eoo[[i]] %>%
            st_as_sf() %>%
            st_transform(32737) %>%
            st_centroid() %>%
            st_coordinates()

       # harvest results
         area_centroid %<>%
           bind_rows(dat %>%
                       data.frame() %>% tibble() %>%
                       mutate(Ecoregion = ecoregion_list[i]))

     ## -- calculate area -- ##
      # get area
        dat <-
          eco_eoo[[i]] %>%
            st_as_sf() %>%
            st_transform(32737) %>%
            st_area()

      # convert to km2
        dat %<>%
          tibble() %>%
          mutate(Ecoregion = ecoregion_list[i]) %>%
          rename(Area = ".") %>%
          mutate(Area = Area %>% as.numeric() / 1e3,
                 Area = Area %>% round(0))

      # harvest results
        eoo_area %<>%
          bind_rows(dat)


    }

 ## -- combine objects -- ##
  # get centroids for ecoregions
    ecoregion_centroids <-
      regional_ecoregions %>%
        group_by(Ecoregion) %>%
        st_centroid() %>%
        st_coordinates()

  # add ecoregion names
    ecoregion_centroids %<>%
      cbind(regional_ecoregions$Ecoregion %>% tibble()) %>%
      rename(Ecoregion = ".")

  # combine centoids and area
    area_centroid %<>%
      left_join(eoo_area)

  # put in order
    area_centroid %<>%
      dplyr::select(Ecoregion,
                    reefs_x = X,
                    reefs_y = Y,
                    Area)

  # join to centroids
    area_centroid %<>%
      left_join(ecoregion_centroids) %>%
      rename(easting  = X,
             northing = Y)

  # have a look
    area_centroid
# # A tibble: 10 × 6
   # Ecoregion             reefs_x reefs_y   Area easting northing
   # <chr>                   <dbl>   <dbl>  <dbl>   <dbl>    <dbl>
 # 1 Cargados Carajos/Tro…  2.47e6  8.33e6 2.19e8  2.60e6   8.52e6
 # 2 Central Somali Coast   1.28e6  1.03e7 2.53e5  1.79e6   1.07e7
 # 3 Delagoa                3.72e4  7.31e6 6.34e7  1.93e5   7.19e6
 # 4 Mascarene Islands      2.61e6  7.62e6 7.35e7  2.67e6   7.61e6
 # 5 Seychelles             1.97e6  9.15e6 4.08e8  2.00e6   9.22e6
 # 6 Southeast Madagascar   1.41e6  7.48e6 2.28e7  1.53e6   7.35e6
 # 7 East African Coral C…  6.37e5  9.03e6 2.53e8  7.56e5   9.03e6
 # 8 Northern Monsoon Cur…  9.60e5  9.99e6 2.31e7  1.14e6   9.92e6
 # 9 Bight of Sofala/Swam…  4.20e5  7.99e6 1.79e7  4.46e5   7.89e6
# 10 Western and Northern…  1.16e6  8.03e6 1.17e9  1.23e6   8.05e6

  # convert areas
    area_centroid %<>%
      mutate(Area = Area / 1e3)

 ## -- set threat categories -- ##
  # classify
    criterion_b1_extent_of_occurrence <-
      area_centroid %>%
        rename(`EOO Area` = Area) %>%
      mutate(Status = ifelse(`EOO Area` <= 2e3,                  "CR",     NA),
             Status = ifelse(`EOO Area` %>% between(2e3, 20e3),  "EN", Status),
             Status = ifelse(`EOO Area` %>% between(20e3, 50e3), "VU", Status),
             Status = ifelse(`EOO Area` > 50e3,               "NT/LC", Status))
# # A tibble: 10 × 7
   # Ecoregion  reefs_x reefs_y `EOO Area` easting northing Status
   # <chr>        <dbl>   <dbl>      <dbl>   <dbl>    <dbl> <chr>
 # 1 Cargados …  2.47e6  8.33e6    218571.  2.60e6   8.52e6 NT/LC
 # 2 Central S…  1.28e6  1.03e7       253.  1.79e6   1.07e7 CR
 # 3 Delagoa     3.72e4  7.31e6     63445.  1.93e5   7.19e6 NT/LC
 # 4 Mascarene…  2.61e6  7.62e6     73508.  2.67e6   7.61e6 NT/LC
 # 5 Seychelles  1.97e6  9.15e6    408230.  2.00e6   9.22e6 NT/LC
 # 6 Southeast…  1.41e6  7.48e6     22834.  1.53e6   7.35e6 VU
 # 7 East Afri…  6.37e5  9.03e6    252534.  7.56e5   9.03e6 NT/LC
 # 8 Northern …  9.60e5  9.99e6     23099.  1.14e6   9.92e6 VU
 # 9 Bight of …  4.20e5  7.99e6     17851.  4.46e5   7.89e6 EN
# 10 Western a…  1.16e6  8.03e6   1167386.  1.23e6   8.05e6 NT/LC

##
## 3. Visualise
##
 ## -- set colour palette -- ##
   # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()

  # get list of ecosystem units
    n_units <-
      ecoregion_list %>% length()

  # set palette
    c_palette <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 4, "continuous")

  # set reef colour
    r_colour <- fishualize::fish(option = "Balistapus_undulatus", 5)[1]

  # set criteria b1 eoo colour
    c_colour <-
      wesanderson::wes_palette("Cavalcanti1", n_units + 4, "continuous")

  # set regional bounding box
    e_zoom <-
      # regional_coral_reefs %>%
      regional_ecoregions %>%
        # st_transform(32737) %>%
        st_bbox()

 ## -- combine eoo polygons -- ##
  # create empty object to hold results
    # criterion_b1_polygons <- st_sfc(crs = 4326) %>% st_sf()
    criterion_b1_polygons <- st_sfc(crs = 32737) %>% st_sf()

  # loop                 # i=1 ## -- for testing -- ##
    for(i in 1:length(eco_eoo)){

      # subset object and convert to sf
        dat <-
          eco_eoo[[i]] %>%
          st_as_sf() # %>%
          # st_set_crs(4326)
          # st_transform(4326)

      # add ecoregion
        dat %<>%
          mutate(Ecoregion = ecoregion_list[i])

      # organise columns
        dat %<>%
          dplyr::select(Ecoregion,
                        geometry)

      # harvest results
        criterion_b1_polygons %<>%
          rbind(dat)


    }

 ## -- round area measurements -- ##
  # remove decimals
    area_centroid %<>%
      mutate(Area = Area %>% round(0))

 ## -- plot regional data -- ##
  # set region name
    region_name <- "Western Indian Ocean"

  # open window
    # quartz("criterion b1 eoo", 7, 7)

       # create figure
         ggplot() +
           geom_sf(aes(colour = Ecoregion,
                       fill   = Ecoregion),
                   alpha = 0.2,
                   data   = regional_ecoregions %>%
                              # st_transform(32737) %>%
                              st_crop(e_zoom)) +
           geom_sf(fill   = r_colour,
                   colour = r_colour,
                   size   = 0.1,
                   alpha  = 0.5,
                   data   = regional_coral_reefs %>%
                              st_transform(32737))  +
           geom_sf(fill   = "grey75",
                   colour = "grey75",
                   size   = 0.1,
                   data   = regional_coastline %>%
                              st_transform(32737) %>%
                              st_crop(e_zoom)) +
           geom_sf(aes(colour = Ecoregion),
                   fill   = NA,
                   size   = 0.5,
                   alpha  = 0.4,
                   data   = criterion_b1_polygons %>%
                              st_transform(32737)) +
           geom_sf_text(aes(colour = Ecoregion,
                            label  = paste0(Ecoregion, "\n",
                                            "EOO = ", Area, " ",
                                            expression(km^2))),
                    data  = area_centroid %>%
                              st_as_sf(coords = c("easting", "northing"),
                                        crs   = 32737)) +
           theme_void() +
           coord_sf(xlim  = c(e_zoom[1], e_zoom[3]),
                    ylim  = c(e_zoom[2], e_zoom[4]),
                    datum = st_crs(32737)) +
           ggspatial::annotation_scale() +
           scale_colour_manual(values = c_palette) +
           scale_fill_manual(values = c_palette) +
           labs(title    = region_name,
                subtitle = "Criterion B1: Extent of Occurrence") +
           theme(legend.position = "none",
                 plot.title      = element_text(hjust = 0.5),
                 plot.subtitle   = element_text(hjust = 0.5,
                                                face  = "italic"))


 ## -- save for wiki -- ##
  # set save locale
    figure_locale <-
      paste0("figures/assessment/criteria/",
             "criterion_b_restricted_geographic_distribution/")

  # save to file
    ggsave(paste0(figure_locale,
                  "criterion_b1_extent_of_occurrence_regional.png"),
      width  = 7,
      height = 7)


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       figure_locale)

  # remove plotting variables
    rm(ecoregion_list,
       e_zoom,
       c_palette,
       r_colour,
       c_colour,
       region_name)

  # remove intermediate objects
    rm(regional_ecoregions,
       # regional_monitoring_sites,
       regional_coastline)

  # remove core objects
    rm(regional_coral_reefs)

