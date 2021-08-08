library(tidyverse)
library(viridis)
library(sf)
library(ggspatial)
library(animation)
library(gridExtra)


# gen-city-map-labeled -----------------------------------------------------
gen_city_map_labeled <- function(
  socal_shp_file = "data/shape-files/socal-zip/Zipcode_boundary_scag_2009.shp",
  ca_shp_file = "data/shape-files/ca-counties/cnty19_1.shp",
  road_shp_file = "data/shape-files/ca-interstates/tl_2015_06_prisecroads.shp",
  zip_code_file = "data/map_zipcodes.csv"
) {

  # Load OC zip code file
  oc_zips <- read_csv(zip_code_file, col_types = cols(Zip = col_character())) %>%
    rename_all(str_to_lower)

  oc_cities <- oc_zips %>%
    group_by(city) %>%
    summarize(population = sum(population))

  # Load CA roads shp
  roads_shp <- st_read(road_shp_file, quiet = TRUE) %>%
    subset(RTTYP == "I")

  # Load usa shp file to get cover below southern CA
  ca_shp <- st_read(ca_shp_file, quiet = TRUE)

  ca_shp_oc_only <- ca_shp %>% filter(COUNTY_NAM == "Orange")

  # Load southern CA shp
  all_shp <- st_read(socal_shp_file, quiet = TRUE)

  all_shp1 <- all_shp %>%
    group_by(NAME) %>%
    summarize()

  all_city_shp1 <- all_shp1 %>% # Plot for entire area of shape file
    left_join(oc_cities, by = c("NAME" = "city")) %>%
    mutate(ID = NAME)

  oc_city_shp1 <- all_shp1 %>% # Only want names for OC cities
    right_join(oc_cities, by = c("NAME" = "city")) %>%
    mutate(ID = NAME)

  names_to_drop <- c(
    "Buena Park",
    "Capistrano Beach",
    "Corona del Mar",
    "Cypress",
    "Fountain Valley",
    "La Palma",
    "Ladera Ranch",
    "Laguna Hills",
    "Laguna Niguel",
    "Laguna Woods",
    "Midway City",
    "Newport Beach",
    "Newport Coast",
    "Placentia",
    "Rancho Santa Margarita",
    "San Juan Capistrano",
    "Stanton",
    "Sunset Beach",
    "Trabuco Canyon",
    "Villa Park",
    "Westminster"
  )

  oc_city_shp_renamed <- oc_city_shp1 %>%  # Drop names that don't place well
    mutate(NAME = ifelse(NAME %in% names_to_drop, "", NAME))


  # Plot of cities with roads and UCI
  ggplot(all_city_shp1) +
    geom_sf(fill = "khaki1", color = "gray60") +
    geom_sf(data = ca_shp, fill = "khaki1", color = "gray60") +
    geom_sf(data = ca_shp_oc_only, fill = "khaki1", color = "black", size = 1.25) +
    geom_sf(data = oc_city_shp1, color = "black", fill = "khaki1") +
    geom_sf(data = roads_shp, color = "gray", fill = "white", size = 1) +
    coord_sf( # Outlines Orange County
      xlim = c(396639.2, 461568.2),
      ylim = c(3694363, 3759819),
      expand = FALSE
    ) +
    annotation_scale(
      location = "br",
      width_hint = 0.2,
      pad_x = unit(0.25, "cm"),
      pad_y = unit(0.25, "cm")
    ) +
    annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_x = unit(0.25, "cm"), pad_y = unit(0.5, "cm"),
      style = north_arrow_fancy_orienteering
    )  +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA),
      panel.background = element_rect(fill = "darkslategray2")
    ) +
    geom_sf_text(
      data = oc_city_shp_renamed,
      mapping = aes(label = NAME),
      size = 4,
      color = "darkgreen"
    ) +
    annotate( # UCI label
      geom = "text",
      x = 422000,
      y = 3722000,
      label = "UC Irvine",
      color = "blue",
      size = 4,
      fontface = "bold"
    ) +
    annotate( # UCI point
      geom = "point",
      x = 421873.3807,
      y = 3723199.945,
      color = "blue",
      size = 4,
      pch = 17
    ) +
    annotate( # I-15 label
      geom = "text",
      x = 456000,
      y = 3740000,
      label = "I-15",
      color = "black",
      size = 4,
      fontface="bold"
    ) +
    annotate( # I-405 label
      geom = "text",
      x = 422000,
      y = 3727500,
      label = "I-405",
      color = "black",
      size = 4,
      fontface="bold"
    ) +
    annotate( # I-5 label
      geom = "text",
      x = 430000,
      y = 3730000,
      label = "I-5",
      color = "black",
      size = 4,
      fontface="bold"
    ) +
    annotate( # I-5 label
      geom = "text",
      x = 403250,
      y = 3753500,
      label = "I-5",
      color = "black",
      size = 4,
      fontface = "bold"
    ) +
    annotate( # I-605 label
      geom = "text",
      x = 400000,
      y = 3750000,
      label = "I-605",
      color = "black",
      size = 4,
      fontface="bold"
    )
}








# gen-map-function-gif --------------------------------------------------------
gen_map_gif <- function(
  plot_data,
  socal_shp_file,
  ca_shp_file,
  legend_label,
  month_seq,
  var_type
){

  socal_shp_og <- st_read(socal_shp_file, quiet = TRUE)

  ca_shp_og <- st_read(ca_shp_file, quiet = TRUE)

  ca_shp_oc_only <- ca_shp_og %>% filter(COUNTY_NAM == "Orange")

  map_labeled <- gen_city_map_labeled()

  prev_directory <- getwd()
  setwd("docs/") # Need to save the map html to docs folder

  saveHTML({

    ani.options(verbose = FALSE)

    for (i in 1:length(month_seq)) {
      curr_time <- month_seq[i]

      plot_data_sub <- plot_data %>% filter(month_date == curr_time)

      socal_shp <- socal_shp_og %>% # Plot for entire area of shape file
        full_join(plot_data_sub, by = c("KEY_" = "zip")) %>%
        mutate(ID = KEY_)

      oc_shp <- socal_shp_og %>% # Only want names for OC zips
        right_join(plot_data_sub, by = c("KEY_" = "zip")) %>%
        mutate(ID = KEY_)

      curr_map <- ggplot(socal_shp_og) +
        geom_sf(fill = "khaki1", color = "gray60") +
        geom_sf(data = ca_shp_og, fill = "khaki1", color = "gray60") +
        geom_sf(data = ca_shp_oc_only, fill = "khaki1", color = "black", size = 1.25) +
        geom_sf(
          data = oc_shp,
          mapping = aes(fill = plot_var_cont, group = ID),
          color = "black"
        ) +
        coord_sf( # Outlines Orange County
          xlim = c(396639.2, 461568.2),
          ylim = c(3694363, 3759819),
          expand = FALSE
        ) +
        labs(fill = paste0(legend_label[1], "\n", legend_label[2])) +
        theme_void() +
        theme(
          panel.background = element_rect(fill = "darkslategray2"),
          #legend.position = "bl",
          legend.justification = c(0, 0),
          legend.position = c(0, 0),
          legend.background = element_rect(fill = "white"),
          legend.title = element_text(size = 15, color = "black"),
          legend.text = element_text(vjust = 0, size = 15, color = "black"),
          legend.text.align = 1,
          legend.margin = margin(1, 1, 1, 1),
          panel.border = element_rect(colour = "black", fill = NA)
        ) +
        annotation_scale(
          location = "br",
          width_hint = 0.2,
          pad_x = unit(0.25, "cm"),
          pad_y = unit(0.25, "cm")
        ) +
        annotation_north_arrow(
          location = "br",
          which_north = "true",
          pad_x = unit(0.25, "cm"),
          pad_y = unit(0.5, "cm"),
          style = north_arrow_fancy_orienteering
        ) +
        annotate(
          geom = "text",
          x = 450000,
          y = 3755000,
          label = curr_time,
          color = "black",
          size = 7,
          fontface="bold"
        ) +
        scale_fill_viridis(
          direction = -1,
          limits = c(0, round(max(plot_data$plot_var_cont), -1))
        )

      grid.arrange(
        curr_map,
        map_labeled,
        ncol = 2
      )

      ani.pause()
    }
  },
  ani.width = 800,
  ani.height = 400,
  img.name = paste0(var_type, "-map-gif.png"),
  htmlfile = paste0(var_type, "-map-gif.html"),
  verbose = FALSE
  )

  setwd(prev_directory)
}
