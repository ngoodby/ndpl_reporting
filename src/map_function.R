make_map <- function(all_dat, dat_count){
  #' @title Make Map
  #'
  #' @description Generate map of relevant GRTS and survey sites.
  #' 

  #bringing in GRTS shapefile
  load(paste0(here::here(), "/data/grts.grid.rda"))
  grts_mapping <- grts.grid %>% 
    st_as_sf() %>%
    dplyr::filter(GRTS_ID %in% dat_count$grts_cell_id) %>% 
    dplyr::filter(country == "US")

  #sf object of site locations to use to write 
  site_locs <- all_dat %>% 
    dplyr::distinct(grts_cell_id, location_name, latitude, longitude) %>% 
    dplyr::group_by(grts_cell_id, location_name) %>% 
    dplyr::summarise(latitude = mean(latitude),
                     longitude = mean(longitude)) %>% 
    dplyr::mutate(country = "US") %>% 
    sf::st_as_sf(coords = c("longitude","latitude")) %>% 
    dplyr::filter(grts_cell_id %in% dat_count$grts_cell_id)
  
  #plotting survey sites
  sites <- all_dat %>% 
    dplyr::distinct(grts_cell_id, location_name, latitude, longitude) %>% 
    dplyr::group_by(grts_cell_id, location_name) %>% 
    dplyr::summarise(latitude = mean(latitude),
                     longitude = mean(longitude)) %>% 
    dplyr::mutate(country = "US") %>% 
    leaflet() %>% 
    addProviderTiles("Esri.WorldTopoMap") %>% 
    addPolygons(data = grts_mapping, label = ~paste("NABat Cell ", GRTS_ID), fillOpacity = 0, weight = 2) %>% 
    addCircleMarkers(lat = ~latitude, lng = ~longitude, weight = 1, label = ~paste("Survey Site Name:", location_name))
  return(list(sites, grts_mapping, site_locs))
}