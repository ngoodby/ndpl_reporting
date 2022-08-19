make_map <- function(all_dat, dat_count){
  #bringing in GRTS shapefile
  load(paste0(here::here(), "/data/grts.grid.rda"))
  grts_mapping <- grts.grid %>% 
    st_as_sf() %>%
    dplyr::filter(GRTS_ID %in% dat_count$grts_cell_id) %>% 
    dplyr::filter(country == "US")
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
  return(sites)
}