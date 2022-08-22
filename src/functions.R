make_map <- function(all_dat, dat_count){
  #' @title Make Map
  #'
  #' @description Generate map of relevant GRTS and survey sites.
  
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


load_nabat_data <- function(username, password, project_id){
  #' @title Load NABat Data
  #'
  #' @description Load and clean data

  exclude <- c("LACITABR", "LANOTABR","LABLPAHE", "LABOPESU", "EUMAEUPE","EUMAIDPH", "MYCAMYYU", "MYEVMYTH",
               "MYLUMYVO", "EPFULANO", "Q10k", "Q15k", "Q20k", "Q25k", "Q40k", "40kMyo", "40k", "Q50k", 
               "LACITABR,HiF", "Q25k,MYCAMYYU", "Q25k,HiF", "Q40k,Q25k", "Q40k,LoF", "MY40", "Social", "25K", 
               "25k", "NOISE", "LowF", "HighF", "NoID", "LoF", "HiF", "Noise", "MYSP")
  token = get_nabat_gql_token(username, password)
  token = get_refresh_token(token)
  project_df = get_projects(token)
  token = get_refresh_token(token)
  sa_survey_df = get_sa_project_summary(token,
                                        project_df,
                                        project_id[1])
  additional_projects <- project_id[-1]
  if (length(additional_projects > 0)){
    for (i in additional_projects){
      sa_survey_df_add = get_sa_project_summary(token,
                                                project_df,
                                                i)
      sa_survey_df <- bind_rows(sa_survey_df, sa_survey_df_add)
    }
  }
  # sa_proj_dates = unique(sa_survey_df$year)
  # this_year = sa_proj_dates[1]
  token = get_refresh_token(token)
  sa_bulk_df = get_sa_bulk_wavs(token,
                                sa_survey_df,
                                year = 'all')
  token = get_refresh_token(token)
  species_df = get_species(token = token)
  all_dat <- left_join(sa_bulk_df, species_df, by = c("manual_id" = "id"))
  all_dat$survey_event_id <- as.numeric(all_dat$survey_event_id)
  sa_survey_df$survey_event_id <- as.numeric(sa_survey_df$survey_event_id)
  all_dat <- left_join(all_dat, sa_survey_df, keep = FALSE) %>% 
    mutate(year = lubridate::year(recording_night)) %>% 
    dplyr::filter(grts_cell_id %in% report_grts)
  if (report_locations[1] != ""){
    all_dat <- all_dat %>% mutate(year = lubridate::year(recording_night)) %>% 
      dplyr::filter(grts_cell_id %in% report_grts) %>% dplyr::filter(location_name %in% report_locations)
  }
  #these two lines to compensate for year not loading on import from NABat API. Can remove if that ever gets resolved. 
  sa_proj_dates = unique(all_dat$year)
  this_year = max(sa_proj_dates)
  dat_count <- all_dat %>% 
    dplyr::filter(!species_code %in% exclude) %>%
    dplyr::filter(nchar(species_code) == 4) %>% 
    dplyr::filter(!is.na(manual_id)) %>%
    separate_rows(species_code) %>% 
    dplyr::group_by(year, grts_cell_id, location_name, species_code) %>%
    dplyr::summarise(n = n()) %>%
    pivot_wider(names_from = species_code, values_from = n) %>% 
    replace(is.na(.), 0) %>%
    pivot_longer(., cols = 4:length(.)) %>% 
    dplyr::mutate(pres_abs = case_when(
      value == 0 ~ "",
      value > 0 ~ "X")) %>%
    dplyr::rename(species = name) %>%
    dplyr::select(-value)
  return(list(all_dat, dat_count))
}