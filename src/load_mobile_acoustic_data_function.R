load_mobile_data <-  function(username, password, project_id, 
                              report_grts_mobile, mobile_exclude){
  #' @title Load mobile NABat data
  #'
  #' @description Load and clean mobile acoustic NABat survey data.

  token = get_nabat_gql_token_edited(username, password)
  token = get_refresh_token(token)
  project_df = get_projects(token)
  token = get_refresh_token(token)
  ma_survey_df = get_ma_project_summary(token, 
                                        project_df, 
                                        project_id[1])
  
  additional_projects <- project_id[-1]
  if (length(additional_projects > 0)){
    for (i in additional_projects){
      ma_survey_df_add = get_ma_project_summary(token,
                                                project_df,
                                                i)
      ma_survey_df <- bind_rows(ma_survey_df, ma_survey_df_add)
    }
  }
  
  if (report_grts_mobile[1] != ""){
    ma_survey_df = ma_survey_df %>% dplyr::filter(grts_cell_id %in% report_grts_mobile)
  }
  
  token = get_refresh_token(token)
  ma_bulk_df = get_ma_bulk_wavs(token,
                                ma_survey_df,
                                year = 'all') %>% 
    mutate(year = lubridate::year(recording_night))
  
  token = get_refresh_token(token)
  species_df = get_species(token = token)
  mobile_all_dat <- left_join(ma_bulk_df, species_df, by = c("manual_id" = "id"), keep=F)
  mobile_all_dat$survey_event_id <- as.numeric(mobile_all_dat$survey_event_id)
  ma_survey_df$survey_event_id <- as.numeric(ma_survey_df$survey_event_id)
  mobile_all_dat <- left_join(mobile_all_dat, ma_survey_df, keep=F) %>% 
    dplyr::filter(!manual_name %in% mobile_exclude) %>% 
    dplyr::filter(!is.na(manual_name)) 
  
  ma_proj_dates <-  unique(mobile_all_dat$year)
  
  this_year <- max(ma_proj_dates)
  
  report_grts <- unique(mobile_all_dat$grts_cell_id)
  
  mobile_all_dat <- mobile_all_dat %>% dplyr::filter(year == this_year, 
                                                     grts_cell_id %in% report_grts)
  
  mobile_dat_count <- mobile_all_dat %>% #filtering on dat_count instead of all_dat allows for a count of calls later on and preserves couplets
    separate_rows(species_code) %>% 
    dplyr::group_by(recording_night, grts_cell_id, species_code) %>%
    dplyr::summarise(n = n()) %>%
    pivot_wider(names_from = species_code, values_from = n) %>% 
    replace(is.na(.), 0) %>%
    pivot_longer(., cols = 3:length(.)) %>% 
    # dplyr::mutate(pres_abs = case_when(
    #   value == 0 ~ FALSE,
    #   value > 0 ~ TRUE)) %>%
    dplyr::rename(species = name)
  return(list(mobile_all_dat, mobile_dat_count, ma_survey_df, ma_bulk_df))
}