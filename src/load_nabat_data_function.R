load_nabat_data <- function(username, password, project_id, report_grts, report_locations){
  #' @title Load NABat Data
  #'
  #' @description Load and clean data
  #' 
  
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
  if (report_grts[1] != ""){
    sa_survey_df = sa_survey_df %>% dplyr::filter(grts_cell_id %in% report_grts)
  }
  token = get_refresh_token(token)
  sa_bulk_df = get_sa_bulk_wavs(token,
                                sa_survey_df,
                                year = 'all')
  token = get_refresh_token(token)
  species_df = get_species(token = token)
  all_dat <- left_join(sa_bulk_df, species_df, by = c("manual_id" = "id"), keep=F)
  all_dat$survey_event_id <- as.numeric(all_dat$survey_event_id)
  sa_survey_df$survey_event_id <- as.numeric(sa_survey_df$survey_event_id)
  all_dat <- left_join(all_dat, sa_survey_df, keep=F) %>% 
    mutate(year = lubridate::year(recording_night))
  if (report_locations[1] != ""){
    all_dat <- all_dat %>% 
      dplyr::filter(location_name %in% report_locations)
  }
  sa_proj_dates = unique(all_dat$year)
  this_year = max(sa_proj_dates)
  dat_count <- all_dat %>% 
    dplyr::filter(!species_code %in% exclude) %>% #filtering on dat_count instead of all_dat allows for a count of calls later on and preserves couplets
    dplyr::filter(nchar(species_code) == 4) %>% 
    dplyr::filter(!is.na(manual_id)) %>%
    separate_rows(species_code) %>% 
    dplyr::group_by(year, grts_cell_id, location_name, species_code) %>%
    dplyr::summarise(n = n()) %>%
    pivot_wider(names_from = species_code, values_from = n) %>% 
    replace(is.na(.), 0) %>%
    pivot_longer(., cols = 4:length(.)) %>% 
    dplyr::mutate(pres_abs = case_when(
      value == 0 ~ FALSE,
      value > 0 ~ TRUE)) %>%
    dplyr::rename(species = name) %>%
    dplyr::select(-value)
  return(list(all_dat, dat_count, sa_survey_df))
}