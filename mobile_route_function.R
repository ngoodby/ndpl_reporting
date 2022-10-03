get_mobile_route <- function(username, password, project_id, survey_event_id,
                             branch = 'prod',
                             url = NULL,
                             aws_gql = NULL,
                             aws_alb = NULL,
                             docker = FALSE){
  # Get headers for token
  token = get_nabat_gql_token(username, password)
  token = get_refresh_token(token)
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  
  get_s3_id = sprintf('query allVwEventGeometriesQuery {
      allVwEventGeometries{
        nodes{
        
        }
      }
    }', survey_event_id)
  
  pbody = list(query = get_s3_id, operationName = 'stationaryAcousticEventByIdQuery')
  res = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res)
}