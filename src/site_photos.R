get_site_photos = function(username, password, project_id, survey_id, survey_event_id,
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
  
  #Get AWS s3 ID number
  get_s3_id = sprintf('query stationaryAcousticEventByIdQuery {
      stationaryAcousticEventById(id: "%s") {
        s3Id
      }
    }', survey_event_id)

  pbody = list(query = get_s3_id, operationName = 'stationaryAcousticEventByIdQuery')
  res = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res)
  
  # print(res[["status_code"]])
  
  if (length(content)==1){
    s3_id = content[["data"]][["stationaryAcousticEventById"]]
  }
  
  # Get photo names from GQL database
  get_keys = sprintf('query s3FileServiceListFilesQuery {
    s3FileServiceListFiles(
      bucket: "nabat-prod-project-files"
      keyPrefix: "%s/surveys/%s/deployment/%s/image/"
      ) {
        message
        objects {
          Key
        }
        success
      }
  }', project_id, survey_id, s3_id)
  
  token = get_nabat_gql_token(username, password)
  token = get_refresh_token(token)
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  
  pbody = list(query = get_keys, operationName = 's3FileServiceListFilesQuery')
  res = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res)
  
  # print(res[["status_code"]])
  # print(paste(length(content[["data"]][["s3FileServiceListFiles"]][["objects"]]), "photos for survey_event_id:", survey_event_id))
  
  if (length(content[["data"]][["s3FileServiceListFiles"]][["objects"]])!=0){
    # Pull out photo names from previous query
    keys = list()
    list_length = length(content[["data"]][["s3FileServiceListFiles"]][["objects"]])
    for (i in seq(1, list_length)){
      keys=append(keys, content[["data"]][["s3FileServiceListFiles"]][["objects"]][[i]]$Key)
    }
  
    # Make query for each photo based on its photo name
    queries = list()
    for(i in seq(1, length(keys))){
      queries = append(queries, 
        sprintf('query s3FileServiceDownloadFileQuery {
          s3FileServiceDownloadFile(
            bucket: "nabat-prod-project-files"
            key: "%s"
          ) {
            message
            s3PresignedUrl
            success
            }
          }', keys[[i]])
        )
      }
    
    # print(queries)
    
    # Query AWS database to download photos and store in "pics"
    pics = list()
    for(q in queries){
      token = get_refresh_token(token)
      tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
      headers = tkn_hdr$headers
      token   = tkn_hdr$token
      url     = tkn_hdr$url
      pbody = list(query = q, operationName = 's3FileServiceDownloadFileQuery')
      res = httr::POST(url, headers, body = pbody, encode='json')
      content   = httr::content(res)
      photo_url = content[["data"]][["s3FileServiceDownloadFile"]][["s3PresignedUrl"]]
      pics = append(pics, image_read(photo_url))
    }
  } else{
    print("No Photos")
  }
  return(pics)
}
