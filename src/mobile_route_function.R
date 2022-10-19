get_mobile_route <- function(username, password, grts_id, project_id,
                             branch = 'prod',
                             url = NULL,
                             aws_gql = NULL,
                             aws_alb = NULL,
                             docker = FALSE){
  # Get headers for token
  token = get_nabat_gql_token_edited(username, password)
  token = get_refresh_token(token)
  tkn_hdr = get_token_headers(token, branch, url, aws_gql, aws_alb, docker)
  headers = tkn_hdr$headers
  token   = tkn_hdr$token
  url     = tkn_hdr$url
  
  pbody = list(
    operationName = 'allVwEventGeometries',
    # variables = sprintf('"grtsId":%s,"surveyTypeId":8,"projectId":%s', grts_id, project_id),
    variables = sprintf('{"grtsId":%s,"surveyTypeId":8,"projectId":%s}', grts_id, project_id),
    query = 'query allVwEventGeometries($grtsId: Int!, $surveyTypeId: Int!, $projectId: Int!) {\n  allVwEventGeometries(filter: {grtsCellId: {equalTo: $grtsId}, surveyTypeId: {equalTo: $surveyTypeId}, projectId: {equalTo: $projectId}}) {\n    nodes {\n      id\n      name\n      description\n      projectId\n      surveyTypeId\n      createdDate\n      grtsCellId\n      grtsCellIds\n      lengthInCell\n      fullLength\n      areaInCell\n      geomWithinCell {\n        geojson\n        __typename\n      }\n      geom {\n        geojson\n        __typename\n      }\n      __typename\n    }\n    __typename\n  }\n}\n'
  )
  
  res = httr::POST(url, headers, body = pbody, encode='json')
  content   = httr::content(res)
  
  transect_info <- list(
    survey_id = content[["data"]][["allVwEventGeometries"]][["nodes"]][[1]][["id"]],
    created_date = content[["data"]][["allVwEventGeometries"]][["nodes"]][[1]][["createdDate"]],
    route_length = content[["data"]][["allVwEventGeometries"]][["nodes"]][[1]][["fullLength"]],
    length_in_cell = content[["data"]][["allVwEventGeometries"]][["nodes"]][[1]][["lengthInCell"]],
    target_grts = content[["data"]][["allVwEventGeometries"]][["nodes"]][[1]][["grtsCellId"]],
    all_grts = content[["data"]][["allVwEventGeometries"]][["nodes"]][[1]][["grtsCellIds"]],
    transect = content[["data"]][["allVwEventGeometries"]][["nodes"]][[1]][["geom"]][["geojson"]]
    )
  
  return(transect_info)
}