get_nabat_gql_token = function(
    username = NULL,
    password = NULL,
    branch = 'prod',
    url = NULL,
    aws_gql = NULL,
    aws_alb = NULL,
    docker = FALSE){
  
  # Prompts password input incase password isn't included in function call
  if (is.null(username)){
    username = rstudioapi::showPrompt(title = "Username",
                                      message = "Username", default = "")
  }
  if (is.null(password)){
    password = .rs.askForPassword('Password')
  }
  
  out = tryCatch({
    # Returns a message with username
    message(paste0("Logging into the NABat database as ", username))
    
    # Set URL based on branch
    if (is.null(url)) url = get_gql_url(branch)
    
    # Set headers for login
    if (docker){
      if(!is.null(aws_gql)){
        url = paste0(aws_alb, '/graphql')
        headers = httr::add_headers(host = aws_gql)
      }else {
        headers = httr::add_headers(Accept = "")
      }
    } else{
      headers = httr::add_headers(Accept = "")
    }
    
    # Username and password
    variables = paste0('{"l":{"userName" : "',username,'", "password" : "',
                       password,'"}}')
    # Mutation to get token
    query = 'mutation RRlogin($l:LoginInput!){
      login(input:$l){
      access_token,
      refresh_token,
      expires_in
      }
    }'
    # Finalize json request
    pbody = list(query = query, variables = variables,
                 operationName = 'RRlogin')
    # Query GQL API
    res = POST(url, headers, body = pbody, encode="json")
    # Remove variables with Password
    rm(password, variables, pbody)
    # Extract token
    content = content(res)
    error  = content$data$login$error
    bearer = content$data$login$access_token
    refresh_token = content$data$login$refresh_token
    
    if (res$status_code != 200){stop(paste0('Status code: ', res$status_code))}
    if (is.null(refresh_token)){stop('Error on login. Check Password/Username ')}
    
    access_token = strsplit(bearer, 'Bearer ')[[1]][2]
    message("Returning a GQL token for NABat.")
    expires = content$data$login$expires_in - (60 * 10)
    refresh_at_this = Sys.time() + expires
    return (list(refresh_token = refresh_token, access_token = access_token,
                 refresh_at = refresh_at_this))
  },
  # If it errors or refresh_token = NULL then function will fail
  error = function(cond) {
    message(cond)
    return(NULL)
  })
  return (out)
}