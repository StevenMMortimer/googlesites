#' Retry a Function With Exponential Backoff
#' 
#' This function is an internal helper that will retry a function 
#' will exponential wait times in case there are issues with the API
#'
#' @keywords internal
#' @param expr an expression to be evaluated with exponential backoff
#' @param n an integer or number indicating the number of times to execute the function
#' before completely failing
#' @param verbose a \code{logical} indicating whether to print messages when invoking
#' the retry attempts
#' @return the result of the evaluated expression
#' @examples
#' \dontrun{
#' # use with an API call that you can't afford to error because of intermittent uptime
#' myFun <- function(){
#'  x <- runif(1)
#'  if(x < .6){
#'    stop('less than 3/5')
#'  }
#'  return(x)
#' }
#' exponential_backoff_retry(expr = myFun(), verbose = TRUE, n = 5)
#' }
exponential_backoff_retry <- function(expr, n = 5, verbose = FALSE){
  
  for (i in seq_len(n)) {
    
    result <- try(eval.parent(substitute(expr)), silent = FALSE)
    
    if (inherits(result, "try-error")){
      
      backoff <- runif(n = 1, min = 0, max = 2 ^ i - 1)
      if(verbose){
        message("Error on attempt ", i,
                ", will retry after a back off of ", round(backoff, 2),
                " seconds.")
      }
      Sys.sleep(backoff)
      
    } else {
      if(verbose){
        message("Succeed after ", i, " attempts")
      }
      break 
    }
  }
  
  if (inherits(result, "try-error")) {
    stop("Failed after max attempts")
  } 
  
  return(result)
}

#' sites_GET
#' 
#' @keywords internal
#' @importFrom httr GET stop_for_status content
#' @importFrom xml2 read_xml
sites_GET <- function(url){
  
  req <- exponential_backoff_retry(GET(url, googleAuthR:::get_google_token()))
  stop_for_status(req)

  req_xml <- read_xml(content(req, as = "text", encoding = "UTF-8"))
  return(req_xml)
}

#' sites_POST
#' 
#' @keywords internal
#' @importFrom httr POST add_headers stop_for_status content
#' @importFrom xml2 read_xml
sites_POST <- function(url, 
                       body,
                       content_type,
                       gdata_version = getOption("googlesites.gdata_version"), 
                       host = getOption("googlesites.host")){

  # taken from http://stackoverflow.com/questions/31080363/how-to-post-multipart-related-content-with-httr-for-google-drive-api
  # need to update metadata and media content simultaneously in the multipart/related type
  req <- exponential_backoff_retry(
    POST(url,
         googleAuthR:::get_google_token(),
         add_headers(.headers = c(`Content-Type` = content_type,
                                  `GData-Version` = gdata_version,
                                  `Host` = host)),
         body = body)
  )
  stop_for_status(req)
  
  req_xml <- read_xml(content(req, as = "text", encoding = "UTF-8"))
  return(req_xml)
}

#' sites_PUT
#' 
#' @keywords internal
#' @importFrom httr PUT add_headers stop_for_status content
#' @importFrom xml2 read_xml
sites_PUT <- function(url, 
                      body, 
                      content_type,
                      gdata_version = getOption("googlesites.gdata_version"), 
                      host = getOption("googlesites.host")){
  
  req <- exponential_backoff_retry(
    PUT(url,
        googleAuthR:::get_google_token(),
        add_headers(.headers = c(`Content-Type` = content_type, 
                                 `GData-Version` = gdata_version,
                                 `Host` = host,
                                 `If-Match` = '*')),
        body = body)
  )
  stop_for_status(req)
  
  req_xml <- read_xml(content(req, as = "text", encoding = "UTF-8"))
  return(req_xml)
}

#' sites_DELETE
#' 
#' @keywords internal
#' @importFrom httr DELETE stop_for_status content
#' @importFrom xml2 read_xml
sites_DELETE <- function(url, 
                         gdata_version = getOption("googlesites.gdata_version"), 
                         host = getOption("googlesites.host")){
  
  req <- exponential_backoff_retry(
    DELETE(url,
           googleAuthR:::get_google_token(),
           add_headers(.headers = c(`GData-Version` = gdata_version,
                                    `Host` = host,
                                    `If-Match` = '*')))
  )
  stop_for_status(req)

  return(content(req, as = "text", encoding = "UTF-8"))
}
