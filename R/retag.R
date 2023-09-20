#' Strip a live post url to minimum details
#' 
#' @param url the live page post url
#' @param new_path a new live page or topic page to which 
#'   you want to point the reader. Set to `NULL` if 
#'   you want to leave as is. 
#' @param goto Do you want to open that new page in a browser (Mac only) or
#'   get the new url back as a message
#' @export 
#' @examples 
#' 

#' url <- "https://www.bbc.co.uk/news/live/business-66861406?ns_mchannel=social&ns_source=twitter&ns_campaign=bbc_live&ns_linkname=650aa0c63fc96e28602bd790%26The%20surprising%20reason%20food%20inflation%20is%20slightly%20overstated...%262023-09-20T08%3A04%3A03.589Z&ns_fee=0&pinned_post_locator=urn:asset:4fd2ba06-a0d5-4784-8825-2f5b0024f806&pinned_post_asset_id=650aa0c63fc96e28602bd790&pinned_post_type=share"
#' retag(url, goto = FALSE)
#' retag(url, new_path = NULL, goto = FALSE)
#' retag(url)
#' retag(url, new_path = NULL)
sretag <- function(url, new_path = "/news/reality_check", goto = TRUE) {
  v01 <- httr2::url_parse(url)
  v02 <- v01
  v02$query <- list(pinned_post_locator = v01$query$pinned_post_locator)
  if(!is.null(new_path)) v02$path <- new_path
  new_url <- httr2::url_build(v02)
  if(goto) system2("open", new_url)
  else message(new_url) 
  invisible(new_url)
}