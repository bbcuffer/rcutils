#' Strip a live post url to minimum details
#'
#' Trim all the silly extra tags out of a live page post
#' url. The examples show a typical example, you only need
#' the pinned_post_urn.
#'
#' By default, will also reset the path of the link
#' from a given live page to the Verify topic
#' page (the old Reality Check one)
#'
#'
#'
#' @param url the live page post url
#' @param new_path a new live page or topic page to which
#'   you want to point the reader. Set to `NULL` if
#'   you want to leave as is.
#' @param goto Do you want to open that new page in a browser (Mac only) or
#'   get the new url back as a message
#' @export
#' @examples

#' url <- "https://www.bbc.co.uk/news/live/business-66861406?ns_mchannel=social&ns_source=twitter&ns_campaign=bbc_live&ns_linkname=650aa0c63fc96e28602bd790%26The%20surprising%20reason%20food%20inflation%20is%20slightly%20overstated...%262023-09-20T08%3A04%3A03.589Z&ns_fee=0&pinned_post_locator=urn:asset:4fd2ba06-a0d5-4784-8825-2f5b0024f806&pinned_post_asset_id=650aa0c63fc96e28602bd790&pinned_post_type=share"
#' retag(url, goto = FALSE)
#' retag(url, new_path = NULL, goto = FALSE)
#' retag(url)
#' retag(url, new_path = NULL)
retag <- function(url = clipr::read_clip(), new_path = "/news/reality_check", goto = TRUE) {
  v01 <- httr2::url_parse(url)
  v02 <- v01
  orig_query <- v01$query
  ## Sometimes there are multiple "pinned_post_locators"
  orig_locators <- names(orig_query) == "pinned_post_locator"
  n_queries <- length(orig_query)
  n_locators <- sum(orig_locators)

  if (n_locators == 1) {
    v02$query <- orig_query[orig_locators]
  } else { # pick the last mention
    the_index <- (1:n_queries)[orig_locators][n_locators]
    v02$query <- orig_query[the_index]
  }

  if (!is.null(new_path)) v02$path <- new_path
  new_url <- httr2::url_build(v02)
  if (goto) {
    system2("open", new_url)
  } else {
    message(new_url)
  }
  invisible(new_url)
}

#' Bitly a link
#'
#' @export
bitme <- function(url, key = Sys.getenv("BITLY_KEY")) {
  req <- httr2::request("https://api-ssl.bitly.com/v4/shorten") |>
    httr2::req_headers("Authorization" = paste("Bearer", key)) |>
    httr2::req_body_json(list(long_url = url))

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  return(resp$link)
}

#' Create a bitlink to Verify home of LP entry
#' 
#' @export
rcbitlink <- function(url = clipr::read_clip()) {
  message(url)
  new_link <- retag(url = url)
  bitlink <- bitme(new_link)
  message(bitlink)
  clipr::write_clip(bitlink)
  invisible(bitlink)
}

#' Create a bitlink to a LP entry
#' 
#' @export
bitlink <- function(url = clipr::read_clip()) {
  message(url)
  new_link <- retag(url = url, new_path = NULL)
  bitlink <- bitme(new_link)
  message(bitlink)
  clipr::write_clip(bitlink)
  invisible(bitlink)
}