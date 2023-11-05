#' BiBTeX ref for an ONS publication
#' 
#' Produce a BibTeX citation for an ONS publication. 
#' @param tag The citation label to be used to refer to the
#' publication in (e.g.) .qmds
#' @param url the webpage for the publication. Default is to take
#'  it from the clipboard. 
#' @export
#' @examples
#'
#' the_url <- "https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/bulletins/crimeinenglandandwales/yearendingjune2023"
#' ons_ref(tag = "the_label", url = the_url)


bib_ons <- function(tag = "tag", url = clipr::read_clip() ){

  message("Reading url: \n", url)

  page <- rvest::read_html(url)

  title <- page |>
    rvest::html_elements("h1") |>
    rvest::html_text()

  pars <- page |> rvest::html_elements("p")

  date <- pars |>
    rvest::html_text() |>
    stringr::str_subset("Release date:") |>
    lubridate::dmy()

  # author <- pars |>
  #   rvest::html_text() |>
  #   stringr::str_subset("Contact:") |>
  #   str_replace_all(c("Contact: Email " = "", "\\n" = ""))
  # author_names <- str_split(author, " ")
  # given_name <- author_names[[1]][1]
  # family_name <- author_names[[1]][2]

  b <- glue::glue('@misc{{ {tag}',
    '  title = "{title}"',
    '  author = "{{ONS}}"',
    '  howpublished = "\\url{{{url}}}"',
    '  month = "{lubridate::month(date, lab=TRUE)}"',
    "  year = {lubridate::year(date)}",
    "}}",
    .sep = ",\n  ",
  ) |>
    stringr::str_replace("\\},$", "\\}")

  clipr::write_clip(b)

  message("Adding to clipboard: \n", b)
}
