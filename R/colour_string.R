#' Prepare multicolour string for ggtext
#'
#' Writes html tags around phrases in a string to show those
#' phrases in a different colour
#'
#' @param original the string of text (e.g. a plot title or annotation)
#' @param to_colour a named vector of colours. The names of each colour are
#' the words in the text that will be drawn that colour.
#' @return the string with html tags to add colour to the specified words
#' @export


colour_string <- function(original, to_colour=NULL){
    words <- names(to_colour)
    new_colours <- unname(to_colour)
    w01 <- paste0("\\b", words, "\\b")
    new_words <- paste0("<span style='color:", new_colours, ";'>",
                        words, "</span>")
    subst <- setNames(new_words, w01)
    new_text <- stringr::str_replace_all(original, subst)
    return(new_text)
}
