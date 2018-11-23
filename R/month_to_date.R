#' Convert a string containing month-and-year to a lubridate date
#'
#' @param months A character vector in a month/year format that lubridate might recognise
#' @param when A flag determining whether you want the get the 1st ("beginning"),
#' 15th ("middle") or last ("end") day of the month
#' @param format A string for the lubridate function that you want to turn your character
#' string into a date (should always start with "d")
#'
#'
#' @return a vector of dates
#' @examples
#' month_to_date("jun2017")
#' month_to_date("jun2017", "e")
#' month_to_date("2017/jun", format="dym")
month_to_date <- function(months, when=c("beginning", "middle", "end"), format="dmy") {
    require(lubridate)
    x01 <- paste("01-", months, sep="")
    when <- match.arg(when)
    x02 <- do.call(format, list(x01))
    if(when=="middle") x02 <- x02 + weeks(2)
    else if(when=="end") x02 <- x02 + months(1) - days(1)
    x02
}
