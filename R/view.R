#' Push data frame df to spreadsheet and (on a mac only) view using system default (e.g. Excel)
#' @param df the data frame to be saved and viewed
#' @param what the format to save the df in (supports csv and xlsx)
#' @param where the folder to which the data will be saved
#' @return a character giving the full path for the sheet
view <- function(df, what=c("csv", "xlsx"), where=Sys.getenv("TMPDIR")){
    require(readr)
    require(readxl)
    what <- match.arg(what)
    fn <- gsub("[^A-Za-z0-9_-]", "", deparse(substitute(df)))
    name <- paste0(fn, format(Sys.time(), "-%Y-%m-%d-%H-%M-%S."), what)
    path <- file.path(where, name)
    ## print(paste("Creating", path))
    switch(what,
           csv = readr::write_csv(df, path, na=""),
           xlsx = xlsx::write.xlsx(df, path)
           )
    ## write.csv(df, path)
    if(get_os() == "osx") {
        system2("open", path)
        print(paste("Opening", path))
    }
    invisible(path)
}
