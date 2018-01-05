## Push data frame df to spreadsheet and view using system default (e.g. Excel)

view <- function(df, what=c("csv", "xlsx"), where=Sys.getenv("TMPDIR")){
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
    system2("open", path)
    print(paste("Opening", path))
}
