#' Look at a dataframe using a spreadsheet package. Mac only.
#'
#' You can't edit the dataframe - it just writes the dataframe to a tempfile which it then opens. 
#'
#' @param df the data frame
#' @param app the program used to show it. Defaults to Apple's Numbers. Excel also an option.
#'
#' @return nothing, just opens a spreadsheet showing the dataframe. 
#'
#' @examples
#'
#' demo <- tibble(alpha=letters, num=rnorm(26))
#' views(demo)
#' views(demo, "Excel")
#' views(demo, "E")
#' @export
views <- function(df, app=c("Numbers", "Excel")){
  fn <- tempfile(fileext=".csv")
  write_csv(df, fn)
  app01 <- match.arg(app)  
  checkscel(fn, app01)
}

#' fix the spaces and brackets in Dropbox (BBC) and Visual Journalism
#' so that they don't break the command line arguments of system2()
#'
#' @param path the file path to be made safe
#' @export
safe_fn <- function(path){
  ## fix the spaces and brackets in Dropbox (BBC) and Visual Journalism
  ## so that they don't break the command line arguments of system2()
  str_replace_all(path,
    c(" "="\\\\ ",
      "\\("="\\\\\\(",
      "\\)"="\\\\\\)"))
}

#' open a spreadsheet
#'
#' @param path where it is
#' @param app how to open it: Excel or Numbers
#' @export
checkscel <- function(path, app=c("Excel", "Numbers")){
  app01 <- match.arg(app)
  app02 <- gsub("Excel", "Microsoft Excel", app01)
  fn <- safe_fn(path)
  arg <- paste0('-a "', app02, '" ', fn)
  system2("open", arg)
}
