% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view.R
\name{view}
\alias{view}
\title{Push data frame df to spreadsheet and (on a mac only) view using system default (e.g. Excel)}
\usage{
view(df, what = c("csv", "xlsx"), where = Sys.getenv("TMPDIR"))
}
\arguments{
\item{df}{the data frame to be saved and viewed}

\item{what}{the format to save the df in (supports csv and xlsx)}

\item{where}{the folder to which the data will be saved}
}
\value{
a character giving the full path for the sheet
}
\description{
Push data frame df to spreadsheet and (on a mac only) view using system default (e.g. Excel)
}
