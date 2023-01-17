#' Get the variable names and code/decode pairs for an OECD dataset
#'
#' @param dataset the name of the dataset on the OECD data system. For a list of
#' OECD datasets, use OECD::get_datasets()
#' @param fancy do I want the output to come as one big data frame (`fancy=F`) or as
#' a list of dataframe, with each dataframe tailored to one part of the structure
#'
#' 
#'
#' @examples
#' oecd_structure("NAAG")
#' oecd_structure("SNA_TABLE8A")
#' oecd_structure("SNA_TABL8A", fancy=F)
#' @export
oecd_structure <- function(dataset, fancy=T){
    url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", 
        dataset)
    data_structure <- readsdmx::read_sdmx(url)
    if(! fancy) return(data_structure) ## Basic data frame 
  else { ## Return a list of slightly-tidied-up data frames
    ds2 <- data_structure %>%
      as_tibble %>%
      select(-id, -agencyID, -fr, -fr_description) %>%
      relocate(en_description, .after=last_col()) %>%
      split(~en, drop=T)
    return(ds2) 
  }
}
