#' Get the variable names and code/decode pairs for an OECD dataset
#'
#' @param dataset the name of the dataset on the OECD data system. For a list of
#' OECD datasets, use OECD::get_datasets()
#'
#' @examples
#' oecd_structure("NAAG")
#' oecd_structure("SNA_TABLE8A")
#' @export
oecd_structure <- function(dataset){
    url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/", 
        dataset)
    data_structure <- readsdmx::read_sdmx(url)
    data_structure
}
