get_crimes_la <- function(code = 'E09000019', type = 'burglary',
                          from = '2018-04', to = '2018-06') {
    require(tidyverse)
    require(httr)
    require(jsonlite)
  lapply(X = unique(format(seq(lubridate::ymd(paste0(from, '-01')),
                               lubridate::ymd(paste0(to, '-01')),
                               lubridate::month(1)), '%Y-%m')),
         FUN = function(code = code, type = type, date) {
           shapefile <- read_csv('~/Dropbox (BBC)/Visual Journalism/Data/2018/rmaps.shapefiles/uk_la_latlong/uk_la.csv') %>%
             filter(id == code)
           shapefile$coords <- paste0(round(shapefile$lat,2),',', round(shapefile$long,2))
           coords <- paste0(shapefile$coords, collapse = ':')
           url <- paste0('https://data.police.uk/api/crimes-street/',
                         type,
                         '?poly=',
                         coords,
                         '&date=',
                         date)
           fromJSON(content(GET(url), "text"), flatten = T)
         },
         code = code,
         type = type) %>%
    bind_rows()
}

## richmond_robberies <- get_crimes_la('E09000027', 'robbery', '2018-04', '2018-06')
## barking_burglaries <- get_crimes_la('E09000002', 'burglary', '2018-01', '2018-07')


# Crime types:
# - all-crime
# - anti-social-behaviour
# - burglary
# - bicycle-theft
# - criminal-damage-arson
# - drugs
# - other-theft
# - possession-of-weapons
# - public-order
# - robbery
# - shoplifting
# - theft-from-the-person
# - vehicle-crime
# - violent-crime
# - other-crime

