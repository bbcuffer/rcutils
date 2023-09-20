#' calculate the centre and width/height of a data range.
#'
#' Used to help make tiles.
#'
#' @param df the data frame
#' @param x_var,y_var the variables to be summarised
#' @param xstretch,ystretch how much extra to expand the width and height
#' of the tiles. Set to 1 for no expansion. NB expanding the data
#' range might expand  the range of your chart.
#' @param add_animation_vars add a selection of variables to aid tweening?
#' 
#'
#' @examples
#' data <- data.frame(x1 = runif(10), y2=runif(10))
#' tiles <- make_tiles(data, x1, y2, ystretch=1, add=FALSE)
#'
#' library(ggplot2)
#' ggplot(data, aes(x=x1, y=y2)) +
#'  geom_tile(data=tiles, aes(width=width, height=height),
#'    colour="red", fill=NA) + 
#'  geom_point()
#'
#' make_tiles(data, x1, y2)
#' @export 
make_tiles <- function(df, x_var, y_var, xstretch=1.2, ystretch=2,
                       add_animation_vars=TRUE){

  get_midpoint <- function(z) mean(range(z))
  get_length <- function(z) diff(range(as.numeric(z))) 
  
  ## Make tiles that cover the range of the data df$x and df$y
  ## tiles are defined by a central point and a width and height. 
  d01 <- 
    dplyr::summarise(df,
      width = get_length( {{x_var}} ) * xstretch, 
      height = get_length( {{y_var}} * ystretch),
      ## Order of these operations      
      ## is important. If the
      ## across statement comes first, then summarise doesn't know
      ## (e.g.) which x_var's length to get (the original data
      ## or the midpoints) when doing get_length(). 
      dplyr::across( c( {{x_var}}, {{y_var}} ), get_midpoint )
    ) |>
    dplyr::relocate(width, height, .after=tidyselect::last_col())

  if(add_animation_vars){
    d02 <- d01  |> 
    dplyr::mutate(
      ## create variables for animation
      state = dplyr::row_number(), # for transition_state()
      appear = state * 2, # for transition_components()
      alpha = "show" # can be used for colouring or hiding
    )

  ## Create another version of the tiles if you want to
  ## animate their entrance.
  ## This version has tiny tiles that live just below the
  ## main ones and can have a different alpha value.
  ## You can then transition from one state to the next in
  ## order to bring them in. 
  d00 <- 
    dplyr::mutate(d02, state = state - 1, 
      {{y_var}} := {{y_var}} - height/(2 - 0.02), # just outside the range of the box
      height = height * 0.05,
      alpha = "hide",
      appear = appear - 1) 

    return(dplyr::bind_rows(d00, d02))
  }
  else return(d01)
  
}
