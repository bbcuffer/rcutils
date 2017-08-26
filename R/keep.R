#' @title The Keep Function
#'
#' @description
#' This function is designed to keep a given list of variables (as opposed to the rm function, which is designed to remove a list of variables)
#'
#' @param ... Variables that should stay in the environment
#' @param x A character vector of variables which you wish to keep
#'
#' @details
#' The function uses the \code{ls} command to find the variables that are defined in the parent environment. It then deletes all variables that are not given as an argument.
#'
#' @author Michael Floren
#'
#  For whatever reason, I CANNOT get this link to the "rm" function to actually create a hyperlink
#' @seealso \code{\link[base]{rm}}
#'
keep <- function(..., x = c())
  {

      if (length(x) > 0)
        {
              if (!is.character(x))
                      stop("x must contain character vector")

                  L <- ls(name = parent.frame())
                  rm(list = L[!L %in% x], pos = parent.frame())

                  return(invisible(ls(name = parent.frame())))
            }

        dots <- match.call(expand.dots = FALSE)$...

        if (length(dots) && !all(sapply(dots, function(x) is.symbol(x) ||
                                                                          is.character(x))))
              stop("... must contain names or character strings")

        names <- sapply(dots, as.character)
        L <- ls(name = parent.frame())
        rm(list = L[!L %in% names], pos = parent.frame())

        return(invisible(ls(name = parent.frame())))

    }
