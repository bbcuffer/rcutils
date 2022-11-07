#' Arrange alignment and save BBC ggplot chart as powerpoint
#'
#' Running this function will save your plot into powerpoint.
#' You need the packages rvg, officer and bbplot2
#'
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param pptx_template The location of the powerpoint template that
#'   officer will use to build your powerpoint slides. Best to stick with
#'   the default
#' @return the filename of the new powerpoint slide
#' @export
pptx <- function(plot_name, source_name=NULL,
                 save_filepath = "~/Downloads/tmp.pptx",
                 pptx_template = file.path(system.file("data", package = 'rcutils'),"layouts.pptx"),
                 ...){
    plot01 <- bbplot2::finalise_plot(plot_name, source_name, ...)
    plot02 <- officer::read_pptx(pptx_template) %>%
        officer::remove_slide() %>%
        officer::add_slide(layout = "Full_Frame_Content",
                           master = "Office Theme") %>%
        rvg::ph_with_vg(code = print(plot01), type = "body")
    print(plot02, target = save_filepath)
}
