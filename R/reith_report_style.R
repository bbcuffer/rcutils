#' Reith-ish styling for figures in .Rmd/.qmd reports
#'
#' @param ... arguments being passed to [ggplot2::theme()]
#' @export
reith_report_style <- function (...) 
{
    theme(text = element_text(family = "BBC Reith Sans", color = "#222222", 
        size = 14), line = element_line(colour = "#222222", size = 0.82), 
        plot.title = element_text(family = "BBC Reith Serif Medium", 
            size = rel(28/20), hjust = 0, lineheight = 0.95), plot.subtitle = element_text(hjust = 0, 
            size = rel(22/20), lineheight = 0.95, margin = unit(c(10, 
              8, 8, 8), "pt")), legend.position = "top", legend.justification = "left",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size=rel(16/20), hjust=0), 
        legend.margin = margin(t = 5.5, r = 0, b = 5.5, l = 0), 
        legend.text.align = 0, legend.background = element_blank(), 
        legend.title = element_blank(), legend.key = element_blank(), 
        legend.text = element_text(margin = margin(l = 5.5, r = 5.5)), 
        axis.title = element_blank(), axis.text = element_text(colour = "#6E6E73", 
            size = rel(1)), axis.text.x = element_text(margin = margin(8, 
            b = 10)), axis.ticks.x = ggplot2::element_line(colour = "#222222", 
            size = 0.45), axis.ticks.y = ggplot2::element_blank(), 
        axis.ticks.length = unit(8, "bigpts"), axis.line = element_line(size = 0.92, 
            colour = "#222222"), axis.line.y = element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major.y = element_line(color = "#cccccc", 
            size = 0.45), panel.grid.major.x = element_blank(), 
        panel.background = element_blank(), plot.margin = unit(c(16, 
            16, 0, 8), "pt"), strip.background = element_rect(fill = "white", 
              colour = "white"),
        strip.text = element_text(hjust = 0, 
          face = "bold", size=rel(16/20), 
          ## size = 20,
          margin = margin(c(2, 2, 2, 0))),
        panel.spacing.y = unit(2, "pt"),
        panel.spacing.x = unit(2, "pt"), complete = T, ...)
}
