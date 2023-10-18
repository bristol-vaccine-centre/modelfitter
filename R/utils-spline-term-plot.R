# based on https://cran.r-project.org/web/packages/survival/vignettes/splines.pdf from Terry Therneau

#' Spline term marginal effects plot
#'
#' @param coxmodel an output of a coxph model
#' @param var_name a variable that is involved in a spline term
#' @param xlab x axis label
#' @param max_y maximium hazard ratio to display on y axis. Inferred from the
#'   central estimates if missing, which will most likely cut off confidence
#'   intervals
#' @param n_breaks The number of divisions on the y axis
#'
#' @return a ggplot
#' @export
spline_term_plot = function(coxmodel, var_name, xlab=var_name, max_y = NULL, n_breaks=7) {
  d <- stats::termplot(coxmodel, se = TRUE, plot = FALSE)
  dat <- d[[var_name]]

  # decide on position of the breaks, with odd number of evenly spaced breaks on
  # log scale.
  if (is.null(max_y)) max_y = 2^(ceiling(max(abs(dat$y))/log(2)))
  ylim = c(1/max_y,max_y)
  tmp = (n_breaks-1)%/%2
  breaks = round(exp(log(max_y)/tmp*(-tmp:tmp)),digits = 2)


  p = ggplot2::ggplot(dat, ggplot2::aes(x, exp(y))) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = exp(y - 1.96 * se), ymax = exp(y + 1.96 * se)), fill = "lightgrey", alpha=0.2) +
    ggplot2::geom_line(color = "black") +
    ggplot2::scale_y_continuous(trans="log", breaks = breaks) +
    ggplot2::coord_cartesian(ylim=ylim)+
    ggplot2::labs(
      x = xlab,
      y = "Relative hazard rate")+
    ggplot2::geom_hline(yintercept = 1, colour="blue", alpha=0.5)

  return(p)
}
