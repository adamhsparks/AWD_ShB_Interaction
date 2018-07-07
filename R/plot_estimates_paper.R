
#' @title Plot MCMCglmm tiller estimates for publication
#'
#' @description Plot estimates of posterior distributions and credible
#'  intervals of \pkg{MCMCglmm} outputs sans intercept for publication figure
#'
#' @name plot_tiller_estimates_paper
#' @param x An\code{\link[coda]{mcmc.list}} object
#' @param main_title Main title for the plot
#'
#' @examples \dontrun{
#' # Plot posterior means and credible intervals
#' plot_estimates(m1)
#'}
#'
#' @author Titus von der Malsburg, \email{malsburg@uni-potsdam.de}
#'
#' @export
plot_tiller_estimates_paper <- function(x, main_title = "") {
  if (class(x) != "summary.mcmc") {
    x <- summary(x)
  }
  n <- dim(x$statistics)[1]
  graphics::par(mar = c(2, 7, 4, 1))
  graphics::plot(
    x$statistics[, 1],
    n:1,
    yaxt = "n",
    ylab = "",
    xlim = range(x$quantiles) * 1.2,
    pch = 19,
    main = main_title,
    bty = "L"
  )

  # extract treatment names and clean them up for the graph
  row.names(x$statistics) <- gsub(pattern = "WMGT", "", row.names(x$statistics))
  row.names(x$statistics) <- gsub(pattern = "NRTE", "", row.names(x$statistics))

  graphics::axis(side = 2, at = n:1, row.names(x$statistics), las = 2)
  graphics::arrows(x$quantiles[, 1], n:1, x$quantiles[, 5], n:1, code = 0)
  graphics::abline(v = 0, lty = 2)
}


#' @title Plot MCMCglmm leaf estimates for publication
#'
#' @description Plot estimates of posterior distributions and credible
#'  intervals of \pkg{MCMCglmm} outputs sans intercept for publication figure
#'
#' @name plot_leaf_estimates_paper
#' @param x An\code{\link[coda]{mcmc.list}} object
#' @param main_title Main title for the plot
#'
#' @examples \dontrun{
#' # Plot posterior means and credible intervals
#' plot_estimates(m1)
#'}
#'
#' @author Titus von der Malsburg, \email{malsburg@uni-potsdam.de}
#'
#' @export
plot_leaf_estimates_paper <- function(x, main_title = "") {
  if (class(x) != "summary.mcmc") {
    x <- summary(x)
  }
  n <- dim(x$statistics)[1]
  graphics::par(mar = c(2, 15, 3, 1))
  graphics::plot(
    x$statistics[, 1],
    n:1,
    yaxt = "n",
    ylab = "",
    xlim = range(x$quantiles) * 1.2,
    pch = 19,
    main = main_title,
    bty = "L"
  )

  # extract treatment names and clean them up for the graph
  row.names(x$statistics) <- gsub(pattern = "WMGT", "", row.names(x$statistics))
  row.names(x$statistics) <- gsub(pattern = "NRTE", "", row.names(x$statistics))

  graphics::axis(side = 2, at = n:1, row.names(x$statistics), las = 2)
  graphics::arrows(x$quantiles[, 1], n:1, x$quantiles[, 5], n:1, code = 0)
  graphics::abline(v = 0, lty = 2)
}

