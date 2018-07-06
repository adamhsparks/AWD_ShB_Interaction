
#' @title Plot MCMCglmm estimates
#'
#' @description Plot estimates of posterior distributions and credible
#'  intervals of \pkg{MCMCglmm} outputs
#'
#' @name plot_estimates
#' @param x An\code{\link[coda]{mcmc.list}} object
#'
#' @examples \dontrun{
#' # Plot posterior means and credible intervals
#' plot_tiller_estimates(m1)
#'}
#'
#' @author Titus von der Malsburg, \email{malsburg@uni-potsdam.de}
#'
#' @export
plot_tiller_estimates <- function(x) {
  graphics::par(mfrow = c(1, 1))
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
    main = "Posterior means and 95% credible intervals",
    bty = "L"
  )

  # extract treatment names and clean them up for the graph
  row.names(x$statistics) <- gsub(pattern = "WMGT", "", row.names(x$statistics))
  row.names(x$statistics) <- gsub(pattern = "NRTE", "", row.names(x$statistics))

  graphics::axis(side = 2, at = n:1, rownames(x$statistics), las = 2)
  graphics::arrows(x$quantiles[, 1], n:1, x$quantiles[, 5], n:1, code = 0)
  graphics::abline(v = 0, lty = 2)
}


#' @title Plot MCMCglmm estimates
#'
#' @description Plot estimates of posterior distributions and credible
#'  intervals of \pkg{MCMCglmm} outputs
#'
#' @name plot_estimates
#' @param x An\code{\link[coda]{mcmc.list}} object
#'
#' @examples \dontrun{
#' # Plot posterior means and credible intervals
#' plot_leaf_estimates(m1)
#'}
#'
#' @author Titus von der Malsburg, \email{malsburg@uni-potsdam.de}
#'
#' @export
plot_leaf_estimates <- function(x) {
  graphics::par(mfrow = c(1, 1))
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
    main = "Posterior means and 95% credible intervals",
    bty = "L"
  )

  # extract treatment names and clean them up for the graph
  row.names(x$statistics) <- gsub(pattern = "WMGT", "", row.names(x$statistics))
  row.names(x$statistics) <- gsub(pattern = "NRTE", "", row.names(x$statistics))

  graphics::axis(side = 2, at = n:1, rownames(x$statistics), las = 2)
  graphics::arrows(x$quantiles[, 1], n:1, x$quantiles[, 5], n:1, code = 0)
  graphics::abline(v = 0, lty = 2)
}

