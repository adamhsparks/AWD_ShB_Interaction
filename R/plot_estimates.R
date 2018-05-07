
#' @title Plot MCMCglmm estimates
#'
#' @description Plot estimates of posterior distributions and credible
#'  intervals of MCMCglmm outputs
#'
#' @name plot_estimates
#' @param x An\code{\link[coda]{mcmc.list}} object
#'
#' @examples \dontrun{
#' # Plot posterior means and credible intervals
#' plot_estimates(m1)
#'}
#'
#' @author Titus von der Malsburg, \email{malsburg@uni-potsdam.de}
#'
#' @export
plot_estimates <- function(x) {
  par(mfrow = c(1, 1))
  if (class(x) != "summary.mcmc") {
    x <- coda:::summary.mcmc.list(x)
  }
  n <- dim(x$statistics)[1]
  par(mar = c(2, 15, 3, 1),
      cex = 0.5)
  plot(
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

  axis(side = 2, at = n:1, rownames(x$statistics), las = 2)
  arrows(x$quantiles[, 1], n:1, x$quantiles[, 5], n:1, code = 0)
  abline(v = 0, lty = 2)
}
