
#' @title Plot MCMCglmm estimates for publication
#'
#' @description Plot estimates of posterior distributions and credible
#'  intervals of MCMCglmm outputs sans intercept for publication figure
#'
#' @name plot_estimates_paper
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
plot_estimates_paper <- function(x) {
  par(mfrow = c(1, 1))
  if (class(x) != "summary.mcmc") {
    x <- summary(x)
  }
  z <- x$statistics[-1, ]
  zz <- x$quantiles[-1, ]
  n <- dim(z)[1]
  par(mar = c(2, 7, 3, 1))
  plot(
    z[, 1],
    n:1,
    yaxt = "n",
    ylab = "",
    xlim = range(x$quantiles) * 1.2,
    pch = 19,
    main = "",
    bty = "L"
  )
  axis(side = 2, at = n:1, rownames(z), las = 2)
  arrows(zz[, 1], n:1, zz[, 5], n:1, code = 0)
  abline(v = 0, lty = 2)
}
