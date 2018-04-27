
#' @title MCMCglmm results table
#'
#' @description Generate a table of MCMCglmm results
#'
#' @export
#' @name mcmc_table
#' @param x An MCMCglmm model
#'
#' @examples \dontrun{
#' mcmc_table(MCMC_model)
#' }
#'
#' @author Adam H Sparks, \email{adamhsparks@gmail.com}

mcmc_table <- function(x) {
  sols <- summary(x)$solutions[-1, -4]

  DV <- c(deparse(substitute(x)), rep(" ", nrow(sols) - 1))
  Predictors <- row.names(sols)
  DIC <- c(round(x$DIC, 2), rep(" ", nrow(sols) - 1))

  y <- data.frame(DV, Predictors, sols, DIC)
  names(y) <- c("Dependent Variable",
                "Predictors",
                "Post Mean",
                "Lower 95% CI",
                "Upper 95% CI",
                "P MCMC",
                "DIC")
  row.names(y) <- NULL
  return(y)
}
