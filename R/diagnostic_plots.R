
#' @title MCMCglmm diagnostic plots
#'
#' @description Plot diagnostic graphs from MCMCglmm outputs
#'
#' @export
#' @name plot_mcmc_glmm
#' @param d A melted data.frame of MCMCglmm diagnostic outputs
#' @param x Dummy x-axis for diagnostic lines plot
#' @param title Title to be used in resulting graph output
#'
#'
#' @section Functions:
#' \itemize{
#'  \item \code{plot_diagnostic_lines()} plots diagnostic lines from MCMCglmm
#'  output
#'  \item \code{plot_replicate_posteriors()} plots a violin plot of posteriors
#'  for experimental plot replicates
#'  \item \code{plot_treatment_posteriors()} plots a violin plot of posteriors
#'  for experimental plot treatments
#'  \item \code{plot_joint_random_error_dist()} plots joint distribution
#' }
#'
#' @examples \dontrun{
#' # Plot diagnostic lines
#' plot_diagnostic_lines(d = reps, x = x, title = "Diagnostic Lines Plot")
#'
#' # Plot replicate posteriors
#' plot_replicate_posteriors(d = reps, title = "Replicate Posteriors")
#'
#' # Plot treatment posteriors
#' plot_treatment_posteriors(d =  trts, title = "Treatment Posteriors")
#'
#' # Plot joint random error
#' plot_joint_random_error_dist(d = rdf, title = ("Random Error Distribution"))
#' }
#'
#' @author Adam H Sparks, \email{adamhsparks@gmail.com}
#'
#'
#' @export
#' @rdname plot_mcmc_glmm
plot_diagnostic_lines <- function(d, x, title) {
  ggplot2::ggplot(data = d, ggplot2::aes(
    x = rep(x, length(unique(d$variable))),
    y = d$value,
    group = d$variable
  )) +
    ggplot2::geom_line() +
    ggplot2::xlab("Iteration Number") +
    ggplot2::ylab(NULL) +
    ggplot2::facet_grid(variable ~ .) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0)) +
    ggplot2::ggtitle(paste(title))
}

#' @export
#' @rdname plot_mcmc_glmm
plot_replicate_posteriors <- function(d, title) {
  d <- d[d$variable != "X.Intercept.", ]
  ggplot2::ggplot(data = d,
                  ggplot2::aes(x = d$value,
                               colour = d$variable)) +
    ggplot2::geom_density() +
    ggplot2::ggtitle(paste(title)) +
    ggplot2::scale_color_brewer(type = "qual") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Treatment")) +
    ggplot2::xlab("Sheath Blight Rating")
}

#' @export
#' @rdname plot_mcmc_glmm
plot_treatment_posteriors <- function(d, title) {
  ggplot2::ggplot(data = d,
                  ggplot2::aes(colour = d$variable,
                               linetype = d$variable,
                               x = d$value)) +
    ggplot2::geom_density() +
    ggplot2::ggtitle(paste(title)) +
    ggplot2::scale_color_brewer(type = "qual") +
    ggplot2::scale_linetype(name = "Treatment") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Treatment")) +
    ggplot2::xlab("Sheath Blight Rating")
}

#' @export
#' @rdname plot_mcmc_glmm
plot_joint_random_error_dist <- function(d, title) {
  ggplot2::ggplot(d,
                  ggplot2::aes(x = sqrt(d$REP), y = sqrt(d$units))) +
    ggplot2::geom_density2d() +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::ggtitle(paste(title))
}
