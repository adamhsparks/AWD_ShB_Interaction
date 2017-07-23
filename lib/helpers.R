helper.function <- function()
{
  return(1)
}

plot_diagnostic_lines <- function(d, x, title) {

  # Diagnostic line plots for replicate
  ggplot(data = d, aes(
    x = rep(x, length(unique(d$variable))),
    y = value,
    group = variable
  )) +
    geom_line() +
    xlab("Iteration Number") +
    ylab(NULL) +
    facet_grid(variable ~ .) +
    theme(strip.text.y = element_text(angle = 0)) +
    ggtitle(paste(title))
}

plot_replicate_posteriors <- function(d, title) {
  ggplot(d[d$variable != "X.Intercept.",], aes(x = value,
                                               color = variable,
                                               fill = variable)) +
    geom_density(alpha = 0.25) +
    scale_fill_brewer(type = "qual", palette = "Set1",
                                name = "Variable") +
    scale_colour_brewer(type = "qual", palette = "Set1",
                                name = "Variable") +
    ggtitle(paste(title))
}


plot_treatment_posteriors <- function(d, title) {
  ggplot(data = d, aes(x = value,
                       color = variable,
                       fill = variable)) +
    geom_density(alpha = 0.25) +
    scale_fill_brewer(type = "qual", palette = "Set1",
                                name = "Variable") +
    scale_colour_brewer(type = "qual", palette = "Set1",
                                  name = "Variable") +
    ggtitle(paste(title))
}

plot_joint_random_error_dist <- function(d, title) {
  ggplot(d, aes(x = sqrt(REP), y = sqrt(units))) +
    geom_density2d() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle(paste(title))
}
