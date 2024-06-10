g_pdp <- function(dt, pdp_plot_sample, feat_labels) {
  # pdp_plot_sample ensures faster rendering
  if (pdp_plot_sample && nrow(dt) > 4e4) {
    set.seed(101)
    g <- ggplot(
      data = dt[sample(nrow(dt), 4e4), ],
      mapping = aes(y = factor(.data$feature, levels = rev(levels(.data$feature))), x = .data$pdp_c)
    ) +
      geom_jitter(shape = 16, size = 1.5, alpha = 0.7, position = position_jitter(seed = 1))
  } else {
    g <- ggplot(
      data = dt,
      mapping = aes(y = factor(.data$feature, levels = rev(levels(.data$feature))), x = .data$pdp_c)
    ) +
      geom_jitter(shape = 16, size = 1.5, alpha = 0.2, position = position_jitter(seed = 1))
  }

  g <- g +
    scale_y_discrete(labels = function(x) feat_labels[x]) +
    labs(x = "Feature Score", y = "Feature") +
    theme_bw()

  return(g)
}

g_imp_abs <- function(dat_var, show_pdp_plot, textsize) {
  nudge_x <- max(dat_var$variance) / 5
  dat_var$variance_vs_top <- dat_var$variance / max(dat_var$variance)

  g <- ggplot(
    dat_var,
    aes(y = factor(.data$feature, levels = rev(levels(.data$feature))), x = .data$variance)
  ) +
    geom_point() +
    geom_text(
      aes(
        x = ifelse(
          .data$variance_vs_top > 0.5,
          .data$variance - 2 * nudge_x,
          .data$variance
        ),
        label = format(round(.data$variance, 3), nsmall = 3, digits = 3)
      ),
      nudge_x = nudge_x,
      size = round(textsize / 3)
    ) +
    labs(
      x = "Direct Variable Importance",
      y = ifelse(show_pdp_plot, "", "Feature")
    ) +
    theme_bw()
  return(g)
}

g_imp_norm <- function(dat_var, show_pdp_plot, textsize) {
  g <- ggplot(
    dat_var,
    aes(y = factor(.data$feature, levels = rev(levels(.data$feature))), x = .data$ratio)
  ) +
    geom_point() +
    geom_text(
      aes(
        x = ifelse(.data$ratio > 0.75, .data$ratio - 0.4, .data$ratio),
        label = sprintf("%.1f%%", round(.data$ratio * 100, 1))
      ),
      nudge_x = 0.2,
      size = round(textsize / 3)
    ) +
    xlim(c(0, 1)) +
    labs(
      x = "Direct Variable Importance",
      y = ifelse(show_pdp_plot, "", "Feature")
    ) +
    theme_bw()
  return(g)
}

g_imp_ice <- function(vars, vars_mean) {
  g <- ggplot() +
    geom_point(
      aes(y = factor(.data$feature, levels = rev(levels(.data$feature))), x = .data$var_y),
      data = vars,
      size = 1,
      colour = "gray50"
    ) +
    geom_point(
      aes(y = factor(.data$feature, levels = rev(levels(.data$feature))), x = .data$mean_var_y),
      data = vars_mean,
      size = 2,
      colour = "black"
    ) +
    labs(
      x = "Direct Variable Importance",
      y = "Feature"
    ) +
    theme_bw()
  return(g)
}

g_cumulR2 <- function(dat_R2_cumul, textsize) {
  ggplot(
    dat_R2_cumul,
    aes(y = factor(.data$feature, levels = rev(levels(.data$feature))), x = round(.data$R2, 4))
  ) +
    geom_point() +
    geom_text(
      aes(
        x = ifelse(.data$R2 < 0.25, .data$R2 + 0.4, .data$R2),
        label = sprintf("%.1f%%", round(.data$R2 * 100, 1))
      ),
      nudge_x = -0.2,
      size = round(textsize / 3)
    ) +
    xlim(c(0, 1)) +
    labs(
      x = expression("Cumulative Approximation " * R^2),
      y = ""
    ) +
    theme_bw() +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
}
