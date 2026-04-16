# From Lei Song

# Some revised functions

plot_timeseries <- function(x,
                            timepoints = c(2000, 2001, 2002, 2003, 2005),
                            vertunits = "millions of variable",
                            xAngle = 90) {

  anualProp <- terra::global(x, fun = "sum", na.rm = TRUE)
  anualProp <- anualProp$sum

  df <- data.frame(
    timepoints = timepoints,
    anualProp = anualProp)

  vertlab <- paste0("Area (as ", vertunits, ")")
  horizlab <- "Time"

  ggplot(df, aes(x = timepoints, y = anualProp)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    scale_x_continuous(
      labels = as.character(df$timepoints),
      breaks = df$timepoints) +
    labs(x = horizlab, y = vertlab)
}

plot_trajectory <- function(input){
  r <- input[[1]]
  traj_info <- input[[2]]
  n_time <- input[[3]]

  vals <- sort(unique(stats::na.omit(terra::values(r, mat = FALSE))))
  vals <- vals[seq_len(min(length(vals), nrow(traj_info)))]

  labs_df <- data.frame(
    value = vals,
    label = traj_info$cl[seq_along(vals)])

  r <- terra::as.factor(r)
  levels(r) <- labs_df

  fill_values <- traj_info$myCol[seq_along(vals)]
  names(fill_values) <- labs_df$label

  ggplot() +
    geom_spatraster(data = r) +
    scale_fill_manual(
      values = fill_values,
      na.translate = FALSE,
      drop = FALSE,
      name = "Trajectories")
}

plot_stackbar <- function(input,
                          axisSize = 12,
                          lbAxSize = 15,
                          lgSize = 12,
                          titleSize = 15,
                          datbreaks = "no",
                          upperlym = 35,
                          lowerlym = -50,
                          lymby = 5,
                          upperlym2 = 0.5,
                          lymby2 = 0.1,
                          xAngle = 0) {

  if (!datbreaks %in% c("yes", "no")) {
    stop("The input must be 'yes' or 'no'", call. = FALSE)
  }

  if (datbreaks == "yes") {
    v1 <- scale_y_continuous(
      breaks = seq(lowerlym, upperlym, by = lymby),
      limits = c(lowerlym, upperlym))
    v2 <- scale_y_continuous(
      limits = c(0, upperlym2),
      breaks = seq(0, upperlym2, by = lymby2),
      expand = c(0, 0))
  } else {
    v1 <- scale_y_continuous()
    v2 <- scale_y_continuous(expand = c(0, 0))
  }

  df1 <- input[[1]]
  df2 <- input[[4]]
  traj_cols <- as.character(input[[9]]$trajCol)
  traj_levels <- input[[9]]$trajNames2

  a <- ggplot(
    df1,
    aes(
      x = factor(Var2),
      y = value,
      fill = factor(Var1, levels = traj_levels),
      width = size)) +
    geom_col() +
    scale_fill_manual(
      values = traj_cols,
      na.translate = FALSE) +
    v1 +
    geom_hline(
      aes(yintercept = input[[2]], color = "Gross Gain"),
      linetype = 4,
      linewidth = 0.8) +
    scale_color_manual(
      name = "",
      values = c("Gross Gain" = "black")) +
    ggnewscale::new_scale_color() +
    geom_hline(
      aes(yintercept = input[[3]], color = "Gross Loss"),
      linetype = 3,
      linewidth = 0.8) +
    scale_color_manual(
      name = "",
      values = c("Gross Loss" = "black")) +
    scale_x_discrete(expand = c(0, 0)) +
    facet_grid(~Var2, scales = "free_x", space = "free_x") +
    guides(fill = guide_legend(title = "")) +
    geom_hline(yintercept = 0, color = "grey", linewidth = 0.5) +
    labs(
      x = "Time Interval",
      y = input[[10]],
      title = input[[5]]) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.title = element_text(size = titleSize, face = "bold"),
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.border = element_rect(color = "white", fill = NA, linewidth = 0),
      axis.ticks.x = element_blank(),
      axis.line.y = element_line(color = "black", linewidth = 1),
      axis.text = element_text(size = axisSize, face = "bold"),
      axis.text.x = element_text(angle = xAngle),
      axis.title = element_text(size = lbAxSize, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = lgSize, face = "bold"),
      legend.spacing.y = unit(-0.2, "lines"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key = element_rect(colour = NA, fill = NA),
      text = element_text(size = 8))

  b <- ggplot(
    df2,
    aes(
      x = variable,
      y = value,
      fill = factor(compNames, levels = c("Alternation", "Exchange", input[[6]])))) +
    geom_col() +
    scale_fill_manual(values = c("#D3D3D3", "#A9A9A9", "#808080")) +
    scale_x_discrete(expand = expansion(add = c(0, 0))) +
    guides(fill = guide_legend(title = "")) +
    labs(
      x = "All time intervals",
      y = input[[10]],
      title = input[[5]]) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      axis.text.x = element_blank(),
      plot.title = element_text(size = titleSize, face = "bold"),
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.border = element_rect(color = "white", fill = NA, linewidth = 0),
      axis.ticks.x = element_blank(),
      axis.line.y = element_line(color = "black", linewidth = 1),
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.text = element_text(size = axisSize, face = "bold"),
      axis.title = element_text(size = 15, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 12, face = "bold"),
      text = element_text(size = 8)) +
    v2

  return(list(a, b))
}
