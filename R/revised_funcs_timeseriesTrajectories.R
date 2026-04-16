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

rasterstackData <- function(x,
                            timePoints = c(2000, 2001, 2002, 2003, 2005),
                            spatialextent = "unified",
                            zeroabsence = "yes",
                            annualchange = "yes",
                            categoryName = "variable",
                            regionName = "region",
                            varUnits = "(squre kilometers)",
                            constant = 1) {

  if (!inherits(x, "SpatRaster")) {
    stop("This function is intended for SpatRasters only", call. = FALSE)
  }
  if (!zeroabsence %in% c("no", "yes")) {
    stop("zeroabsence must have a yes or no input", call. = FALSE)
  }
  if (!annualchange %in% c("no", "yes")) {
    stop("annualchange must have a yes or no input", call. = FALSE)
  }
  if (length(timePoints) != terra::nlyr(x)) {
    stop("length(timePoints) must match nlyr(x)", call. = FALSE)
  }

  j <- terra::blocks(x)
  pb <- txtProgressBar(min = 0, max = j$n, initial = 0, width = 50, style = 3)

  d_gains <- vector("list", j$n)
  d_loss <- vector("list", j$n)
  lengthSpext <- vector("list", j$n)
  sumLastFirst <- vector("list", j$n)
  sumLastFirst_2 <- vector("list", j$n)

  ncl_noxy <- terra::nlyr(x)
  m <- ncl_noxy + 1
  v <- ncl_noxy - 1

  clone2 <- data.frame(matrix(1, nrow = 1, ncol = ncl_noxy)) / j$n

  trans <- data.frame(
    X1 = timePoints[-length(timePoints)],
    X2 = timePoints[-1]
  )
  timeIntervals <- trans$X2 - trans$X1
  trans$transitions <- paste(trans$X1, "-", trans$X2, sep = "")
  transitions <- trans$transitions

  gains <- c("gainYellow", "gainYellow2",
             "gainLblue", "gainLred",
             "gainDblue", "gainDred")

  lossess <- c("lossYellow", "lossYellow2",
               "lossLblue", "lossLred",
               "lossDblue", "lossDred")

  terra::readStart(x)
  on.exit({
    terra::readStop(x)
    close(pb)
  }, add = TRUE)

  for (i in seq_along(j$row)) {

    d <- terra::readValues(
      x,
      row = j$row[i],
      nrows = j$nrows[i],
      dataframe = TRUE
    )
    d <- stats::na.omit(d)

    if (nrow(d) == 0) {
      zero_gain <- as.data.frame(matrix(0, nrow = length(timeIntervals), ncol = 6))
      names(zero_gain) <- gains
      zero_loss <- as.data.frame(matrix(0, nrow = length(timeIntervals), ncol = 6))
      names(zero_loss) <- lossess

      d_gains[[i]] <- cbind(timeIntervals = timeIntervals, zero_gain)
      d_loss[[i]] <- cbind(timeIntervals = timeIntervals, zero_loss)
      lengthSpext[[i]] <- 0
      sumLastFirst[[i]] <- 0
      sumLastFirst_2[[i]] <- 0
      setTxtProgressBar(pb, i)
      next
    }

    newdf <- d
    newdf$max <- apply(newdf, 1, max, na.rm = TRUE)

    d2 <- sum(abs(d[[ncl_noxy]] - d[[1]]))
    sumLastFirst[[i]] <- d2

    d3 <- abs(sum(d[[ncl_noxy]] - d[[1]]))
    sumLastFirst_2[[i]] <- d3

    if (spatialextent == "unified" & zeroabsence == "yes" & annualchange == "no") {
      lengthSpext[[i]] <- sum(newdf$max)
      stackTitle <- paste("Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- "Change (% of region)"
    } else if (spatialextent == 1 & zeroabsence == "yes" & annualchange == "no") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Change in", categoryName, "category where extent is", regionName)
      yaxislable <- paste("Change", varUnits)
    } else if (!spatialextent %in% c("unified", 1) & zeroabsence == "yes" & annualchange == "no") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Change in", categoryName, "category where extent is", regionName)
      yaxislable <- "Change (% of region)"
    } else if (spatialextent == "unified" & zeroabsence == "no" & annualchange == "no") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- "Change (% of region)"
    } else if (spatialextent == 1 & zeroabsence == "no" & annualchange == "no") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Change in", categoryName, "category where extent is", regionName)
      yaxislable <- paste("Change", varUnits)
    } else if (!spatialextent %in% c("unified", 1) & zeroabsence == "no" & annualchange == "no") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- "Change (% of region)"
    } else if (spatialextent == "unified" & zeroabsence == "no" & annualchange == "yes") {
      lengthSpext[[i]] <- sum(newdf$max)
      stackTitle <- paste("Annual Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- "Annual Change (% of region)"
    } else if (spatialextent == 1 & zeroabsence == "no" & annualchange == "yes") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Annual Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- paste("Annual Change", varUnits)
    } else if (!spatialextent %in% c("unified", 1) & zeroabsence == "no" & annualchange == "yes") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Annual Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- "Annual Change (% of region)"
    } else if (spatialextent == "unified" & zeroabsence == "yes" & annualchange == "yes") {
      lengthSpext[[i]] <- sum(newdf$max)
      stackTitle <- paste("Annual Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- "Annual Change (% of region)"
    } else if (spatialextent == 1 & zeroabsence == "yes" & annualchange == "yes") {
      lengthSpext[[i]] <- nrow(clone2) / j$n
      stackTitle <- paste("Annual Change in presence of", categoryName, "category where extent is", regionName)
      yaxislable <- paste("Change", varUnits)
    }

    input2 <- d[, -1, drop = FALSE] - d[, -ncol(d), drop = FALSE]
    input2 <- cbind(input2, d[, 1, drop = FALSE], d[, ncl_noxy, drop = FALSE])

    input2 <- input2[!apply(input2[, 1:v, drop = FALSE] == 0, 1, all), , drop = FALSE]

    if (nrow(input2) == 0) {
      zero_gain <- as.data.frame(matrix(0, nrow = length(timeIntervals), ncol = 6))
      names(zero_gain) <- gains
      zero_loss <- as.data.frame(matrix(0, nrow = length(timeIntervals), ncol = 6))
      names(zero_loss) <- lossess

      d_gains[[i]] <- cbind(timeIntervals = timeIntervals, zero_gain)
      d_loss[[i]] <- cbind(timeIntervals = timeIntervals, zero_loss)
      setTxtProgressBar(pb, i)
      next
    }

    input2 <- dplyr::mutate(
      input2,
      pos_index = apply(input2[, 1:v, drop = FALSE] > 0, 1, which.max),
      neg_index = apply(input2[, 1:v, drop = FALSE] < 0, 1, which.max),
      pos = rowSums(input2[, 1:v, drop = FALSE] > 0),
      neg = rowSums(input2[, 1:v, drop = FALSE] < 0),
      pos_uni = apply(input2[, 1:v, drop = FALSE], 1, function(x) sum(unique(stats::na.omit(x)) > 0)),
      neg_uni = apply(input2[, 1:v, drop = FALSE], 1, function(x) sum(unique(stats::na.omit(x)) < 0))
    )

    clone <- data.frame(matrix(0, nrow = 1, ncol = v))
    names(clone) <- names(input2[, 1:v, drop = FALSE])

    traj <- subset(input2, input2[[ncl_noxy]] > input2[, m] & pos == 0 & neg >= 1)
    if (nrow(traj) == 0) {
      seg1GainRed <- as.data.frame(colSums(clone))
      seg1LossRed <- as.data.frame(colSums(clone))
    } else {
      traj <- t(stats::na.omit(traj[, 1:v, drop = FALSE]))
      seg1GainRed <- as.data.frame(apply(traj, 1, function(x) sum(x[x > 0])))
      seg1LossRed <- as.data.frame(apply(traj, 1, function(x) sum(x[x < 0])))
    }

    traj2 <- subset(input2, input2[[ncl_noxy]] > input2[, m] & pos >= 1 & neg >= 1)
    if (nrow(traj2) == 0) {
      seg2GainRed2 <- as.data.frame(colSums(clone))
      seg2LossRed2 <- as.data.frame(colSums(clone))
    } else {
      traj2 <- t(stats::na.omit(traj2[, 1:v, drop = FALSE]))
      seg2GainRed2 <- as.data.frame(apply(traj2, 1, function(x) sum(x[x > 0])))
      seg2LossRed2 <- as.data.frame(apply(traj2, 1, function(x) sum(x[x < 0])))
    }

    traj3 <- subset(input2, pos >= 1 & neg == 0)
    if (nrow(traj3) == 0) {
      seg3GainBlue <- as.data.frame(colSums(clone))
      seg3LossBlue <- as.data.frame(colSums(clone))
    } else {
      traj3 <- t(stats::na.omit(traj3[, 1:v, drop = FALSE]))
      seg3GainBlue <- as.data.frame(apply(traj3, 1, function(x) sum(x[x > 0])))
      seg3LossBlue <- as.data.frame(apply(traj3, 1, function(x) sum(x[x < 0])))
    }

    traj4 <- subset(input2, input2[[ncl_noxy]] < input2[, m] & pos >= 1 & neg >= 1)
    if (nrow(traj4) == 0) {
      seg4GainBlue2 <- as.data.frame(colSums(clone))
      seg4LossBlue2 <- as.data.frame(colSums(clone))
    } else {
      traj4 <- t(stats::na.omit(traj4[, 1:v, drop = FALSE]))
      seg4GainBlue2 <- as.data.frame(apply(traj4, 1, function(x) sum(x[x > 0])))
      seg4LossBlue2 <- as.data.frame(apply(traj4, 1, function(x) sum(x[x < 0])))
    }

    traj5 <- subset(input2, input2[, ncl_noxy] == input2[, m] &
                      rowSums(input2[, 1:v, drop = FALSE]) == 0 &
                      pos_index > neg_index)
    if (nrow(traj5) == 0) {
      seg5GainBrown <- as.data.frame(colSums(clone))
      seg5LossBrown <- as.data.frame(colSums(clone))
    } else {
      traj5 <- t(stats::na.omit(traj5[, 1:v, drop = FALSE]))
      seg5GainBrown <- as.data.frame(apply(traj5, 1, function(x) sum(x[x > 0])))
      seg5LossBrown <- as.data.frame(apply(traj5, 1, function(x) sum(x[x < 0])))
    }

    traj6 <- subset(input2, input2[, ncl_noxy] == input2[, m] &
                      rowSums(input2[, 1:v, drop = FALSE]) == 0 &
                      pos_index < neg_index)
    if (nrow(traj6) == 0) {
      seg6GainBrown <- as.data.frame(colSums(clone))
      seg6LossBrown <- as.data.frame(colSums(clone))
    } else {
      traj6 <- t(stats::na.omit(traj6[, 1:v, drop = FALSE]))
      seg6GainBrown <- as.data.frame(apply(traj6, 1, function(x) sum(x[x > 0])))
      seg6LossBrown <- as.data.frame(apply(traj6, 1, function(x) sum(x[x < 0])))
    }

    gainDf2 <- as.data.frame(cbind(
      seg5GainBrown, seg6GainBrown,
      seg4GainBlue2, seg2GainRed2, seg3GainBlue,
      seg1GainRed
    ))
    names(gainDf2) <- gains

    lossDf2 <- abs(as.data.frame(cbind(
      seg5LossBrown, seg6LossBrown,
      seg4LossBlue2, seg2LossRed2, seg3LossBlue,
      seg1LossRed
    )))
    names(lossDf2) <- lossess

    d_gains[[i]] <- cbind(timeIntervals = timeIntervals, gainDf2)
    d_loss[[i]] <- cbind(timeIntervals = timeIntervals, lossDf2)

    setTxtProgressBar(pb, i)
  }

  d_loss <- Reduce(`+`, d_loss)
  d_loss$timeIntervals <- d_loss$timeIntervals / j$n

  d_gains <- Reduce(`+`, d_gains)
  d_gains$timeIntervals <- d_gains$timeIntervals / j$n

  if (!spatialextent %in% c("unified", 1)) {
    lengthSpext <- sum(unlist(lengthSpext)) * spatialextent
  } else {
    lengthSpext <- sum(unlist(lengthSpext))
  }

  sumLastFirst <- Reduce(`+`, sumLastFirst)
  sumLastFirst_2 <- Reduce(`+`, sumLastFirst_2)

  if (annualchange == "no" & spatialextent == 1) {
    gainStack <- d_gains[-1] / lengthSpext
    gainStack$timeIntervals <- d_gains$timeIntervals

    lossStack <- (d_loss[-1] * -1) / lengthSpext
    lossStack$timeIntervals <- d_loss$timeIntervals

  } else if (annualchange == "yes" & spatialextent == 1) {
    gainStack <- d_gains[-1] / (d_gains$timeIntervals * lengthSpext)
    gainStack$timeIntervals <- d_gains$timeIntervals

    lossStack <- (d_loss[-1] * -1) / (d_loss$timeIntervals * lengthSpext)
    lossStack$timeIntervals <- d_loss$timeIntervals

  } else if (annualchange == "no" & spatialextent != 1) {
    gainStack <- (d_gains[-1] / lengthSpext) * 100
    gainStack$timeIntervals <- d_gains$timeIntervals

    lossStack <- ((d_loss[-1] * -1) / lengthSpext) * 100
    lossStack$timeIntervals <- d_loss$timeIntervals

  } else {
    gainStack <- (d_gains[-1] / (d_gains$timeIntervals * lengthSpext)) * 100
    gainStack$timeIntervals <- d_gains$timeIntervals

    lossStack <- (d_loss[-1] * -1 / (d_loss$timeIntervals * lengthSpext)) * 100
    lossStack$timeIntervals <- d_loss$timeIntervals
  }

  trajNames <- c("All Alternation Loss First",
                 "All Alternation Gain First",
                 "Gain with Alternation",
                 "Loss with Alternation",
                 "Gain without Alternation",
                 "Loss without Alternation",
                 "Time_intervals")

  names(gainStack) <- trajNames
  names(lossStack) <- trajNames

  gainStack2b <- t(gainStack[0:6])
  lossStack2b <- t(lossStack[0:6])
  lossgainStacked2b <- rbind(gainStack2b, lossStack2b)
  names(lossgainStacked2b) <- transitions

  mergLossGain <- rbind(gainStack, lossStack)
  trajOnly <- mergLossGain[1:6]

  mergLossGain$interval_2 <- transitions
  mergLossGain3 <- mergLossGain[, c("Time_intervals",
                                    names(mergLossGain)[names(mergLossGain) != "Time_intervals"])]
  mergLossGain4 <- mergLossGain3[1:7]
  transLossGain4 <- t(mergLossGain4)
  transLossGain5 <- transLossGain4[-1, , drop = FALSE]
  colnames(transLossGain5) <- mergLossGain$interval_2

  meltLossGain5 <- reshape2::melt(transLossGain5)
  meltLossGain5$value <- meltLossGain5$value * constant

  colnames(transLossGain5) <- mergLossGain$Time_intervals
  meltLossGain5b <- reshape2::melt(transLossGain5)
  meltLossGain5$size <- meltLossGain5b$Var2

  prodGainLossInt <- meltLossGain5$value * meltLossGain5$size

  gainLine <- sum(prodGainLossInt[prodGainLossInt > 0]) / (timePoints[ncl_noxy] - timePoints[1])
  lossLine <- sum(prodGainLossInt[prodGainLossInt < 0]) / (timePoints[ncl_noxy] - timePoints[1])
  net <- gainLine + lossLine

  if (net < 0) {
    Net <- "Quantity Loss"
  } else if (net > 0) {
    Net <- "Quantity Gain"
  } else {
    Net <- "Zero Quantity"
  }

  netAbs <- abs(net)

  if (spatialextent == 1) {
    allocation <- ((sumLastFirst * lengthSpext) / (timePoints[ncl_noxy] - timePoints[1]) * constant) - netAbs
    alternation <- gainLine - lossLine - allocation - netAbs
  } else {
    allocation <- ((sumLastFirst * (100 / lengthSpext)) / (timePoints[ncl_noxy] - timePoints[1]) * constant) - netAbs
    alternation <- gainLine - lossLine - allocation - netAbs
  }

  compNames <- c(Net, "Exchange", "Alternation")
  compVals <- c(netAbs, allocation, alternation)
  dfCompnents <- data.frame(compVals = compVals)
  dfCompnents$compNames <- compNames
  dfCompnents2 <- reshape2::melt(dfCompnents, id = "compNames")

  trajNames2 <- c("All Alternation Loss First",
                  "All Alternation Gain First",
                  "Gain with Alternation",
                  "Loss with Alternation",
                  "Gain without Alternation",
                  "Loss without Alternation")

  trajCol <- c("#a8a803", "#e6e600", "#14a5e3",
               "#ff6666", "#020e7a", "#941004")

  nameCol1 <- data.frame(trajNames2 = trajNames2, trajCol = trajCol)
  trajNames3 <- colSums(abs(mergLossGain[1:6]))
  trajNames3 <- data.frame(trajNames2 = names(trajNames3[trajNames3 != 0]))
  nameCol2 <- dplyr::left_join(trajNames3, nameCol1, by = "trajNames2")

  return(list(
    "Factor dataframe for trajectory stacke bar plot" = meltLossGain5,
    "Value of gain line" = gainLine,
    "Value of loss line" = lossLine,
    "Dataframe for components of change" = dfCompnents2,
    "Title of stackbar plot" = stackTitle,
    "Size of net component" = Net,
    "Name of category of ineterst" = categoryName,
    "Dataframe for stackbar plot" = mergLossGain,
    "Colors and trajectories for stacked bars" = nameCol2,
    "vertical axis labe" = yaxislable,
    sumLastFirst,
    sumLastFirst_2,
    lengthSpext
  ))
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
