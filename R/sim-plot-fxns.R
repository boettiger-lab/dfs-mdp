# solution point plot with threshold annotated
soln_plot <- function(soln_df, tpt) {
  ggplot(soln_df, aes(state,action)) +
    geom_point(size = 1) +
    geom_vline(xintercept = tpt, linetype="dashed", color = "red3", size=.7) +
    annotate('label', x = tpt + .15, y = .375, label = "Tipping \n point", hjust = 0, vjust = .5,
             family = "Roboto", size = 3.25, label.padding = unit(.15, "lines"), label.size = 0, alpha = .65) +
    annotate("segment", x = tpt + .15, xend = tpt + .025, y = .375, yend = .375, size=.5, arrow=arrow(length = unit(0.22, "cm"))) +
    labs(x="Ecosystem service state", y="Optimal action", title = "A. Decision strategy") +
    scale_x_continuous(limits = c(0,NA), expand = c(.01,.01), breaks=c(0, 0.5,1)) +
    scale_y_continuous(limits = c(0,1), expand = c(.01,.01), labels = scales::percent_format(accuracy = 1)) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = "plot",
          plot.margin=grid::unit(c(5,5,5,5), "mm"))
}


# timeseries plot of multiple repetitions
sim_plot_ts <- function(sims, title = ggtitle(NULL), tpos = "plot", ytxtoff = FALSE, endcol = pal[1], annotate = FALSE, an_x = NULL, an_lab = NULL, dnmarmod = 0, upmarmod = 0, lmarmod = 0, rmarmod = 0){
  df <- sims %>%
    select(-value) %>% # tidy
    mutate(state = states[state], action = actions[action]) # rescale
  Tmax <- max(sims$time)

  stcol <- col2rgb(endcol)
  stcol <- stcol/5
  stcol <- rgb(t(stcol), maxColorValue=255)

  ytitc <- "black"
  ytxtc = "gray30"
  if (ytxtoff) {
    ytitc <- NA
    ytxtc <- NA
  }

  p <- df %>%
    ggplot(aes(time, state, group = reps, col = time)) +
    geom_path(alpha = 0.1, show.legend = FALSE) +
    title +
    labs(x="Decision cycle", y="Ecosystem service state") +
    scale_x_continuous(limits = c(0,Tmax), breaks = seq(0, Tmax, by=5), expand = c(.01,.01)) +
    scale_y_continuous(limits = c(0,1.5), expand = c(.02,.02)) +
    scale_color_gradient(low=stcol, high=endcol) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10, color = ytxtc),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10, color = ytitc),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = tpos,
          panel.grid.minor = element_blank(),
          plot.margin=grid::unit(c(5+upmarmod,5+rmarmod,5+dnmarmod,5+lmarmod), "mm"))
  if(annotate) {
    p <- p +
      geom_vline(xintercept = an_x, linetype="dashed", color = "red3", size=.5) +
      annotate('label', x = an_x + 2, y = 1.3, label = an_lab, hjust = 0, vjust = .5,
               family = "Roboto", size = 3.25, label.padding = unit(.15, "lines"), label.size = 0, alpha = .6) +
      annotate("segment", x = an_x + 1.75, xend = an_x + .5, y = 1.3, yend = 1.3, size=.5, arrow=arrow(length = unit(0.22, "cm")))
  }
  p
}



# density plot showing initial ES distribution and final distribution
sim_plot_dens <- function(sims, title = ggtitle(NULL), tpos = "plot", endcol = pal[1], lab_lo_peak = FALSE, lab_hi_peak = FALSE, dnmarmod = 0, upmarmod = 0, lmarmod = 0, rmarmod = 0){

  df <- sims %>%
    mutate(state = states[state]) %>% # rescale
    select(state, time)
  Tmax <- max(sims$time)

  stcol <- col2rgb(endcol)
  stcol <- stcol/5
  stcol <- rgb(t(stcol), maxColorValue=255)

  p <- df %>% filter(time %in% c(0, Tmax))  %>%
    ggplot() + geom_density(aes(state, group = time, fill = time, color = time), alpha=0.8) +
    coord_flip() +
    title +
    labs(x="", y="Density", fill="Time") +
    scale_x_continuous(limits = c(0,1.5), expand = c(.02,.02)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
    scale_fill_gradient(low=stcol, high=endcol, guide = guide_colorbar(barwidth = .5), breaks=c(0, Tmax)) +
    scale_color_gradient(low=stcol, high=endcol, guide = NULL) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_blank(),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10),
          legend.box.margin=margin(0,0,0,-5),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = tpos,
          panel.grid.minor = element_blank(),
          plot.margin=grid::unit(c(5+upmarmod,5+rmarmod,5+dnmarmod,5+lmarmod), "mm"))

  if(lab_lo_peak) {
    ymax <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]

    peak_lo <- ggplot_build(p)$data[[1]] %>%
      filter(group == 2, x < .4) %>%
      arrange(desc(y)) %>%
      select(x,y) %>%
      filter(row_number()==1)

    # dotted line
    p <- p +
      annotate('segment', x = peak_lo$x, xend = peak_lo$x, y = peak_lo$y - .1 * ymax, yend = peak_lo$y + .1 * ymax, linetype="dotted")

    # text annotation (decide whether to place left or right of line)
    if(peak_lo$y > .7 * ymax) {
      p <- p +
        annotate('label', x = peak_lo$x, y = peak_lo$y - .32 * ymax, label = sprintf('%.2f', peak_lo$x), hjust = .5, vjust = .5,
                 family = "Roboto Condensed", size = 3, label.padding = unit(.15, "lines"), label.size = 0, alpha = .8)
    } else {
      p <- p +
        annotate('label', x = peak_lo$x, y = peak_lo$y + .26 * ymax, label = sprintf('%.2f', peak_lo$x), hjust = .5, vjust = .5,
                 family = "Roboto Condensed", size = 3, label.padding = unit(.15, "lines"), label.size = 0, alpha = .8)
    }
  }

  if(lab_hi_peak){
    ymax <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]

    peak_hi <- ggplot_build(p)$data[[1]] %>%
      filter(group == 2, x > .6) %>%
      arrange(desc(y)) %>%
      select(x,y) %>%
      filter(row_number()==1)

    # dotted line
    p <- p +
      annotate('segment', x = peak_hi$x, xend = peak_hi$x, y = peak_hi$y - .1 * ymax, yend = peak_hi$y + .1 * ymax, linetype="dotted")

    # text annotation (decide whether to place left or right of line)
    if(peak_hi$y > .7 * ymax) {
      p <- p +
        annotate('label', x = peak_hi$x, y = peak_hi$y - .32 * ymax, label = sprintf('%.2f', peak_hi$x), hjust = .5, vjust = .5,
                 family = "Roboto Condensed", size = 3, label.padding = unit(.15, "lines"), label.size = 0, alpha = .8)
    } else {
      p <- p +
        annotate('label', x = peak_hi$x, y = peak_hi$y + .26 * ymax, label = sprintf('%.2f', peak_hi$x), hjust = .5, vjust = .5,
                 family = "Roboto Condensed", size = 3, label.padding = unit(.15, "lines"), label.size = 0, alpha = .8)
    }
  }
  p
}


# function to increase vertical spacing between legend keys
# @clauswilke
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)

  grid::rectGrob(
    width = grid::unit(0.3, "npc"),
    height = grid::unit(0.8, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

# register new key drawing function,
# the effect is global & persistent throughout the R session
GeomDensity$draw_key = draw_key_polygon3


# density plot comparing two simulation results, with initial distribution
sim_plot_dens_comp <- function(sims1, sims2, sims_base = "init", label1 = "Gp. A Final", label2 = "Gp. B Final", label_base = "Initial", title = ggtitle(NULL), tpos = "plot", cvec = c(pal[2], pal[1], pal[3]), dnmarmod = 0, upmarmod = 0, lmarmod = 0, rmarmod = 0){

  if (sims_base == "init") {
    df_base <- sims1 %>%
      mutate(state = states[state]) %>% # rescale
      filter(time %in% 0) %>%
      select(state) %>%
      add_column(id = label_base)
  } else {
    Tmax_base <- max(sims_base$time)
    df_base <- sims_base %>%
      mutate(state = states[state]) %>% # rescale
      filter(time %in% Tmax_base) %>%
      select(state) %>%
      add_column(id = label_base)
  }

  Tmax1 <- max(sims1$time)
  df1 <- sims1 %>%
    mutate(state = states[state]) %>% # rescale
    filter(time %in% Tmax1) %>%
    select(state) %>%
    add_column(id = label1)

  Tmax2 <- max(sims2$time)
  df2 <- sims2 %>%
    mutate(state = states[state]) %>% # rescale
    filter(time %in% Tmax1) %>%
    select(state) %>%
    add_column(id = label2)

  rbind(df_base, df1, df2) %>% mutate(id = factor(id, unique(id))) %>%
    ggplot() + geom_density(aes(state, group = id, fill = id, color = id), alpha=0.4) +
    coord_flip() +
    title +
    labs(x="", y="Density") +
    scale_x_continuous(limits = c(0,1.5), expand = c(.02,.02)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
    scale_fill_manual(values = cvec) +
    scale_color_manual(values = cvec) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_blank(),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          legend.text=element_text(size=9),
          legend.title = element_blank(),
          legend.key.size = unit(12, "mm"),
          legend.spacing.x = unit(-3, 'mm'),
          legend.box.margin=margin(0,0,0,-18),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = tpos,
          panel.grid.minor = element_blank(),
          plot.margin=grid::unit(c(5+upmarmod,5+rmarmod,5+dnmarmod,5+lmarmod), "mm"))
}



# prop decreasing or increasing ES over the run (print txt to console)
p_up_dn <- function(sims){
  i_state <- sims %>%
    filter(time == 0) %>%
    select(state) %>%
    mutate(state = states[state])

  f_state <- sims %>%
    filter(time == 20) %>%
    select(state) %>%
    mutate(state = states[state])

  state <- data.frame(i = i_state$state, f = f_state$state)

  state$up <- state$i < .5 & state$f > .5
  state$dn <- state$i > .5 & state$f < .5

  # c(mean(state$up), mean(state$dn))
  print(sprintf('prop high final = %.2f, prop low final = %.2f', mean(state$f > .5), mean(state$f < .5)))
  print(sprintf('prop increasing = %.2f, prop decreasing = %.2f', mean(state$up), mean(state$dn)))
}


# short vs. long tenure bar plot of probability that ES ended up over some threshold
sim_tenure_thresh_bar <- function(sims_long_tenure, sims_short_tenure, thresh, title = ggtitle(NULL), tpos = "plot") {
  lt_p_over <- sims_long_tenure %>% filter(time == 20) %>% mutate(state = states[state]) %>% select(state) %>% dplyr::summarize(p_over_thresh = sum(state > thresh) / n())
  st_p_over <- sims_short_tenure %>% filter(time == 20) %>% mutate(state = states[state]) %>% select(state) %>% dplyr::summarize(p_over_thresh = sum(state > thresh) / n())
  plot_data <- tibble(cats = c("st", "lt"), vals = c(st_p_over[1,1], lt_p_over[1,1]))
  plot_data$cats <- factor(plot_data$cats, levels = plot_data$cats)

  p_mod_ten <- plot_data %>% ggplot(aes(x = cats, y = vals, fill=cats)) +
    geom_bar(width = .75, position=position_dodge(width=.84), stat="identity", color=NA, alpha = .8,) +
    geom_text(aes(label=sprintf("%.2f", round(vals,2))), vjust=1.6, color="white", position = position_dodge(0.9), size=3, family = "Roboto Condensed") +
    xlab("") +
    ylab(expression("p. ES">="0.125")) +
    title +
    scale_x_discrete(expand = c(0,0), labels = c("Short\ntenure", "Long\ntenure")) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = c(pal[3], pal[1])) +
    theme(axis.text.x=element_text(size=10, color = "black", family = "Roboto Condensed"),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = tpos,
          axis.ticks.x=element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          plot.margin=grid::unit(c(5,5,0,2.5), "mm"))
}

# find peaks fxn for benefit sweep experiment
b_sweep_peaks <- function(sims, smoothing = 1.5, ythresh = 0.125, output_bizone = FALSE) {
  bimin = 0
  bimax = 0
  peaks <- tibble(cbr = numeric(), peak = numeric())
  for(this_cbr in unique(sims$cbr)) {
    dens <- density((sims %>% filter(cbr == this_cbr))$state, adjust = smoothing)
    dens <- tibble(x = dens$x, y = dens$y) %>% filter(y >= ythresh)
    ps <- dens$x[findPeaks(dens$y)]
    for(p in ps) {
      peaks <- add_row(peaks, cbr = this_cbr, peak = p)
    }
    if(output_bizone && length(ps) > 1) {
      if(bimin == 0) {
        bimin = this_cbr
      }
      bimax = this_cbr
    }
  }
  if(output_bizone) {
    c(bimin, bimax)
  } else {
    peaks
  }
}

# plot benefit sweep experiment results
plt_sim_b_sweep <- function(sims, title = ggtitle(NULL), ylab = "", tpos = "plot", col = pal[1], upmarmod = 0, lmarmod = 0, dnmarmod = 0, rmarmod = 0) {

  dkcol <- col2rgb(col)
  dkcol <- dkcol/5
  dkcol <- rgb(t(dkcol), maxColorValue=255)

  sims %>%
    ggplot(aes(x = state, group = cbr, color = cbr)) +
    geom_line(stat='density', size=.5, alpha=.4) +
    title +
    labs(x = "Ecosystem service state", y = ylab, color = "c:b") +
    scale_x_continuous(limits = c(0, 1.5), breaks = scales::pretty_breaks(n = 3), expand = c(.01,.01)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 2), expand = c(.01,.01)) +
    scale_color_gradient(low = dkcol, high = col, guide = guide_colorbar(barwidth = .5),
                         breaks=c(min(sims$cbr), max(sims$cbr))) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10),
          legend.box.margin=margin(0,0,0,-5),
          # legend.key.height = unit(.02, "npc"),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = tpos,
          panel.grid.minor = element_blank(),
          plot.margin=grid::unit(c(5+upmarmod,5+rmarmod,5+dnmarmod,5+lmarmod), "mm"))
}

# plot benefit sweep experiment peaks by cbr
plt_b_sweep_peaks <- function(peaks, bizone = NULL, title = ggtitle(NULL), ylab = "", tpos = "plot", col = pal[1], upmarmod = 0, lmarmod = 0, dnmarmod = 0, rmarmod = 0) {

  dkcol <- col2rgb(col)
  dkcol <- dkcol/5
  dkcol <- rgb(t(dkcol), maxColorValue=255)

  p <- peaks %>%
    ggplot(aes(x = peak, y = cbr, color = cbr)) +
    geom_point(size=1) +
    title +
    labs(x = "State density peak(s)", y = ylab) +
    scale_x_continuous(limits = c(0, 1.5), breaks = scales::pretty_breaks(n = 3), expand = c(.01,.01)) +
    scale_y_continuous(limits = c(min(peaks$cbr), max(peaks$cbr)), breaks = scales::pretty_breaks(n = 3), expand = c(.01,.01)) +
    scale_color_gradient(low = dkcol, high = col, guide = NULL) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = tpos,
          panel.grid.minor = element_blank(),
          plot.margin=grid::unit(c(5+upmarmod,5+rmarmod,5+dnmarmod,5+lmarmod), "mm"))

  if(!is.null(bizone)) {
    p <- p +
      geom_hline(yintercept = bizone[1], linetype="dashed", color = "red3", size=.5) +
      geom_hline(yintercept = bizone[2], linetype="dashed", color = "red3", size=.5) +
      annotate('label', x = .55, y = mean(bizone), label = "Bimodal\nregion", hjust = .5, vjust = .5,
               family = "Roboto", size = 3, label.padding = unit(.15, "lines"), label.size = 0, alpha = .65)
  }

  p
}