# functions returning vars of interest and dp use vars
caorwa_vars_of_intrst <- function() {
  c("CR1_binomial",
    "CC1_binomial",
    "IC1_binomial",
    "IP1_binomial",
    "RB1_binomial",
    "BP1_binomial",
    "CM1_binomial",
    "RT1_binomial",
    "FC1_Owned.ac",
    "FC1_Total.ac")
}

caorwa_dp_vars <- function() {
  c("CR1_binomial",
    "CC1_binomial",
    "IC1_binomial",
    "IP1_binomial",
    "RB1_binomial",
    "BP1_binomial",
    "CM1_binomial",
    "RT1_binomial")
}

# filter dataset and remove rows where all dp vars are na
filter_caorwa <- function(caorwa_data) {
  caorwa_data <- caorwa_data %>% filter(FC8_Csys.3t == "Specialty crops") # filter by farm type
  caorwa_data <- caorwa_data[, caorwa_vars_of_intrst()] # filter out vars of interest
  caorwa_data[rowSums(is.na(caorwa_data)) <= length(caorwa_dp_vars()) - 1,] # include only if all dp vars are not NA
}

# generate counts of num. dps used by each farmer in the ca_or_wa dataset
dp_counts <- function(caorwa_data) {
  caorwa_data[, caorwa_dp_vars()] %>% # select binary DP vars
    rowSums(na.rm = T) # count use
}


# histogram of num. dps used by each farmer
caorwa_dp_count_hst <- function(caorwa_data, title = ggtitle(NULL), tpos = "plot") {
  as.data.frame(table(dp_counts(caorwa_data))) %>%
    ggplot(aes(x=Var1)) +
    geom_bar(aes(y=Freq), color=NA, fill="black", alpha = .8, stat = "identity") +
    geom_text(aes(y = Freq, label=Freq), hjust=1.2, color="white", position = position_dodge(0.9), size=3, family = "Roboto Condensed") +
    coord_flip() +
    xlab("Num. DPs adopted") +
    ylab("Frequency") +
    title +
    scale_x_discrete(expand = c(.08,.08)) +
    scale_y_continuous(expand = c(.015,.015)) +
    theme(axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10),
          axis.title.x=element_text(size=10),
          axis.title.y=element_text(size=10),
          plot.title = element_text(size = 10, face = "bold"),
          plot.title.position = tpos,
          axis.ticks.x=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.margin=grid::unit(c(5,5,5,0), "mm"))
}

# prop_owned <- caorwa_data[,"FC1_Owned.ac"] / caorwa_data[,"FC1_Total.ac"]
# ownrshp <- data.frame(prop_owned) %>%
#   # mutate(mo_ml = case_when(prop_owned >= .75 ~ 'Mostly Owned',
#   #                          prop_owned <= .25 ~ 'Mostly Leased',
#   #                          TRUE ~ 'Neither'))
# mutate(mo_ml = case_when(prop_owned >= .5 ~ 'Mostly Owned',
#                          TRUE ~ 'Mostly Leased'))
# caorwa_data$mo_ml <- ownrshp$mo_ml
# caorwa_data$dp_count <- dp_counts(caorwa_data)
# caorwa_dp_count_avg <- caorwa_data %>% group_by(mo_ml) %>% dplyr::summarise(mean_dp_count = mean(dp_count))


# mostly-owned vs. mostly-leased bar plot of probability of using one or more dps
caorwa_tenure_thresh_bar <- function(caorwa_data, title = ggtitle(NULL), tpos = "plot") {
  prop_owned <- caorwa_data[,"FC1_Owned.ac"] / caorwa_data[,"FC1_Total.ac"]
  ownrshp <- data.frame(prop_owned) %>%
    mutate(mo_ml = case_when(prop_owned >= .75 ~ 'Mostly Owned',
                             prop_owned <= .25 ~ 'Mostly Leased',
                             TRUE ~ 'Neither'))
    # mutate(mo_ml = case_when(prop_owned >= .5 ~ 'Mostly Owned',
    #                          TRUE ~ 'Mostly Leased'))
  mo_ml <- ownrshp$mo_ml

  caorwa_dp_counts <- dp_counts(caorwa_data)
  dp_count_cum_probs <- data.frame(caorwa_dp_counts) %>%
    mutate(over_thresh = case_when(caorwa_dp_counts >= 3 ~ 1, TRUE ~ 0))

  moml_dp_thresh <- data.frame(mo_ml, dp_count_cum_probs$over_thresh) %>%
    filter(mo_ml != "Neither")

  group_by(moml_dp_thresh, mo_ml) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ggplot(aes(x = mo_ml, y = dp_count_cum_probs.over_thresh, fill = mo_ml)) +
    geom_bar(width = .75, position=position_dodge(width=.84), stat="identity", color=NA, alpha = .8,) +
    geom_text(aes(label=sprintf("%.2f", round(dp_count_cum_probs.over_thresh,2))), vjust=1.6, color="white", position = position_dodge(0.9), size=3, family = "Roboto Condensed") +
    xlab("") +
    ylab(expression("p.">="3 DP")) +
    title +
    scale_x_discrete(expand = c(0,0), labels = c("Mostly\nleased", "Mostly\nowned")) +
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
          plot.margin=grid::unit(c(2.5,5,2.5,2.5), "mm"))
}