# functions returning vars of interest and dp use vars
cornell_vars_of_intrst <- function() {
  c("CR1_binary",
    "CC1_binary",
    "IC1_binary",
    "IP1_binary",
    "RB1_binary",
    "BP1_binary",
    "CM1_binary",
    "RT1_binary",
    "FC1_Owned.ac",
    "FC1_Total.ac",
    "FC6_Mngt.3t",
    "FC8_Csys_3type")
}

cornell_dp_vars <- function() {
  c("CR1_binary",
    "CC1_binary",
    "IC1_binary",
    "IP1_binary",
    "RB1_binary",
    "BP1_binary",
    "CM1_binary",
    "RT1_binary")
}

# filter dataset and remove rows where all dp vars are na
filter_cornell <- function(cornell_data) {
  cornell_data <- cornell_data %>% filter(FC8_Csys_3type == "Specialty") # filter by farm type
  cornell_data <- cornell_data[, cornell_vars_of_intrst()] # filter out vars of interest
  # cornell_data[rowSums(is.na(cornell_data)) <= length(cornell_dp_vars()) - 1,] # include only if all dp vars are not NA
}

# generate counts of num. dps used by each farmer in the ca_or_wa dataset
dp_counts <- function(cornell_data) {
  cornell_data[, cornell_dp_vars()] %>% # select binary DP vars
    rowSums(na.rm = T) # count use
}


# histogram of num. dps used by each farmer
cornell_dp_count_hst <- function(cornell_data, title = ggtitle(NULL), tpos = "plot") {
  as.data.frame(table(dp_counts(cornell_data))) %>%
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

# prop_owned <- cornell_data[,"FC1_Owned.ac"] / cornell_data[,"FC1_Total.ac"]
# ownrshp <- data.frame(prop_owned) %>%
#   # mutate(mo_ml = case_when(prop_owned >= .75 ~ 'Mostly Owned',
#   #                          prop_owned <= .25 ~ 'Mostly Leased',
#   #                          TRUE ~ 'Neither'))
# mutate(mo_ml = case_when(prop_owned >= .5 ~ 'Mostly Owned',
#                          TRUE ~ 'Mostly Leased'))
# cornell_data$mo_ml <- ownrshp$mo_ml
# cornell_data$dp_count <- dp_counts(cornell_data)
# cornell_dp_count_avg <- cornell_data %>% group_by(mo_ml) %>% dplyr::summarise(mean_dp_count = mean(dp_count))


# mostly-owned vs. mostly-leased bar plot of probability of using one or more dps
cornell_tenure_thresh_bar <- function(cornell_data, title = ggtitle(NULL), tpos = "plot") {
  prop_owned <- cornell_data[,"FC1_Owned.ac"] / cornell_data[,"FC1_Total.ac"]
  ownrshp <- data.frame(prop_owned) %>%
    # mutate(mo_ml = case_when(prop_owned >= .75 ~ 'Mostly Owned',
    #                          prop_owned <= .25 ~ 'Mostly Leased',
    #                          TRUE ~ 'Neither'))
    mutate(mo_ml = case_when(prop_owned >= .5 ~ 'Mostly Owned',
                             TRUE ~ 'Mostly Leased'))
  mo_ml <- ownrshp$mo_ml

  cornell_dp_counts <- dp_counts(cornell_data)
  dp_count_cum_probs <- data.frame(cornell_dp_counts) %>%
    mutate(over_thresh = case_when(cornell_dp_counts >= 3 ~ 1, TRUE ~ 0))

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