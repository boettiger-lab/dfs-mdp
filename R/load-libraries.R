# libraries necessary to knit dfs-mdp manuscript

# data / MDP libraries
library(tidyverse)
library(Hmisc)
library(dplyr)
library(MDPtoolbox)

# plotting libraries
library(corrplot)
library(RColorBrewer)
library(ggthemes)
library(ggpubr)
library(ggplot2)
library(hrbrthemes)
library(Cairo)
library(extrafont)
extrafont::loadfonts()
ggplot2::theme_set(hrbrthemes::theme_ipsum_rc())
library(patchwork) # devtools::install_github("thomasp85/patchwork")
library(ggtext) # install cli, gh, devtools::install_github("clauswilke/ggtext")