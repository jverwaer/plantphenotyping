################################################################################
# @dateCreated: 2021/04/07
# @authors: Jan Verwaeren, Noemie De Zutter
#
# @topic: main analysis pipeling for processing CropReporter data
#         for the course on Plant Phenotyping (edition 2020-2021)
#
################################################################################

library(emdist)
library(MASS)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggcorrplot)
library(tidyr)
library(randomForest)

df <- read.csv("../../../data/Spectral_Nutrient.csv", sep = ";")



################################################################################
# Exploratory plots (influence time and supplied N on several spectra)
# indices
################################################################################

# create a plot that shows the influence of time and supplied nitrogen (0-100%)
# on ChlIdx
# ------------------------------------------------------------------------------
ggplot(df %>% filter(startsWith(Leaf,"L")), 
       aes(x = Day, y = ChlIdx, fill = as.factor(Concentration))) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Greens') +
  facet_grid(cols = vars(Leaf)) +
  theme_bw() +
  labs(fill = 'Concentration N (%)')



# create a plot that shows the influence of time and supplied nitrogen (0-100%)
# on the 'greenness'
# ------------------------------------------------------------------------------
df$greenness <- 1 - df$Red / (df$Red + df$Green + df$Blue)

ggplot(df %>% filter(startsWith(Leaf,"L")), 
       aes(x = Day, y = greenness, fill = as.factor(Concentration))) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Greens') +
  facet_grid(cols = vars(Leaf)) +
  theme_bw() +
  labs(fill = 'Greenness')