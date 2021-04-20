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


# Read the data
df <- read.csv("/data/Data_Plant_phenotyping_technologies_2021.csv", sep = ";")

################################################################################
# Select a subset of the data
################################################################################

# Create a subset of variables that we could be interested in
# ------------------------------------------------------------------------------
variables <- c("ChlIdx", "FarRed", "NDVI", "Fv.Fm", "AriIdx", "Length", "SPAD",
               "Dualex.NBI", "Dualex.Chl", "Dualex.Flav", "Dualex.Anth")

design_variables <- c("Day", "Leaf", "Bio", "Rep", "Nconc....")


df <- df %>% select(all_of(variables),
                    all_of(design_variables)) %>%
             rename(Nconc = Nconc....)             # remove strange dots at the end of Nconc....

################################################################################
# Create a dose-response curve
################################################################################

# Create a dose-response curve
# ------------------------------------------------------------------------------

df_selection <- df %>% 
  filter(Day %in% c("Day 07", "Day 14", "Day 21", "Day 28"), 
         Bio == 0)

ggplot(df_selection, 
       aes(x = Day, y = ChlIdx, fill = as.factor(Nconc))) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Greens') +
  facet_grid(cols = vars(Leaf)) +
  theme_bw() +
  labs(fill = 'Concentration N (%)')


################################################################################
# Compute serial correlation and make simple plots to show influence of 
# the presence of a biostimulant on the ChlIdx
################################################################################

# Compute serial correlation between Bio and ChlIdx at "Day 07" with Nconc of 50
# ------------------------------------------------------------------------------

df_selection <- df %>% 
                  filter(Leaf == 3, Nconc == 25, Day == "Day 14") %>%
                  select(-one_of(c("Leaf", "Rep", "Nconc", "Day")))

# make correlation plot
ggcorrplot(cor(df_selection),
           method= "circle",
           type = "lower",
           lab = TRUE)


# Make a simple plot of the observations
# ------------------------------------------------------------------------------

df_selection <- df %>% 
  filter(Leaf == 3, Nconc == 25) %>%
  select(-one_of(c("Leaf", "Rep", "Nconc")))


ggplot(df_selection, aes(x = as.factor(Day), y = ChlIdx)) +
  geom_boxplot() +
  facet_grid(cols = vars(Bio)) +
  theme_bw() +
  xlab("Biostimulant (no/yes -> 0/1)")






