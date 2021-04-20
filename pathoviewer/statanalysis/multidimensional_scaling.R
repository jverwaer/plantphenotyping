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
library(xlsx)

df_dens <- read.xlsx("/data/densities_chlidx.xlsx", sheetIndex = 2)

################################################################################
# visualize distributions of ChlIdx
################################################################################



# compute means and standard deviations for each day/treatment combination
df_dens_agg <- df_dens %>% 
  group_by(Treatment, Day) %>%
  summarise(across(starts_with("X"),
                   .fns = list(meanDensity = mean, sdDensity = sd)))


# transform to long format
df_dens_agg_long <- df_dens_agg %>%
  pivot_longer(cols = starts_with("X"),
               names_to = c("ChlIdx", "statistic"),
               names_pattern = "X(.*)_(.*)") %>%
  pivot_wider(names_from = statistic,
              values_from = value) %>%
  mutate(ChlIdx = as.numeric(ChlIdx), 
         Treatment = factor(Treatment, 
                            levels = c("0% N",
                                       "0% N + Biostimulants",
                                       "25% N",
                                       "25% N + Biostimulants",
                                       "50% N",
                                       "90% N")))


# visualize distributions
ggplot(df_dens_agg_long %>%
         filter(Day %in% c("Day 07", "Day 14", "Day 21", "Day 28")),
       aes(x = ChlIdx, y = meanDensity)) +
  geom_ribbon(aes(ymin = meanDensity - sdDensity,
                  ymax = meanDensity + sdDensity),
              fill = "grey70") +
  geom_line() + 
  facet_grid(vars(Day), vars(Treatment)) +
  theme_bw()


################################################################################
# use multidimensional scaling with earth movers distance on distributions
################################################################################


# select the average densities
densities <- df_dens_agg %>% 
  filter(Day %in% c("Day 07", "Day 14", "Day 21", "Day 28")) %>%
  select(Treatment, Day, ends_with("meanDensity")) %>%
  ungroup()

# number of densities
n <- nrow(densities)

# pre-allocate distance matrix
EMD_ChlIdx <- matrix(nrow = n, ncol = n)

for(i in 1:n){
  for(j in 1:n){
    EMD_ChlIdx[i,j] <- emd2d(as.matrix(densities[i, 3:ncol(densities)]),
                             as.matrix(densities[j, 3:ncol(densities)]))
  }
}

# use sammon mapping to visualize the data
sammonMapping <- sammon(EMD_ChlIdx, k = 2)

# append coordinates to densities data frame
densities$sammonCoord1 <- sammonMapping$points[,1]
densities$sammonCoord2 <- sammonMapping$points[,2]
densities$N_gift <- as.numeric(sub("%.*","", densities$Treatment))
densities$bio <- ifelse(grepl(".*Biost.*", densities$Treatment), "Bio", "Contr.")

ggplot(densities, aes(x = sammonCoord1,
                      y = sammonCoord2,
                      color = N_gift,
                      shape = Day)) +
  geom_point(size = 4) +
  geom_text(aes(x = sammonCoord1+0.1,
                y = sammonCoord2+0.1,
                label = bio)) +
  theme_bw()



