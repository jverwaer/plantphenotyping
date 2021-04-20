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


################################################################################
# Add estimate of nitrogen content (and also uptake) using regression equations
################################################################################

# show relation between CHlIdx and N_Content
# ------------------------------------------------------------------------------
ggplot(filter(df, startsWith(Leaf, "L"), Day == "Day 21"),
       aes(x=ChlIdx, y = N_Content)) +
  geom_point() +
  facet_grid(cols = vars(Leaf)) +
  geom_smooth(method = "lm") +
  theme_bw()


# do a regression analysis (predict N_Content using ChlIdx at day 21)
# ------------------------------------------------------------------------------
my_regression <- function(x, y){
  model <- summary(lm(y~x))
  return(data.frame(
    intercept = model$coefficients[1,1],
    slope = model$coefficients[2,1],
    Rsquared = model$r.squared
  ))
}

regressions <- 
  filter(df, startsWith(Leaf, "L"), Day == "Day 21") %>%
    group_by(Leaf) %>%
    summarise(my_regression(ChlIdx, N_Content))


# add predicted N_Contend to dataframe
# ------------------------------------------------------------------------------
leaf_to_rowIdx = match(df$Leaf, regressions$Leaf)
df$predicted_N_content = regressions$intercept[leaf_to_rowIdx] + 
                                         df$ChlIdx * regressions$slope[leaf_to_rowIdx]


# plot predicted versus observed N_Content
# ------------------------------------------------------------------------------
ggplot(filter(df, startsWith(Leaf, "L"), Day == "Day 21"),
       aes(x=predicted_N_content, y = N_Content)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_grid(cols = vars(Leaf)) +
  theme_bw()


################################################################################
# early diagnosis: predict N-Content at end of experiment in an early stage
#  -> compute correlations
################################################################################

# only keep relevant records (measured on days 7, 14 and 21 and containing 
# information per leaf)
df_SS <- filter(df, Day %in% c("Day 07", "Day 14", "Day 21"), startsWith(Leaf, "L"))

# transform to wide format
df_wide <- df_SS %>% pivot_wider(
                        id_cols = c(Concentration, Replicate),
                        names_from = c(Day, Leaf),
                        values_from = c(ChlIdx, Fv.Fm, greenness)
                        ) %>%
                     left_join(
                        df_SS %>% filter(Day == "Day 21", Leaf == "L4") %>%
                                  select(Concentration, Replicate, predicted_N_content),
                        by = c("Concentration", "Replicate")
                        )

# compute correlation matrix
correlation_matrix <- cor(df_wide %>% select(-Concentration, -Replicate),
                          use = "pairwise.complete.obs",
                          method = "spearman")

# make correlation plot
ggcorrplot(correlation_matrix,
           method= "circle",
           type = "lower",
           lab = TRUE,
           hc.order = TRUE)

# combine correlations with final N_content in single dataframe
corr_with_N_content <- data.frame(variable = rownames(correlation_matrix),
                                  cor = as.data.frame(correlation_matrix)$predicted_N_content) %>%
                        arrange(cor)

corr_with_N_content$variable <- factor(corr_with_N_content$variable, 
                                       levels = corr_with_N_content$variable)


# make bar plot of correlations with final N_content
ggplot(corr_with_N_content, aes(x = variable, y = cor)) +
  geom_col() +
  ylim(0, 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


################################################################################
# early diagnosis: predict N-Content at end of experiment in an early stage
#  -> build Random Forest Model on days 7 and 14
################################################################################


# build reduced datasets (only containing predictors at day 7 and day 7 & 14)
# ------------------------------------------------------------------------------

# all data at day 7 and earlier
df_wide_7 <- df_wide %>% select(contains("Day 07"), predicted_N_content)
# NOTE: random Forests does not like spaces in column names
colnames(df_wide_7) <- gsub(" ", "_", colnames(df_wide_7))

# all data at day 14 and earlier
df_wide_7_14 <- df_wide %>% select(contains("Day 07"), contains("Day 14"), predicted_N_content)
# NOTE: random Forests does not like spaces in column names
colnames(df_wide_7_14) <- gsub(" ", "_", colnames(df_wide_7_14))


# build rf models (only containing predictors at day 7 and day 7 & 14)
# ------------------------------------------------------------------------------

# build model
model_7 <- randomForest(predicted_N_content~., data = df_wide_7)
model_7_14 <- randomForest(predicted_N_content~., data = df_wide_7_14)

# combine predictions in a dataframe and make a simple visualization
results <- data.frame(measured_N_content = df_wide_7$predicted_N_content,
                      preds_day_7 = model_7$predicted,
                      preds_day_7_14 = model_7_14$predicted)

# put unto long format to get everything on a single figure
results_long <- pivot_longer(results, 
                             cols = c(preds_day_7, preds_day_7_14), 
                             names_to = "days",
                             values_to = "predictions")

# visualize prediction versus measured value
ggplot(results_long, 
       aes(x = measured_N_content, 
           y = predictions, 
           color = days)) +
    geom_point() +
    geom_smooth(se = FALSE, span = 0.8) +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw()

# compute mean relative absolute error
rel_abs_error_7 = mean(abs((results$preds_day_7 - results$measured_N_content)) / abs(results$measured_N_content))
rel_abs_error_7_14 = mean(abs((results$preds_day_7_14 - results$measured_N_content)) / abs(results$measured_N_content))

cor(results$preds_day_7_14,results$measured_N_content)

# visualize variable importance
varImpPlot(model_7)
varImpPlot(model_7_14)


################################################################################
# visualize distributions of ChlIdx
################################################################################

df_dens <- read.csv("../../../data/ChlIdx_Densities.csv", sep = ";", dec = ".")

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
                                                         "10% N",
                                                         "25% N",
                                                         "50% N",
                                                         "75% N",
                                                         "90% N",
                                                         "100% N")))


# visualize distributions
ggplot(df_dens_agg_long %>% filter(Day %in% c("Day 07", "Day 14", "Day 21")),
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
              filter(Day %in% c("Day 07", "Day 14", "Day 21")) %>%
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
densities$N_gift <- as.numeric(gsub("% N","", densities$Treatment))

ggplot(densities, aes(x = sammonCoord1,
                      y = sammonCoord2,
                      col = N_gift,
                      shape = Day)) +
  geom_point(size = 4) +
  theme_bw()

