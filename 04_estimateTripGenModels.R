# =============================================================================
# 04_estimateTripGenModels.R
#
# Description: Estimates statistical trip generation models on the engineered
#              datasets produced by 03_prepareModelInputs.R.
#
#              Three model types are fitted:
#
#              1. HB Mandatory (HBW + HBE combined)
#                 - Binary part : quasibinomial GLM  → zeroModelHBmandatory.rds
#                 - Count part  : ordinal logit (POLR) → countModelHBmandatory.rds
#
#              2. HB Discretionary (HBA + HBO + HBR + HBS)
#                 - Hurdle negative-binomial          → hurdleModelHBdiscretionary.rds
#
#              3. Non-Home Based (NHBO + NHBW)
#                 - Hurdle negative-binomial          → hurdleModelNHB.rds
#
#              Predictors: age group dummies, occupation status dummies, sex,
#              and (for discretionary/NHB) mandatory trip count groups.
#
# Input:  datasets/tripGenInputDataZero.csv
#         datasets/tripGenInputDataCount.csv
#
# Output: models/zeroModelHBmandatory.rds
#         models/countModelHBmandatory.rds
#         models/hurdleModelHBdiscretionary.rds
#         models/hurdleModelNHB.rds
# =============================================================================

rm(list = ls())

library(tidyr)
library(fastDummies)
library(dplyr)
library(ggplot2)
library(MASS)
library(pscl)
library(purrr)

source("utils.R",  encoding = "UTF-8")
source("utils2.R")

# ── 1. READ DATA ──────────────────────────────────────────────────────────────

df  <- read.csv("datasets/tripGenInputDataZero.csv",  sep = ",")
df1 <- read.csv("datasets/tripGenInputDataCount.csv", sep = ",")

# ── 2. ENGINEER MANDATORY TRIP GROUP DUMMIES ─────────────────────────────────

# Group mandatory trips into three bands used as predictors in downstream models:
#   0 = no mandatory trips, 123 = 1–3 trips, 45 = 4+ trips
df <- df %>%
  mutate(p.HBmandatory = ifelse(is.na(p.HBmandatory), 0, p.HBmandatory)) %>%
  mutate(p.HBmandatory_group = factor(
    case_when(p.HBmandatory %in% 1:3 ~ "123",
              p.HBmandatory >= 4      ~ "45",
              TRUE                    ~ "0"),
    levels = c("0", "123", "45"))) %>%
  dummy_cols("p.HBmandatory_group", remove_first_dummy = FALSE) %>%
  dplyr::select(-p.HBmandatory_group) %>%
  rename_with(~ gsub("p.HBmandatory_group_", "p.HBmandatory_", .x),
              starts_with("p.HBmandatory_group_"))

# ── 3. MODEL: HB MANDATORY ───────────────────────────────────────────────────

# Common formula: age groups 4/6–9 (relative to 18–29 base), occupation, sex
hbm_formula_terms <- (~ p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
                      + p.status_retired + p.status_student + p.status_unemployed
                      + p.female)

# Binary part: does the person make any mandatory trip? (quasibinomial for overdispersion)
m.tripGenYn <- list()
m.tripGenYn$HBmandatory <- glm(
  update(hbm_formula_terms, p.has_HBmandatory ~ .),
  data    = df,
  weights = p.weight,
  family  = "quasibinomial"
)
summary(m.tripGenYn$HBmandatory)
saveRDS(m.tripGenYn$HBmandatory, file = "models/zeroModelHBmandatory.rds")

# Count part: how many mandatory trips (among those who make at least one)?
# Ordinal logit (POLR) with outcome bounded at 7
m.tripGenPolr <- list()
df1$p.HBmandatoryBounded <- as.factor(df1$p.HBmandatoryBounded)
m.tripGenPolr$HBmandatory <- MASS::polr(
  update(hbm_formula_terms, p.HBmandatoryBounded ~ .),
  data = df1,
  Hess = TRUE
)
summary(m.tripGenPolr$HBmandatory)
saveRDS(m.tripGenPolr$HBmandatory, file = "models/countModelHBmandatory.rds")

# ── 4. VALIDATE: HB MANDATORY ────────────────────────────────────────────────

df$predictedHBmandatory <- predict_trips(
  df, m.tripGenYn$HBmandatory, m.tripGenPolr$HBmandatory,
  dim(df1)[1] / dim(df)[1], TRUE
)

result <- df %>%
  group_by(p.age_gr, p.female) %>%
  summarise(
    mean_trips_sim = 2 * sum(predictedHBmandatory * p.weight, na.rm = TRUE) / sum(p.weight, na.rm = TRUE),
    mean_trips_obs = 2 * sum(p.HBmandatory        * p.weight, na.rm = TRUE) / sum(p.weight, na.rm = TRUE),
    .groups = "drop"
  )

result_long <- result %>%
  pivot_longer(cols = c(mean_trips_sim, mean_trips_obs),
               names_to = "metric", values_to = "average_value") %>%
  mutate(metric    = factor(metric,
                            levels = c("mean_trips_sim", "mean_trips_obs"),
                            labels = c("Simulated Trips", "Observed Trips")),
         sex_group = ifelse(p.female == 1, "Female", "Male"))

ggplot(result_long,
       aes(x = interaction(p.age_gr, sex_group), y = average_value, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(title = "Average number of mandatory trips by age/gender",
       x = "Age group and sex", y = "Average trips", fill = "Metric") +
  scale_x_discrete(labels = function(x) gsub("\\.", " ", x)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Simulated Trips" = "skyblue", "Observed Trips" = "salmon"))

plot_trips_comparison(result)

# ── 5. MODEL: HB DISCRETIONARY ───────────────────────────────────────────────

# Hurdle negative-binomial: whether any discretionary trip + how many
# Mandatory trip groups (0 / 1–3 / 4+) included as predictors
m.tripGenHurdle <- list()
m.tripGenHurdle$HBdiscretionary <- hurdle(
  df$p.HBdiscretionary ~
    p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
  + p.status_retired + p.status_student + p.status_unemployed
  + p.female + p.HBmandatory_123 + p.HBmandatory_45
  | p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
  + p.status_retired + p.status_student + p.status_unemployed
  + p.female + p.HBmandatory_123 + p.HBmandatory_45,
  dist = "negbin", data = df, weights = p.weight
)
saveRDS(m.tripGenHurdle$HBdiscretionary, file = "models/hurdleModelHBdiscretionary.rds")

# ── 6. MODEL: NON-HOME BASED ─────────────────────────────────────────────────

m.tripGenHurdle$NHB <- hurdle(
  df$p.NHB ~
    p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
  + p.status_retired + p.status_student + p.status_unemployed
  + p.female + p.HBmandatory_123 + p.HBmandatory_45
  | p.age_gr_4 + p.age_gr_6 + p.age_gr_7 + p.age_gr_8 + p.age_gr_9
  + p.status_retired + p.status_student + p.status_unemployed
  + p.female + p.HBmandatory_123 + p.HBmandatory_45,
  dist = "negbin", data = df, weights = p.weight
)
saveRDS(m.tripGenHurdle$NHB, file = "models/hurdleModelNHB.rds")

# ── 7. VALIDATE: DISCRETIONARY & NHB ─────────────────────────────────────────

df$predictedHBdiscretionary <- predict_discretionary_trips(m.tripGenHurdle, "HBdiscretionary", df)

results2 <- df %>%
  group_by(p.age_gr, p.female) %>%
  summarise(
    mean_trips_obs = 2 * sum(p.HBdiscretionary,        na.rm = TRUE) / n(),
    mean_trips_sim = 2 * sum(predictedHBdiscretionary, na.rm = TRUE) / n(),
    .groups = "drop"
  )
plot_trips_comparison(results2)

df$predictedNHB <- predict_discretionary_trips(m.tripGenHurdle, "NHB", df)

results3 <- df %>%
  group_by(p.age_gr, p.female) %>%
  summarise(
    mean_trips_obs = sum(p.NHB,          na.rm = TRUE) / n(),
    mean_trips_sim = sum(predictedNHB,   na.rm = TRUE) / n(),
    .groups = "drop"
  )
plot_trips_comparison(results3)

# ── 8. SCATTER PLOTS: OBSERVED vs SIMULATED BY AGE/GENDER ─────────────────────

# Age group labels for readability
age_labels <- c("1" = "0-4", "2" = "5-10", "3" = "11-15", "4" = "16-17",
                "5" = "18-29", "6" = "30-49", "7" = "50-59", "8" = "60-69", "9" = "70+")

# Combine all three trip purposes into one dataframe
scatter_data <- bind_rows(
  result   %>% mutate(purpose = "HB Mandatory"),
  results2 %>% mutate(purpose = "HB Discretionary"),
  results3 %>% mutate(purpose = "Non-Home Based")
) %>%
  mutate(sex   = ifelse(p.female == 1, "Female", "Male"),
         age   = age_labels[as.character(p.age_gr)],
         purpose = factor(purpose, levels = c("HB Mandatory", "HB Discretionary", "Non-Home Based")))

# Faceted scatter plot: one panel per trip purpose
ggplot(scatter_data, aes(x = mean_trips_obs, y = mean_trips_sim,
                         colour = sex, label = age)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3) +
  geom_text(vjust = -0.8, size = 3, show.legend = FALSE) +
  facet_wrap(~ purpose, scales = "free") +
  scale_colour_manual(values = c("Female" = "salmon", "Male" = "skyblue")) +
  labs(title  = "Observed vs simulated trip rates by age group and gender",
       x      = "Observed (mean trips)",
       y      = "Simulated (mean trips)",
       colour = "Sex") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# ── 9. SUMMARY: PREDICTED TOTAL WEEKLY TRIPS ─────────────────────────────────

# 2× HB (symmetric outward/return), 1× NHB
finalDF <- df %>%
  dplyr::select(p.age_gr, p.female,
                predictedHBmandatory, predictedHBdiscretionary, predictedNHB)

finalDF$predictedTotalWeeklyTrips <- 2 * finalDF$predictedHBmandatory +
                                     2 * finalDF$predictedHBdiscretionary +
                                         finalDF$predictedNHB

cat("Mean predicted weekly trips:", mean(finalDF$predictedTotalWeeklyTrips), "\n")
