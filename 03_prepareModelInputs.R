# =============================================================================
# 03_prepareModelInputs.R
#
# Description: Reads the person-level dataset produced by 02_buildPersonDataset.R
#              and engineers the feature set required by the trip generation
#              models. Produces two model-ready datasets:
#
#                - tripGenInputDataZero.csv  : full sample (includes zero-trip
#                    persons) for the zero-inflation / binary part of the model.
#                - tripGenInputDataCount.csv : persons with at least one
#                    mandatory trip, for the count / ordinal part of the model.
#
#              Also exports an occupation-status cross-tabulation for QA.
#
# Input:  datasets/dataTripGen.csv
#
# Output: datasets/tripGenInputDataZero.csv
#         datasets/tripGenInputDataCount.csv
#         occupation_status.csv
# =============================================================================

rm(list = ls())

library(tidyr)
library(fastDummies)
library(dplyr)

# ── 1. READ DATA ──────────────────────────────────────────────────────────────

df <- read.csv("datasets/dataTripGen.csv", sep = ";")

# ── 2. AGE-BASED CONSTRAINTS ─────────────────────────────────────────────────

# Age groups 1–3 correspond to under-16s who cannot have work/commute trips
# Age groups: 1=0-4, 2=5-10, 3=11-15, 4=16-17, 5=18-29, ..., 9=70+
df <- df %>%
  mutate(HBW  = if_else(p.age_gr <= 3, 0, HBW),
         NHBW = if_else(p.age_gr <= 3, 0, NHBW))

# ── 3. MANDATORY TRIP INDICATORS (HBW & HBE) ─────────────────────────────────

# Binary: has any work trip?
df$p.has_HBW <- as.integer(df$HBW > 0)

# Capped work trip count (0–5+)
df <- df %>%
  mutate(p.workTrips = pmin(HBW, 5L))
df <- dummy_cols(df, select_columns = "p.workTrips")

# Binary thresholds used as alternative predictors
df <- df %>%
  mutate(p.workTrips_15 = as.integer(HBW > 0),          # any work trip
         p.workTrips_14 = as.integer(HBW > 0 & HBW < 5)) # 1–4 work trips

# Binary: has any education trip?
df$p.has_HBE <- as.integer(df$HBE > 0)

# Capped education trip count (0–5+)
df <- df %>%
  mutate(p.eduTrips = pmin(HBE, 5L))
df <- dummy_cols(df, select_columns = "p.eduTrips")

df <- df %>%
  mutate(p.eduTrips_15 = as.integer(HBE > 0),
         p.eduTrips_14 = as.integer(HBE > 0 & HBE < 5))

# ── 4. DEMOGRAPHIC & HOUSEHOLD VARIABLES ─────────────────────────────────────

df$p.female  <- as.integer(df$p.female)
df$hh.urban  <- as.integer(df$hh.urban)

# Household size (capped at 5+), dummies, and grouped indicators
df <- df %>%
  mutate(hh.sized = pmin(hh.size, 5L))
df <- dummy_cols(df, select_columns = "hh.sized")

df <- df %>%
  mutate(hh.sized_45 = as.integer(hh.size > 3),
         hh.sized_23 = as.integer(hh.size > 1 & hh.size < 4))

# Number of children (capped at 3+), dummies
df <- df %>%
  mutate(hh.childrend = pmin(hh.children, 3L))
df <- dummy_cols(df, select_columns = "hh.childrend")

# Age group dummies and grouped indicators
df <- dummy_cols(df, select_columns = "p.age_gr")

df <- df %>%
  mutate(p.age_gr_23 = as.integer(p.age_gr > 1 & p.age_gr < 4),  # 5–15
         p.age_gr_56 = as.integer(p.age_gr > 4 & p.age_gr < 7),  # 18–59
         p.age_gr_78 = as.integer(p.age_gr >= 7),                 # 50+
         p.age_gr_89 = as.integer(p.age_gr > 7 & p.age_gr < 10)) # 60+

# Commute mode: recode bus/train → pt
df <- df %>%
  mutate(p.workModeRecoded = case_when(p.workMode == "bus"   ~ "pt",
                                       p.workMode == "train" ~ "pt",
                                       TRUE                   ~ p.workMode))
df <- dummy_cols(df, select_columns = "p.workModeRecoded")

# Car ownership
df$has_car <- as.integer(df$hh.cars > 0)
df <- df %>% filter(!is.na(hh.cars))

df <- df %>%
  mutate(hh.carsd = pmin(hh.cars, 3L))
df <- dummy_cols(df, select_columns = "hh.carsd")

df <- df %>%
  mutate(hh.carsd13 = as.integer(hh.cars > 0))

# Income (exclude unknown codes)
df <- df %>% filter(hh.income != -8)
df <- dummy_cols(df, select_columns = "hh.income")

# ── 5. OCCUPATION STATUS ──────────────────────────────────────────────────────

# NA values are all age groups <=3 (children) — reclassify as "student"
df <- df %>%
  mutate(p.status = case_when(
    is.na(p.occupationStatus)                                   ~ "student",
    p.occupationStatus %in% c("full-time", "part-time")        ~ "employed",
    p.occupationStatus == "student"                             ~ "student",
    p.occupationStatus == "unemployed"                          ~ "unemployed",
    TRUE                                                         ~ "retired"))
df <- dummy_cols(df, select_columns = "p.status")

# ── 6. TRIP DISTANCES ─────────────────────────────────────────────────────────

# Log-transform mean trip distances; set to 0 for persons with no such trips
df$p.log_km_mean_HBW <- log(df$p.mean_distance.hbw)
df$p.log_km_mean_HBE <- log(df$p.mean_distance.hbe)
df$p.log_km_mean_HBW[!df$p.has_HBW] <- 0
df$p.log_km_mean_HBE[!df$p.has_HBE] <- 0
df$p.mean_distance.hbw[!df$p.has_HBW] <- 0
df$p.mean_distance.hbe[!df$p.has_HBE] <- 0

# ── 7. AGGREGATE TRIP COUNTS ──────────────────────────────────────────────────

# Combined trip totals used as predictors in discretionary / NHB models
df$p.HBmandatory     <- df$HBW + df$HBE
df$p.HBdiscretionary <- df$HBA + df$HBO + df$HBR + df$HBS
df$p.NHB             <- df$NHBO + df$NHBW
df$p.has_HBmandatory <- as.integer(df$p.HBmandatory > 0)

# ── 8. SELECT OUTPUT COLUMNS ─────────────────────────────────────────────────

dataSubset <- df %>%
  dplyr::select(
    p.has_HBmandatory,
    p.HBmandatory,
    p.HBdiscretionary,
    p.NHB,
    p.age_gr,
    starts_with("p.age_gr_"),
    starts_with("p.status_"),
    p.female,
    p.weight
  )

# Count model dataset: persons with ≥1 mandatory trip; outcome bounded at 7
dataSubsetCount <- dataSubset %>%
  filter(p.HBmandatory > 0) %>%
  mutate(p.HBmandatoryBounded = factor(pmin(p.HBmandatory, 7)))

# ── 9. WRITE OUTPUTS ──────────────────────────────────────────────────────────

write.csv(dataSubset,      file = "datasets/tripGenInputDataZero.csv",  quote = FALSE, row.names = FALSE)
write.csv(dataSubsetCount, file = "datasets/tripGenInputDataCount.csv", quote = FALSE, row.names = FALSE)

# Cross-tabulation of occupation status by age group and sex (QA / reporting)
ct <- df %>%
  group_by(p.age_gr, p.female, p.status) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = p.status, values_from = count, values_fill = 0)

write.csv(ct, "occupation_status.csv", row.names = FALSE)
