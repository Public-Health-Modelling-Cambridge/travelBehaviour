# =============================================================================
# 02_buildPersonDataset.R
#
# Description: Merges the person, household, and trip datasets produced by
#              01_readNTS.R into a single person-level dataset ready for trip
#              generation modelling. Each row represents one person; trip counts
#              are pivoted wide by purpose (HBW, HBE, HBS, etc.).
#
# Input:  datasets/persons.csv
#         datasets/households.csv
#         datasets/trips.csv
#
# Output: datasets/dataTripGen.csv  (one row per person)
# =============================================================================

library(tidyverse)

# ── 1. READ DATASETS ──────────────────────────────────────────────────────────

NTS <- list()
NTS$persons    <- read.csv(file = "datasets/persons.csv")
NTS$households <- read.csv(file = "datasets/households.csv")
NTS$trips      <- read.csv(file = "datasets/trips.csv")

# ── 2. SELECT RELEVANT VARIABLES ─────────────────────────────────────────────

NTS$persons <- NTS$persons %>%
  dplyr::select(hh.id, p.id, p.ID, p.age_gr, p.female, p.freqWFH, p.weight,
                p.occupationStatus, p.ownBicycle, p.driversLicence, p.workMode)

NTS$households <- NTS$households %>%
  dplyr::select(hh.id, hh.weight, hh.size, hh.children, hh.cars, hh.income, hh.urban)

NTS$trips <- NTS$trips %>%
  dplyr::select(hh.id, p.id, p.ID, t.day.id, t.id, t.weight,
                t.purpose, t.travelTime.wtd, t.distance, t.is_RRT)

# Cars per adult (household-level)
NTS$households$hh.cars_per_adult <- NTS$households$hh.cars /
  (NTS$households$hh.size - NTS$households$hh.children)

# ── 3. PIVOT TRIP COUNTS BY PURPOSE ──────────────────────────────────────────

# Wide table: one column per trip purpose, counting observed trips per person
cross_tab <- NTS$trips %>%
  group_by(p.ID, t.purpose) %>%
  tally() %>%
  spread(t.purpose, n) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

# ── 4. MEAN TRIP DISTANCES BY PURPOSE ────────────────────────────────────────

# Used as predictors in trip generation models (trips < 40 km only)
NTS$HBW_trip_lengths <- NTS$trips %>%
  filter(t.distance < 40, t.purpose == "HBW") %>%
  group_by(p.ID) %>%
  summarise(p.mean_distance.hbw = mean(t.distance))

NTS$HBE_trip_lengths <- NTS$trips %>%
  filter(t.distance < 40, t.purpose == "HBE") %>%
  group_by(p.ID) %>%
  summarise(p.mean_distance.hbe = mean(t.distance))

# ── 5. ASSEMBLE PERSON-LEVEL DATASET ─────────────────────────────────────────

NTS$data_for_model <- NTS$persons %>%
  left_join(NTS$households,      by = "hh.id") %>%
  left_join(NTS$HBE_trip_lengths, by = "p.ID") %>%
  left_join(NTS$HBW_trip_lengths, by = "p.ID") %>%
  left_join(cross_tab,            by = "p.ID")

# ── 6. WRITE OUTPUT ───────────────────────────────────────────────────────────

write.table(NTS$data_for_model, file = "datasets/dataTripGen.csv",
            row.names = FALSE, sep = ";")
