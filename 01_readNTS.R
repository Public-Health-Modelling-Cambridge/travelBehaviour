# =============================================================================
# 01_readNTS.R
#
# Description: Reads raw National Travel Survey (NTS) tab-delimited files and
#              produces clean, analysis-ready datasets for persons, households,
#              trips, and stages.
#
# Input:  UKDA-5340-tab/tab/  (raw NTS files, years 2002–2021)
#           - psu_eul_2002-2021.tab
#           - household_eul_2002-2021.tab
#           - individual_eul_2002-2021.tab
#           - trip_eul_2002-2021.tab
#           - stage_eul_2002-2021.tab
#
# Output: datasets/persons.csv
#         datasets/households.csv
#         datasets/trips.csv
#
# Filters applied:
#   - Survey years 2014–2019 (W1 == 1 ensures main sample only)
#   - Persons aged 16+ (Age_B01ID > 5)
# =============================================================================

pacman::p_load(dplyr, readr, tidyr)

# ── 1. READ RAW DATA ──────────────────────────────────────────────────────────

rawData <- list()

# Primary Sampling Units (for region-based analysis)
rawData$PSUs <- read_tsv("UKDA-5340-tab/tab/psu_eul_2002-2021.tab", col_names = TRUE,
                         cols_only(PSUID             = "i",
                                   PSUStatsReg_B01ID = "i",
                                   PSUCountry_B01ID  = "i"))

# Households
rawData$households <- read_tsv("UKDA-5340-tab/tab/household_eul_2002-2021.tab", col_names = TRUE,
                               cols_only(PSUID                    = "i",
                                         SurveyYear               = "i",
                                         HouseholdID              = "i",
                                         HHoldCountry_B01ID       = "i",
                                         HHoldNumPeople           = "i",
                                         NumBike                  = "i",
                                         NumCar                   = "i",
                                         NumMCycle                = "i",
                                         HHIncome2002_B02ID       = "i",
                                         Settlement2011EW_B03ID   = "i",
                                         W1 = "n", W2 = "n", W3 = "n")) %>%
  left_join(rawData$PSUs)

# Individuals
rawData$persons <- read_tsv("UKDA-5340-tab/tab/individual_eul_2002-2021.tab", col_names = TRUE,
                            cols_only(SurveyYear          = "i",
                                      HouseholdID         = "i",
                                      PersNo              = "i",
                                      WkMode_B01ID        = "i",
                                      PrivCar_B01ID       = "i",
                                      OrdBusFreq_B01ID    = "i",
                                      CoachFreq_B01ID     = "i",
                                      TrainFreq_B01ID     = "i",
                                      TaxiCabFreq_B01ID   = "i",
                                      BicycleFreq_B01ID   = "i",
                                      PlaneFreq_B01ID     = "i",
                                      WalkFreq_B01ID      = "i",
                                      Age_B01ID           = "i",
                                      Sex_B01ID           = "i",
                                      DrivLic_B02ID       = "i",
                                      TicketHolding_B01ID = "i",
                                      OwnCycle_B01ID      = "i",
                                      CarsEas_B01ID       = "i",
                                      EcoStat_B02ID       = "i",
                                      OftHome_B01ID       = "i"))

# Trips
rawData$trips <- read_tsv("UKDA-5340-tab/tab/trip_eul_2002-2021.tab", col_names = TRUE,
                          cols_only(SurveyYear          = "i",
                                    HouseholdID         = "i",
                                    PersNo              = "i",
                                    TravDay             = "i",
                                    JourSeq             = "i",
                                    HowComp_B01ID       = "i",
                                    ShortWalkTrip_B01ID = "i",
                                    TripPurpFrom_B01ID  = "i",
                                    TripPurpTo_B01ID    = "i",
                                    TripPurpose_B01ID   = "i",
                                    MainMode_B03ID      = "i",
                                    TripStart           = "n",
                                    TripEnd             = "n",
                                    TripDisIncSW        = "n",
                                    JD                  = "n",
                                    TripTotalTime       = "n",
                                    JJXSC               = "i",
                                    JOTXSC              = "n",
                                    JTTXSC              = "n",
                                    W5                  = "n"))

# Stages (used for access & egress mode breakdowns)
rawData$stages <- read_tsv("UKDA-5340-tab/tab/stage_eul_2002-2021.tab", col_names = TRUE,
                           cols_only(SurveyYear      = "i",
                                     HouseholdID     = "i",
                                     PersNo          = "i",
                                     TravDay         = "i",
                                     JourSeq         = "i",
                                     StageSeq        = "i",
                                     StageMode_B03ID = "i",
                                     StageDistance   = "n",
                                     StageTime       = "n"))

# ── 2. CREATE WORKING DATASETS & APPLY FILTERS ───────────────────────────────

households <- rawData$households %>% mutate(hh.id = HouseholdID)
persons    <- rawData$persons    %>% mutate(hh.id = HouseholdID, p.id = PersNo,
                                            p.ID = HouseholdID * 10 + PersNo - 1)
trips      <- rawData$trips      %>% mutate(hh.id = HouseholdID, p.id = PersNo,
                                            p.ID = HouseholdID * 10 + PersNo - 1,
                                            t.day.id = TravDay, t.id = JourSeq)
stages     <- rawData$stages     %>% mutate(hh.id = HouseholdID, p.id = PersNo,
                                            p.ID = HouseholdID * 10 + PersNo - 1,
                                            t.day.id = TravDay, t.id = JourSeq, s.id = StageSeq)

# Households: survey years 2014–2019, main sample only (W1 == 1)
# Optional regional filters (uncomment to restrict to a specific region):
#   filter(PSUStatsReg_B01ID %in% c(12, 13))  # North West (metro + non-metro)
#   filter(PSUStatsReg_B01ID %in% c(8))        # London boroughs
households <- households %>%
  filter(SurveyYear >= 2014, SurveyYear < 2020, W1 == 1)

# Persons: age 16+ only (Age_B01ID > 5)
persons <- persons %>%
  filter(Age_B01ID > 5) %>%
  semi_join(households["hh.id"])

# Trips & stages: keep only records linked to filtered persons/trips
trips  <- trips  %>% semi_join(persons[c("hh.id", "p.id")]) %>% filter(!is.na(W5))
stages <- stages %>% semi_join(trips[c("hh.id", "p.id", "t.day.id", "t.id")])

trips <- arrange(trips, hh.id, p.id, t.day.id, t.id)

# ── 3. WEIGHTS ────────────────────────────────────────────────────────────────

households[["hh.weight"]] <- households$W2
persons[["p.weight"]]     <- households$hh.weight[match(persons$hh.id, households$hh.id)]
trips[["t.weight"]]       <- trips$W5 * nrow(trips) / sum(trips$W5)

trips[["t.JJXSC"]]           <- trips$JJXSC
trips[["t.day_week_factor"]]  <- with(trips, pmax(JJXSC, 1))

# ── 4. HOUSEHOLD VARIABLES ────────────────────────────────────────────────────

households[["hh.year"]]   <- households$SurveyYear
households[["hh.region"]] <- households$PSUStatsReg_B01ID
households[["hh.size"]]   <- households$HHoldNumPeople

# Count adults and children per household (uses unfiltered persons for accuracy)
households <- left_join(households,
                        rawData$persons %>%
                          group_by(HouseholdID) %>%
                          summarise(hh.adults   = sum(Age_B01ID > 5),
                                    hh.children = sum(Age_B01ID <= 5)))

# Household structure: 1A = single adult, 2A = couple, 3A = 3+ adults, p = with children
households[["hh.structure"]] <- with(households,
  case_when(hh.size == 1                                    ~ "1A",
            hh.size == 2 & hh.children == 0                ~ "2A",
            hh.size >= 3 & hh.children == 0                ~ "3A",
            hh.size >= 2 & (hh.size - hh.children) == 1   ~ "p",
            hh.size >= 3 & (hh.size - hh.children) >= 2   ~ "p"))

# Adjusted household size: children count as 0.5
households[["hh.sizeAdj"]]      <- with(households, (hh.size - hh.children) + 0.5 * hh.children)
households[["hh.cars"]]         <- households$NumCar
households[["hh.motorcycles"]]  <- households$NumMCycle
households[["hh.bicycles"]]     <- households$NumBike
households[["hh.income"]]       <- households$HHIncome2002_B02ID
households[["hh.urban"]]        <- with(households, na_if(Settlement2011EW_B03ID, -8) == 1)

# ── 5. PERSON VARIABLES ───────────────────────────────────────────────────────

# Helper: recode mode-frequency variables (-9 = "never used" → 8)
mode_restriction <- function(freqencyVar) {
  case_when(freqencyVar > 0  ~ freqencyVar,
            freqencyVar == -9 ~ as.integer(8))
}

persons <- persons %>% within({

  p.female <- Sex_B01ID == 2

  # Age groups: 1=0-4, 2=5-10, 3=11-15, 4=16-17, 5=18-29, 6=30-49, 7=50-59, 8=60-69, 9=70+
  p.age_gr <- case_when(Age_B01ID %in% c(-10, -8) ~ NA,
                        Age_B01ID <= 3  ~ 1,
                        Age_B01ID == 4  ~ 2,
                        Age_B01ID == 5  ~ 3,
                        Age_B01ID <= 7  ~ 4,
                        Age_B01ID <= 12 ~ 5,
                        Age_B01ID <= 14 ~ 6,
                        Age_B01ID <= 15 ~ 7,
                        Age_B01ID <= 17 ~ 8,
                        TRUE            ~ 9)

  p.driversLicence <- na_if(DrivLic_B02ID, -8) == 1
  p.freqWFH        <- OftHome_B01ID

  p.occupationStatus <- case_when(EcoStat_B02ID < 1  ~ "NA",
                                  EcoStat_B02ID == 1  ~ "full-time",
                                  EcoStat_B02ID == 2  ~ "part-time",
                                  EcoStat_B02ID == 3  ~ "unemployed",
                                  EcoStat_B02ID == 4  ~ "retired, sick, disabled",
                                  EcoStat_B02ID == 5  ~ "student",
                                  EcoStat_B02ID == 6  ~ "other")

  p.ownBicycle  <- na_if(OwnCycle_B01ID, -8) <= 3
  p.seasonTicket <- na_if(TicketHolding_B01ID, -8) <= 2

  # Usual commute mode
  p.workMode <- case_when(WkMode_B01ID <= 0  ~ "NA",
                          WkMode_B01ID == 1  ~ "carD",
                          WkMode_B01ID == 2  ~ "carP",
                          WkMode_B01ID <= 5  ~ "car",
                          WkMode_B01ID == 6  ~ "cycle",
                          WkMode_B01ID == 7  ~ "bus",
                          WkMode_B01ID <= 10 ~ "train",
                          WkMode_B01ID == 11 ~ "walk",
                          WkMode_B01ID <= 13 ~ "other")

  # Mode use frequency (used for access restrictions in downstream models)
  p.freq_car     <- mode_restriction(PrivCar_B01ID)
  p.freq_bus     <- mode_restriction(OrdBusFreq_B01ID)
  p.freq_coach   <- mode_restriction(CoachFreq_B01ID)
  p.freq_train   <- mode_restriction(TrainFreq_B01ID)
  p.freq_pt      <- pmin(p.freq_bus, p.freq_coach, p.freq_train, na.rm = TRUE)
  p.freq_bicycle <- mode_restriction(BicycleFreq_B01ID)
  p.freq_walk    <- mode_restriction(WalkFreq_B01ID)
})

# ── 6. TRIP VARIABLES ─────────────────────────────────────────────────────────

trips[["t.recordType"]] <- trips$HowComp_B01ID
trips[["t.shortWalk"]]  <- with(trips, ShortWalkTrip_B01ID == 1)

# First and last trip flags per person
trips[["t.firstTrip"]] <- with(trips,
  (hh.id != lag(hh.id, default = 0)) |
  (hh.id == lag(hh.id, default = 0) & p.id != lag(p.id, default = 0)))

trips[["t.lastTrip"]] <- with(trips,
  (hh.id != lead(hh.id, default = 0)) |
  (hh.id == lead(hh.id, default = 0) & p.id != lead(p.id, default = 0)))

# Trip mode (detailed and simplified)
trips[["t.mode"]] <- with(trips,
  case_when(MainMode_B03ID <= 2  ~ "walk",
            MainMode_B03ID == 3  ~ "cycle",
            MainMode_B03ID == 4  ~ "other",
            MainMode_B03ID <= 6  ~ "carD",
            MainMode_B03ID <= 8  ~ "carP",
            MainMode_B03ID <= 10 ~ "carD",
            MainMode_B03ID <= 12 ~ "carP",
            MainMode_B03ID <= 17 ~ "other",
            MainMode_B03ID <= 21 ~ "PT",
            MainMode_B03ID <= 24 ~ "PT",
            TRUE                 ~ "other"))

trips[["t.mode2"]] <- with(trips,
  case_when(MainMode_B03ID <= 2  ~ "walk",
            MainMode_B03ID <= 3  ~ "cycle",
            MainMode_B03ID <= 4  ~ "other",
            MainMode_B03ID <= 8  ~ "car",
            MainMode_B03ID <= 12 ~ "motorcycle",
            MainMode_B03ID <= 17 ~ "other",
            MainMode_B03ID <= 24 ~ "PT",
            TRUE                 ~ "other"))

# Location type helper for classifying trip origins & destinations
getLocationType <- function(rawLocation) {
  case_when(rawLocation == -10 ~ "unknown",
            rawLocation == -8  ~ "unknown",
            rawLocation == 1   ~ "W",   # work
            rawLocation == 2   ~ "B",   # business
            rawLocation == 3   ~ "E",   # education
            rawLocation <= 5   ~ "S",   # shopping
            rawLocation <= 8   ~ "O",   # other
            rawLocation <= 13  ~ "R",   # recreational
            rawLocation == 14  ~ "2",   # holiday base
            rawLocation == 15  ~ "J",   # day trip / just walk
            rawLocation == 16  ~ "O",
            rawLocation <= 22  ~ "A",   # accompanying
            rawLocation == 23  ~ "H")   # home
}

trips[["t.purpose.nts"]] <- trips$TripPurpFrom_B01ID
trips[["t.origin"]]      <- with(trips, getLocationType(TripPurpFrom_B01ID))
trips[["t.destination"]] <- with(trips, getLocationType(TripPurpTo_B01ID))

# Trip purpose (home-based outward legs only; return legs and business excluded)
trips[["t.purpose"]] <- with(trips,
  case_when((t.destination %in% c("unknown", "B"))                      ~ "NA",
            (t.destination == "J" & !(t.mode2 %in% c("cycle","walk")))  ~ "NA",
            (t.destination == "J")                                       ~ "RRT",
            (t.origin == "H" & t.destination == "2")                    ~ "HB2",
            (t.origin == "H" & t.destination == "W")                    ~ "HBW",
            (t.origin == "H" & t.destination == "E")                    ~ "HBE",
            (t.origin == "H" & t.destination == "S")                    ~ "HBS",
            (t.origin == "H" & t.destination == "A")                    ~ "HBA",
            (t.origin == "H" & t.destination == "R")                    ~ "HBR",
            (t.origin == "H" & t.destination == "O")                    ~ "HBO",
            (t.destination == "H")                                       ~ "NA",
            (t.origin == "J")                                            ~ "NA",
            (t.origin == "W" | t.destination == "W")                    ~ "NHBW",
            TRUE                                                         ~ "NHBO"))

# Full trip purpose (includes both outward and return legs)
trips[["t.full_purpose"]] <- with(trips,
  case_when((t.origin == "J" | t.destination == "J")                                               ~ "RRT",
            ((t.origin == "H" & t.destination == "2") | (t.origin == "2" & t.destination == "H")) ~ "HB2",
            ((t.origin == "H" & t.destination == "W") | (t.origin == "W" & t.destination == "H")) ~ "HBW",
            ((t.origin == "H" & t.destination == "E") | (t.origin == "E" & t.destination == "H")) ~ "HBE",
            ((t.origin == "H" & t.destination == "S") | (t.origin == "S" & t.destination == "H")) ~ "HBS",
            ((t.origin == "H" & t.destination == "A") | (t.origin == "A" & t.destination == "H")) ~ "HBA",
            ((t.origin == "H" & t.destination == "R") | (t.origin == "R" & t.destination == "H")) ~ "HBR",
            ((t.origin == "H" & t.destination == "O") | (t.origin == "O" & t.destination == "H")) ~ "HBO",
            (t.destination == "H")                                                                  ~ "NA",
            (t.origin == "W" | t.destination == "W")                                               ~ "NHBW",
            TRUE                                                                                     ~ "NHBO"))

# Round-trip flags
trips[["t.is_RRT"]]  <- with(trips, t.origin == t.destination)
trips[["t.is_RRTH"]] <- with(trips, t.origin == t.destination & t.origin == "H")

# Distance (miles → km), times, speed
trips[["t.distance"]]      <- with(trips, TripDisIncSW * 1.60934)
trips[["t.departureTime"]] <- trips$TripStart
trips[["t.arrivalTime"]]   <- trips$TripEnd
trips[["t.travelTime.wtd"]] <- trips$JTTXSC
trips[["t.tripTime.wtd"]]   <- trips$JOTXSC
trips[["t.speed"]]          <- with(trips, TripDisIncSW * 1.60934 * 60 / TripTotalTime)

# Flag unrealistic speeds (walking > 10 km/h; cycling < 2 or > 25 km/h)
trips[["t.badSpeed"]] <- with(trips,
  (MainMode_B03ID <= 2 & t.speed > 10) |
  (MainMode_B03ID == 3 & t.speed < 2)  |
  (MainMode_B03ID == 3 & t.speed > 25))

trips[["t.dow"]] <- trips$TravDay

# ── 7. STAGE VARIABLES ────────────────────────────────────────────────────────

stages <- stages %>% within({
  s.mode <- case_when(StageMode_B03ID <= 2  ~ "walk",
                      StageMode_B03ID == 3  ~ "cycle",
                      StageMode_B03ID == 4  ~ "other",
                      StageMode_B03ID <= 6  ~ "carD",
                      StageMode_B03ID <= 8  ~ "carP",
                      StageMode_B03ID <= 10 ~ "carD",
                      StageMode_B03ID <= 12 ~ "carP",
                      StageMode_B03ID <= 17 ~ "other",
                      StageMode_B03ID <= 21 ~ "bus",
                      StageMode_B03ID <= 24 ~ "train",
                      TRUE                  ~ "other")
  s.distance <- StageDistance * 1.60934
  s.time     <- StageTime * 60
})

# ── 8. FINALISE & WRITE ───────────────────────────────────────────────────────

# Retain only derived variables (drop raw NTS column names)
NTS <- list()
NTS$rawData    <- rawData
NTS$households <- dplyr::select(households, starts_with("hh."))
NTS$persons    <- dplyr::select(persons,    starts_with(c("hh.", "p.")))
NTS$trips      <- dplyr::select(trips,      starts_with(c("hh.", "p.", "t.")))
NTS$stages     <- dplyr::select(stages,     starts_with(c("hh.", "p.", "t.", "s.")))

rm(rawData, households, persons, trips, stages)

write.csv(NTS$trips,      file = "datasets/trips.csv")
write.csv(NTS$persons,    file = "datasets/persons.csv")
write.csv(NTS$households, file = "datasets/households.csv")
