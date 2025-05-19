library(dplyr)
library(readr)
library(tidyr)
library(fastDummies)

source("utils2.R")

# step 0: empty synthetic dataset
preparedData <- data.frame()

# step 1: load the synthetic population here
pop <- read_csv("https://raw.githubusercontent.com/ITHIM/ITHIM-R/refs/heads/global/inst/extdata/local/bogota/population_bogota.csv")
ct <- read_csv("occupation_status.csv")
print(pop, n=100)

# Map population age groups to model age groups
map_to_model_age <- function(age) {
  age_map <- list(
    "0-4" = "0-4",
    "5-9" = "5-10",
    "10-14" = "11-15",  # Alternative: split 1/5 to "5-10", 4/5 to "11-15"
    "15-19" = "16-17",  # Alternative: split across "16-17" and "18-29"
    "20-24" = "18-29",
    "25-29" = "18-29",
    "30-34" = "30-49",
    "35-39" = "30-49",
    "40-44" = "30-49",
    "45-49" = "30-49",
    "50-54" = "50-59",
    "55-59" = "50-59",
    "60-64" = "60-69",
    "65-69" = "60-69",
    "70-74" = "70+",
    "75-79" = "70+",
    "80-84" = "70+",
    "85-89" = "70+",
    "90-94" = "70+",
    "95-99" = "70+",
    "100-150" = "70+"
  )
  
  mapped_age <- vapply(age, function(a) {
    if (a %in% names(age_map)) age_map[[a]] else NA_character_
  }, character(1))
  
  # # Handle unmapped ages
  # mapped_age[is.na(mapped_age)] <- "0-4"  # Default fallback
  # if (!all(mapped_age %in% model_age_groups)) {
  #   stop("Mapped ages contain invalid categories: ", paste(setdiff(mapped_age, model_age_groups), collapse = ", "))
  # }
  
  return(mapped_age)
}

# Set fraction
fraction <- 1/100  # 1% of population
total_population <- sum(pop$population)
sample_size <- round(total_population * fraction)
cat("Sample size:", sample_size, "\n")
# Sample size: 78007

pop$prob <- pop$population / total_population

set.seed(123)  # For reproducibility
sample_counts <- rmultinom(1, size = sample_size, prob = pop$prob)
pop$sample_count <- as.vector(sample_counts)

# Expand counts into individual rows using uncount
sample_df <- pop %>%
  filter(sample_count > 0) %>%
  uncount(sample_count) %>%
  dplyr::select(sex, age)

sample_df$age_mapped <- map_to_model_age(sample_df$age)
sample_df$p.age_gr <- as.numeric(factor(sample_df$age_mapped, levels = c("0-4", "5-10", "11-15", "16-17", "18-29", "30-49", "50-59", "60-69", "70+"), labels = 1:9))
sample_df$p.female <- ifelse(sample_df$sex == "Female", 1, 0)
head(sample_df)
  
# Verify sample size and distribution
cat("Sampled dataframe size:", nrow(sample_df), "\n")
cat("Sampled age distribution:\n")
print(table(sample_df$sex, sample_df$age))

# Attach occupation status
ct<-ct %>%
  mutate(total = student + employed + unemployed + retired) %>%
  # Handle cases where total=0 to avoid division by zero
  mutate(total = ifelse(total == 0, 1, total)) %>%
  mutate(
    student_prob = student / total,
    employed_prob = employed / total,
    unemployed_prob = unemployed / total,
    retired_prob = retired / total
  ) %>%
  dplyr::select(-c(student,employed,unemployed,retired,total))

sample_df <- sample_df %>%
  left_join(ct, by = c("p.age_gr","p.female"))

# Assuming your tibble is named 'df'
sample_df <- sample_df %>%
  rowwise() %>%
  mutate(
    p.status = sample(
      c("student", "employed", "unemployed", "retired"),
      size = 1,
      prob = c(student_prob, employed_prob, unemployed_prob, retired_prob)
    )
  ) %>%
  ungroup() %>%
  dplyr::select(p.age_gr, p.female, p.status)

sample_df %>% sample_n(10)

preparedData <- dummy_cols(sample_df, select_columns= c('p.age_gr'))
preparedData <- dummy_cols(preparedData, select_columns= c('p.status'))

# step 2: generate the weekly number of trips by person and purpose

zeroModelHBmandatory<-readRDS(file = "models/zeroModelHBmandatory.rds")
countModelHBmandatory<-readRDS(file = "models/countModelHBmandatory.rds")
hurdleModelHBdiscretionary<-readRDS(file = "models/hurdleModelHBdiscretionary.rds")
hurdleModelNHB<-readRDS(file = "models/hurdleModelNHB.rds")

# HBmandatory
preparedData$predictedHBmandatory <- predict_trips(preparedData, zeroModelHBmandatory, countModelHBmandatory, 0.5, TRUE)

# HBdiscretionary
preparedData$predictedHBdiscretionary <- predict_hurdle(hurdleModelHBdiscretionary, preparedData)

# NHB
preparedData$predictedNHB <- predict_hurdle(hurdleModelNHB, preparedData)

# Total
preparedData <- preparedData %>% 
  mutate(Total = 2*predictedHBmandatory + 2*predictedHBdiscretionary + predictedNHB)

#
preparedData %>%
  group_by(p.age_gr, p.female, p.status) %>%
  summarise(average_weeklyTrips = mean(Total, na.rm = TRUE)) %>%
  ungroup()

write_csv(preparedData, "datasets/bogotaSP1.csv")