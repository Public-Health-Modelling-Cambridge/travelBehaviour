library(MASS)
library(ggplot2)
library(tidyr)

predict_hurdle <- function(model, data, seed = NULL) {
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Predict probability of having >0 trips (binary part)
  prob_positive <- predict(model, 
                           newdata = data, 
                           type = "zero")  # P(Y > 0)
  
  # Predict expected counts *if* positive (count part)
  expected_counts <- predict(model, 
                             newdata = data, 
                             type = "count")  # E(Y | Y > 0)
  
  # Simulate values
  simulated <- sapply(1:nrow(data), function(i) {
    # Skip if probability or expected_counts is NA
    if (is.na(prob_positive[i]) || is.na(expected_counts[i])) {
      return(NA)
    }
    
    # Clamp probability to [0, 1]
    prob <- pmin(pmax(prob_positive[i], 0), 1)
    
    # Generate positive count with probability prob
    if (rbinom(1, 1, prob) == 1) {
      rnbinom(1, 
              mu = expected_counts[i], 
              size = model$theta)
    } else {
      0
    }
  })
  
  return(simulated)
}

plot_trips_comparison <- function(df) {
  # Reshape data to long format
  df_long <- pivot_longer(df, 
                          cols = c(mean_trips_sim, mean_trips_obs), 
                          names_to = "type", 
                          values_to = "trips")
  
  # Create barplot
  ggplot(df_long, aes(x = interaction(p.age_gr, p.female), 
                      y = trips, 
                      fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("mean_trips_sim" = "blue", "mean_trips_obs" = "red"),
                      labels = c("Observed", "Simulated")) +
    labs(x = "Age Group and Gender", 
         y = "Mean Trips", 
         fill = "Type",
         title = "Comparison of Simulated vs Observed Mean Trips") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

predict_discretionary_trips <- function(model, component, data, seed = NULL) {
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Predict probability of having >0 trips (binary part)
  prob_positive <- predict(model[[component]], 
                           newdata = data, 
                           type = "zero")  # P(Y > 0)
  
  # Predict expected counts *if* positive (count part)
  expected_counts <- predict(model[[component]], 
                             newdata = data, 
                             type = "count")  # E(Y | Y > 0)
  
  # Simulate values
  simulated <- sapply(1:nrow(data), function(i) {
    # Skip if probability or expected_counts is NA
    if (is.na(prob_positive[i]) || is.na(expected_counts[i])) {
      return(NA)
    }
    
    # Clamp probability to [0, 1]
    prob <- pmin(pmax(prob_positive[i], 0), 1)
    
    # Generate positive count with probability prob
    if (rbinom(1, 1, prob) == 1) {
      rnbinom(1, 
              mu = expected_counts[i], 
              size = model[[component]]$theta)
    } else {
      0
    }
  })
  
  return(simulated)
}


predict_trips <- function(df, glm_model, m_tripGenPolr, threshold = 0.5, sample_from_probs = FALSE) {
  #' Predict trip generation using a two-stage hurdle model approach
  #'
  #' @param df Dataframe containing predictor variables
  #' @param glm_model Binary model (e.g., logistic) for trip/no-trip decision
  #' @param m_tripGenPolr Count model for number of trips (POLR or similar)
  #' @param threshold Probability threshold for considering a trip (default 0.5)
  #' @param sample_from_probs Whether to sample from probabilities (TRUE) or take class prediction (FALSE)
  #' @return Dataframe with added prob_trip and num_trips columns
  
  # 1. Predict probability of making at least one trip
  df$prob_trip <- predict(glm_model, newdata = df, type = "response")
  
  # Initialize num_trips with 0 (no trips)
  df$num_trips <- 0
  
  # Identify potential trip makers
  trip_makers_idx <- which(runif(nrow(df)) < df$prob_trip)
  # trip_makers_idx <- which(df$prob_trip > threshold)
  
  
  
  if (length(trip_makers_idx) > 0) {
    trip_makers <- df[trip_makers_idx, ]
    
    if (sample_from_probs) {
      # Get probability matrix for each count level
      count_probs <- predict(m_tripGenPolr, newdata = trip_makers, type = "probs")
      
      # Sample from the distribution for each individual
      df$num_trips[trip_makers_idx] <- apply(count_probs, 1, function(p) {
        sample(0:(length(p)-1), size = 1, prob = p)
      })
    } else {
      # Use predicted class directly
      df$num_trips[trip_makers_idx] <- as.numeric(as.character(
        predict(m_tripGenPolr, newdata = trip_makers, type = "class")
      ))
    }
  }
  
  return(df$num_trips)
}
