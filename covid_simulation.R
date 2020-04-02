library(dplyr)

# Function that returns how many tests are needed to verify who is ill.
# This function is stochastic so in cases where there is more than 1 ill person
# it might return different results every time. Example
# If the sample contains 4 people out of which 2 are ill we can have two situations:
#
# First situation:
# First stage: (1, 1, 0, 0) -> at least one person sick
# Second stage: (1, 0) , (1, 0) -> in both samples there is at least one sick person
# Third stage: (1), (0), (1), (0) -> all sick patatients are identified
# 7 tests in total
#
# Second sitation:
# First stage: (1, 1, 0, 0) -> at least one person sick
# Second stage: (1, 1), (0, 0) -> at least one patient in first sample is sick
# Third stage: (1), (1) -> both patients from first sample are sick
# 5 tests in total
#
# Input:
#  o results - vertor containing 0/1 values. Length of the vector represents number of patients to test
#     o 0 - indicates healthy person
#     o 1 - indicates sick person
#
# Output:
# Number of tests need to verify which patients are sick.
test <- function(results){
  
  # If there is only 1 patient then only 1 test is needed
  if (length(results) == 1){
    return(1)
  }

  # If all patients are healthy then only 1 test is needed
  if (sum(results) == 0){
    return(1)
  # Otherwise at least one patient is sick.
  # We split it recursively into two samples and test them separately
  } else {
    # Random patients to test in first sample
    v <- sample(1:length(results), floor(length(results) / 2))
    return(1 + test(results[v]) + test(results[-v]))
  }
}

# Number of simulations to be made. The more simulations the more precise results
n <- 1000000

# Indicates possible values of how many patients we want to test together
possible_k <- c(2, 4, 8, 16)

# Data frame with results
# p - probability that person is infected
# k - number of patients test together
# impact_factor - how many more patients can be test due to that method.
#                 Example: Value 2.5 indicates that instead of 100 patients we can check 250 patients.
result <- data.frame(p = numeric(0), k = numeric(0), impact_factor = numeric(0))

for (p in seq(0.005, 0.1, 0.005)){
  for (k in possible_k){
    test_results <- sapply(1:n, function(x) {test(rbinom(k, 1, p))})
    impact_factor <- 1 / (mean(test_results) / k)
    result <- rbind(result, data.frame(p = p, k = k, impact_factor = impact_factor))
  }
}

print(result)