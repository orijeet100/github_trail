# Survey data
survey_data <- data.frame(
  Alzheimer = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),  # 1 for Yes, 0 for No
  AgeGroup = factor(c('60-69', '70-79', '80+', '60-69', '80+', '70-79', '70-79', '60-69', '80+', '70-79')),
  Gender = factor(c('Male', 'Female', 'Male', 'Female', 'Female', 'Female', 'Male', 'Male', 'Male', 'Female'))
)

print(survey_data)



# Census data
census_data <- data.frame(
  AgeGroup = factor(c('60-69', '60-69', '70-79', '70-79', '80+', '80+')),
  Gender = factor(c('Male', 'Female', 'Male', 'Female', 'Male', 'Female')),
  Proportion = c(0.1, 0.12, 0.15, 0.16, 0.13, 0.14)
)

print(census_data)


install.packages("rstan")
library(rstan)


survey_data$AgeGroup <- as.integer(survey_data$AgeGroup)
survey_data$Gender <- as.integer(survey_data$Gender)



# Stan model code for Bayesian logistic regression
stan_code <- "
data {
    int<lower=0> N;           // number of observations
    int<lower=0> K;           // number of predictors
    matrix[N, K] X;           // predictor matrix
    int<lower=0,upper=1> y[N]; // binary response variable
}
parameters {
    vector[K] beta;           // coefficients for predictors
}
model {
    beta ~ normal(0, 5);      // priors on coefficients
    y ~ bernoulli_logit(X * beta); // logistic regression
}
"

# Prepare data for Stan model
X <- model.matrix(~ AgeGroup + Gender, data=survey_data)[, -1]
y <- survey_data$Alzheimer
stan_data <- list(
  N = nrow(X),
  K = ncol(X),
  X = X,
  y = y
)

# Compile and fit the model
stan_model <- stan(model_code = stan_code, data = stan_data, iter = 1000, chains = 4)

# Print summary of the model
print(summary(stan_model))



# Prepare census data for prediction
census_X <- model.matrix(~ AgeGroup + Gender, data=census_data)[, -1]

# Extract the samples of beta coefficients
beta_samples <- extract(stan_model)$beta

# Calculate predictions for each combination
preds <- census_X %*% t(beta_samples)
alzheimer_pred <- colMeans(1 / (1 + exp(-preds)))

# Add predictions to census data
census_data$Pred <- alzheimer_pred
