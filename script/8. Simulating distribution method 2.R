# Load required libraries
library(survey)
library(dplyr)
library(Hmisc)  # for weighted quantiles if needed

set.seed(123)  # For reproducibility

# ========= 1. Extract Data and Compute Original Mean =========
# Convert the survey design object to a data.frame and extract weights
brazil_income_data  <- readRDS("./intermediarios/renda_PNADc_brasil2023visita_1.rds")
df <- brazil_income_data$variables

# Compute the original (weighted) mean income of Brazil
mean_income <- as.numeric(svymean(~VD5008_DEF, brazil_income_data, na.rm = TRUE))

# ========= 2. Calibrate a Lognormal Distribution Using Uruguay's Gini =========
# For a lognormal distribution, the Gini is given by:
#   G = 2 * pnorm(sigma/sqrt(2)) - 1
# Inverting, we obtain:
#   sigma_target = sqrt(2) * qnorm((G + 1)/2)

uruguai_gini_index <- 0.406
uruguay_income_share <- readRDS("./intermediarios/income_share_accumulated_uruguay.rds")
uruguay_decile_shares  <- uruguay_income_share$IncomeShareAccumulated/100
uruguay_decile_shares[10] <- 1

# Define the optimization function
objective_function <- function(sigma) {
  mu <- log(mean_income) - (sigma^2) / 2

  # Simulate a large sample from the candidate lognormal distribution
  sim_sample <- rlnorm(100000, meanlog = mu, sdlog = sigma)

  # Calculate cumulative decile shares
  p <- seq(0, 1, by = 0.1)
  decile_breaks <- quantile(sim_sample, probs = p)
  total_income <- sum(sim_sample)

  cum_income_shares <- sapply(2:length(decile_breaks), function(i) {
    sum(sim_sample[sim_sample <= decile_breaks[i]]) / total_income
  })

  # Theoretical cumulative decile shares (excluding 0%)
  theoretical_cum_shares <- cum_income_shares

  # Compute error for decile shares
  error_shares <- sum((theoretical_cum_shares - uruguay_decile_shares)^2)

  # Theoretical Gini
  theoretical_gini <- 2 * pnorm(sigma / sqrt(2)) - 1
  error_gini <- (theoretical_gini - uruguai_gini_index)^2

  # Total loss function (weights can be adjusted if needed)
  return(error_shares + error_gini)
}

# Optimize sigma
initial_sigma <- sqrt(2) * qnorm((uruguai_gini_index + 1) / 2)
opt_result <- optim(par = initial_sigma, fn = objective_function, method = "BFGS")
sigma_target <- opt_result$par
mu_target <- log(mean_income) - (sigma_target^2) / 2


# Define the target quantile function based on these parameters.
target_quantile <- function(q) {
  exp(mu_target + sigma_target * qnorm(q))
}

# ========= 3. Quantile Mapping =========
df <- df %>%
  arrange(VD5008_DEF) %>%
  mutate(cum_weight = cumsum(V1032),
         total_weight = sum(V1032, na.rm = TRUE),
         emp_quantile = cum_weight / total_weight)

df <- df %>%
  mutate(VD5008_DEF_adjusted = target_quantile(emp_quantile),
         VD5008_DEF_adjusted = if_else(VD5008_DEF_adjusted == Inf, 46130.58, VD5008_DEF_adjusted))

# Adjust mean
adjusted_mean <- sum(df$VD5008_DEF_adjusted * df$V1032, na.rm = TRUE) / sum(df$V1032, na.rm = TRUE)
scaling_factor <- mean_income / adjusted_mean
df <- df %>%
  mutate(VD5008_DEF_adjusted = VD5008_DEF_adjusted * scaling_factor)

# ========= 4. Update Survey Design =========
brazil_income_data_adjusted <- update(brazil_income_data,
                                      VD5008_DEF_adjusted = df$VD5008_DEF_adjusted)

# ========= 5. Poverty Calculation =========
poverty_line <- 1320 / 2
brazil_income_data <- update(brazil_income_data,
                             poor = as.numeric(VD5008_DEF < poverty_line))
brazil_income_data_adjusted <- update(brazil_income_data_adjusted,
                                      poor_adjusted = as.numeric(VD5008_DEF_adjusted < poverty_line))

# Compute poverty rates
original_poverty_rate <- as.numeric(svymean(~poor, brazil_income_data, na.rm = TRUE))
adjusted_poverty_rate <- as.numeric(svymean(~poor_adjusted, brazil_income_data_adjusted, na.rm = TRUE))

# ========= 6. Output =========
cat("Optimal sigma:", sigma_target, "\n")
cat("Optimal mu:", mu_target, "\n")
cat("Poverty line:", poverty_line, "\n")
cat("Original poverty rate:", original_poverty_rate, "\n")
cat("Adjusted poverty rate:", adjusted_poverty_rate, "\n")