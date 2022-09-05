# Install and load the survival package
install.packages(survival)
library(survival)

# Load data in CSV format.
# Ensure that data with CSV includes variable names in the CSV header.
# Assume delimiter (i.e., the separator of values) is a comma.

your_data <- read.csv(file = 'C:/Users/YourName/TheFolder/TheFile.csv', header = TRUE)

# Perform exploratory data analysis.

exp_data <- function(censor) {
  table(censor)
  barplot(censor) }

# Replace the "censor" following the "$" symbol with your censoring variable.
# The censoring variable is the outcome of interest.
# For example, if you are studying time until a patient dies, then the censor would be the variable indicating death.

your_censor <- your_data$censor
exp_data(your_censor)

# Replace the "time" following the "$" symbol with the time until event
your_time <- your_data$time

# Now create a new survival objectsurv_obj <- Surv(your_time, your_censor)

# Determine the Kaplan-Meier estimate and plot it
# The table shows the number at risk at each time point. The dotted line displays the median time-to-event of interest.

km <- survfit(Surv(your_time, your_censor) ~ 1)
ggsurvplot(km, risk.table = TRUE, surv.median.line = 'hv', pval = TRUE)

# In all likelihood, you want to include covariates (or predictors) in your model.
# If so, you should use a Weibull estimator.
# Syntax for Weibull estimator is below. Replace x1, x2, and x3 with your covariates of interest.
# Add more covariates if desired.

wb <- survreg(Surv(your_time, your_censor) ~ x1 + x2 + x3)
