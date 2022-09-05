# Install and load the survival and reshape2 packages
install.packages(survival)
install.packages(reshape2)
library(survival)
library(reshape2)

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
coef(wb)

# Visualizing your Weibull Model
# In order to create visualizations, you should set criteria for personas (i.e., prototypical participants).
# x1 supplies syntax for a categorical covariate.
# x2 supplies syntax for a continuous covariate and visualizes at the 25th, 50th, and 75th percentiles.

vis_dat <- expand.grid(
  x1 = levels(your_data$x1),
  x2 = quantile(your_data$x2, probs = c(0.25, 0.50, 0.75)))

# Now we must compute time for each probability and bind time to visdat.
# In the final line, replace all items in the list, denoted by c, with your covariates of interest. Retain the quotation marks. 

surv <- seq(.99, .01, by = -.01)
time <- predict(wb, type = 'quantile', p = 1 - surv, newdata = data.frame(1))
surv_vis_dat_w <- cbind(vis_dat, time)
surv_vis_dat <- melt(surv_vis_dat_w, id.vars = c('x1', 'x2', 'x3'), variable.name = 'id', value.name = 'time')

# Create your plot.
ggsurvplot_df(surv_vis_dat, surv.geom = geom_line,
              linetype = 'x1', color = 'x2', legend.title = NULL)
