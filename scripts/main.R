################################################################################
#' Replication for Benson, Newton, Roy, and Tucker: Can UN Peacekeeping
#' Improve Health Outcomes Long After the Shooting Stops?
#'
#' Bayesian models are using rstan, so make sure you have Stan installed first
#' and that it can communicate with R.
#'
#' The Stan models can take awhile, so multiple cores are used to speed up the
#' process. You can changes the number of core in the scripts/bayes_models.R
#' and scripts/bayes_models_standardized.R
#'
#'
#' This file will produce the complete results. Individual scripts can be run
#' seperately, but packges have to be leaded first, and the scripts must be run
#' in order.
################################################################################
library(bayesplot)
library(dplyr)
library(ggplot2)
library(haven)
library(patchwork)
library(rethinking)
library(sandwich)

# Generate the raw and standardized results from table 1 and the appendix to
# use to calculate priors.
source("scripts/ghobarah_etal_replication.R")

# Take the coeffecients from the above script and average them for priors.
# Standard errors are first converted to standard deviations and then averaged.
source("scripts/generate_priors.R")

# Run the models with standardized variables.
source("scripts/bayes_models_standardized.R")

# Generate the coeffecient plots seen in figures 2 and 3.
source("scripts/coef_plots.R")

# Run the models with unstandized variables.
source("scripts/bayes_models.R")

# Generate the distribution plots seen in figure 4.
source("scripts/dist_plots.R")
