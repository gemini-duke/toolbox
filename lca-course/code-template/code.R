# -------------------------------------------------------------------------
# Codes for Latent Class Analysis -----------------------------------------
# -------------------------------------------------------------------------



# Load packages -----------------------------------------------------------

if (!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if (!require(correlation)) {install.packages("correlation")}; library(correlation)
if (!require(gtsummary)) {install.packages("gtsummary")}; library(gtsummary)
if (!require(huxtable)) {install.packages("huxtable")}; library(huxtable)
if (!require(poLCA)) {install.packages("poLCA")}; library(poLCA)


# Load supplementary functions --------------------------------------------
source("lca-functions.R")




# LCA ---------------------------------------------------------------------

# Model formula ----
my_formula <- cbind(CK, PCK, TLM, THS, CMD, ILS, CSP, KKY) ~ 1

# Model estimation ----
set.seed(42)
n_models <- 6 # change this number
lca <- list()
for (i in seq_len(n_models)) {
  lca[[i]] <- poLCA::poLCA(formula = my_formula,
                           data = dataset,
                           nclass = i,
                           maxiter = 5000,
                           nrep = 100)
}


# Table of fit indices ----
fit_table <- tbl_lca(lca_models = lca)
fit_table


# Fit indices ----
# sample size
n <- lca[[1]]$Nobs

# Smallest class size (%)
small_class <- map_chr(lca, function(x) gtsummary::style_percent(min(x$P), digits = 1))
# Log likelihood
llik <- map_dbl(lca, function(x) x$llik)

# BIC
bic <- map_dbl(lca, function(x) x$bic)

# AIC
aic <- map_dbl(lca, function(x) x$aic)

# CAIC and SABIC
npar <- map_dbl(lca, function(x) x$npar)
caic <- (-2 * llik) + npar * (log(n) + 1)
sabic <- (-2 * llik) + npar * log((n + 2) / 24)

# Approximate Weight of Evidence
awe <- (-2 * llik) + npar * (log(n) + 1.5)

# Bayes Factor
bf <- c()
for (i in 1:length(lca)) {
  if (i + 1 <= length(lca)) {
    a <- -0.05 * bic[i]
    b <- -0.05 * bic[i + 1]
    bf[i] <- gtsummary::style_sigfig(exp(a - b), digits = 3)
  } else {bf[i] <- NA}
}

# Entropy
entropy <- function(x) {
  numerator <- -sum(x$posterior * log(x$posterior), na.rm = TRUE)
  denominator <- x$N * log(ncol(x$posterior))
  1 - (numerator / denominator)
}
ent <- map_dbl(lca, entropy)




# Elbow plot ----
elbow_plot(lca_models = lca)


# Average latent class posterior probability (AvePP) ----
my_model <- lca[[5]] # 5 represents the 5-class solution
avepp <- tbl_avepp(lca_model = my_model)


# Inspect indicators probabilities ----
my_model$probs


# Profile plot (item probability plot)
variables_names <- c("Cont. Know.", 
                     "P. Cont. Know.",
                     "Teach./Learn. M.",
                     "Teach. Het. Stud.",
                     "Class. M. and D",
                     "Ind. Learn. S.",
                     "C. Stud/Par",
                     "Know. K and Y")
class_names <- c("High on general pedagogical topics",
                 "High on teaching methods",
                 "High on all activities",
                 "Low on all activities",
                 "High on CK and TM")


profile_plot(lca_model = my_model,
             var_names = variables_names,
             class_names = class_names,
             binary = TRUE)



# Creating a column with estimated classes ----
dataset$lca_classes <- my_model$predclass

