## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dev = "svglite",
  fig.ext = "svg"
)
library(corrselect)

## ----eval = FALSE-------------------------------------------------------------
# # Install from CRAN
# install.packages("corrselect")
# 
# # Or install development version from GitHub
# # install.packages("devtools")
# devtools::install_github("GillesColling/corrselect")

## -----------------------------------------------------------------------------
data(mtcars)

# Remove correlated predictors (threshold = 0.7)
pruned <- corrPrune(mtcars, threshold = 0.7)

# Results
cat(sprintf("Reduced from %d to %d variables\n", ncol(mtcars), ncol(pruned)))
names(pruned)

## -----------------------------------------------------------------------------
attr(pruned, "removed_vars")

## -----------------------------------------------------------------------------
# Prune based on VIF (limit = 5)
model_data <- modelPrune(
  formula = mpg ~ .,
  data = mtcars,
  limit = 5
)

# Results
cat("Variables kept:", paste(attr(model_data, "selected_vars"), collapse = ", "), "\n")
cat("Variables removed:", paste(attr(model_data, "removed_vars"), collapse = ", "), "\n")

## -----------------------------------------------------------------------------
results <- corrSelect(mtcars, threshold = 0.7)
show(results)

## -----------------------------------------------------------------------------
as.data.frame(results)[1:5, ]  # First 5 subsets

## -----------------------------------------------------------------------------
subset_data <- corrSubset(results, mtcars, which = 1)
names(subset_data)

## -----------------------------------------------------------------------------
# Create mixed-type data
df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  cat1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
  ord1 = ordered(sample(1:5, 100, replace = TRUE))
)

# Handle mixed types automatically
results_mixed <- assocSelect(df, threshold = 0.5)
show(results_mixed)

# Verify all pairwise associations are below threshold
cat("Max pairwise association:", max(results_mixed@max_corr), "\n")

## -----------------------------------------------------------------------------
# Force "mpg" to remain in all subsets
pruned_force <- corrPrune(
  data = mtcars,
  threshold = 0.7,
  force_in = "mpg"
)

# Verify forced variable is present
"mpg" %in% names(pruned_force)

## -----------------------------------------------------------------------------
sessionInfo()

