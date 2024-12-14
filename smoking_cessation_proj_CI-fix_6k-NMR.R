## ----setup, include = FALSE, message = FALSE-----------------------------------------------------
# --- Preamble ---
# Date of last update: Nov. 12, 2024
# R Version: 4.3.1
# Package Versions:
#   tidyverse: 2.0.0
#   knitr: 1.45
#   kableExtra: 1.3.4
#   ggplot2: 3.4.3
#   naniar 1.0.0
#   visdat 0.6.0
#   car 3.1-2
#   lme4 1.1-34
#   ggpubr 0.6.0
#   glmnet 4.1-8
#   mice 3.16.0
#   caret 6.0-94
#   pROC 1.18.4


setwd("~/GitHub/smoking_cessation_proj")

# Knitr Engine Setup
knitr::opts_chunk$set(message=F, 
                      warning=F, 
                      error=F, 
                      echo=F, 
                      fig.pos = "H" ,
                      fig.align = 'center')

# Packages
options(kableExtra.latex.load_packages = FALSE) # Required to avoid floatrow error
library(knitr)
library(kableExtra)
library(ggplot2)
library(naniar) # For mcar_test()
library(visdat) # For vis_dat()
library(tidyverse)
library(car) # For qqPlot(), vif()
library(lme4)
library(lmerTest) # Satterthwaite approximation for computing p-values on lme4
library(ggpubr)
library(glmnet)
library(mice)
library(caret)
library(pROC)




## ----read-in and clean---------------------------------------------------------------------------
data <- read.csv("data/project2.csv", comment.char="#")





## ------------------------------------------------------------------------------------------------


# Convert variables 
data <- data %>%
  mutate(
    abst = factor(abst, levels = c(0, 1), labels = c("No", "Yes")), 
    Var = factor(Var, levels = c(0, 1),
                         labels = c("Placebo", "Varenicline")), 
    BA = factor(BA, levels = c(0, 1), labels = c("Standard", "BA")),
    age_ps = as.numeric(age_ps),
    sex_ps = factor(sex_ps, levels = c(1, 2), labels = c("Male", "Female")),
    NHW = factor(NHW, levels = c(0, 1), labels = c("No", "Yes")),
    Black = factor(Black, levels = c(0, 1), labels = c("No", "Yes")),
    Hisp = factor(Hisp, levels = c(0, 1), labels = c("No", "Yes")),
    inc = factor(inc, levels = 1:5,
                 labels = c("Less than $20,000",
                            "$20,000–35,000",
                            "$35,001–50,000",
                            "$50,001–75,000",
                            "More than $75,000"), ordered = TRUE),
    edu = factor(edu, levels = 1:5,
                 labels = c("Grade School",
                            "Some High School",
                            "High School Graduate/GED",
                            "Some College/Technical School", 
                            "College Graduate"), ordered = TRUE),
    ftcd_score = as.integer(ftcd_score),
    ftcd.5.mins = factor(ftcd.5.mins, levels = c(0, 1), labels = c("No", "Yes")),
    bdi_score_w00 = as.integer(bdi_score_w00), 
    cpd_ps = as.integer(cpd_ps),
    crv_total_pq1 = as.integer(crv_total_pq1), 
    hedonsum_n_pq1 = as.integer(hedonsum_n_pq1),
    hedonsum_y_pq1 = as.integer(hedonsum_y_pq1), 
    shaps_score_pq1 = as.integer(shaps_score_pq1), 
    otherdiag = factor(otherdiag, levels = c(0, 1), labels = c("No", "Yes")),
    antidepmed = factor(antidepmed, levels = c(0, 1), labels = c("No", "Yes")),
    mde_curr = factor(mde_curr, levels = c(0, 1), labels = c("Past", "Current")),
    NMR = as.numeric(NMR),
    Only.Menthol = factor(Only.Menthol, levels = c(0, 1), labels = c("No", "Yes")),
    readiness = as.integer(readiness) 
  )





## ------------------------------------------------------------------------------------------------
# prep data for analysis
data <- data %>%
  mutate(abst = as.numeric(abst) - 1,
    edu = factor(recode_factor(edu,
                               "Grade School" = "High School or Less",
                               "Some High School" = "High School or Less",
                               "High School Graduate/GED" = "High School or Less"),
                 levels = c("High School or Less",
                            "Some College/Technical School",
                            "College Graduate"),
                 ordered = TRUE),
    inc = factor(recode_factor(inc,
                               "$50,001–75,000" = "More than $50,000",
                               "More than $75,000" = "More than $50,000"),
                 levels = c("Less than $20,000",
                            "$20,000–35,000",
                            "$35,001–50,000",
                            "More than $50,000"),
                 ordered = TRUE)
         )

data <- data %>%
  #mutate(logNMR = log(NMR)) %>% # Toggle off to test model fit w/o transform
  select(-c(Hisp, NHW))



## ------------------------------------------------------------------------------------------------
# --- Validation / Train split ---

set.seed(123)
split_indices <- createDataPartition(y = data$abst, p = 0.8, list = FALSE)

# split data training / validation sets
train_data <- data[split_indices, ]
validation_data <- data[-split_indices, ]
data <- train_data


## ----main effects lasso w bootstrap--------------------------------------------------------------
#GOOD; Q1; baseline
# Def All Params
B <- 6000      # num of bootstraps 
m <- 5       # num of multiple imputations per bootstrap
k <- 5       # num of cv folds for cv.glmnet
poly_degree <- 3  # degree for polynomial terms, set to 1 to test model fit w/o poly

# remove id 
data_complete <- data %>% select(-id) 

# Flag predictor vars
# exclude 'abst' from predictors
predictor_vars <- setdiff(names(data_complete), c("abst")) 

# Flag trt vars
treatment_vars <- c("BA", "Var")
baseline_vars <- setdiff(predictor_vars, treatment_vars)

# Flag numeric and int baseline vars
num_baseline_vars <- baseline_vars[sapply(data_complete[baseline_vars],
                                          function(x) is.numeric(x) & !is.integer(x))]

non_binary_factor_vars <- baseline_vars[sapply(data_complete[baseline_vars],
                                               function(x) is.integer(x))]
# Flag Binary baseline vars
binary_vars <- baseline_vars[sapply(data_complete[baseline_vars],
                                  function(x) length(unique(na.omit(x))) == 2)]

# Flag factor vars and store levels
factor_vars <- names(data_complete)[sapply(data_complete, is.factor)]
levels_list <- lapply(data_complete[, factor_vars, drop = FALSE], levels)

# --- Build Formula ---
# Main Effects 
main_effects <- paste("BA + Var +", 
                      paste(c(
                        paste0("poly(", non_binary_factor_vars,
                               ", ", poly_degree, ", raw=TRUE)"),
                        num_baseline_vars,  
                        binary_vars  
                      ), collapse = " + "))



# Full Model Formula
full_formula <- as.formula(paste("abst ~", main_effects))

# fix factor levels in the entire dataset (ensure consistency) BUGFIX
if (length(factor_vars) > 0) {
  for (var in factor_vars) {
    data_complete[[var]] <- factor(data_complete[[var]], levels = levels_list[[var]])
  }
}

# Gen master model matrix 
master_model_matrix <- model.matrix(full_formula, data = data_complete)
master_coef_names <- colnames(master_model_matrix)
total_coeffs <- length(master_coef_names)

# init storage for coefs
coef_matrix <- matrix(NA, nrow = B, ncol = total_coeffs)
colnames(coef_matrix) <- master_coef_names

# init object for nonzero coef indicator
nonzero_matrix <- matrix(0, nrow = B, ncol = total_coeffs)
colnames(nonzero_matrix) <- master_coef_names




## ----main effects coef lasso, eval = T-----------------------------------------------------------

# --- Main estimation and bootstrap loop ---

# --- Primary model fit on original data with imputation ---
# Initial imputation for primary fit
predictor_matrix <- make.predictorMatrix(data_complete)
predictor_matrix[, "abst"] <- 0
predictor_matrix["abst", ] <- 0
imp_primary <- mice(data_complete, m = m, printFlag = FALSE, seed = 123,
                   predictorMatrix = predictor_matrix, method = 'pmm')

# Stack imputations for primary fit
stacked_data_primary <- complete(imp_primary, action = "long", include = FALSE)
stacked_data_primary <- stacked_data_primary %>% dplyr::select(-.id, -.imp)

# Fix factor levels
if (length(factor_vars) > 0) {
  for (var in factor_vars) {
    stacked_data_primary[[var]] <- factor(stacked_data_primary[[var]], 
                                        levels = levels_list[[var]])
  }
}

# Create model matrix for primary fit
model_data_full <- model.matrix(full_formula, data = stacked_data_primary)
x_primary <- model_data_full[, -1]
y_primary <- stacked_data_primary$abst

# Remove zero variance predictors and align columns
zero_var_cols <- apply(x_primary, 2, function(x) var(x) == 0)
if (any(zero_var_cols)) {
  x_primary <- x_primary[, !zero_var_cols]
  current_coef_names <- colnames(x_primary)
} else {
  current_coef_names <- colnames(x_primary)
}

# Align columns with master names
master_coef_names_no_intercept <- master_coef_names[-1]
missing_cols <- setdiff(master_coef_names_no_intercept, current_coef_names)
if (length(missing_cols) > 0) {
  zeros_matrix <- matrix(0, nrow = nrow(x_primary), ncol = length(missing_cols))
  colnames(zeros_matrix) <- missing_cols
  x_primary <- cbind(x_primary, zeros_matrix)
}

extra_cols <- setdiff(current_coef_names, master_coef_names_no_intercept)
if (length(extra_cols) > 0) {
  x_primary <- x_primary[, !(colnames(x_primary) %in% extra_cols)]
}

x_primary <- x_primary[, master_coef_names_no_intercept]

# Fit primary Lasso model
cv_lasso_primary <- cv.glmnet(
  x = x_primary,
  y = y_primary,
  family = "binomial",
  alpha = 1,
  standardize = TRUE,
  relax = FALSE
)

primary_fit <- glmnet(
  x = x_primary,
  y = y_primary,
  family = "binomial",
  alpha = 1,
  lambda = cv_lasso_primary$lambda.min,
  standardize = TRUE,
  relax = FALSE
)

# Get aligned primary coefficients
primary_coef_values <- as.vector(coef(primary_fit))
primary_coef_names <- rownames(coef(primary_fit))
names(primary_coef_values) <- primary_coef_names
aligned_primary_coef <- setNames(rep(0, length(master_coef_names)), master_coef_names)
matched_primary <- names(primary_coef_values) %in% master_coef_names
aligned_primary_coef[names(primary_coef_values)[matched_primary]] <- 
  primary_coef_values[matched_primary]




primary_nonzero <- abs(aligned_primary_coef) > 1e-8  # New







## ----main effects bootstrap loop, eval = F-------------------------------------------------------
## 
## # --- Bootstrap loop for confidence intervals ---
coef_matrix <- matrix(NA, nrow = B, ncol = length(master_coef_names))
colnames(coef_matrix) <- master_coef_names

nonzero_matrix <- matrix(0, nrow = B, ncol = length(master_coef_names))
colnames(nonzero_matrix) <- master_coef_names

set.seed(1)
for (b in 1:B) {
  cat("Bootstrap iteration:", b, "of", B, "\n")

  bootstrap_sample <- data_complete %>%
    group_by(abst) %>%
    sample_frac(size = 1, replace = TRUE) %>%
    ungroup()

  predictor_matrix <- make.predictorMatrix(bootstrap_sample)
  predictor_matrix[, "abst"] <- 0
  predictor_matrix["abst", ] <- 0
  imp <- mice(bootstrap_sample, m = m, printFlag = FALSE, seed = b,
             predictorMatrix = predictor_matrix, method = 'pmm')

  stacked_data <- complete(imp, action = "long", include = FALSE)
  stacked_data <- stacked_data %>% dplyr::select(-.id, -.imp)

  if (length(factor_vars) > 0) {
    for (var in factor_vars) {
      stacked_data[[var]] <- factor(stacked_data[[var]], levels = levels_list[[var]])
    }
  }

  model_data_full <- model.matrix(full_formula, data = stacked_data)
  x <- model_data_full[, -1]
  y_impute <- stacked_data$abst

  zero_var_cols <- apply(x, 2, function(x) var(x) == 0)
  if (any(zero_var_cols)) {
    x <- x[, !zero_var_cols]
    current_coef_names <- colnames(x)
  } else {
    current_coef_names <- colnames(x)
  }

  missing_cols <- setdiff(master_coef_names_no_intercept, current_coef_names)
  if (length(missing_cols) > 0) {
    zeros_matrix <- matrix(0, nrow = nrow(x), ncol = length(missing_cols))
    colnames(zeros_matrix) <- missing_cols
    x <- cbind(x, zeros_matrix)
  }

  extra_cols <- setdiff(current_coef_names, master_coef_names_no_intercept)
  if (length(extra_cols) > 0) {
    x <- x[, !(colnames(x) %in% extra_cols)]
  }

  x <- x[, master_coef_names_no_intercept]

  # Fit bootstrap model using same lambda as primary fit
  boot_fit <- tryCatch({
    glmnet(
      x = x,
      y = y_impute,
      family = "binomial",
      alpha = 1,
      lambda = cv_lasso_primary$lambda.min,
      standardize = TRUE,
      relax = FALSE
    )
  }, error = function(e) {
    cat("Error in glmnet for bootstrap", b, ":", e$message, "\n")
    return(NULL)
  })

  if (is.null(boot_fit)) {
    next
  }

  coef_values <- as.vector(coef(boot_fit))
  coef_names <- rownames(coef(boot_fit))
  names(coef_values) <- coef_names

  aligned_coef_values <- setNames(rep(0, length(master_coef_names)), master_coef_names)
  matched <- names(coef_values) %in% master_coef_names
  aligned_coef_values[names(coef_values)[matched]] <- coef_values[matched]

  coef_matrix[b, ] <- aligned_coef_values
  nonzero_matrix[b, ] <- ifelse(aligned_coef_values != 0, 1, 0)
}




## ----eval = F------------------------------------------------------------------------------------
saveRDS(coef_matrix, file = "baseline_coef_CI-fix_6k-NMR.rds")
saveRDS(nonzero_matrix, file = "baseline_nonzero_CI-fix_6k-NMR.rds")
 


## ----eval = T------------------------------------------------------------------------------------
# coef_matrix <- readRDS(file = "baseline_coef_CI-fix_6k-NMR.rds")
# nonzero_matrix <- readRDS(file = "baseline_nonzero_CI-fix_6k-NMR.rds")



## ------------------------------------------------------------------------------------------------
coef_matrix[, !primary_nonzero] <- 0

# --- Pool coefs ---

# identify bad runs
bad_runs <- apply(coef_matrix, 1, function(x) all(is.na(x)) | all(x == 0)) &
            apply(nonzero_matrix, 1, function(x) all(is.na(x)) | all(x == 0))

# rm bad runs from objects
coef_matrix <- coef_matrix[!bad_runs, ]
nonzero_matrix <- nonzero_matrix[!bad_runs, ]

# make coef data frame
coef_mean <- colMeans(coef_matrix, na.rm = TRUE)
coef_median <- apply(coef_matrix, 2, median, na.rm = TRUE)
coef_lower <- apply(coef_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
coef_upper <- apply(coef_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
proportion_nonzero <- colSums(nonzero_matrix, na.rm = TRUE) / sum(nonzero_matrix[,1])

coef_ci <- data.frame(
  Predictor = colnames(coef_matrix),
  Mean = coef_mean,
  Median = coef_median,
  Lower_2.5 = coef_lower,
  Upper_97.5 = coef_upper,
  Proportion_Nonzero = proportion_nonzero
)

coef_ci$Primary_Estimate <- aligned_primary_coef






## ------------------------------------------------------------------------------------------------
# --- Placeholder for: Enforce Sparsity (not done for main effects model) ---

coef_matrix_sparse <- coef_matrix

coef_mean <- colMeans(coef_matrix_sparse, na.rm = TRUE)
coef_median <- apply(coef_matrix_sparse, 2, median, na.rm = TRUE)
coef_lower <- apply(coef_matrix_sparse, 2, quantile, probs = 0.025, na.rm = TRUE)
coef_upper <- apply(coef_matrix_sparse, 2, quantile, probs = 0.975, na.rm = TRUE)
proportion_nonzero <- colSums(nonzero_matrix, na.rm = TRUE) / sum(nonzero_matrix[,1])

coef_ci_sparse <- data.frame(
  Predictor = colnames(coef_matrix_sparse),
  Mean = coef_mean,
  Median = coef_median,
  Lower_2.5 = coef_lower,
  Upper_97.5 = coef_upper,
  Proportion_Nonzero = proportion_nonzero
)




## ----interactions lasso w bootstrap--------------------------------------------------------------
#GOOD; Q2; Moderator 
# Def All Params
B <- 6000      # num of bootstraps
m <- 5       # num of multiple imputations per bootstrap
k <- 5       # num of cv folds for cv.glmnet
poly_degree <- 3  # degree for polynomial terms, set to 1 to test model fit w/o poly

# remove id 
data_complete <- data %>% select(-id) 

# Flag predictor vars
# exclude 'abst' from predictors
predictor_vars <- setdiff(names(data_complete), c("abst")) 

# Flag trt vars
treatment_vars <- c("BA", "Var")
baseline_vars <- setdiff(predictor_vars, treatment_vars)

# Flag numeric and int baseline vars
num_baseline_vars <- baseline_vars[sapply(data_complete[baseline_vars],
                                          function(x) is.numeric(x) & !is.integer(x))]

non_binary_factor_vars <- baseline_vars[sapply(data_complete[baseline_vars],
                                               function(x) is.integer(x))]
# Flag Binary baseline vars
binary_vars <- baseline_vars[sapply(data_complete[baseline_vars],
                                  function(x) length(unique(na.omit(x))) == 2)]

# Flag factor vars and store levels
factor_vars <- names(data_complete)[sapply(data_complete, is.factor)]
levels_list <- lapply(data_complete[, factor_vars, drop = FALSE], levels)

# --- Build Formula ---
# Main Effects 
main_effects <- paste("BA + Var +", 
                      paste(c(
                        paste0("poly(", non_binary_factor_vars,
                               ", ", poly_degree, ", raw=TRUE)"),
                        num_baseline_vars,  
                        binary_vars  
                      ), collapse = " + "))

# Interaction Terms
interaction_terms <- paste(c(
  paste0("BA:", c(paste0("poly(", non_binary_factor_vars,
                         ", ", poly_degree, ", raw=TRUE)"), num_baseline_vars, binary_vars))
), collapse = " + ")

# Full Model Formula
full_formula <- as.formula(paste("abst ~", main_effects, "+", interaction_terms))

# fix factor levels in the entire dataset (ensure consistency) BUGFIX
if (length(factor_vars) > 0) {
  for (var in factor_vars) {
    data_complete[[var]] <- factor(data_complete[[var]], levels = levels_list[[var]])
  }
}

# Gen master model matrix 
master_model_matrix <- model.matrix(full_formula, data = data_complete)
master_coef_names <- colnames(master_model_matrix)
total_coeffs <- length(master_coef_names)



## ----interactions coef lasso, eval = T-----------------------------------------------------------

# --- Main estimation and bootstrap loop ---


# --- Primary model fit with imputation ---
# Initial imputation for primary fit
predictor_matrix <- make.predictorMatrix(data_complete)
predictor_matrix[, "abst"] <- 0
predictor_matrix["abst", ] <- 0
imp_primary <- mice(data_complete, m = m, printFlag = FALSE, seed = 123,
                   predictorMatrix = predictor_matrix, method = 'pmm')

# Stack imputations for primary fit
stacked_data_primary <- complete(imp_primary, action = "long", include = FALSE)
stacked_data_primary <- stacked_data_primary %>% dplyr::select(-.id, -.imp)

# Fix factor levels
if (length(factor_vars) > 0) {
  for (var in factor_vars) {
    stacked_data_primary[[var]] <- factor(stacked_data_primary[[var]], 
                                        levels = levels_list[[var]])
  }
}

# Create model matrix for primary fit
model_data_full <- model.matrix(full_formula, data = stacked_data_primary)
x_primary <- model_data_full[, -1]
y_primary <- stacked_data_primary$abst

# Remove zero variance predictors and align columns
zero_var_cols <- apply(x_primary, 2, function(x) var(x) == 0)
if (any(zero_var_cols)) {
  x_primary <- x_primary[, !zero_var_cols]
  current_coef_names <- colnames(x_primary)
} else {
  current_coef_names <- colnames(x_primary)
}

# Align columns with master names
master_coef_names_no_intercept <- master_coef_names[-1]
missing_cols <- setdiff(master_coef_names_no_intercept, current_coef_names)
if (length(missing_cols) > 0) {
  zeros_matrix <- matrix(0, nrow = nrow(x_primary), ncol = length(missing_cols))
  colnames(zeros_matrix) <- missing_cols
  x_primary <- cbind(x_primary, zeros_matrix)
}

extra_cols <- setdiff(current_coef_names, master_coef_names_no_intercept)
if (length(extra_cols) > 0) {
  x_primary <- x_primary[, !(colnames(x_primary) %in% extra_cols)]
}

x_primary <- x_primary[, master_coef_names_no_intercept]

# Set up penalty factors for primary fit
penalty_factors_primary <- ifelse(grepl(":", colnames(x_primary)), 1, 0)

# Verify penalty factor dimensions
if (ncol(x_primary) != length(penalty_factors_primary)) {
  stop("Mismatch between x columns and penalty_factors length in primary fit")
}

# Fit primary Lasso model with penalty factors
cv_lasso_primary <- cv.glmnet(
  x = x_primary,
  y = y_primary,
  family = "binomial",
  alpha = 1,
  penalty.factor = penalty_factors_primary,
  standardize = TRUE,
  relax = FALSE
)

primary_fit <- glmnet(
  x = x_primary,
  y = y_primary,
  family = "binomial",
  alpha = 1,
  lambda = cv_lasso_primary$lambda.min,
  penalty.factor = penalty_factors_primary,
  standardize = TRUE,
  relax = FALSE
)

# Get aligned primary coefficients
primary_coef_values <- as.vector(coef(primary_fit))
primary_coef_names <- rownames(coef(primary_fit))
names(primary_coef_values) <- primary_coef_names
aligned_primary_coef <- setNames(rep(0, length(master_coef_names)), master_coef_names)
matched_primary <- names(primary_coef_values) %in% master_coef_names
aligned_primary_coef[names(primary_coef_values)[matched_primary]] <- 
  primary_coef_values[matched_primary]



primary_nonzero <- abs(aligned_primary_coef) > 1e-8  # New



## ----interactions bootstrap loop, eval = F-------------------------------------------------------

# --- Bootstrap loop for confidence intervals ---
coef_matrix <- matrix(NA, nrow = B, ncol = length(master_coef_names))
colnames(coef_matrix) <- master_coef_names

nonzero_matrix <- matrix(0, nrow = B, ncol = length(master_coef_names))
colnames(nonzero_matrix) <- master_coef_names


set.seed(1)
for (b in 1:B) {
  cat("Bootstrap iteration:", b, "of", B, "\n")

  bootstrap_sample <- data_complete %>%
    group_by(abst) %>%
    sample_frac(size = 1, replace = TRUE) %>%
    ungroup()

  predictor_matrix <- make.predictorMatrix(bootstrap_sample)
  predictor_matrix[, "abst"] <- 0
  predictor_matrix["abst", ] <- 0
  imp <- mice(bootstrap_sample, m = m, printFlag = FALSE, seed = b,
             predictorMatrix = predictor_matrix, method = 'pmm')

  stacked_data <- complete(imp, action = "long", include = FALSE)
  stacked_data <- stacked_data %>% dplyr::select(-.id, -.imp)

  if (length(factor_vars) > 0) {
    for (var in factor_vars) {
      stacked_data[[var]] <- factor(stacked_data[[var]], levels = levels_list[[var]])
    }
  }

  model_data_full <- model.matrix(full_formula, data = stacked_data)
  x <- model_data_full[, -1]
  y_impute <- stacked_data$abst

  zero_var_cols <- apply(x, 2, function(x) var(x) == 0)
  if (any(zero_var_cols)) {
    x <- x[, !zero_var_cols]
    current_coef_names <- colnames(x)
  } else {
    current_coef_names <- colnames(x)
  }

  missing_cols <- setdiff(master_coef_names_no_intercept, current_coef_names)
  if (length(missing_cols) > 0) {
    zeros_matrix <- matrix(0, nrow = nrow(x), ncol = length(missing_cols))
    colnames(zeros_matrix) <- missing_cols
    x <- cbind(x, zeros_matrix)
  }

  extra_cols <- setdiff(current_coef_names, master_coef_names_no_intercept)
  if (length(extra_cols) > 0) {
    x <- x[, !(colnames(x) %in% extra_cols)]
  }

  x <- x[, master_coef_names_no_intercept]

  # Set penalty factors for bootstrap iterations
  penalty_factors <- ifelse(grepl(":", colnames(x)), 1, 0)

  # Verify penalty factor dimensions
  if (ncol(x) != length(penalty_factors)) {
    stop("Mismatch between x columns and penalty_factors length in bootstrap")
  }

  # Fit bootstrap model using same lambda as primary fit and penalty factors
  boot_fit <- tryCatch({
    glmnet(
      x = x,
      y = y_impute,
      family = "binomial",
      alpha = 1,
      lambda = cv_lasso_primary$lambda.min,
      penalty.factor = penalty_factors,
      standardize = TRUE,
      relax = FALSE
    )
  }, error = function(e) {
    cat("Error in glmnet for bootstrap", b, ":", e$message, "\n")
    return(NULL)
  })

  if (is.null(boot_fit)) {
    next
  }

  coef_values <- as.vector(coef(boot_fit))
  coef_names <- rownames(coef(boot_fit))
  names(coef_values) <- coef_names

  aligned_coef_values <- setNames(rep(0, length(master_coef_names)), master_coef_names)
  matched <- names(coef_values) %in% master_coef_names
  aligned_coef_values[names(coef_values)[matched]] <- coef_values[matched]

  coef_matrix[b, ] <- aligned_coef_values
  nonzero_matrix[b, ] <- ifelse(aligned_coef_values != 0, 1, 0)
}





## ----eval = F------------------------------------------------------------------------------------
saveRDS(coef_matrix, file = "moderators_coef_CI-fix_6k-NMR.rds")
saveRDS(nonzero_matrix, file = "moderators_nonzero_CI-fix_6k-NMR.rds")
 


## ----eval = T------------------------------------------------------------------------------------
# coef_matrix <- readRDS(file = "moderators_coef_CI-fix_6k-NMR.rds")
# nonzero_matrix <- readRDS(file = "moderators_nonzero_CI-fix_6k-NMR.rds")



## ------------------------------------------------------------------------------------------------
coef_matrix[, !primary_nonzero] <- 0

# --- Pool coefs ---

# identify bad runs
bad_runs <- apply(coef_matrix, 1, function(x) all(is.na(x)) | all(x == 0)) &
            apply(nonzero_matrix, 1, function(x) all(is.na(x)) | all(x == 0))

# rm bad runs from objects
coef_matrix <- coef_matrix[!bad_runs, ]
nonzero_matrix <- nonzero_matrix[!bad_runs, ]

# make coef data frame
coef_mean <- colMeans(coef_matrix, na.rm = TRUE)
coef_median <- apply(coef_matrix, 2, median, na.rm = TRUE)
coef_lower <- apply(coef_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
coef_upper <- apply(coef_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
proportion_nonzero <- colSums(nonzero_matrix, na.rm = TRUE) / sum(nonzero_matrix[,1])

coef_ci <- data.frame(
  Predictor = colnames(coef_matrix),
  Mean = coef_mean,
  Median = coef_median,
  Lower_2.5 = coef_lower,
  Upper_97.5 = coef_upper,
  Proportion_Nonzero = proportion_nonzero
)




## ------------------------------------------------------------------------------------------------
# --- Enforce Sparsity ---

# first criteria
threshold <- .90 # make this dynamic later
predictors_to_keep <- names(proportion_nonzero)[proportion_nonzero >= threshold]
coef_matrix_sparse <- coef_matrix

# columns (predictors) to zero out
predictors_to_zero <- setdiff(colnames(coef_matrix_sparse), predictors_to_keep)

# second criteria
zero_ci_predictors <- coef_ci$Predictor[
  round(coef_ci$Median, 4) == 0 &
  round(coef_ci$Lower_2.5, 4) == 0 &
  round(coef_ci$Upper_97.5, 4) == 0]

predictors_to_zero <- union(predictors_to_zero, zero_ci_predictors)

# zero out the coefs 
if(length(predictors_to_zero) > 0){
  coef_matrix_sparse[, predictors_to_zero] <- 0
}

coef_mean <- colMeans(coef_matrix_sparse, na.rm = TRUE)
coef_median <- apply(coef_matrix_sparse, 2, median, na.rm = TRUE)
coef_lower <- apply(coef_matrix_sparse, 2, quantile, probs = 0.025, na.rm = TRUE)
coef_upper <- apply(coef_matrix_sparse, 2, quantile, probs = 0.975, na.rm = TRUE)
proportion_nonzero <- colSums(nonzero_matrix, na.rm = TRUE) / sum(nonzero_matrix[,1])

coef_ci_sparse <- data.frame(
  Predictor = colnames(coef_matrix_sparse),
  Mean = coef_mean,
  Median = coef_median,
  Lower_2.5 = coef_lower,
  Upper_97.5 = coef_upper,
  Proportion_Nonzero = proportion_nonzero
)


## -----------