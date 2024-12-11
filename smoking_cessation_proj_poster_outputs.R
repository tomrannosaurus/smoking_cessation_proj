## ----setup, include = FALSE, message = FALSE--------------------------------------------------------
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

#library(Hmisc)
#library(vcd)


source("_helpers.R")



## ----read-in and clean------------------------------------------------------------------------------
data <- read.csv("data/project2.csv", comment.char="#")





## ---------------------------------------------------------------------------------------------------


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




## ---------------------------------------------------------------------------------------------------
#| label: tbl-1
#| tbl-cap: "Summary of Variables"
# Note: using tbl-cap rather than caption= in kbl() breaks repeated title is span page


# Table 1
summary_table(data[-1]) %>%
  kbl(#caption = "Summary of Variables", # Conflicts with Quarto referencing
    booktabs = T,
    longtable = T, # LONGTABLE
    escape = T,
    align = "c") %>%
  column_spec(1, width="2.2cm", latex_valign = "m") %>%
  column_spec(2, width="1.3cm", latex_valign = "m") %>%
  column_spec(3, width="6cm", latex_valign = "m") %>%
  column_spec(4, width="1.0cm", latex_valign = "m") %>%
  column_spec(5, width="1.0cm", latex_valign = "m") %>%
  column_spec(6, width="1.25cm", latex_valign = "m") %>%
  column_spec(7, width="1.25cm", latex_valign = "m") %>%
  kable_styling(
    font_size = 8, # Added for LONGTABLE
    latex_options = c(#'HOLD_position', # Removed for LONGTABLE
      #'scale_down', # Removed for LONGTABLE
      "repeat_header",  # Added for LONGTABLE
      'striped'), 
    full_width = F, # Note: TRUE does not work with LONGTABLE
    position = 'center'# Added for LONGTABLE
  ) %>%
  footnote(general = "Shapiro-Wilk test for normality;
             Grubb's test for outliers.", escape = F)




## ----table 2, eval = F------------------------------------------------------------------------------
#| label: tbl-2
#| tbl-cap: "Summary of Variables by Outcome"

## # Table 2
## summary_table(data[-1], stratify_var = "abst") %>%
##   kbl(
##       booktabs = T,
##       longtable = T, # LONGTABLE
##       escape = T,
##       align = "c") %>%
##   column_spec(1, width="2.21cm", latex_valign = "m") %>%
##   column_spec(2, width="4cm", latex_valign = "m") %>%
##   column_spec(3, width="4cm", latex_valign = "m") %>%
##   column_spec(4, width=".35cm", latex_valign = "m") %>%
##   kable_styling(
##     font_size = 7.6, # Added for LONGTABLE
##     latex_options = c(#'HOLD_position', # Removed for LONGTABLE
##     #'scale_down', # Removed for LONGTABLE
##     "repeat_header",  # Added for LONGTABLE
##     'striped'),
##     full_width = F, # Note: TRUE does not work with LONGTABLE
##     position = 'center'# Added for LONGTABLE
##   ) %>%
##   footnote(general = "ns = P > 0.05,
##            * = P $\\\\leq$ 0.05,
##            ** = P $\\\\leq$ 0.01,
##            *** = P $\\\\leq$ 0.001,
##            **** = P $\\\\leq$ 0.0001
##            ", escape = F) %>%
##   footnote(general = "Kruskal–Wallis test for continuous variables,
##            Chi-Square test for categorical variables.
##            Bonferroni correction applied.", escape = F)
## 


## ----table 3, eval = F------------------------------------------------------------------------------
#| label: tbl-3
#| tbl-cap: "Summary of Variables by BA"

## # Table 2
## summary_table(data[-1], stratify_var = "BA") %>%
##   kbl(
##       booktabs = T,
##       longtable = T, # LONGTABLE
##       escape = T,
##       align = "c") %>%
##   column_spec(1, width="2.21cm", latex_valign = "m") %>%
##   column_spec(2, width="4cm", latex_valign = "m") %>%
##   column_spec(3, width="4cm", latex_valign = "m") %>%
##   column_spec(4, width=".35cm", latex_valign = "m") %>%
##   kable_styling(
##     font_size = 7.6, # Added for LONGTABLE
##     latex_options = c(#'HOLD_position', # Removed for LONGTABLE
##     #'scale_down', # Removed for LONGTABLE
##     "repeat_header",  # Added for LONGTABLE
##     'striped'),
##     full_width = F, # Note: TRUE does not work with LONGTABLE
##     position = 'center'# Added for LONGTABLE
##   ) %>%
##   footnote(general = "ns = P > 0.05,
##            * = P $\\\\leq$ 0.05,
##            ** = P $\\\\leq$ 0.01,
##            *** = P $\\\\leq$ 0.001,
##            **** = P $\\\\leq$ 0.0001
##            ", escape = F) %>%
##   footnote(general = "Kruskal–Wallis test for continuous variables,
##            Chi-Square test for categorical variables.
##            Bonferroni correction applied.", escape = F)
## 


## ----table 4, eval = F------------------------------------------------------------------------------
#| label: tbl-4
#| tbl-cap: "Summary of Variables by Var"

## # Table 4
## summary_table(data[-1], stratify_var = "Var") %>%
##   kbl(
##       booktabs = T,
##       longtable = T, # LONGTABLE
##       escape = T,
##       align = "c") %>%
##   column_spec(1, width="2.21cm", latex_valign = "m") %>%
##   column_spec(2, width="4cm", latex_valign = "m") %>%
##   column_spec(3, width="4cm", latex_valign = "m") %>%
##   column_spec(4, width=".35cm", latex_valign = "m") %>%
##   kable_styling(
##     font_size = 7.6, # Added for LONGTABLE
##     latex_options = c(#'HOLD_position', # Removed for LONGTABLE
##     #'scale_down', # Removed for LONGTABLE
##     "repeat_header",  # Added for LONGTABLE
##     'striped'),
##     full_width = F, # Note: TRUE does not work with LONGTABLE
##     position = 'center'# Added for LONGTABLE
##   ) %>%
##   footnote(general = "ns = P > 0.05,
##            * = P $\\\\leq$ 0.05,
##            ** = P $\\\\leq$ 0.01,
##            *** = P $\\\\leq$ 0.001,
##            **** = P $\\\\leq$ 0.0001
##            ", escape = F) %>%
##   footnote(general = "Kruskal–Wallis test for continuous variables,
##            Chi-Square test for categorical variables.
##            Bonferroni correction applied.", escape = F)
## 


## ----eval = F---------------------------------------------------------------------------------------
## # Histograms of variable distributions
## 
## # Identify numeric columns
## numeric_cols <- sapply(data, is.numeric)
## 
## # Create histograms for each numeric column
## for (col in names(data)[numeric_cols]) {
##   hist(data[[col]], main = paste("Histogram of", col), xlab = col)
## }
## 


## ----corr mat, fig.cap = "Correlation Matrix", fig.width=10.5, fig.height=8-------------------------
#| label: fig-corr

# Generate psuedo correlation matrix
psuedo_cor_matrix <- psuedo_cor_mat(data[-1])

# Plot the heatmap (with variable names and values)
ggplot(melt(psuedo_cor_matrix), aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +  
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.8, color = "black") +
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "white",
                       midpoint = 0,
                       limits = c(-1, 1),
                       space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 45, hjust = 1, size = 9),
        legend.position="none") +  
  coord_fixed(ratio = .55)



## ----missingness, eval = F--------------------------------------------------------------------------
## 
## missing_by_variable <- data %>%
##   summarise(across(everything(), ~ sum(is.na(.)))) %>%
##   pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count") %>%
##   mutate(Missing_Percentage = round((Missing_Count / nrow(data)) * 100, 2))
## 
## missing_by_record <- data %>%
##   mutate(Row = row_number()) %>%
##   mutate(Missing_Count = rowSums(is.na(across(everything())))) %>%
##   select(Row, Missing_Count)
## 
## total_complete_cases <- data %>%
##   summarise(Complete_Cases = sum(complete.cases(.))) %>%
##   pull(Complete_Cases)
## 


## ----missingness mechanism, eval = F----------------------------------------------------------------
## # Test for MCAR
## naniar::mcar_test(data)
## 
## 


## ---------------------------------------------------------------------------------------------------
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
  mutate(logNMR = log(NMR)) %>%
  select(-c(NMR, Hisp, NHW))



## ---------------------------------------------------------------------------------------------------
# --- Validation / Train split ---

set.seed(123)
split_indices <- createDataPartition(y = data$abst, p = 0.8, list = FALSE)

# split data training / validation sets
train_data <- data[split_indices, ]
validation_data <- data[-split_indices, ]
data <- train_data


## ---------------------------------------------------------------------------------------------------
#GOOD; Q1; baseline

# Def All Params
B <- 50      # num of bootstraps
m <- 5      # num of multiple imputations per bootstrap
k <- 5      # num of cv folds for cv.glmnet
poly_degree <- 3  # degree for polynomial terms

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




## ----eval = F---------------------------------------------------------------------------------------
## # --- Bootstrap Loop ---
## set.seed(1)
## 
## for (b in 1:B) {
##   # Print progress
##   cat("Bootstrap iteration:", b, "of", B, "\n")
## 
##   # Stratified resampling with replacement (maintain class distribution in 'abst')
##   bootstrap_sample <- data_complete %>%
##     group_by(abst) %>%
##     sample_frac(size = 1, replace = TRUE) %>%
##     ungroup()
## 
##   # impute data
##   predictor_matrix <- make.predictorMatrix(bootstrap_sample)
##   predictor_matrix[, "abst"] <- 0  # dont use 'abst' to predict others
##   predictor_matrix["abst", ] <- 0  # dont impute 'abst' (not necessary bc no missing?)
##   imp <- mice(bootstrap_sample, m = m, printFlag = FALSE, seed = b,
##               predictorMatrix = predictor_matrix, method = 'pmm')
## 
##   # Stack imputations into one dataset
##   stacked_data <- complete(imp, action = "long", include = FALSE)
##   stacked_data <- stacked_data %>% dplyr::select(-.id, -.imp) # remove '.id' / '.imp'
## 
##   # fix factor levels in the entire dataset (ensure consistency) BUGFIX
##   if (length(factor_vars) > 0) {
##     for (var in factor_vars) {
##       stacked_data[[var]] <- factor(stacked_data[[var]], levels = levels_list[[var]])
##     }
##   }
## 
##   # boot model matrix and X / y
##   model_data_full <- model.matrix(full_formula, data = stacked_data)
##   x <- model_data_full[, -1] # remove intercept for glmnet
##   y_impute <- stacked_data$abst # response var for glmnet
## 
##   # Remove predictors with zero variance BUGFIX
##   zero_var_cols <- apply(x, 2, function(x) var(x) == 0)
##   if (any(zero_var_cols)) {
##     x <- x[, !zero_var_cols]
##     current_coef_names <- colnames(x)
##   } else {
##     current_coef_names <- colnames(x)}
## 
##   # align columns w/ master_coef_names excluding intercept BUGFIX
##   master_coef_names_no_intercept <- master_coef_names[-1]  # now exclude intercept
##   missing_cols <- setdiff(master_coef_names_no_intercept, current_coef_names)
##   if (length(missing_cols) > 0) {
##     zeros_matrix <- matrix(0, nrow = nrow(x), ncol = length(missing_cols))
##     colnames(zeros_matrix) <- missing_cols
##     x <- cbind(x, zeros_matrix)
##   }
## 
##   # rm extra columns not in master_coef_names_no_intercept BUGFIX
##   extra_cols <- setdiff(current_coef_names, master_coef_names_no_intercept)
##   if (length(extra_cols) > 0) {
##     x <- x[, !(colnames(x) %in% extra_cols)]
##   }
## 
##   # reorder x colums to match master_coef_names_no_intercept BUGFIX
##   x <- x[, master_coef_names_no_intercept]
## 
## 
## 
##   # Run cv.glmnet
##   cv_lasso <- tryCatch({
##     cv.glmnet(
##       x = x,
##       y = y_impute,
##       family = "binomial",
##       alpha = 1,
##       standardize = TRUE,
##       relax = FALSE
##     )
##   }, error = function(e) {
##     cat("Error in cv.glmnet for bootstrap", b, ":", e$message, "\n")
##     return(NULL)
##   })
## 
##   if (is.null(cv_lasso)) {
##     next  # skip/move to next bootstrap iteration
##   }
## 
##   # Fit Lasso
##   lasso_fit <- glmnet(
##     x = x,
##     y = y_impute,
##     family = "binomial",
##     alpha = 1,
##     lambda = cv_lasso$lambda.min,
##     standardize = TRUE,
##     relax = FALSE
##   )
## 
## 
##   # grab coefs including intercept
##   coef_values <- as.vector(coef(lasso_fit))
##   coef_names <- rownames(coef(lasso_fit))
##   names(coef_values) <- coef_names
## 
##   # init object to store coefs names
##   aligned_coef_values <- setNames(rep(0, length(master_coef_names)), master_coef_names)
## 
##   # match coefs to the aligned vector
##   matched <- names(coef_values) %in% master_coef_names
##   aligned_coef_values[names(coef_values)[matched]] <- coef_values[matched]
## 
##   # store coefs
##   coef_matrix[b, ] <- aligned_coef_values
## 
##   # store nonzero indicators
##   nonzero_matrix[b, ] <- ifelse(aligned_coef_values != 0, 1, 0)
## }
## 
## 
## 


## ----eval = F---------------------------------------------------------------------------------------
## saveRDS(coef_matrix, file = "baseline_coef.rds")
## saveRDS(nonzero_matrix, file = "baseline_nonzero.rds")
## 


## ----eval = T---------------------------------------------------------------------------------------
coef_matrix <- readRDS(file = "baseline_coef.rds")
nonzero_matrix <- readRDS(file = "baseline_nonzero.rds")



## ---------------------------------------------------------------------------------------------------

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






## ----eval = F---------------------------------------------------------------------------------------
## # plot the CIs
## ggplot(coef_ci, aes(x = Predictor, y = Median)) +
##   geom_point() +
##   geom_errorbar(aes(ymin = Lower_2.5, ymax = Upper_97.5), width = 0.2) +
##   theme_bw() +
##   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
##   labs(
##     title = "Bootstrap Confidence Intervals for Lasso Coefficients",
##     y = "Coefficient Estimate",
##     x = "Predictors"
##   )
## 
## # plot nonzero prop
## ggplot(coef_ci, aes(x = Predictor, y = Proportion_Nonzero)) +
##   geom_bar(stat = "identity", fill = "skyblue") +
##   theme_bw() +
##   labs(
##     title = "Proportion of Bootstrap Samples with Non-Zero Coefficients",
##     y = "Proportion",
##     x = "Predictors"
##   )
## 
## 
## 
## 


## ---------------------------------------------------------------------------------------------------
# --- Enforce Sparsity ---

# Note: this is essentially skipped in this regression run because only having
# main effects in the model allows for few enough parameters that we are not overfitting.

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


## ---------------------------------------------------------------------------------------------------

# Results Table

coef_ci_sparse <- coef_ci_sparse %>%
  mutate(
    Odds_Ratio = exp(Median)  # Odds Ratio
  )

coef_ci_sparse <- coef_ci_sparse %>%
  mutate(
    Rounded_Lower_2.5 = round(Lower_2.5, 4),
    Rounded_Upper_97.5 = round(Upper_97.5, 4),
    
    # Determine significance
    Significant = ifelse(
      Rounded_Lower_2.5 > 0 | Rounded_Upper_97.5 < 0,
      "Yes",
      "No"
    )
  ) %>%
  select(-Rounded_Lower_2.5, -Rounded_Upper_97.5)

table_data <- coef_ci_sparse %>%
  select(
    Predictor,
    Median,
    Odds_Ratio,
    Lower_2.5,
    Upper_97.5,
    Proportion_Nonzero,
    Significant
  ) %>%
  rename(
    "Estimate (Median)" = Median,
    "Odds Ratio" = Odds_Ratio,
    "Lower 2.5%" = Lower_2.5,
    "Upper 97.5%" = Upper_97.5,
    "Proportion Non-Zero" = Proportion_Nonzero,
    "Sig." = Significant
    
  )

table_data_significant <- table_data %>%
  filter(`Sig.` == "Yes")

table_data_significant %>%
  kbl(row.names = F,
      booktabs = TRUE,
      longtable = TRUE,        
      escape = TRUE,
      align = "c",              
      digits = 3,               
      caption = "Lasso Regression Coefficient Estimates"  
  ) %>%
  column_spec(1, width = "4cm", latex_valign = "m") %>%  
  column_spec(2, width = "2cm", latex_valign = "m") %>%   
  column_spec(3, width = "2cm", latex_valign = "m") %>%   
  column_spec(4, width = "2cm", latex_valign = "m") %>%   
  column_spec(5, width = "2cm", latex_valign = "m") %>%   
  column_spec(6, width = "2cm", latex_valign = "m") %>%    
  column_spec(7, width = ".5cm", latex_valign = "m") %>%   
  kable_styling(
    font_size = 7.6,                                     
    latex_options = c("repeat_header", "striped"),      
    full_width = FALSE,
    position = "center"                                  
  )



## ----fig.cap="Relationship between FTCD Score and Estimates"----------------------------------------

# FTCD

ftcd_coefs <- coef_ci[grep("poly\\(ftcd_score, 3, raw = TRUE\\)", coef_ci$Predictor), ]


ftcd_range <- seq(min(na.omit(data$ftcd_score)),
                  max(na.omit(data$ftcd_score)), length.out = 100)

# calc predicted log odds
predicted_log_odds <- coef_ci$Median[coef_ci$Predictor == "(Intercept)"] +  
  ftcd_coefs$Median[1] * ftcd_range +  
  ftcd_coefs$Median[2] * ftcd_range^2 +     
  ftcd_coefs$Median[3] * ftcd_range^3        

# convert to probabilities
predicted_probs <- 1/(1 + exp(-predicted_log_odds))

plot_data <- data.frame(
  ftcd_score = ftcd_range,
  log_odds = predicted_log_odds,
  probability = predicted_probs
)



# plot log odds
p1 <- ggplot(plot_data, aes(x = ftcd_score, y = log_odds)) +
  geom_line() +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  labs(x = "FTCD Score", 
       y = "Predicted Log Odds",
       title = "")

# plot probabilities
p2 <- ggplot(plot_data, aes(x = ftcd_score, y = probability)) +
  geom_line() +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  labs(x = "FTCD Score", 
       y = "Predicted Probability",
       title = "")


ftcd_plot <- ggarrange(
  p1,
  p2, 
  ncol = 2,
  nrow = 1
)


annotate_figure(
  ftcd_plot,
  top = text_grob("Relationship between FTCD Score and Estimates", face = "bold", size = 14)
)



## ----fig.cap="Relationship between Hedonsum Score and Estimates"------------------------------------
# Hedonsum
hedon_coefs <- coef_ci[grep("poly\\(hedonsum_y_pq1, 3, raw = TRUE\\)", coef_ci$Predictor), ]
hedon_range <- seq(min(na.omit(data$hedonsum_y_pq1)),
                   max(na.omit(data$hedonsum_y_pq1)), length.out = 100)

# calc predicted log odds
predicted_log_odds <- coef_ci$Median[coef_ci$Predictor == "(Intercept)"] +  
  hedon_coefs$Median[1] * hedon_range +     
  hedon_coefs$Median[2] * hedon_range^2 +      
  hedon_coefs$Median[3] * hedon_range^3               

# convert to probabilities
predicted_probs <- 1/(1 + exp(-predicted_log_odds))

plot_data <- data.frame(
  hedonsum_score = hedon_range,
  log_odds = predicted_log_odds,
  probability = predicted_probs
)

# plot log odds
p1 <- ggplot(plot_data, aes(x = hedonsum_score, y = log_odds)) +
  geom_line() +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  labs(x = "Hedonsum Score", 
       y = "Predicted Log Odds",
       title = "")

# plot probabilities
p2 <- ggplot(plot_data, aes(x = hedonsum_score, y = probability)) +
  geom_line() +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  labs(x = "Hedonsum Score", 
       y = "Predicted Probability",
       title = "")

hedon_plot <- ggarrange(
  p1,
  p2, 
  ncol = 2,
  nrow = 1
)
annotate_figure(
  hedon_plot,
  top = text_grob("Relationship between Hedonsum Score and Estimates", face = "bold", size = 14)
)


## ----fig.cap="Lasso ROC / AUC Plot"-----------------------------------------------------------------

# --- Generate Predictions ---

# fix factor levels in the entire dataset (ensure consistency) BUGFIX
if (length(factor_vars) > 0) {
  for (var in factor_vars) {
    validation_data[[var]] <- factor(validation_data[[var]], levels = levels_list[[var]])
  }
}

# compute ROC for valid data

# prepare valid data
model_data_valid_full <- model.matrix(full_formula, data = validation_data)
current_coef_names_valid <- colnames(model_data_valid_full) # align cols
missing_cols <- setdiff(master_coef_names, current_coef_names_valid)
if (length(missing_cols) > 0) {
  zeros_matrix <- matrix(0,
                         nrow = nrow(model_data_valid_full),
                         ncol = length(missing_cols))
  colnames(zeros_matrix) <- missing_cols
  model_data_valid_full <- cbind(model_data_valid_full, zeros_matrix)
}
extra_cols <- setdiff(current_coef_names_valid, master_coef_names)
if (length(extra_cols) > 0) {
  model_data_valid_full <-
    model_data_valid_full[, !(colnames(model_data_valid_full) %in% extra_cols)]
}

model_data_valid_full <- model_data_valid_full[, master_coef_names] # reorder
predictor_coefs <- coef_ci_sparse$Median
names(predictor_coefs) <- coef_ci_sparse$Predictor
predictor_coefs <- predictor_coefs[master_coef_names] # ensure match

# predictions for valid data
linear_predictor <- as.vector(model_data_valid_full %*% predictor_coefs)
pred_prob <- 1 / (1 + exp(-linear_predictor))

# BUGFIX
if (length(validation_data) < length(pred_prob)) { 
  validation_data <- validation_data[complete.cases(validation_data),]
}

validation_data$pred_prob <- pred_prob



# compute ROC for training data

# prepare training data
data_complete_cc <- data_complete[complete.cases(data_complete),]
model_data_train <- model.matrix(full_formula, data = data_complete_cc)
current_coef_names_train <- colnames(model_data_train)
missing_cols_train <- setdiff(master_coef_names, current_coef_names_train)
if (length(missing_cols_train) > 0) {
  zeros_matrix_train <- matrix(0, nrow = nrow(model_data_train),
                               ncol = length(missing_cols_train))
  colnames(zeros_matrix_train) <- missing_cols_train
  model_data_train <- cbind(model_data_train, zeros_matrix_train)
}
extra_cols_train <- setdiff(current_coef_names_train, master_coef_names)
if (length(extra_cols_train) > 0) {
  model_data_train <-
    model_data_train[, !(colnames(model_data_train) %in% extra_cols_train)]
}
model_data_train <- model_data_train[, master_coef_names]

# predictions for training data
linear_predictor_train <- as.vector(model_data_train %*% predictor_coefs)
pred_prob_train <- 1 / (1 + exp(-linear_predictor_train))
data_complete_cc$pred_prob <- pred_prob_train


# ROCs
roc_valid <- roc(validation_data$abst, validation_data$pred_prob)
roc_train <- roc(data_complete_cc$abst, data_complete_cc$pred_prob)

# plot ROC curves
plot(roc_train, col = "red", main = "Main Effects ROC")
plot(roc_valid, add = TRUE, col = "blue")
legend("bottomright",
       legend = c(paste("Validation, AUC=", round(roc_valid$auc, 4)),
                  paste("Training, AUC=", round(roc_train$auc, 4))),
       fill = c("blue", "red"))



## ----q1resid, eval = F------------------------------------------------------------------------------
## # calc residuals
## data_complete_cc$residuals <- data_complete_cc$abst - data_complete_cc$pred_prob
## 
## # plot residuals v. fitted
## ggplot(data_complete_cc, aes(x = pred_prob, y = residuals)) +
##   geom_point() +
##   geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
##   labs(x = "Predicted Probability", y = "Residuals") +
##   ggtitle("Residuals vs. Predicted Probabilities")
## 
## # Histogram of Residuals
## hist(data_complete_cc$residuals, main = "Histogram of Residuals")
## 
## # Q-Q Plot of Residuals
## qqnorm(data_complete_cc$residuals)
## qqline(data_complete_cc$residuals)


## ----fig.cap="Lasso Calibration Plot on Validation Data"--------------------------------------------

# calibration plot
num_cuts <- 100

# calibration for validation data
test_calib <- data.frame(
  prob = validation_data$pred_prob,
  bin = cut(validation_data$pred_prob, breaks = num_cuts),
  class = as.numeric(as.character(validation_data$abst))
)

test_calib <- test_calib %>% 
  group_by(bin) %>% 
  summarize(observed = sum(class)/n(), 
            expected = sum(prob)/n(), 
            se = sqrt(observed * (1-observed) / n()))

cols <- c("Ideal"="red","Loess Smooth"="black","LM Smooth"="blue")

ggplot(test_calib) + 
  geom_abline(aes(intercept = 0, slope = 1, color="Ideal")) + 
  geom_smooth(aes(x = expected,
                  y = observed,
                  color="Loess Smooth"), se=TRUE) +
  geom_smooth(aes(x = expected,
                  y = observed,
                  color="LM Smooth"), se=FALSE, method="lm") +
  scale_color_manual(values=cols) +
  labs(x = "Expected Proportion",
       y = "Observed Proportion",
       title="Main Effects Calibration Plot") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     legend.position = "bottom",
                     plot.title = element_text(
                       face = "bold",      
                       hjust = 0.5          
                     )) +
  guides(color = guide_legend(title = element_blank()))


## ---------------------------------------------------------------------------------------------------
#GOOD; Q2; Moderator

# Def All Params
B <- 50      # num of bootstraps
m <- 5      # num of multiple imputations per bootstrap
k <- 5      # num of cv folds for cv.glmnet
poly_degree <- 3  # degree for polynomial terms

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

# init storage for coefs
coef_matrix <- matrix(NA, nrow = B, ncol = total_coeffs)
colnames(coef_matrix) <- master_coef_names

# init object for nonzero coef indicator
nonzero_matrix <- matrix(0, nrow = B, ncol = total_coeffs)
colnames(nonzero_matrix) <- master_coef_names


## ----eval = F---------------------------------------------------------------------------------------
## # --- Bootstrap Loop ---
## set.seed(1)
## 
## for (b in 1:B) {
##   # Print progress
##   cat("Bootstrap iteration:", b, "of", B, "\n")
## 
##   # Stratified resampling with replacement (maintain class distribution in 'abst')
##   bootstrap_sample <- data_complete %>%
##     group_by(abst) %>%
##     sample_frac(size = 1, replace = TRUE) %>%
##     ungroup()
## 
##   # impute data
##   predictor_matrix <- make.predictorMatrix(bootstrap_sample)
##   predictor_matrix[, "abst"] <- 0  # dont use 'abst' to predict others
##   predictor_matrix["abst", ] <- 0  # dont impute 'abst' (not necessary bc no missing?)
##   imp <- mice(bootstrap_sample, m = m, printFlag = FALSE, seed = b,
##               predictorMatrix = predictor_matrix, method = 'pmm')
## 
##   # Stack imputations into one dataset
##   stacked_data <- complete(imp, action = "long", include = FALSE)
##   stacked_data <- stacked_data %>% dplyr::select(-.id, -.imp) # remove '.id' / '.imp'
## 
##   # fix factor levels in the entire dataset (ensure consistency) BUGFIX
##   if (length(factor_vars) > 0) {
##     for (var in factor_vars) {
##       stacked_data[[var]] <- factor(stacked_data[[var]], levels = levels_list[[var]])
##     }
##   }
## 
##   # boot model matrix and X / y
##   model_data_full <- model.matrix(full_formula, data = stacked_data)
##   x <- model_data_full[, -1] # remove intercept for glmnet
##   y_impute <- stacked_data$abst # response var for glmnet
## 
##   # Remove predictors with zero variance BUGFIX
##   zero_var_cols <- apply(x, 2, function(x) var(x) == 0)
##   if (any(zero_var_cols)) {
##     x <- x[, !zero_var_cols]
##     current_coef_names <- colnames(x)
##   } else {
##     current_coef_names <- colnames(x)}
## 
##   # align columns w/ master_coef_names excluding intercept BUGFIX
##   master_coef_names_no_intercept <- master_coef_names[-1]  # now exclude intercept
##   missing_cols <- setdiff(master_coef_names_no_intercept, current_coef_names)
##   if (length(missing_cols) > 0) {
##     zeros_matrix <- matrix(0, nrow = nrow(x), ncol = length(missing_cols))
##     colnames(zeros_matrix) <- missing_cols
##     x <- cbind(x, zeros_matrix)
##   }
## 
##   # rm extra columns not in master_coef_names_no_intercept BUGFIX
##   extra_cols <- setdiff(current_coef_names, master_coef_names_no_intercept)
##   if (length(extra_cols) > 0) {
##     x <- x[, !(colnames(x) %in% extra_cols)]
##   }
## 
##   # reorder x colums to match master_coef_names_no_intercept BUGFIX
##   x <- x[, master_coef_names_no_intercept]
## 
##   # penalty factors
##   penalty_factors <- ifelse(grepl(":", master_coef_names_no_intercept), 1, 0)
## 
##   # verify dims BUGFIX
##   if (ncol(x) != length(penalty_factors)) {
##     stop("Mismatch between x columns and penalty_factors length")
##   }
## 
##   # Run cv.glmnet
##   cv_lasso <- tryCatch({
##     cv.glmnet(
##       x = x,
##       y = y_impute,
##       family = "binomial",
##       alpha = 1,
##       penalty.factor = penalty_factors,
##       standardize = TRUE,
##       relax = FALSE
##     )
##   }, error = function(e) {
##     cat("Error in cv.glmnet for bootstrap", b, ":", e$message, "\n")
##     return(NULL)
##   })
## 
##   if (is.null(cv_lasso)) {
##     next  # skip/move to next bootstrap iteration
##   }
## 
##   # Fit Lasso
##   lasso_fit <- glmnet(
##     x = x,
##     y = y_impute,
##     family = "binomial",
##     alpha = 1,
##     lambda = cv_lasso$lambda.min,
##     penalty.factor = penalty_factors,
##     standardize = TRUE,
##     relax = FALSE
##   )
## 
## 
##   # grab coefs including intercept
##   coef_values <- as.vector(coef(lasso_fit))
##   coef_names <- rownames(coef(lasso_fit))
##   names(coef_values) <- coef_names
## 
##   # init object to store coefs names
##   aligned_coef_values <- setNames(rep(0, length(master_coef_names)), master_coef_names)
## 
##   # match coefs to the aligned vector
##   matched <- names(coef_values) %in% master_coef_names
##   aligned_coef_values[names(coef_values)[matched]] <- coef_values[matched]
## 
##   # store coefs
##   coef_matrix[b, ] <- aligned_coef_values
## 
##   # store nonzero indicators
##   nonzero_matrix[b, ] <- ifelse(aligned_coef_values != 0, 1, 0)
## }
## 
## 
## 


## ----eval = F---------------------------------------------------------------------------------------
## saveRDS(coef_matrix, file = "moderators_coef.rds")
## saveRDS(nonzero_matrix, file = "moderators_nonzero.rds")
## 


## ----eval = T---------------------------------------------------------------------------------------
coef_matrix <- readRDS(file = "moderators_coef.rds")
nonzero_matrix <- readRDS(file = "moderators_nonzero.rds")



## ---------------------------------------------------------------------------------------------------

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





## ----eval = F---------------------------------------------------------------------------------------
## 
## # plot the CIs
## ggplot(coef_ci, aes(x = Predictor, y = Median)) +
##   geom_point() +
##   geom_errorbar(aes(ymin = Lower_2.5, ymax = Upper_97.5), width = 0.2) +
##   theme_bw() +
##   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
##   labs(
##     title = "Bootstrap Confidence Intervals for Lasso Coefficients",
##     y = "Coefficient Estimate",
##     x = "Predictors"
##   )
## 
## # plot nonzero prop
## ggplot(coef_ci, aes(x = Predictor, y = Proportion_Nonzero)) +
##   geom_bar(stat = "identity", fill = "skyblue") +
##   theme_bw() +
##   labs(
##     title = "Proportion of Bootstrap Samples with Non-Zero Coefficients",
##     y = "Proportion",
##     x = "Predictors"
##   )
## 
## 
## 
## 


## ---------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------

# Results Table

coef_ci_sparse <- coef_ci_sparse %>%
  mutate(
    Odds_Ratio = exp(Median)  # Odds Ratio
  )

coef_ci_sparse <- coef_ci_sparse %>%
  mutate(
    Rounded_Lower_2.5 = round(Lower_2.5, 4),
    Rounded_Upper_97.5 = round(Upper_97.5, 4),
    
    # Determine significance
    Significant = ifelse(
      Rounded_Lower_2.5 > 0 | Rounded_Upper_97.5 < 0,
      "Yes",
      "No"
    )
  ) %>%
  select(-Rounded_Lower_2.5, -Rounded_Upper_97.5)

table_data <- coef_ci_sparse %>%
  select(
    Predictor,
    Median,
    Odds_Ratio,
    Lower_2.5,
    Upper_97.5,
    Proportion_Nonzero,
    Significant
  ) %>%
  rename(
    "Estimate (Median)" = Median,
    "Odds Ratio" = Odds_Ratio,
    "Lower 2.5%" = Lower_2.5,
    "Upper 97.5%" = Upper_97.5,
    "Proportion Non-Zero" = Proportion_Nonzero,
    "Sig." = Significant
    
  )

table_data_significant <- table_data %>%
  filter(`Sig.` == "Yes")

table_data_significant %>%
  kbl(row.names = F,
      booktabs = TRUE,
      longtable = TRUE,        
      escape = TRUE,
      align = "c",              
      digits = 3,               
      caption = "Lasso Regression Coefficient Estimates"  
  ) %>%
  column_spec(1, width = "4cm", latex_valign = "m") %>%  
  column_spec(2, width = "2cm", latex_valign = "m") %>%   
  column_spec(3, width = "2cm", latex_valign = "m") %>%   
  column_spec(4, width = "2cm", latex_valign = "m") %>%   
  column_spec(5, width = "2cm", latex_valign = "m") %>%   
  column_spec(6, width = "2cm", latex_valign = "m") %>%    
  column_spec(7, width = ".5cm", latex_valign = "m") %>%   
  kable_styling(
    font_size = 7.6,                                     
    latex_options = c("repeat_header", "striped"),      
    full_width = FALSE,
    position = "center"                                  
  )



## ----fig.cap="Relationship between SHAPS Score and Estimates"---------------------------------------
# polynomial coefficients
shaps_coefs <- coef_ci[grep("poly\\(shaps_score_pq1, 3, raw = TRUE\\)", coef_ci$Predictor), ]
# interaction coefficients
shaps_baba_coefs <- coef_ci[grep("BABA:poly\\(shaps_score_pq1, 3, raw = TRUE\\)", coef_ci$Predictor), ]
#  BABA main effect
baba_coef <- coef_ci$Median[coef_ci$Predictor == "BABA"]

shaps_range <- seq(min(na.omit(data$shaps_score_pq1)),
                   max(na.omit(data$shaps_score_pq1)), length.out = 100)

# Function to calculate predicted values for a given BABA value
calculate_predictions <- function(baba_value) {
  predicted_log_odds <- coef_ci$Median[coef_ci$Predictor == "(Intercept)"] +  
    baba_coef * baba_value +  
    shaps_coefs$Median[1] * shaps_range +     
    shaps_coefs$Median[2] * shaps_range^2 +      
    shaps_coefs$Median[3] * shaps_range^3 +
    baba_value * shaps_baba_coefs$Median[1] * shaps_range +
    baba_value * shaps_baba_coefs$Median[2] * shaps_range^2 +
    baba_value * shaps_baba_coefs$Median[3] * shaps_range^3
  
  predicted_probs <- 1/(1 + exp(-predicted_log_odds))
  
  return(data.frame(
    shaps_score = shaps_range,
    log_odds = predicted_log_odds,
    probability = predicted_probs,
    BABA = as.factor(baba_value)
  ))
}

plot_data <- rbind(
  calculate_predictions(0),
  calculate_predictions(1)
)

# plot log odds
p1 <- ggplot(plot_data, aes(x = shaps_score, y = log_odds, color = BABA)) +
  geom_line() +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Standard", "BA")) +
  labs(x = "SHAPS Score", 
       y = "Predicted Log Odds",
       title = "") +
  guides(color = guide_legend(title = element_blank()))

# plot probabilities
p2 <- ggplot(plot_data, aes(x = shaps_score, y = probability, color = BABA)) +
  geom_line() +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Standard", "BA")) +
  labs(x = "SHAPS Score", 
       y = "Predicted Probability",
       title = "") +
  guides(color = guide_legend(title = element_blank()))

shaps_plot <- ggarrange(
  p1,
  p2, 
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)


annotate_figure(
  shaps_plot,
  top = text_grob("Relationship between SHAPS Score and Estimates", face = "bold", size = 14)
)



## ----fig.cap="Lasso ROC / AUC Plot"-----------------------------------------------------------------

# --- Generate Predictions ---

# fix factor levels in the entire dataset (ensure consistency) BUGFIX
if (length(factor_vars) > 0) {
  for (var in factor_vars) {
    validation_data[[var]] <- factor(validation_data[[var]], levels = levels_list[[var]])
  }
}

# compute ROC for valid data

# prepare valid data
model_data_valid_full <- model.matrix(full_formula, data = validation_data)
current_coef_names_valid <- colnames(model_data_valid_full) # align cols
missing_cols <- setdiff(master_coef_names, current_coef_names_valid)
if (length(missing_cols) > 0) {
  zeros_matrix <- matrix(0,
                         nrow = nrow(model_data_valid_full),
                         ncol = length(missing_cols))
  colnames(zeros_matrix) <- missing_cols
  model_data_valid_full <- cbind(model_data_valid_full, zeros_matrix)
}
extra_cols <- setdiff(current_coef_names_valid, master_coef_names)
if (length(extra_cols) > 0) {
  model_data_valid_full <-
    model_data_valid_full[, !(colnames(model_data_valid_full) %in% extra_cols)]
}

model_data_valid_full <- model_data_valid_full[, master_coef_names] # reorder
predictor_coefs <- coef_ci_sparse$Median
names(predictor_coefs) <- coef_ci_sparse$Predictor
predictor_coefs <- predictor_coefs[master_coef_names] # ensure match

# predictions for valid data
linear_predictor <- as.vector(model_data_valid_full %*% predictor_coefs)
pred_prob <- 1 / (1 + exp(-linear_predictor))

# BUGFIX
if (length(validation_data) < length(pred_prob)) { 
  validation_data <- validation_data[complete.cases(validation_data),]
}

validation_data$pred_prob <- pred_prob



# compute ROC for training data

# prepare training data
data_complete_cc <- data_complete[complete.cases(data_complete),]
model_data_train <- model.matrix(full_formula, data = data_complete_cc)
current_coef_names_train <- colnames(model_data_train)
missing_cols_train <- setdiff(master_coef_names, current_coef_names_train)
if (length(missing_cols_train) > 0) {
  zeros_matrix_train <- matrix(0, nrow = nrow(model_data_train),
                               ncol = length(missing_cols_train))
  colnames(zeros_matrix_train) <- missing_cols_train
  model_data_train <- cbind(model_data_train, zeros_matrix_train)
}
extra_cols_train <- setdiff(current_coef_names_train, master_coef_names)
if (length(extra_cols_train) > 0) {
  model_data_train <-
    model_data_train[, !(colnames(model_data_train) %in% extra_cols_train)]
}
model_data_train <- model_data_train[, master_coef_names]

# predictions for training data
linear_predictor_train <- as.vector(model_data_train %*% predictor_coefs)
pred_prob_train <- 1 / (1 + exp(-linear_predictor_train))
data_complete_cc$pred_prob <- pred_prob_train


# ROCs
roc_valid <- roc(validation_data$abst, validation_data$pred_prob)
roc_train <- roc(data_complete_cc$abst, data_complete_cc$pred_prob)

# plot ROC curves
plot(roc_train, col = "red", main = "Interactions Model ROC")
plot(roc_valid, add = TRUE, col = "blue")
legend("bottomright",
       legend = c(paste("Validation, AUC=", round(roc_valid$auc, 4)),
                  paste("Training, AUC=", round(roc_train$auc, 4))),
       fill = c("blue", "red"))



## ----q2resid, eval = F------------------------------------------------------------------------------
## # calc residuals
## data_complete_cc$residuals <- data_complete_cc$abst - data_complete_cc$pred_prob
## 
## # plot residuals v. fitted
## ggplot(data_complete_cc, aes(x = pred_prob, y = residuals)) +
##   geom_point() +
##   geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
##   labs(x = "Predicted Probability", y = "Residuals") +
##   ggtitle("Residuals vs. Predicted Probabilities")
## 
## # Histogram of Residuals
## hist(data_complete_cc$residuals, main = "Histogram of Residuals")
## 
## # Q-Q Plot of Residuals
## qqnorm(data_complete_cc$residuals)
## qqline(data_complete_cc$residuals)


## ----fig.cap="Lasso Calibration Plot on Validation Data"--------------------------------------------

# calibration plot
num_cuts <- 100

# calibration for validation data
test_calib <- data.frame(
  prob = validation_data$pred_prob,
  bin = cut(validation_data$pred_prob, breaks = num_cuts),
  class = as.numeric(as.character(validation_data$abst))
)

test_calib <- test_calib %>% 
  group_by(bin) %>% 
  summarize(observed = sum(class)/n(), 
            expected = sum(prob)/n(), 
            se = sqrt(observed * (1-observed) / n()))

cols <- c("Ideal"="red","Loess Smooth"="black","LM Smooth"="blue")

ggplot(test_calib) + 
  geom_abline(aes(intercept = 0, slope = 1, color="Ideal")) + 
  geom_smooth(aes(x = expected,
                  y = observed,
                  color="Loess Smooth"), se=TRUE) +
  geom_smooth(aes(x = expected,
                  y = observed,
                  color="LM Smooth"), se=FALSE, method="lm") +
  scale_color_manual(values=cols) +
  labs(x = "Expected Proportion",
       y = "Observed Proportion",
       title="Interactions Model Calibration Plot") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   legend.position = "bottom",
                   plot.title = element_text(
                     face = "bold",      
                     hjust = 0.5          
                   )) +   guides(color = guide_legend(title = element_blank()))

