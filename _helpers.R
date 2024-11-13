# Helper Functions for Smoking Cessation Project

# --- Preamble ---
# Date of last update: Oct. 17, 2024
# R Version: 4.3.1
# Package Versions:
#   outliers: 0.15
#   tidyverse: 2.0.0
#   reshape2: 1.4.4
#   moments: 0.14.1
#   vcd: 1.4-12


# Libraries for functions 
suppressMessages(library(tidyverse))
library(outliers) # for grubbs.test()
library(reshape2) # for melt()
library(moments) # for skewness() and kurtosis()
library(vcd) # for assocstats()








# -- BEGIN Psuedo-correlation Matrix Section --

# Helper function to calculate Eta^2
eta_squared <- function(aov_model) {
  #' This function calculates the Eta^2 stat from an anova model. Eta^2 is a measure
  #' of effect size. Eta^2 describes the proportion of the total variance attributable 
  #' to a given factor
  #'
  #' @param aov_model An aov object
  #' @return A numeric value of Eta^2 (proportion of the total variance explained) 
  #'
  sum_of_squares_model <- summary(aov_model)[[1]]$"Sum Sq"[1]
  sum_of_squares_total <- sum(summary(aov_model)[[1]]$"Sum Sq")
  eta_sq <- sum_of_squares_model / sum_of_squares_total
  return(eta_sq)
}


# Main function to compute correlations or psuedo-correlations
# NOTE: Below still does not work with logical variables, but now fixed for NAs
psuedo_cor_mat <- function(data) {
  #' Builds a pseudo correlation matrix for a given dataset, flexible for numerical
  #' and categorical variables. Uses Pearson correlation for numerical-numerical pairs,
  #' Cramer's V for categorical-categorical pairs, point biserial correlation for 
  #' numerical-binary categorical pairs, and the square root of Eta^2 for 
  #' numerical-multi-category pairs.
  #'
  #' @param data A df with any mix of numerical and categorical variables.
  #' @return A symmetric matrix with correlation coefficients 
  #' (or equivalent measures) for all variable pairs.
  #'
  variables <- names(data)
  n <- length(variables)
  cor_matrix <- matrix(NA, n, n, dimnames = list(variables, variables))
  
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        cor_matrix[i, j] <- 1
      } else {
        var_i <- data[[i]]
        var_j <- data[[j]]
        
        if (is.numeric(var_i) && is.numeric(var_j)) {
          # Pearson correlation for continuous-continuous
          cor_matrix[i, j] <- cor_matrix[j, i] <- cor(var_i, var_j,
                                                      use = "pairwise.complete.obs")
          
        } else if (is.factor(var_i) && is.factor(var_j)) {
          # Remove NAs for both variables
          valid_idx <- !is.na(var_i) & !is.na(var_j)
          var_i_clean <- var_i[valid_idx]
          var_j_clean <- var_j[valid_idx]
          
          # Drop unused levels
          var_i_clean <- droplevels(var_i_clean)
          var_j_clean <- droplevels(var_j_clean)
          
          # Cramer's V for categorical-categorical
          if (length(var_i_clean) > 0 && length(var_j_clean) > 0) {
            cramers_v <- sqrt(assocstats(table(var_i_clean, var_j_clean))$cramer)
            cor_matrix[i, j] <- cor_matrix[j, i] <- cramers_v
          } else {
            cor_matrix[i, j] <- cor_matrix[j, i] <- NA
          }
          
        } else {
          # Continuous-categorical pairs
          if (is.numeric(var_i) && (is.factor(var_j) || is.character(var_j))) {
            continuous <- var_i
            factor_var <- as.factor(var_j)
          } else if ((is.factor(var_i) || is.character(var_i)) && is.numeric(var_j)) {
            continuous <- var_j
            factor_var <- as.factor(var_i)
          } else {
            next  # Skip if variables are not appropriate types
          }
          
          # Remove NAs in both variables
          valid_idx <- !is.na(continuous) & !is.na(factor_var)
          continuous <- continuous[valid_idx]
          factor_var <- factor_var[valid_idx]
          
          # Drop unused levels
          factor_var <- droplevels(factor_var)
          
          if (length(unique(factor_var)) == 2) {
            # Point biserial correlation for binary factors
            factor_numeric <- as.numeric(factor_var) - 1
            cor_matrix[i, j] <- cor_matrix[j, i] <- cor(continuous, factor_numeric)
          } else if (length(unique(factor_var)) > 2) {
            # Eta-squared for multi-category factors
            if (length(factor_var) > 0) {
              aov_result <- aov(continuous ~ factor_var)
              eta_sq <- eta_squared(aov_result)
              cor_matrix[i, j] <- cor_matrix[j, i] <- sqrt(eta_sq)
            } else {
              cor_matrix[i, j] <- cor_matrix[j, i] <- NA
            }
          } else {
            # Cannot compute correlation if factor_var has insufficient levels
            cor_matrix[i, j] <- cor_matrix[j, i] <- NA
          }
        }
      }
    }
  }
  return(cor_matrix)
}

# -- END Psuedo-correlation Matrix Section --


















# Function to generate summary table aka "Table 1"
# NOTE: Does not like the following variable types: Date, `hms` num / difftime 
# NOTE: Will likely need custom subroutines to handle dates / hms / difftime
summary_table <- function(df, stratify_var = NULL) {
  #' Create summary table for a data frame with option for stratification. It 
  #' summarizes numeric variables by mean, standard deviation, and IQR. 
  #' Categorical variables by count and percentage. Optionally stratifies by a 
  #' specified variable.
  #'
  #' @param df A data frame 
  #' @param stratify_var (Optional) A variable on which to stratify the summaries.
  #'
  #' @return A data frame (or tibble) containing the summarized statistics.
  
  # Internal function to handle the actual summarization
  summarize_internal <- function(df, stratify_var = NULL, is_recursive_call = FALSE) {
    
    # --- SUMMARIZER SECTION ---
    # Function to summarize continuous variable
    summarize_continuous <- function(data, var) {
      var_data <- data[[var]]
      mean_sd <- paste0(round(mean(var_data, na.rm = TRUE), 2),
                        " (",
                        round(sd(var_data, na.rm = TRUE), 2),
                        ")")
      quantiles <- quantile(var_data, probs = c(0.25, 0.75), na.rm = TRUE)
      quantile_range <- paste0("(",
                               round(quantiles[1], 2),
                               " - ",
                               round(quantiles[2], 2),
                               ")")
      paste(mean_sd, quantile_range)
    }
    
    # Func to summarize categorical variable
    summarize_categorical <- function(data, var) {
      var_data <- table(data[[var]])
      percentages <- prop.table(var_data) * 100
      paste(names(var_data), ": ",
            var_data, " (",
            round(percentages, 2),
            "%)",
            sep = "",
            collapse = ", ")
    }
    
    # Handler for strata
    variable_names <- names(df)
    if (!is.null(stratify_var) && stratify_var %in% variable_names) {
      variable_names <- variable_names[variable_names != stratify_var]
    }
    
    # Summarize all into a list for table
    summary_list <- map(variable_names, ~ {
      var_type <- ifelse(is.numeric(df[[.x]]) | is.integer(df[[.x]]),
                         "Numeric", "Categorical")
      summarizer <- ifelse(var_type == "Numeric",
                           summarize_continuous,
                           summarize_categorical)
      summary <- summarizer(df, .x)
      variable_name <- ifelse(var_type == "Numeric",
                              paste(.x, "[Mean (SD) (Quantile)]"),
                              paste(.x, "[n (%)]"))
      tibble(Variable = variable_name, Type = var_type, Summary = summary)
    })
    
    # Bind to table
    summary_table <- bind_rows(summary_list)
    
    # --- NORMALITY AND OUTLIERS SECTION ---
    # Perform Shapiro-Wilk and Grubbs tests only if it's not a recursive call 
    # and no stratify_var is given
    if (!is_recursive_call && is.null(stratify_var)) {
      # Get numeric variables
      numeric_vars <- variable_names[sapply(df[variable_names],
                                            function(x) is.numeric(x) | is.integer(x))]
      
      # Init object for S-W / Grubbs results
      test_results <- map(numeric_vars, ~ {
        x <- df[[.x]]
        x <- na.omit(x)
        n <- length(x)
        normality <- NA
        outlier <- NA
        skewness_label <- NA
        kurtosis_label <- NA
        
        # Shapiro-Wilk test (Note: now accommodates n > 5000)
        # NOTE: Consider in future -- KS test, AD test, Jarque-Bera test
        if (n >= 3) {
          if (n > 5000) {
            shapiro_test <- shapiro.test(sample(x, 4999))
          } else {
            shapiro_test <- shapiro.test(x)
          }
          normality <- ifelse(shapiro_test$p.value < 0.05, "No", "Yes")
        } else {
          normality <- "Insufficient data"
        }
        
        # Grubbs test
        if (n >= 3) {
          grubbs_test <- grubbs.test(x)
          outlier <- ifelse(grubbs_test$p.value < 0.05, "Yes", "No")
        } else {
          outlier <- "Insufficient data"
        }
        
        # Skewness and Kurtosis
        if (n >= 2) {
          skewness <- skewness(x)
          kurtosis <- kurtosis(x)
          
          # Skewness label
          if (abs(skewness) < 0.5) {
            skewness_label <- "Centered"
          } else if (skewness > 0.5) {
            skewness_label <- "Right-skewed"
          } else {
            skewness_label <- "Left-skewed"
          }
          
          # Kurtosis label
          if (kurtosis < 2.5) {
            kurtosis_label <- "Platykurtic"
          } else if (kurtosis > 3.5) {
            kurtosis_label <- "Leptokurtic"
          } else {
            kurtosis_label <- "Mesokurtic"
          }
        } else {
          skewness_label <- "Insufficient data"
          kurtosis_label <- "Insufficient data"
        }
        
        tibble(Variable = .x,
               `Normal Distribution` = normality,
               `Outlier(s) Present` = outlier,
               `Skewness` = skewness_label,
               `Kurtosis` = kurtosis_label)
      })
      
      # Bind test results into df
      test_results_df <- bind_rows(test_results)
      
      # Get actual variable names from summary_table for merge
      # Note: this could break in some datasets if the variable names include "["
      # therefore a more general solution later is preferred
      summary_table$ActualVariable <- gsub(" \\[.*\\]", "", summary_table$Variable)
      
      # Merge summary_table with test_results_df
      summary_table <- left_join(summary_table,
                                 test_results_df,
                                 by = c("ActualVariable" = "Variable"))
      
      # Remove temp matching ActualVariable column
      summary_table <- summary_table %>% dplyr::select(-ActualVariable)
    } else if (!is.null(stratify_var)) {
      # --- STRATA SECTION ---
      
      # Stratification handling
      stratified_summaries <- df %>%
        group_by(!!sym(stratify_var)) %>%
        do(summarize_internal(., stratify_var = NULL, is_recursive_call = TRUE))
      
      summary_table <- stratified_summaries %>%
        ungroup() %>%
        dplyr::select(-group_cols()) %>% # This doesn't seem necessary, why'd I do it?
        pivot_wider(names_from = !!sym(stratify_var), values_from = Summary)
      
      # Function for significance testing (numeric variables)
      test_continuous <- function(data, var, group_var) {
        formula <- as.formula(paste0("`", var, "` ~ `", group_var, "`")) #BUGFIX: spaces
        num_groups <- length(unique(data[[group_var]]))
        if (num_groups == 2) {
          test_result <- t.test(formula, data = data)
          p_value <- test_result$p.value
        } else {
          test_result <- kruskal.test(formula, data = data)
          p_value <- test_result$p.value
        }
        p_value
      }
      
      # Function for significance testing (categorical variables)
      test_categorical <- function(data, var, group_var) {
        table_data <- table(data[[var]], data[[group_var]])
        table_data <- table_data[rowSums(table_data) > 0, # This prevents NaN error
                                 colSums(table_data) > 0, # in ChiSq test.
                                 drop = FALSE]
        test_result <- chisq.test(table_data)
        p_value <- test_result$p.value
        p_value
      }
      
      # Perform significance testing (if stratify_var is specified)
      significance_tests <- map(variable_names, ~ {
        var_type <- ifelse(is.numeric(df[[.x]]) | is.integer(df[[.x]]),
                           "Numeric", "Categorical")
        tester <- ifelse(var_type == "Numeric",
                         test_continuous,
                         test_categorical)
        p_value <- tester(df, .x, stratify_var)
        tibble(Variable = .x, `P-value` = p_value)
      })
      
      significance_table <- bind_rows(significance_tests)
      
      # Adjust p-values using Bonferroni correction
      significance_table$`Adjusted P-value` <- p.adjust(significance_table$`P-value`,
                                                        method = "bonferroni")
      
      # Get actual variable names from summary_table for merge
      # Note: this could break in some datasets if the variable names include "["
      # therefore a more general solution later is preferred
      summary_table$ActualVariable <- gsub(" \\[.*\\]", "", summary_table$Variable)
      
      # Merge summary_table with significance_table on actual variable names
      summary_table <- left_join(summary_table,
                                 significance_table,
                                 by = c("ActualVariable" = "Variable"))
      
      # Remove rows corresponding to stratify_var
      summary_table <- summary_table %>% filter(ActualVariable != stratify_var)
      
      # Create 'Sig.' column based on Adjusted P-value
      summary_table$`Sig.` <- case_when(
        summary_table$`Adjusted P-value` <= 0.0001 ~ "****",
        summary_table$`Adjusted P-value` <= 0.001 ~ "***",
        summary_table$`Adjusted P-value` <= 0.01  ~ "**",
        summary_table$`Adjusted P-value` <= 0.05  ~ "*",
        TRUE                                      ~ "ns"
      )
      
      # Remove temp matching ActualVariable, adjusted/unadjusted P-Values
      summary_table <- dplyr::select(summary_table,
                                     c(-ActualVariable,
                                       -`P-value`,
                                       -`Adjusted P-value`))
    }
    
    # Remove "Type" column for stratified tables
    if (!is.null(stratify_var) && !is_recursive_call) { 
      summary_table <- summary_table %>% dplyr::select(-Type)
    }
    
    return(summary_table)
  }
  
  # Call internal function to initiate procedure
  summarize_internal(df, stratify_var)
}



  










  
# Function to count number of variables where missing data occours
count_missing_vars <- function(df) {
  #' Calc the number of variables in a data frame that have at least one missing 
  #' value.
  #'
  #' @param df A data frame 
  #'
  #' @return A data frame with a single row and column indicating the count 
  #' of variables with any missing values.
  #'
  missing_count <- df %>%
    summarise_all(~ sum(is.na(.))) %>%
    pivot_longer(everything()) %>%
    filter(value > 0) %>%
    nrow()
  
  return(data.frame(Num_Missing_Vars = missing_count))
}














# Count by variable type
summarize_variables_types <- function(data) {
  #' Summarizes the types of variables in a dataset by type. Possible types: 
  #' Numeric, or Categorical (factors or logical).
  #'
  #' @param data A data frame 
  #' @return A table with the counts of variables categorized by type
  #'
  var_types <- sapply(data, function(x) {
    if(is.numeric(x) | is.integer(x)) {
      return("Numeric")
    } 
    else {
      return("Categorical")
    }
  })
  
  var_counts <- table(var_types)
  return(var_counts)
}














# Function to find groups of correlated variables
findCorrelatedGroups <- function(corr_matrix, threshold) {
  #' Find Groups of Correlated Variables
  #'
  #' This function identifies groups of variables in a correlation matrix that 
  #' exceed a specified correlation threshold.
  #'
  #' @param corr_matrix A square symmetric matrix representing variable correlations.
  #' @param threshold A numeric threshold for defining significant correlation.
  #' @return A list of numeric vectors, each containing indices of a group of 
  #' correlated variables.
  correlation <- abs(corr_matrix)
  diag(correlation) <- 0 # blank out diagonal to ignore self-correlation
  groups <- list()
  visited <- rep(FALSE, ncol(correlation))
  
  for (i in 1:ncol(correlation)) {
    if (!visited[i]) {
      # Find index of variables correlated w/ the current var
      high_cor_vars <- which(correlation[, i] > threshold)
      if (length(high_cor_vars) > 0) {
        group <- unique(c(i, high_cor_vars)) # include current var in the group
        groups[[length(groups) + 1]] <- group
        visited[group] <- TRUE
      } 
      else {
        visited[i] <- TRUE
      }
    }
  }
  return(groups)
}




# Function to drop select highly correlated vars
reduceDataset <- function(df, corr_matrix, threshold) {
  #' Reduce Dataset by Dropping Highly Correlated Variables
  #'
  #' This function reduces a dataset by identifying and dropping highly correlated 
  #' variables based on the provided correlation matrix and threshold.
  #'
  #' @param df A dataframe containing the dataset to be reduced.
  #' @param corr_matrix A square symmetric matrix of correlations between 
  #' variables in `df`.
  #' @param threshold A numeric threshold to identify high correlations.
  #' @return A dataframe with reduced variables.

  groups <- findCorrelatedGroups(corr_matrix, threshold)
  to_drop <- numeric()
  
  for (group in groups) {
    # Subroutine to select which to drop (inverse of variance explained)
    variances <- sapply(group, function(index) {
      # Below is to fix bug with fread/data.table (depending on how data is read in)
      if (is.data.table(df)) {
        var(as.vector(df[[index]])) 
      } else {
        var(df[, index, drop = FALSE])
      }
    })
    
    # Drop variable with minimum variance
    min_variance_index <- which.min(variances)
    
    to_drop_var <- group[min_variance_index] 
    
    to_drop <- c(to_drop, to_drop_var)
  }
  
  to_drop <- unique(to_drop)
  
  # only drop columns that exist in the dataset (bug fix)
  to_drop <- to_drop[to_drop <= ncol(df)]
  
  if (length(to_drop) > 0) {
    reduced_df <- dplyr::select(df, -to_drop)
  } 
  else {
    reduced_df <- df # If nothing to drop, return the original dataset (bug fix)
  }
  
  return(reduced_df)
}









check_mar <- function(data) {
  #' Check Missing At Random (MAR) in Data
  #'
  #' This function tests each variable in the dataset for missingness being at 
  #' random, conditional on all other variables, using logistic regression models.
  #'
  #' @param data A dataframe with variables to test for MAR.
  #' @return A list where each entry corresponds to a variable in `data` and 
  #' contains results of MAR tests.

  
  all_vars <- names(data)
  
  # Init object for results
  results <- list()
  
  for (var in all_vars) {
    # Generate missing indicator
    missing_var_name <- paste0("missing_", make.names(var))
    data[[missing_var_name]] <- as.integer(is.na(data[[var]]))
    
    # formula for GLM
    formula_vars <- sapply(all_vars[all_vars != var], function(v) paste0("`", v, "`"))
    formula_str <- paste0(missing_var_name, " ~ ", paste(formula_vars, collapse = " + "))
    formula <- as.formula(formula_str)
    
    model <- tryCatch(
      {
        glm(formula, data = data, family = binomial())
      },
      error = function(e) {
        return(NULL)
      }
    )
    
    if (!is.null(model)) {
      # check for sig. variables
      summary_model <- summary(model)
      pvalues <- summary_model$coefficients[, "Pr(>|z|)"]
      significant_vars <- names(pvalues[pvalues < 0.05 & !is.na(pvalues)])
      
      # removve intercept from significant vars (if present)
      significant_vars <- significant_vars[significant_vars != "(Intercept)"]
      
      # rm backticks from significant vars for cleaner output
      significant_vars <- gsub("`", "", significant_vars)
      
      results[[var]] <- list(
        is_mar = length(significant_vars) == 0,
        significant_vars = significant_vars
      )
    } else {
      results[[var]] <- list(
        is_mar = NA,
        significant_vars = NA,
        error = "Error in fitting GLM"
      )
    }
    
    # remove temporary missing indicator col
    data[[missing_var_name]] <- NULL
  }
  
  return(results)
}




calculate_mse <- function(actual, predicted) {
  #' Calculate Mean Squared Error 
  #'
  #' This function computes the mean squared error between actual and predicted 
  #' numerical values.
  #'
  #' @param actual A numeric vector of actual values.
  #' @param predicted A numeric vector of predicted values.
  #' @return Numeric value representing the mean squared error.
  
  # inputs must be numeric
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  
  # calc squared differences
  squared_diff <- (actual - predicted)^2
  
  # Remove NA / infinite values
  valid_diff <- squared_diff[is.finite(squared_diff) & !is.na(squared_diff)]
  
  # calc MSE
  mse <- sum(valid_diff) / length(valid_diff)
  
  return(mse)
}



variable_importance_reg <- function(model, data, target_column, n_iterations = 20) {
  #' Calculate Variable Importance for Linear Regression Models
  #'
  #' This function estimates the importance of each predictor variable in a 
  #' regression model by measuring the increase in prediction error after 
  #' permuting each predictor variable.
  #'
  #' @param model A regression model object (e.g., lm, lmer).
  #' @param data A dataframe containing the data used in the model.
  #' @param target_column The name of the target (dependent) variable in `data`.
  #' @param n_iterations The number of iterations to perform for estimating 
  #' importance (default is 20).
  #' @return A dataframe with variables and their relative importance scores.
  
  predictor_vars <- all.vars(formula(model))[-1]
  original_preds <- as.numeric(predict(model, data))
  original_mse <- calculate_mse(data[[target_column]], original_preds)
  
  importance_scores <- matrix(0, nrow = n_iterations, ncol = length(predictor_vars))
  colnames(importance_scores) <- predictor_vars
  
  for (i in 1:n_iterations) {
    for (var in predictor_vars) {
      data_shuffled <- data
      data_shuffled[[var]] <- sample(data_shuffled[[var]])
      
      shuffled_preds <- as.numeric(predict(model, data_shuffled))
      shuffled_mse <- calculate_mse(data_shuffled[[target_column]], shuffled_preds)
      
      importance_scores[i, var] <- shuffled_mse - original_mse
    }
  }
  
  avg_importance <- abs(colMeans(importance_scores))
  se_importance <- apply(importance_scores, 2, sd) / sqrt(n_iterations)
  
  result <- data.frame(
    Variable = predictor_vars,
    Importance = avg_importance,
    SE = se_importance
  )
  
  result <- result[order(-result$Importance), ]
  result$`Relative Importance` <- result$Importance / max(result$Importance) * 100
  
  return(result)
}

















select_best_model <- function(model_backward, model_forward) {
  #TODO: INSERT ROXYGEN
  # Compare the formulas of the two models
  if ((formula(model_backward) == formula(model_forward))) {
    # Formulas are the same, models are the same
    return(model_backward)
  } else {
    
    # --- Formulas are different ---
    
    # Extract terms from models
    terms_backward <- attr(terms(model_backward), "term.labels")
    terms_forward <- attr(terms(model_forward), "term.labels")
    
    # Check if models are nested -- BUG FIX 10/17/24
    if (all(terms_backward %in% terms_forward)) {
      # Backward model is nested within forward model
      smaller_model <- model_backward
      larger_model <- model_forward
    } else if (all(terms_forward %in% terms_backward)) {
      # Forward model is nested within backward model
      smaller_model <- model_forward
      larger_model <- model_backward
    } else {
      # Models are not nested; cannot perform LRT therefore we compare BIC to 
      # select the best model
      aic_backward <- BIC(model_backward)
      aic_forward <- AIC(model_forward)
      
      if (aic_backward < aic_forward) {
        return(model_backward)
      } else {
        return(model_forward)
      }
    }
    
    # Perform LRT
    lrt_result <- tryCatch({
      anova(smaller_model, larger_model, test = "LRT")
    }, error = function(e) {
      stop("Error in performing LRT: ", e$message)
    })
    
    p_value <- lrt_result$`Pr(>Chi)`[2]
    
    if (is.na(p_value)) {
      stop("LRT failed: p-value is NA")
    }
    
    # Decide which model to choose based on LRT
    if (p_value < 0.05) {
      return(larger_model) # Larger model is significantly better
    } else {
      return(smaller_model) # Smaller model is sufficient

    }
  }}














create_cross_table <- function(data) {
  vars <- names(data)
  n_vars <- length(vars)
  
  results <- list()
  
  for (i in 1:(n_vars-1)) {
    for (j in (i+1):n_vars) {
      var1 <- vars[i]
      var2 <- vars[j]
      
      # variable types
      type1 <- class(data[[var1]])[1]
      type2 <- class(data[[var2]])[1]
      
      # result based on variable types
      if (type1 %in% c("factor", "character") && type2 %in% c("factor", "character")) {
        result <- table(data[[var1]], data[[var2]])
        
      } else if (type1 %in% c("numeric", "integer") && type2 %in% c("numeric", "integer")) {
        result <- cor(data[[var1]], data[[var2]], use = "complete.obs")
        
      } else {
        if (type1 %in% c("factor", "character")) {
          result <- tapply(data[[var2]], data[[var1]], mean, na.rm = TRUE)
        } else {
          result <- tapply(data[[var1]], data[[var2]], mean, na.rm = TRUE)
        }
      }
      
      #  result
      results[[paste(var1, "x", var2)]] <- result
    }
  }
  
  return(results)
}

