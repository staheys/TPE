cochran_critical <- function(alpha, experiment_count, variance_df) {
  q <- qf(
    1 - alpha / experiment_count,
    df1 = variance_df,
    df2 = (experiment_count - 1) * variance_df
  )
  q / (q + experiment_count - 1)
}

analyze_ffe <- function(response_matrix, alpha = 0.05, label = "Data set") {
  if (!all(dim(response_matrix) == c(4, 4))) {
    stop("The response matrix must contain 4 replicates and 4 experiments")
  }

  factor_1 <- c(-1,  1, -1,  1)
  factor_2 <- c(-1, -1,  1,  1)
  design_matrix <- cbind(
    x0 = 1,
    x1 = factor_1,
    x2 = factor_2,
    x3 = factor_1 * factor_2
  )

  experiment_count <- nrow(design_matrix)
  replicate_count <- nrow(response_matrix)
  response_means <- colMeans(response_matrix)
  variances <- apply(response_matrix, 2, var)

  cochran_value <- max(variances) / sum(variances)
  cochran_limit <- cochran_critical(
    alpha,
    experiment_count,
    replicate_count - 1
  )

  cat("\n", label, "\n")
  cat("Means:", paste(round(response_means, 4), collapse = "; "), "\n")
  cat("Variances:", paste(round(variances, 4), collapse = "; "), "\n")
  cat(sprintf(
    "Cochran test: G = %.4f; critical G = %.4f\n",
    cochran_value,
    cochran_limit
  ))

  if (cochran_value >= cochran_limit) {
    cat("Variances are not homogeneous. Further analysis was stopped.\n")
    return(invisible(list(
      reproducible = FALSE,
      cochran_value = cochran_value
    )))
  }

  coefficients <- as.vector(
    crossprod(design_matrix, response_means) / experiment_count
  )
  names(coefficients) <- paste0("b", 0:3)
  reproducibility_variance <- mean(variances)
  reproducibility_df <- experiment_count * (replicate_count - 1)
  coefficient_se <- sqrt(
    reproducibility_variance / (experiment_count * replicate_count)
  )
  t_values <- abs(coefficients) / coefficient_se
  t_limit <- qt(1 - alpha / 2, df = reproducibility_df)
  significant <- t_values > t_limit
  reduced_coefficients <- ifelse(significant, coefficients, 0)
  names(reduced_coefficients) <- names(coefficients)

  cat(
    "Coefficients:",
    paste(
      sprintf("%s = %.4f", names(coefficients), coefficients),
      collapse = "; "
    ),
    "\n"
  )
  cat(
    "t values:", paste(round(t_values, 4), collapse = "; "),
    sprintf("; critical t = %.4f\n", t_limit)
  )
  cat(
    "Significant coefficients:",
    paste(names(coefficients)[significant], collapse = ", "),
    "\n"
  )

  predicted_response <- drop(design_matrix %*% reduced_coefficients)
  significant_count <- sum(significant)
  adequacy_df <- experiment_count - significant_count

  if (adequacy_df == 0) {
    cat("The model is saturated: adequacy cannot be tested.\n")
    return(invisible(list(
      reproducible = TRUE,
      saturated = TRUE,
      coefficients = coefficients,
      significant = significant
    )))
  }

  adequacy_variance <- replicate_count / adequacy_df *
    sum((response_means - predicted_response)^2)
  fisher_value <- adequacy_variance / reproducibility_variance
  fisher_limit <- qf(
    1 - alpha,
    df1 = adequacy_df,
    df2 = reproducibility_df
  )
  adequate <- fisher_value < fisher_limit

  cat(sprintf(
    "Reproducibility variance = %.4f; adequacy variance = %.4f\n",
    reproducibility_variance,
    adequacy_variance
  ))
  cat(sprintf(
    "Fisher test: F = %.4f; critical F = %.4f\n",
    fisher_value,
    fisher_limit
  ))
  cat(if (adequate) "The model is adequate.\n" else "The model is not adequate.\n")

  invisible(list(
    reproducible = TRUE,
    saturated = FALSE,
    adequate = adequate,
    design_matrix = design_matrix,
    means = response_means,
    variances = variances,
    coefficients = coefficients,
    significant = significant,
    fitted = predicted_response,
    cochran_value = cochran_value,
    fisher_value = fisher_value
  ))
}

# Factor limits and coding parameters.
factor_min <- c(z1 = 198, z2 = 83, z3 = 7.2)
factor_max <- c(z1 = 239, z2 = 200, z3 = 23)
factor_center <- (factor_max + factor_min) / 2
factor_step <- (factor_max - factor_min) / 2

# Rows contain replicate measurements; columns contain experiments.
base_data <- rbind(
  c(42, 193, 294, 112),
  c(107, 116, 229, 137),
  c(64, 137, 271, 115),
  c(44, 146, 244, 160)
)

main_result <- analyze_ffe(base_data, label = "Main analysis")

# Test 1: a large outlier should make the variances non-homogeneous.
outlier_data <- base_data
outlier_data[1, 1] <- 350
outlier_result <- analyze_ffe(outlier_data, label = "Outlier test")

# Test 2: all four coefficients should be significant, making the model saturated.
target_means <- c(40, 180, 220, 160)
saturated_data <- rbind(
  target_means - 3,
  target_means - 1,
  target_means + 1,
  target_means + 3
)
saturated_result <- analyze_ffe(
  saturated_data,
  label = "Saturated model test"
)
