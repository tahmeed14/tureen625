#' @title Ordinary Least Squares
#'
#' @description This Package
#'
#' @param design_X the design matrix for the regression model. Must be matrix type
#'
#' @param Y the target variable for the regression model. Must be vector type
#'
#' @return
#'
#' @examples my_fit <- function(design_X = my_matrix, Y = my_Y)
#'
#' @export


# Fit the OLS function

fit_OLS <- function(design_X, Y) {

    # Calculate beta estimates using OLS estimator equation
    beta_estimate <- solve(t(design_X) %*% design_X) %*% t(design_X) %*% Y

    # Calculate Residuals
    residuals <- Y - design_X %*% beta_estimate

    # Calculate sigma hat squared
    df <- nrow(design_X) - ncol(design_X)

    sigma_Sq <- (t(residuals) %*% residuals) / df

    # Calculate the Variance(beta_estimate)
    Var_beta <- as.vector(sigma_Sq) * (solve(t(design_X) %*% design_X))

    # Calculate the Standard Errors of the Beta Estimates
    se_betas <- sqrt(diag(Var_beta))

    # Create a 95% Confidence Interval Data Frame that can be used for visualization later
    lower_bound <- beta_estimate - 1.96 * se_betas
    upper_bound <- beta_estimate + 1.96 * se_betas

    CI_df <- cbind(lower_bound, upper_bound)

    # Store the variables into a list that will be returned
    tureenObject <- list(param_estimates = as.vector(beta_estimate),
                         residuals = residuals,
                         sigma_Sq = sigma_Sq, var_Mat = Var_beta,
                         param_StdErrors = se_betas,
                         conf_int = CI_df)

    return(tureenObject)

}

