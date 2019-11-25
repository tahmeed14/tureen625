#' @title Ordinary Least Squares
#'
#' @description This function allows an user to fit a linear regression model using Ordinary Least Squares. For the function to work, the requirements for OLS need to be satisfied
#' (i.e. design matrix has to be full rank etc.). A complete tutorial is provided as a vignette that can be accessed
#' using ```vignette(package = "tureen625)````
#'
#' @param design_X the design matrix for the regression model. Must be matrix type
#'
#' @param Y the target variable for the regression model. Must be vector type
#'
#' @return Returns a list that contains several features of the linear regression model that is fit (see the provided Vignette for more details)
#'
#' `param_estimate`: a vector of the parameter estimates for the regression model
#'
#' `residuals`: a vector of the residuals for the regression model
#'
#' `sigma_Sq` : the variance (sigma squared) associated with the regression model
#'
#' `var_Mat` : the covariance-variance matrix of the beta estimates associated with the regression model
#'
#' `param_StdErrors` : the std. errors associated with the parameter estimates for the regression model
#'
#' `conf_int` : the 95 percent confidence intervals for the parameter estimates
#'
#' `results` : a dataframe that organizes the parameter estimates, std. erros, and confidence intervals
#'
#' @examples my_fit <- fit_OLS(design_X = matrix_object, Y = vector_object)
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

    # Organize results into a nice dataframe that can be used to view results
    results_df <- data.frame(cbind(term = seq(length(beta_estimate)),
                                   estimate = beta_estimate,
                                   std_error = se_betas,
                                   conf_low = lower_bound,
                                   conf_high = upper_bound))

    colnames(results_df) <- c("Term", "Estimate", "Std. Error",
                              "95% CI Lower Bound", "95% CI Upper Bound")


    # Store the variables into a list that will be returned
    tureenObject <- list(param_estimates = as.vector(beta_estimate),
                         residuals = residuals,
                         sigma_Sq = sigma_Sq, var_Mat = Var_beta,
                         param_StdErrors = se_betas,
                         conf_int = CI_df,
                         results = results_df)

    return(tureenObject)

}

