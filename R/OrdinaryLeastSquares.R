#' @title Ordinary Least Squares
#'
#' @description This Package
#'
#' @param design_X
#'
#' @return NULL
#'
#' @examples hello("World")
#'
#' @export


# Fit the OLS function

fit_OLS <- function(design_X, Y, penalty = "None") {

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

    tureenObject <- list(param_estimates = beta_estimate, residuals = residuals,
                         sigma_Sq = sigma_Sq, var_Mat = Var_beta,
                         param_StdErrors = se_betas)

}

