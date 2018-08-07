#' @title SAE bootstrap ys
#' @description Beschreibung der Funktion
#' @param x Beschreibung von \code{x}
#' @param y Beschreibung von \code{y}
#' @export yes
#' @return Was die Funktion ausspuckt.
#' @references
#' @seealso
#' @keywords
#' @examples



all.vars(log(y) ~ x + z + d)

# bootstrap predicted ys

bootstrap.y <- function(model1, model_fit1, censusdata1, n_boot1, n_obs){

  # # Variante 1
  # # extract variables that are used in the model
  # model.vars <- unlist(strsplit(model1, split="~")) # splits responses from the Y
  # model.vars <- gsub(pattern = " ", replacement="" , model.vars[2])
  # # removes all the blanks
  # vars <- unlist(strsplit(model.vars, split="\\+"))
  # # vars contains all the variables used in the model also the ones with means

  # Variante 2
  vars <- all.vars(model1)[-1] # wir können ja nochmal gucken, welche Variante richtig ist

  # subset the censusdata set so that all explanatory variables in the model remain.
  # calculuate x'beta. This makes only sense if the model is a linear model
  x <- as.matrix(cbind(rep(1, times = n_obs), subset(censusdata1, select = vars)))


  # extract the variances of the coefficients beta_hat estimated in the survey
  # caluclate var(beta_hat) (formula would be: sigma^2 * (X'X)^-1)
  cov_coefficients <- vcov(summary(model_fit1))

  # for every predicted y_hat, calculate std of y_hat and put it in a vector
  # eventuell Matrix außerhalb der Loop transponieren
  var_y <- rep(NA, n_obs)
  for(i in 1:n_obs){
    var_y[i] <- t(x[i,]) %*% cov_coefficients %*% x[i,]
  }
  sd_y <- sqrt(var_y)

  # now draw a random sample of ys with the appropriate Variance

  # N: wir müssen mal gucken, ob es schneller geht, aus einer MVNORM zu ziehen, oder das per
  # Schleife zu machen.
  # N: vielleicht ist rapply nützlich?
  xbeta <- predict(model_fit1, newdata = censusdata1)

  y_bootstrap <- matrix(NA, nrow = n_obs, ncol = n_boot1)
  for (i in 1:n_obs){

    y_bootstrap[i,] <- rnorm(n = n_boot1, mean = xbeta[i], sd = sd_y[i])

  }


  #   y_bootstrap <- MASS::mvrnorm(n = n_boot1*n_obs, mu = model_fit1$coefficients, Sigma = cov_coefficients)

  return(y_bootstrap)

  #### General idea:
  # instead of drawing the betas from a normal distribution and calculcating y.b = Xbeta.b we
  # we calculate the variance of y.i = x.i'b and draw a normal distr. sample of the y's.

}








# # this function bootstraps the betas
#
# bootstrap.betas <- function(model_fit1 = model_fit, n_boot1 = n_boot, n_obs = n_obs_census){
#
#   summary_fit <- summary(model_fit1)
#
#   cov_coefficients <- vcov(summary_fit)
#
#   MASS::mvrnorm(n = n_boot1*n_obs, mu = model_fit1$coefficients, Sigma = cov_coefficients)
#
# }


