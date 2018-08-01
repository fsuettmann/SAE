#' @title Testscript
#' @description Beschreibung der Funktion
#' @param x Beschreibung von \code{x}
#' @param y Beschreibung von \code{y}
#' @export
#' @return Was die Funktion ausspuckt.
#' @references
#' @seealso
#' @keywords
#' @examples


#
# #----------------- preliminary set up -----------------#
#
# # load required packages
# library(knitr)
# library(ggplot2)
# library(foreign) # load SPSS data files
# library(dplyr)
# library(devtools) #temporary
# library(xtable)
# library(scales)
# library(lme4)
# library(nlme)
#
#
#
# # specify user
# Felix <- F
# Nikos <- T
#
# # set seed
# set.seed(100)
#
# #-------------------------------------------------#
# #----------------- load data set -----------------#
# #-------------------------------------------------#
#
# if(Felix){
#   setwd("C:\\Users\\felix\\OneDrive\\Dokumente\\Master Angewandte Statistik\\SS2018\\Statistische Programmierung mit R\\RCode and Data")
#   data <- read.dta("brazil_art_2010.dta", convert.factors = F)
# }
# if(Nikos){
#   data <- read.dta("C:/Users/nikos/Google Drive/Uni/Statistik/Statistikmaster 2. Semester/Programmierung mit R/Projekt SAE/brazil_art_2010.dta", convert.factors=FALSE)
# }
# #------------------------------------------------------------------------------#
# #---------- seperate dataset to create a traing set and a test set ------------#
# #------------------------------------------------------------------------------#
#
# #---------- make the data set smaller to improve speed ------------#
# helper <- sample(helper <- sample(x = 1:nrow(data), size = nrow(data)/20, replace = F))
# helper <- sort(helper)
# data <- data[helper,]
#
# #------- generate indexes for the rows to keep. order indexes to keep ---------#
# helper <- sample(x = 1:nrow(data), size = nrow(data)/5, replace = F)
# helper <- sort(helper)
#
#
# #--- create training and test set from the originial data set using the indexes --#
# training <- data[helper,]
# test <- data[-helper,]
# rm(list = "helper") #delete helper
# rm(list = "data")
#
# nrow_traing <- nrow(training)
# nrow_test <- nrow(test)
#
#
#
#
# #----------------------------------------------------------------------------------#
# #----------------------------------------------------------------------------------#
# #----------------- First stage: inference from the small data set -----------------#
# #----------------------------------------------------------------------------------#
# #----------------------------------------------------------------------------------#
#
#
# #--------------- overview over the variables included in the dataset---------------#
# names(training)
#
# # locations are defined by the variable geo2_br
#
# # F: apply a step function based on AIC to find important parameters?
#
#
# #----------------------------------------------------------------------------------#
# #------------------- Fit a random effects model for consumption -------------------#
# #----------------------------------------------------------------------------------#
#
#
# #------------------------------ Model specification -------------------------------#
# # variable of interest: y_ch = income of household h in cluster c
# # parameters: beta = population wide coefficients
# # covariates: x_ch = observed characteristics of household h in cluster c
# # random effect: eta_c = random effect of cluster c (e.g. cluster is a productive area)
# # epsilon_ch = random error independent of random cluster effect
# #----------------------------------------------------------------------------------#
#
#
# #---------------------------- Define model parameters -----------------------------#
#
# model <- hh_inc ~ age + urban + rooms + sex + religion + race + adults + children
#
# # for fitting a random model:
# # model_re <- hh_inc ~ age + urban + rooms + sex + religion + race + adults + children
# # random_effects <- ~ 1 | geo2_br
#
# #------------------------------ Fit model with lme --------------------------------#
#
# model_fit <- lm(model, data = training)
# summary(model_fit)
#
# # for fitting a random model:
# # model_fit <- lme(model_re, random = random_effects, data = training)
# # summary(model_fit)
#
# #----------------------------------------------------------------------------------#
# #----------------------------- save model parameters ------------------------------#
# #----------------------------------------------------------------------------------#
#
#
# #------------------------ save fixed effects coefficients -------------------------#
# beta_hat <- model_fit$coefficients
#
#
# #---------_---------------------- extract residuals -------------------------------#
# e_res <- residuals(model_fit)
#
#
# #----------------- combine residuals with the training data set -------------------#
# # this serves to identify which residual belongs to which observation
# # also we can now know in which cluster/region/location this residual was observed
# training <- cbind(training, e_res)
#
#
# #--------------- calculate random location effects for later bootstrap ------------#
# # residuals can be written as e_res = location_effect + (e_res - location_effect)
# # location effect is the average of the residuals of all observations that belong to
# # one location.
#
# # specify variable in which the location is coded in the data set
# location <- as.vector(training$geo2_br)
#
# # for every location, calculate the mean
# location_effect <- by(data = training$e_res, INDICES =location, FUN = mean)
#
# # include the id of the location to make sure the location effects are identifiable
# location_effect <- as.data.frame(cbind(location_effect, unique(location)))
# colnames(location_effect)[2] <- "geo2_br"
# ?names()
#
# # merge average location effect with the original training data set
# training <- merge(training, location_effect,by="geo2_br")
#
#
#
# #--------------------------------- Update residuals -------------------------------#
# # residuals can be written as e_res = location_effect + (e_res - location_effect)
# # the latter part in brackets is now the residual for future computation
#
# training$e_res <- training$e_res - training$location_effect
#
#
#
#
# ##################### in case of random effects model:
# #----------------- save random location effects for later bootstrap ---------------#
# # eta_c <- random.effects(model_fit)
# #
# #----------------------- save residuals for later bootstrap -----------------------#
# # e_res <- residuals(third_fit)
# #####################
#
#
#
#
# #----------------------------------------------------------------------------------#
# #----------------------------------------------------------------------------------#
# #------------------ Second stage: imputation in the large census ------------------#
# #----------------------------------------------------------------------------------#
# #----------------------------------------------------------------------------------#
#
# #------------------------------ general idea --------------------------------------#
# # make a bootstrap sample according to the formula
# # y(b) = x'beta + location_effect(b) + epsilon_ch(b)
# # thus:
# # 1. predict x'beta
# # 2. draw a B location effects
# # 3. draw one residual for every one of the B location effects
# # this yields B y(b) over which we can average to get our estimate
#
# #----------------------------------------------------------------------------------#
# #------------------------------- preliminaries ------------------------------------#
# #----------------------------------------------------------------------------------#
#
# #------ add an observation id to the dataset to make observations identifiable ----#
# id <- 1:nrow(test)
# test <- cbind(test,id)
#
#
# #----------------------------------------------------------------------------------#
# #-------------- predict the "unknown" values y = x'beta in the census -------------#
# #----------------------------------------------------------------------------------#
#
# # here the test data set is the census
# xbeta <- predict.lm(lm(model, data=training), newdata=test)
# #tut das, was ich will?
#
#
# #----------------------------------------------------------------------------------#
# #-----------------make a bootstrap sample for the location effects ----------------#
# #----------------------------------------------------------------------------------#
#
# #----------------------- define number of bootstrap samples -----------------------#
# n_boot <- 2
#
# #---------------------------- define some other numbers ---------------------------#
# number_of_locations_survey <- nrow(location_effect)
# n_obs_census <- nrow(test)
#
#
# #--- for every hh in the census draw n_boot draw (indexes for) location effects ---#
#
# helper_index <- sample(x = 1:number_of_locations_survey,
#                        size = n_boot*n_obs_census, replace = T)
# # instead of directly drawing the location effects we draw indices indicating where
# # we can find the location effect we drew in the vector of all location effects.
# # since we draw independently, we can just make one big draw
#
#
# #----- use the indexes to make a matrix with draws from the location effects ------#
#
# random_location_boot <- location_effect[helper_index,]
# # for every observation in the census we draw n_boot location effects
# # random_location_boot therefore has n_obs_census*n_boot rows
# # nrow(random_location_boot) == n_obs_census*n_boot
#
#
# #----------------------------------------------------------------------------------#
# #--------- for every random location effect, draw corresponding epsilon  ----------#
# #----------------------------------------------------------------------------------#
#
# epsilon_boot <- rep(NA, length(random_location_boot)) #initialise vector for loop.
#
# for(i in 1:nrow(random_location_boot)){
#   epsilon_boot[i] <- sample(x = training$e_res[training$geo2_br ==
#                                                  random_location_boot$geo2_br[i]],
#                             size = 1)
# }
#
# # this loop
# # 1) checks for every drawn random location effect to which location (cluster) it
# # belongs
# # 2) draws one epsilon from all the epsilons that were observed in that cluster
#
#
#
# #----------------------------------------------------------------------------------#
# #------- put x'beta, location effect and residual from bootstrap together ---------#
# #----------------------------------------------------------------------------------#
#
# xbeta <- rep(xbeta, each = 2)
# # duplicate the rows of xbeta to be able to combine them with the
# # location effects and residuals
# # for matrices: xbeta[rep(seq(nrow(xbeta)), each = n_boot), ] (check whether this is faster than xbeta <- rbind(xbeta, xbeta))
#
# # also duplicate the id numbers so that the person can still be identified
# id_times_n_boot <- rep(id, each = 2)
#
#
# y_hat_bootstrap <- as.data.frame(cbind(y_predict = (xbeta + random_location_boot$location_effect + epsilon_boot),
#                                        id_times_n_boot))
#
#
# #----------------------------------------------------------------------------------#
# #---------------------- calculate the predicted values for y ----------------------#
# #----------------------------------------------------------------------------------#
#
# y_predicted <- as.vector(by(data=y_hat_bootstrap$y_predict, INDICES = y_hat_bootstrap$id, FUN = mean))
#
# y_predicted <- cbind(y_predicted, id)
#
