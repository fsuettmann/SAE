#' @title Spielwiese
#' @description Beschreibung der Funktion
#' @param x Beschreibung von \code{x}
#' @param y Beschreibung von \code{y}
#' @export yes
#' @return Was die Funktion ausspuckt.
#' @references
#' @seealso
#' @keywords
#' @examples

# library(sae)
# library(foreign) # load SPSS data files
#
#
#
#
#
#
#
# ############# test of example
#
# Felix <- F
# Nikos <- T
#
# # set seed
# set.seed(100)
#
# #-------------------------------------------------#
# #----------------- load data set -----------------#
# #-------------------------------------------------#
# Nikos <- T
#
# if(Felix){
#   setwd("C:\\Users\\felix\\OneDrive\\Dokumente\\Master Angewandte Statistik\\SS2018\\Statistische Programmierung mit R\\RCode and Data")
#   data <- read.dta("brazil_art_2010.dta", convert.factors = F)
# }
# if(Nikos){
#   data <- read.dta("C:/Users/nikos/Google Drive/Uni/Statistik/Statistikmaster 2. Semester/Programmierung mit R/Projekt SAE/brazil_art_2010.dta", convert.factors=FALSE)
# }
#
# #----------------------- create test and training data set --------------------#
# #------- generate indexes for the rows to keep. order indexes to keep ---------#
# helper <- sample(x = 1:nrow(data), size = nrow(data)/5, replace = F)
# helper <- sort(helper)
# # training data set = 20%, test data set = 80%
#
#
# #--- create training and test set from the originial data set using the indexes --#
# training <- data[helper,]
# test <- data[-helper,]
# rm(list = "helper") #delete helper
#
# a <- Sys.time()
# y_pred <- sae(model = model, surveydata = training, location_survey = training$geo2_br, n_boot = 3,
#               censusdata = test)
#
# b <- Sys.time() - a;b
#
# y_pred
#
#
#
#
#
#
#
#
#
#
# identifier <- function(location, residual){
#   cppFunction('double sumC(NumericVector residual, StringVector location) {
#               int n = residual.size();
#               double total = 0;
#               for(int i = 0; i < n; ++i) {
#               total += x[i];
#               }
#               return total;
# }')
# }


