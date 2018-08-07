
# This is the loop for the residuals, it can be implemented in two different ways,
# either the first, completely disregarding the location drawn
# (F: I think this is oversimplifying it too much) and the second where depending on the location
# drawn a random residual from that cluster is drawn



bootstrap.residuals <- function(n_boot1, sample_size, num_residuals, residuals){

  bootstap_indices <- sample(x = 1:num_residuals, size = sample_size * n_boot1, replace = T)

  matrix(residuals[bootstap_indices], ncol = n_boot1)
}
# This bootstrap function just randomly draws from all residuals and completely ignores the locations






#
#
#
# ### complicated bootstrap formula. We could also use the easy one.
# # this one draws residuals that correspond to the random location effect that was drawn previously
#
#
# bootstrap.residuals.compl <- function(n_boot = n_boot,
#                                       location_and_obs = num_obs_in_location,
#                                       bootstrap_location = random_location_boot){
#
#   #create a dataframe where we know how often each region got bootstrapped
#   num_obs_bootstrap <- num.obs.in.location(bootstrap_location$location)
#
#   #draw indices. This assumes that the original data is ordered according to locations.
#   # N: (we should do that somewhere)
#   n_locations <- nrow(location_and_obs)
#
#   bootstrap_res <- rep(NA, times = nrow(random_location_boot))
#   counter <- 0
#   for (i in 1:n_locations){
#
#     bootstrap_res[seq.int(from = i, to = i + num_obs_bootstrap[i,2])] <-
#       sample(x = seq.int(from = i, to = i + num_obs_in_location[i,2]),
#              size = num_obs_bootstrap[i,2], replace = T) + counter
#
#     counter <- counter + num_obs_bootstrap[i,2]
#   }
#   # this loop does the following:
#   # for every one of the different locations it wants to create a random samples of indices that point
#   # to the residuals that were observed in these locations. To do so it
#   # 1) initialises a counter = 0
#   # 2) looks at the first location and sees how often this location was drawn as a random location effect
#   #    in the first bootstrapping round
#   # 3) if location 1 appeared 599 times in the bootstrap samples it will draw 599 indices that point to
#   #    a random sample of residuals drawn from all residuals of location 1.
#   # 4) to make sure the draws are residuals the counter is increased by the number of observations in the
#   #    last location. Given that the data is ordered after locations this will generate indices that can
#   #    be used for subsetting and drawing the wanted residuals.
#   return(bootstrap_res)
# }
#
#
#
#
#
#
#
# # old code
# #--------- for every random location effect, draw corresponding epsilon  ----------#
# epsilon_boot <- rep(NA, length(random_location_boot)) #initialise vector for loop.
#
# for(i in 1:nrow(random_location_boot)){
#   epsilon_boot[i] <- sample(x = training$e_res[training$geo2_br ==
#                                                  random_location_boot$geo2_br[i]],
#                             size = 1)
# }
#
#
