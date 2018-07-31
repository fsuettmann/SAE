# This function outputs a data.frame with one column for the different locations and in the other
# column we find how many observations there are in that column. This is done for efficient bootstrapping
# later on

# only needed if we do the complicated residual bootstrapping


num.obs.in.location <- function(location){

  unique_location <- unique(location)

  n_locations <- length(unique_location)


  num_obs <- rep(NA, times = n_locations)

  for (i in 1:n_locations){
    num_obs[i] <- sum(location == i)
  }

  cbind(unique_location, num_obs)
}


