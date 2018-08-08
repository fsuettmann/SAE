# diese Funktion codiert intern die Locations einfach nur als aufsteigende Zahlen
# Das ergibt nur Sinn, wenn uns egal sind, wie Census- und
# Survey Locations miteinander in Beziehung stehen

# alte funktion
#location.simplifier <- function(location){
#  loc <- as.factor(location)
#  levels(loc) <- seq.int(length.out = length(loc))
#  return(as.integer(loc))
#}


# This function takes all the location effects from the survey data (correct?)
# and converts them to an ordered sequence of real numbers from 1 to length(location)
# it can either take a string with the name of the variable or a separate vector.

# ACHTUNG: Das ergibt nur Sinn, wenn uns egal sind, wie Census- und
# Survey Locations miteinander in Beziehung stehen


location.simplifier <-
  function(surv_data, location, n_obs_survey1) {
    # if location is string this part is executed
    if (is.character(location) & length(location) == 1) {
      if (any(location == names(surv_data))) {
        #only execute the following if the string specified is the name of a variable
        t <- paste("surv_data$", location, sep = "")
        loc <- as.factor(eval(parse(text = t)))
      } else {
        stop(
          "String that was specified as location is not the name of one of the variables in the survey data set."
        )
      }
      levels(loc) <- seq.int(length.out = length(loc))
      return(as.integer(loc))
    } else if (length(location) == n_obs_survey1) {
      # if location is instead supplied as a separate vector
      loc <- as.factor(location)
      levels(loc) <- seq.int(length.out = length(loc))
      return(as.integer(loc))
    } else
      stop(
        'location has to be a string indicating one of the variabeles in the data set or a vector of the same length as the surveydata'
      )
    # the function pasts an error if none of the two possible types of data are supplied
    # or if there is no location given at all
  }
