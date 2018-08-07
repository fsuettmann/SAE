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

location.simplifier <- function(surv_data, location) {
  if(is.character(location)){
    # if location is string this part is executed
    t <- paste("surv_data$", location, sep = "")
    loc <- as.factor(eval(parse(text=t)))
    levels(loc) <- seq.int(length.out = length(loc))
    return(as.integer(loc))
  } else if(length(location) == nrow(surv_data)) {
    # if location is instead supplied as a separate vector
    loc <- as.factor(location)
    levels(loc) <- seq.int(length.out = length(loc))
    return(as.integer(loc))
  } else stop('location has to be a string or a vector of the same length as the surveydata')
  # the function pasts an error if none of the two possible types of data are supplied
  # or if there is no location given at all
}
