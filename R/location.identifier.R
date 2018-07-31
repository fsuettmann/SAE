# diese Funktion codiert intern die Locations einfach nur als aufsteigende Zahlen
# Das ergibt nur Sinn, wenn uns egal sind, wie Census- und
# Survey Locations miteinander in Beziehung stehen

location.identifier <- function(location){
  loc <- as.factor(location)
  levels(loc) <- seq.int(length.out = length(loc))
  return(as.integer(loc))
}
