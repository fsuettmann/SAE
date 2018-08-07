# this function is supposed to automatically generate ne mean or median vriables from the census
# dataset to boost the precission of our survey prediction
# it automatically determines if a variable is metric or not
# and then use the mean or median on the larger (census) sample to account for lacking
# details in the survey sample. The newly generate vaiabkes of the mean are added to the survey data
# and the model equation is updated


#################################
###    The function itself   ####
#################################

mean.for.regression <- function(mResponse, censusdata, surveydata, model){
  if(mResponse == "all"){ # instead of "all" I want this to be a simpe dot (.)
    # like in the regression as a placeholder for all varaibles except y
    model.split <- unlist(strsplit(mResponse, split="~")) # splits responses from the Y
    model.split <- gsub(pattern = " ", replacement="" , model.split[2])
    # removes all the blanks
    model.split <- unlist(strsplit(model.split, split="\\+"))
  } else {
    model.split <- gsub(pattern = " ", replacement="" , mResponse)
    # removes all the blanks
    model.split <- unlist(strsplit(model.split, split="\\+"))
  }
  # creates vector based in individual vars


  # for-loop that determines if a varaible is metric or not
  var.names <- rep(NA, length(model.split)) # new variable names
  var.means <- as.data.frame(matrix(NA, nrow = nrow(surveydata), ncol = length(model.split)))
  # matrix of means and medians

  for (i in 1:length(model.split)) {
    # first we select the variable we want from our input and connect it to the
    # census dataset for the mean or median calculation
    varcheck <- eval(parse(text=paste("censusdata$", model.split[i], sep = "")))
    # if statement that determines if mean or median as computet and saved in a vector
    if(is.numeric(varcheck) || is.integer(varcheck)){
      var.means[,i] <- rep(mean(varcheck), nrow(surveydata)) # mean
      var.names[i] <- paste(model.split[i], ".Cmean" , sep = "")
    } else {
      var.means[,i] <- rep(median(varcheck), nrow(surveydata)) # median
      var.names[i] <- paste(model.split[i], ".Cmedian" , sep = "")
    }
  }
  colnames(var.means) <- var.names # defines the new variable names
  #######------------------------------------------------########
  ### this could be sped up by using data.table's rbindlist() ###
  surveydata <- cbind(surveydata, var.means)
  #######------------------------------------------------########

  # the last step is now to recreate tha part that gets pasted into the model
  model <- paste( model , paste(model.split, collapse = " + "), sep = " + ")
  # we should check if this hyperparametrisation works!!
  return(list(model, surveydata))
}
