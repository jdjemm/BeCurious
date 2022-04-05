#################################################
# Name: Be Curious Functions Source             #
# Purpose: To conduct the analysis required for #
#          Be Curious Application in functions  #
#################################################

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

CombineData <- function(inputdata){
  if (exists("inputdata")){
    # First make the colour groups data;
    ColourGroups <- expand.grid(c(0,1,2), c(0,1),c(0,1))
    colnames(ColourGroups) <- c("BLHappy", "Age", "Chocolate")
    ColourGroups$Chocolate <- factor(ColourGroups$Chocolate, levels=c(0,1), labels=c("White","Milk"))
    ColourGroups$Age <- factor(ColourGroups$Age, levels=c(0,1), labels=c("Adult","Child"))
    ColourGroups$BLHappy <- factor(ColourGroups$BLHappy, levels=c(0,1,2), labels=c("Not Happy","Happy","Very Happy"))
    GroupID <- c("Sparkly Pink", "Orange", "Dark Blue", "Sparkly Purple", "Red", "Light Blue", "Teal", "Yellow", "Purple", "Sparkly Red", "Pink", "Green")
    ColourGroupData <- cbind(GroupID, ColourGroups)
    colnames(ColourGroupData) <- c("Colour", "BLHappy", "Age", "Chocolate")
    #Now let's put the responses data into something we can uses;
    data2 <- inputdata
    #data2$Colour <- as.numeric(paste(data2$Colour))
    if("A" %in% data2$Group) {data2$Group <- relevel(factor(data2$Group), "A")}
    data2$`How Happy Are You?`<- ordered(data2$`How Happy Are You?`, levels=c("Not Happy", "Happy","Very Happy"))
    # Now let's merge the data together;
    CombinedData <- merge(data2, ColourGroupData)
    return(CombinedData)
  }
}


