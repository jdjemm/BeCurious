
# Set the seed so all the data simulated will be the same each time.
set.seed(20200328)

# Let's just make some outcome data first;
ParticipantID <- seq(1,1000,1)
Colour <- sample(c("Sparkly Pink", "Orange", "Dark Blue", "Sparkly Purple", "Red", "Light Blue", "Teal", "Yellow", "Purple", "Sparkly Red", "Pink", "Green"), size = 1000, replace=TRUE)
Group <- rbinom(1000,1,0.5)
Group.F <-factor(Group, levels=c(0,1), labels=c("A", "B"))
OutcomeHappy <- rbinom(1000,2,0.6)
OutcomeHappy.F <- factor(OutcomeHappy, levels=c(0,1,2), labels=c("Not Happy","Happy","Very Happy"))

FakeOutcomeData <- data.frame(ParticipantID, Colour, Group.F, OutcomeHappy.F)

# Now let's link this with what the colours mean to the baseline characteristics

#Create a 12*3 grid of all the options
Chocolate <- c(0,1)
Age <- c(0,1)
BLHappy <- c(0,1,2)
ColourGroups <- expand.grid(c(0,1,2), c(0,1),c(0,1))
colnames(ColourGroups) <- c("BLHappy", "Age", "Chocolate")
ColourGroups$Chocolate <- factor(ColourGroups$Chocolate, levels=c(0,1), labels=c("White","Milk"))
ColourGroups$Age <- factor(ColourGroups$Age, levels=c(0,1), labels=c("Adult","Child"))
ColourGroups$BLHappy <- factor(ColourGroups$BLHappy, levels=c(0,1,2), labels=c("Not Happy","Happy","Very Happy"))
GroupID <- c("Sparkly Pink", "Orange", "Dark Blue", "Sparkly Purple", "Red", "Light Blue", "Teal", "Yellow", "Purple", "Sparkly Red", "Pink", "Green")
ColourGroupData <- cbind(GroupID, ColourGroups)
colnames(ColourGroupData) <- c("Colour", "BLHappy", "Age", "Chocolate")

#Merge the baseline data with the outcome data
FakeData <- merge(FakeOutcomeData, ColourGroupData)

#First we want to summarise our fake population

#So the plot matches the colours defined at the start (as best they can) we'll define them here
PlotColours = c("Sparkly Pink" = rgb(255,0,102, maxColorValue=255), "Orange" = rgb(255,122,31, maxColorValue=255), "Dark Blue"= rgb(68,117,161, maxColorValue=255), "Sparkly Purple"= rgb(153,51,102, maxColorValue=255), "Red"= rgb(255,0,0, maxColorValue=255), "Light Blue"= rgb(190,215,239, maxColorValue=255), "Teal" = rgb(0,128,128, maxColorValue=255), "Yellow" = rgb(255,255,25, maxColorValue=255), "Purple" = rgb(204,153,255, maxColorValue=255), "Sparkly Red" = rgb(255,0,0, maxColorValue=255), "Pink" = rgb(255,153,204, maxColorValue=255), "Green" = rgb(146,208,80, maxColorValue=255))
PlotLabels = c("Sparkly Pink" = "Sparkly\nPink" , "Orange" = "Orange", "Dark Blue"= "Dark\nBlue", "Sparkly Purple"= "Sparkly\nPurple", "Red"= "Red", "Light Blue"= "Light\nBlue", "Teal" = "Teal", "Yellow" = "Yellow", "Purple" = "Purple", "Sparkly Red" = "Sparkly\nRed", "Pink" = "Pink", "Green" = "Green")



SummaryPlot <- ggplot(data=FakeData, aes(x=Colour,fill=Colour)) +
  geom_bar(stat="count") +
  geom_text(stat="count", aes(label=..count..), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5) +
  theme_minimal() +
  scale_fill_manual(values = PlotColours, aesthetics = "fill") +
  theme(axis.text.x = element_text(size=10), legend.position = "none", plot.title = element_text(size = 12,hjust=0.5)) +
  labs(title="Population Summary", y= "Number of people in each group", x = "Colour Group") + labs(fill="Colour Group") +
  geom_point(aes(x="Sparkly Pink", y=0), shape=8) +
  geom_point(aes(x="Sparkly Red", y=0), shape=8) +
  geom_point(aes(x="Sparkly Purple", y=0), shape=8) +
  scale_x_discrete(labels=PlotLabels)
  
# We then want to make the same plot as on the overall tab

OverallPlotData <- data.frame(table(FakeData$Group, FakeData$OutcomeHappy.F))
colnames(OverallPlotData) <- c("Chocolate Group", "Happy", "Freq")

OverallPlot <- ggplot(OverallPlotData, aes(fill=`Chocolate Group`, x=Happy, y=Freq)) + 
  geom_bar(position="dodge", stat="identity") + theme_minimal() + theme(plot.title = element_text(size = 12,hjust=0.5)) +
  scale_fill_brewer(palette="Paired") + 
  geom_text(aes(label=Freq), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) +
  labs(title="Current Happiness by Chocolate Groups", x="Current Happiness", y="Number of Participants")


# Now we want to fit an ordinal regression model;

OrdinalLogistic <- polr(OutcomeHappy.F ~ Group.F + BLHappy + Age + Chocolate, data=FakeData)
# Obtain the model coefficients
ctable <- coef(summary(OrdinalLogistic))
# Obtain the p-values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# Calculate the confidence intervals
ci <- confint.default(OrdinalLogistic)
# Add in two extra rows to acount for the intercepts, these don't get a confidence interval.
ci_merge <- rbind(ci, c(NA,NA),c(NA,NA))
# Add the p-values and CI's into the coefficients table
(ctable_full <- cbind(ctable, ci_merge, "p value" = p))

# Add this into the 
#Exp everything so we can interpret it into odds
exp_ctable <- exp(cbind(OR = coef(OrdinalLogistic), ci))
#Add in the p-value
ResultsTable <- merge(ctable_full, exp_ctable,by="row.names", all.x=TRUE)

#Extract Chocolate and baselines
ResultsSubset <- subset(ResultsTable, Row.names=="Group.FB")
ResultsSubset$ChocolateGroupResults <- paste(round(ResultsSubset$OR,3), " (", round(ResultsSubset$`2.5 %.y`,3), ", ", round(ResultsSubset$`97.5 %.y`,3), ")")

#Extract age 
ResultsSubsetage <- subset(ResultsTable, Row.names=="AgeChild")
ResultsSubsetage$AgeResults <- paste(round(ResultsSubsetage$OR,3), " (", round(ResultsSubsetage$`2.5 %.y`,3), ", ", round(ResultsSubsetage$`97.5 %.y`,3), ")")


