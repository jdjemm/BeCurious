
#https://shiny.rstudio.com/articles/persistent-data-storage.html
#https://stackoverflow.com/questions/48925576/r-shiny-toggle-text-of-actionlink

library(ggplot2)
#install.packages('shiny')
#install.packages('shinyjs')
#install.packages('MASS')
#install.packages('DT')


library(shiny)
library(shinyjs)
library(MASS)
library(DT)

# Source the functions we need - UPDATE THIS
source("U:/Be curious/R code/Amy/Functions.R")

# Source the analysis for the modelling tab - UPDATE THIS
source("U:/Be curious/R code/Amy/ModellingTab.R")

#save the data to an excel file- UPDATE THIS 
excelpath<-"U:/Be curious/R code/Be curious 2022 participant data.csv"

# Define the fields we want to save from the form
fields <- c("Colour", "Group", "How Happy Are You?")

# Shiny app with 3 fields that the user can submit data for
    ui = fluidPage(
        useShinyjs(),
        titlePanel("The Chocolate Trial - Data Collection and Results"),
        sidebarLayout(
        sidebarPanel(h3("Please enter the following information:"),
        tags$hr(),
        radioButtons("Colour", "Which colour group are you in?", c("Sparkly Pink", "Orange", "Dark Blue", "Sparkly Purple", "Red", "Light Blue", "Teal", "Yellow", "Purple", "Sparkly Red", "Pink", "Green"), selected=NULL),
        radioButtons("Group", "Which chocolate group are you in?", c("A","B"), selected=NULL),
        radioButtons("How Happy Are You?", "How Happy Are You?", c("Not Happy","Happy","Very Happy"), selected=NULL),
        actionButton("submit", "Submit")),

        
        mainPanel(
            tabsetPanel(
                tabPanel("Overall",
                        br(),
                        plotOutput("plot"),
                        br(),
                        actionButton("OverallSummary", "Show Summary"),
                        br(),
                        hidden(div(id="OverallToggle", br(), textOutput("OverallText"), br(), actionButton("OverallExtension", "Extend"))),
                        br(),
                        hidden(div(id="OverallExtensionTog", textOutput("OverallExtensionText")))),
                tabPanel("By Colour Group",
                         br(),
                         plotOutput("popplot"),
                         actionButton("GroupSummary", "Show Summary"),
                         br(),
                         hidden(div(id="GroupToggle", br(), textOutput("GroupText"), br(), actionButton("GroupExtension", "Extend"))),
                         br(),
                         hidden(div(id="GroupExtensionTog", textOutput("GroupExtensionText")))),
                tabPanel("Association Test",
                         br(),
                         p("We can also show our data in a table like the one below. Look to see which box has the highest number in. Have a think about what this might mean"),
                         tableOutput("CrossTab"), 
                         textOutput("Test?"),
                         br(),
                         actionButton("ApplyTest", "Yes"),
                         br(),
                         hidden(div(id="ApplyAssociationTest", br(),
                                        textOutput("AssTest"), br(),
                                        actionButton("MoreInfoTestAction", "Explanation"))),br(),
                         hidden(div(id="MoreInfoTestTog", textOutput("MoreInfoTestText"), br(), actionButton("TestExtension", "Extend"))), br(), 
                         hidden(div(id="TestExtensionTog", textOutput("TestExtensionText")))),
                tabPanel("Mathematical Model", br(),
                         p("In order to apply a mathematical model we need to have collected all our information and Be Curious isn't over yet!"),
                         p("To show how we would do this in the real world we've collected some different data and after today we'll summarise all the data from Be Curious and let you know the answer to our question!"),
                         p("To start off we have summarised our population, just like you have today."),
                         p("  The bar chart on the left is the same as your stickers counted up."),
                         p("  The bar chart on the right is the same as the first bar chart you saw on this laptop!"),
                         fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("SimSummaries"), plotOutput("SimGroupSummaries"))),
                         p("Should we see if chocolate group changes this group of peoples happiness?"),
                         actionButton("ApplyModel", "Yes"),
                         br(),
                         hidden(div(id="MathemToggle", br(), textOutput("ModelResults"), br(), htmlOutput("ModelInterp"))),br()),
                tabPanel("Data",br(), DT::dataTableOutput("responses", width = 450),  downloadButton("downloadData", "Download")),
            )
    )))

    server = function(input, output, session) {
        
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            data
        })
        

        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
        })
        
        
        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$responses <- DT::renderDataTable({
           input$submit
            loadData()
        })
        
        output$downloadData <- downloadHandler(
          filename = function() {
            paste(input$responses, excelpath, sep = "")
          },
          content = function(file) {
            write.csv(loadData(), file, row.names = FALSE)})
        
        # Make the overall plot
        output$plot <- renderPlot({
            input$submit
            if(exists("responses")){
            # Combine the data ready for summarising
            CombinedData <- CombineData(responses)
             #Make the data for the plot
            PlotData <- data.frame(table(CombinedData$Group, CombinedData$`How Happy Are You?`))
            colnames(PlotData) <- c("Chocolate Group", "Happy", "Freq")
            
            plot <- ggplot(PlotData, aes(fill=`Chocolate Group`, x=Happy, y=Freq)) + 
                geom_bar(position="dodge", stat="identity",color="black") + theme_minimal() + theme(text=element_text(size=16),plot.title = element_text(size = 20,hjust=0.5)) +
              # scale_fill_brewer(palette="Paired") + 
              scale_fill_manual("legend", values = c("A" = "Snow", "B" = "chocolate"))+
               geom_text(aes(label=Freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5) +
                labs(title="Current Happiness by Chocolate Groups", x="Current Happiness", y="Number of Participants")
            
            plot(plot)
        }})
        
        #Summary about the plot
        observeEvent(input$OverallSummary,{
          toggle('OverallToggle')
          output$OverallText <- renderText({"This is a bar chart. It is a nice way to look at all the data in one go. What can you tell from everyone's results? Is everyone very happy? Or is there a mix of answers? What about the different chocolate groups? Are more people who are very happy from Chocolate Group A or Chocolate Group B?"})
          
          
          if (input$OverallSummary %% 2 == 1) {txt <- "Hide Summary"}
          else {txt <- "Show Summary"}
          updateActionButton(session, "OverallSummary", label = txt)  
        }) 
        
        #Extension
        observeEvent(input$OverallExtension,{
          toggle('OverallExtensionTog')
          output$OverallExtensionText <- renderText({"This is showing us information on everyone that has taken part in this activity so far, what about everyone in your colour group? Click the By Colour Group tab to find out more!"})
          
          
          if (input$OverallExtension %% 2 == 1) {txt <- "Hide Extension"}
          else {txt <- "Show Extension"}
          updateActionButton(session, "OverallExtension", label = txt)  
        }) 
        
        
        
        #Subset responses based on input;
        responses_subset <- reactive({
                    if(exists("responses")){
                    responses2 <- subset(responses,Colour == input$Colour)
                    return(responses2)
                }})
        
        #Make the population plot
        output$popplot <- renderPlot({
            input$submit
            # Combine the data ready for summarising
            CombinedData <- CombineData(responses_subset())
            # Subset the data based on the response this person gave
            ColGroup <- CombinedData$Colour[length(CombinedData$Colour)]
            SubsetData <- CombinedData[which(CombinedData$Colour == ColGroup),]
            # Make the data for the plot
            PlotData2 <- data.frame(table(SubsetData$Group, SubsetData$`How Happy Are You?`))
            colnames(PlotData2) <- c("Chocolate Group", "Happy", "Freq")
            
            Popplot <- ggplot(PlotData2, aes(fill=`Chocolate Group`, x=Happy, y=Freq)) + 
                geom_bar(position="dodge", stat="identity",color="black") + theme_minimal() + theme(text=element_text(size=16), plot.title = element_text(size = 20,hjust=0.5), plot.subtitle = element_text(size = 20,hjust=0.5)) +
              # scale_fill_brewer(palette="Paired") + 
              scale_fill_manual("legend", values = c("A" = "Snow", "B" = "chocolate"))+
                geom_text(aes(label=Freq), vjust=1.6, color="black", position = position_dodge(0.9), size=3.5) +
                labs(title="Current Happiness by Chocolate Groups", subtitle = "In Your Colour Group", x="Current Happiness", y="Number of Participants")
            
            plot(Popplot)
            
        })
        
        #Summary about the plot
        observeEvent(input$GroupSummary,{
          toggle('GroupToggle')
          output$GroupText <- renderText({"This bar chart shows all the answers from everyone who was in the same colour group as you. Look to see which bar is the highest for each chocolate group. Did you have the same answer?"})
          
          
          if (input$GroupSummary %% 2 == 1) {txt <- "Hide Summary"}
          else {txt <- "Show Summary"}
          updateActionButton(session, "GroupSummary", label = txt)  
        }) 
        
        #Extension
        observeEvent(input$GroupExtension,{
          toggle('GroupExtensionTog')
          output$GroupExtensionText <- renderText({"Bar charts are good picture of the results but we still don't know whether chocolate group changes how happy you are. We can use a statistical test in order to determine whether being given chocolate from Group A makes you more happier than being given chocolate from Group B. This test helps us to establish whether there is a relationship between our outcome of interest = happiness and our exploratory varibles of interest chocolate.  Click the tab called Association Test to find out more!"})
          
          
          if (input$GroupExtension %% 2 == 1) {txt <- "Hide Extension"}
          else {txt <- "Show Extension"}
          updateActionButton(session, "GroupExtension", label = txt)  
        }) 
        
        #Make the cross tabulation
        observeEvent(input$submit,{
            if(exists("responses")){
            if("A" %in% responses$Group){responses$Group <- relevel(factor(responses$Group), ref="A")}
            Table <- as.data.frame.matrix(table(responses$Group,ordered(responses$`How Happy Are You?`, levels=c("Not Happy", "Happy","Very Happy"))))
            output$CrossTab <- renderTable(Table, striped=TRUE, bordered = TRUE , rownames = TRUE)}})
        
        # Blurb for conducting chi-squared test
        output$`Test?` <- renderText({"Should we compare the chocolate groups using a test?"})
        
        # Conduct the chisq test
       observeEvent(input$ApplyTest,{
           toggle("ApplyAssociationTest")
           chisq <- chisq.test(table(responses$Group, responses$`How Happy Are You?`))
           fishers <- fisher.test(table(responses$Group, responses$`How Happy Are You?`))
           if(chisq$expected[1,1] < 5 || chisq$expected[1,2] < 5 || chisq$expected[1,3] < 5 || chisq$expected[2,1] < 5 || chisq$expected[2,2] < 5 || chisq$expected[2,3] < 5){
               if(fishers$p.value<0.05){output$AssTest <- renderText({"Fishers Test: Happiness and Chocolate Group are not independent"})}
               else{output$AssTest <- renderText({"Fishers Test: Happiness and Chocolate Group are independent"})}
               
           }
           else{
               if(chisq$p.value<0.05){output$AssTest <- renderText({"Chi-Squared Test: Happiness and Chocolate Group are not independent"})}
               else{output$AssTest <- renderText({"Chi-Squared Test: Happiness and Chocolate Group are independent"})}
           }
           
           if (input$ApplyTest %% 2 == 1) {txt <- "Hide Test"}
           else {txt <- "Apply Test"}
           updateActionButton(session, "ApplyTest", label = txt)  
           
           
           
        })

       

        observeEvent(input$MoreInfoTestAction,{
            toggle('MoreInfoTestTog')
            output$MoreInfoTestText <- renderText({"Here we have tested to see if Chocolate Group and Happiness are independent. That means does the type of chocolate egg you eat change your answer? For example, if the test found chocolate and happienss to be independent, then this means the type of chocolate you eat does not affect your happiness. If the test found chocolate and happiness to not be independent, then the type of chocolate you eat may affect your happiness. This only tells us that there is a change. It doesn't say whether one group is happier than the other. It also doesn't think that adults might have different answers to children. What do you think about that?"})
            
    
            if (input$MoreInfoTestAction %% 2 == 1) {txt <- "Hide Explanation"}
            else {txt <- "Show Explanation"}
            updateActionButton(session, "MoreInfoTestAction", label = txt)  
       })    
        
        #Extension
        observeEvent(input$TestExtension,{
          toggle('TestExtensionTog')
          output$TestExtensionText <- renderText({"The way to think about whether things like age, chocolate preferences and how happy you were at the start of this activity is a mathematical model based on a relationship expressed through an equation. Click the tab called Mathematical Model to find out more!"})
          
          
          if (input$TestExtension %% 2 == 1) {txt <- "Hide Extension"}
          else {txt <- "Show Extension"}
          updateActionButton(session, "TestExtension", label = txt)  
        }) 
       
       #################
       # MODELLING TAB #
       #################
       
       # First output the plot summarising the simulated data.
        output$SimSummaries <- renderPlot(print(SummaryPlot))
        
       # Then output the plot which summarises the simulated data by outcome
        output$SimGroupSummaries <- renderPlot(print(OverallPlot))
        
       # Model Explanation
       observeEvent(input$ApplyModel,{
       # Toggle it for new users
       toggle('MathemToggle')
         #output$ModelResults <- renderText({print(ResultsSubset$ChocolateGroupResults)})
            #Print out blurb
            output$ModelInterp <- renderUI({
              HTML(paste(paste("Using the model, we can interpret the odds ratio of the results. The odds ratio can tell us whether there is an association between the type of chocolate you are given and your happiness i.e. does the type of chocolate affect your happiness. ", 
              "If there is no difference between the two group answers then the odds ratio is 1, and we can interpret ths result as 'the type of chocolate you have does not affect your happiness.' If the odds ratio is greater than 1, this means the odds of being happy in group B is higher than that of group A", 
              "If the odds ratio is lower than 1 then the odds of being happy in Group B is lower than that of Group A. In these 2 scenarios, we may be able to conclude that the type of chocolate egg you have affects your happiness, though it is important we also take into account the confidence intervals too!", 
              "We can use this principle to intrepret the odds ratio of the chocolate group."),
              " ",
              print(ResultsSubset$ChocolateGroupResults),
              " ",
              paste("Here ", round(ResultsSubset$OR,3), " is the odds ratio and (", round(ResultsSubset$`2.5 %.y`,3), ", ", round(ResultsSubset$`97.5 %.y`,3), ") is called the 95% confidence interval."),
              paste("This means that for people in Chocolate group B, the odds of being happy or very happy are ", round(ResultsSubset$OR,3), " compared to chocolate group A."),
              " ",
              "Now, because we have only asked 1000 people our question we only have an estimate, what we think the answer is based on the data we have.",
              "If we want to answer our question for everyone we would have to ask everyone and that would take a lot of time!",
              paste("The confidence interval can tell us how sure we are that the real value is inside our confidence interval. When interpreting the odds ratio, it is important that we also use the confidence interval in order to make a conclusion about our test. As we have used 95% confidence intervals here, we can say we are 95% sure (so pretty sure), that the real value is inside our confidence intervals, so bigger than ", round(ResultsSubset$`2.5 %.y`,3), "but smaller than ", round(ResultsSubset$`97.5 %.y`,3), "."),
              "Because 1 is in the confidence interval we can't say that the real value of the odds ratio is not 1, meaning there is no difference between the two groups.",
              "So the answer to our question in this data is:",
              " ",
              tags$b("The type of chocolate egg you have doesn't affect your happiness."),
              paste("We can also see the effect that age, happiness at the start of the activity and liking milk or white chocolate has on the outcome. For example, looking at the odds ratio and 95% confidence intervals for age:"),
                      " ",
                    print(ResultsSubsetage$AgeResults),
                      " ", 
              paste("As the confidence interval also includes 1 we can conclude that age (Adult/Child) does not affect your happiness."),sep="<br/>"))
            })
      # Update Action button for toggling
      if (input$ApplyModel %% 2 == 1) {txt <- "Hide Model Results"}
      else {txt <- "Show Model Results"}
      updateActionButton(session, "ApplyModel", label = txt) 
  
      
       })
       
    }

    
    
# Run the application 
shinyApp(ui = ui, server = server)