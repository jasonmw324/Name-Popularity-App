library(shiny)


source("functions/NameAppFunctions.R")





ui <- navbarPage("Popularity of Baby Names",
                 
                 
                 tabPanel("Most Popular Names",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("location", label = h3("Choose the Location"), 
                                          choices = list("US" = "US", "Alabama" = "AL", "Alaska" = "AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO","Connecticut"="CT","Delaware"="DE","Florida"="FL","Georiga"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD","Tennesee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"), 
                                          selected = "US"),
                              selectInput("sex", label = h3("Choose the sex"), 
                                          choices = list("Female" = "F", "Male" = "M"), 
                                          selected = "Female"),
                              numericInput("n", label = h3("How many names should be on the top ranked list?"), min = 1,max = 20,value = 10,step = 1),
                              sliderInput("year", label = h3("What year should the top names be plotted for?"), min = 1880, max = 2021, value = 2021,step = 1,sep = "")
                            ),
                            mainPanel(plotOutput(outputId = "NameAppPlot")
                            
                            )
                          )
                          
                          
                          
                          
                          
                          
                          
                          ),
                 tabPanel("Track Name Popularity Over Time",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("LOCATION", label = h3("Choose the Location"), 
                                          choices = list("US" = "US", "Alabama" = "AL", "Alaska" = "AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO","Connecticut"="CT","Delaware"="DE","Florida"="FL","Georiga"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD","Tennesee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"), 
                                          selected = "US"),
                              selectInput("SEX", label = h3("Choose the sex"), 
                                          choices = list("Female" = "F", "Male" = "M"), 
                                          selected = "Female"),
                              sliderInput("YearRange", label = h3("Choose the range of years to plot popularity"), min = 1880, 
                                          max = 2021, value = c(1880, 2021),sep=""),
                              textInput("Name1", label = h3("First Name")),
                              textInput("Name2", label = h3("Second Name")),
                              textInput("Name3", label = h3("Third Name")),
                              textInput("Name4", label = h3("Fourth Name")),
                              textInput("Name5", label = h3("Fifth Name")),
                              textInput("Name6", label = h3("Sixth Name")),
                              textInput("Name7", label = h3("Seventh Name")),
                              textInput("Name8", label = h3("Eighth Name"))
                              
                              
                            ),
                            mainPanel(plotOutput(outputId = "PopularityPlotOneLoc")
                                      
                            )
                          )
                          
                          
                          
                          
                          ),
        navbarMenu("Compare States",
                            tabPanel("Most Popular Names",plotOutput("TwoBar"),
                                     selectInput("LocationOne", label = h3("Choose the First Location"), 
                                                 choices = list("US" = "US", "Alabama" = "AL", "Alaska" = "AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO","Connecticut"="CT","Delaware"="DE","Florida"="FL","Georiga"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD","Tennesee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"), 
                                                 selected = "US"),
                                     selectInput("LocationTwo", label = h3("Choose the Second Location"), 
                                                 choices = list("US" = "US", "Alabama" = "AL", "Alaska" = "AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO","Connecticut"="CT","Delaware"="DE","Florida"="FL","Georiga"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD","Tennesee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"), 
                                                 selected = "US"),
                                     
                                     selectInput("SeX", label = h3("Choose the sex"), 
                                                 choices = list("Female" = "F", "Male" = "M"), 
                                                 selected = "Female"),
                                     numericInput("N", label = h3("How many names should be on the top ranked list?"), min = 1,max = 20,value = 10,step = 1),
                                     sliderInput("YEAR", label = h3("What year should the top names be plotted for?"), min = 1880, max = 2021, value = 2021,step = 1,sep = "")
                            ),
                            mainPanel(
                              #plotOutput(outputId = "NameAppPlotCompare")
                                     
                                     
                                     
                                     ),
                            tabPanel("Track Name Popularity Over Time",plotOutput("TwoLines"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("FirstLocation", label = h3("Choose the Location"), 
                                                     choices = list("US" = "US", "Alabama" = "AL", "Alaska" = "AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO","Connecticut"="CT","Delaware"="DE","Florida"="FL","Georiga"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD","Tennesee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"), 
                                                     selected = "US"),
                                         selectInput("SecondLocation", label = h3("Choose the Location to compare to"), 
                                                     choices = list("US" = "US", "Alabama" = "AL", "Alaska" = "AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO","Connecticut"="CT","Delaware"="DE","Florida"="FL","Georiga"="GA","Hawaii"="HI","Idaho"="ID","Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA","Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS","Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ","New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK","Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD","Tennesee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA","West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"), 
                                                     selected = "US"),
                                         selectInput("gender", label = h3("Choose the sex"), 
                                                     choices = list("Female" = "F", "Male" = "M"), 
                                                     selected = "Female"),
                                         sliderInput("YearRangeTwo", label = h3("Choose the range of years to plot popularity"), min = 1880, 
                                                     max = 2021, value = c(1880, 2021),sep=""),
                                         textInput("NameOne", label = h3("First Name")),
                                         textInput("NameTwo", label = h3("Second Name")),
                                         textInput("NameThree", label = h3("Third Name")),
                                         textInput("NameFour", label = h3("Fourth Name"))
                                         
                                         
                                         
                                       ),
                                       mainPanel(
                                         #plotOutput(outputId = "PopularityPlotCompare" )
                                                 
                                       )
                                     )
                                     
                                    
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     )),
                
                            
                            
  
  
  
  
  
)
server<-function(input,output){
  output$NameAppPlot<-renderPlot({
    
    NationalGraphFunction(location = input$location, n = input$n, year = input$year, sex =input$sex )
  })
  
  
  output$PopularityPlotOneLoc<-renderPlot({
   
   range=input$YearRange
   
   PopularityOverTimeFunction(Location = input$LOCATION,Name1 = input$Name1,Name2 = input$Name2 ,Name3 = input$Name3,Name4 = input$Name4 ,Name5 = input$Name5 ,Name6 = input$Name6 ,Name7 =  input$Name7 ,Name8 = input$Name8,LowerYear = range[1],UpperYear = range[2],sex =input$SEX ) 
  })
  
    
  output$TwoBar<-renderPlot({
    
    NationalGraphFunctionComparison(location1 =input$LocationOne ,location2 = input$LocationTwo,n = input$N,year = input$YEAR,sex = input$SeX)
    
    
  })
    
 output$TwoLines<-renderPlot({
   
   RANGE=input$YearRangeTwo
   
   PopularityCompareOverTime(Location1 = input$FirstLocation,Location2 = input$SecondLocation,Name1 = input$NameOne,Name2 =input$NameTwo ,Name3 = input$NameThree,Name4 =input$NameFour ,LowerYear = RANGE[1],UpperYear = RANGE[2],sex = input$gender)
 })   

  
  
}




shinyApp(ui = ui, server = server)