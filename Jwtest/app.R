library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)

#importing the data 
airline_cleaned <- read_csv("data/Airline_cleaned.csv")
summary(airline_cleaned)
str(airline_cleaned)

#making the integers into factors so that 1 to 5 are treated as levels rather than integers
airline_cleaned$Gender <- factor(airline_cleaned$Gender)
airline_cleaned$CustomerType <- factor(airline_cleaned$CustomerType)
airline_cleaned$TypeOfTravel <- factor(airline_cleaned$TypeOfTravel)
airline_cleaned$Class <- factor(airline_cleaned$Class)
airline_cleaned$Satisfaction <- factor(airline_cleaned$Satisfaction)

str(airline_cleaned)

#drop certain columns not required for logistic regression model 
airline_lm<- select(airline_cleaned, -c(Gender, FlightDistance))
summary(airline_lm)
str(airline_lm)

#logistic Regression model
log.model2 <- glm(Satisfaction ~ . , family = binomial(link = 'logit'), data = airline_lm)

# R Shiny dashboard UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Airline Passenger Satisfaction",
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem("Data Overview", tabName = "DataOverview"),
      menuItem("Descriptive Satistics", tabName = "DescriptiveSatistics"),
      menuItem("Inferential Statistics", tabName = "InferentialStatistics")
    )),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "DataOverview",
        fluidRow(box(title = "Customer Profile", width = 12,
                     fluidRow(column(3, plotOutput("pie_gender")), 
                              column(3, plotOutput("pie_customer_type")),
                              column(3, plotOutput("pie_travel_type")),
                              column(3, plotOutput("pie_class_type"))))),
        
        
        fluidRow(column(12, align = "center", plotOutput("overall_satisfaction"))),
        fluidRow(column(12, align = "center", plotOutput("overall_boxplot")))
        
        
      ),
      tabItem(
        tabName = "DescriptiveSatistics",
        
        
        fluidRow(box(title = "Satisfaction Levels According To Customer Profiles", width = 12,
                     fluidRow(column(3, selectInput("factor", "Select a factor:", 
                                                    choices = list("Gender" = "Gender", "Age Group" = "Age", "Customer Type" = "CustomerType", "Travel Type" = "TypeOfTravel", "Travel Class" = "Class", "Flight Distance" = "FlightDistance"))),
                              column(6, plotOutput("factor_satisfaction"))
                     )
        )
        
        ),
        fluidRow(box(title = "Satisfaction Level Group By Travel Class", width = 12,
                     fluidRow(column(4, plotOutput("bar_inflightwifi_class")),
                              column(4, plotOutput("bar_foodanddrink_class")),
                              column(4, plotOutput("bar_cleanliness_class")))
        )
        ),
        fluidRow(box(title = "Satisfaction Level Group By Customer Loyalty", width = 12,
                     fluidRow(column(4, plotOutput("bar_inflightwifi_customertype")),
                              column(4, plotOutput("bar_foodanddrink_customertype")),
                              column(4, plotOutput("bar_cleanliness_customertype")))
        )
        ),
        
        fluidRow(box(title = "Before Flight Factors", width = 12,
                     fluidRow(column(12, plotOutput("before_flight_boxplot")))
        )
        ),
        fluidRow(box(title = "During Flight Factors", width = 12,
                     fluidRow(column(12, plotOutput("during_flight_boxplot")))
        )
        )
        
      ),
      tabItem(
        tabName = "InferentialStatistics",
        titlePanel("Logistic Regression - Predict Customer Satisfaction"),
        fluidRow(column(3, h3("Please key in the following to predict customer satisfaction:"),
                        helpText("Note: For the sliding scales, 1 is least satisfied and 5 is most satisfied")),
                 column(3, radioButtons("Rcusttype", h3("Customer Type"),
                                        choices = list("Loyal" = "Loyal Customer", "Disloyal" = "disloyal Customer"),selected = "Loyal Customer")),
                 column(3, radioButtons("Rtraveltype", h3("Type of Travel"),
                                        choices = list("Business" = "Business travel", "Leisure" = "Personal Travel"),selected = "Business travel")),
                 column(3, radioButtons("Rclass", h3("Flight Class"),
                                        choices = list("Business" = "Business", "Eco Plus" = "Eco Plus", "Eco" = "Eco"),selected = "Business"))),
        
        fluidRow(column(4, numericInput("nage", h3("Customer Age"), value = 30)),
                 column(4, numericInput("ndeparturedelay", h3("Departure Delay in Minutes"), value = 5)),
                 column(4, numericInput("narrivaldelay", h3("Arrival Delay in Minutes"), value = 5))),
        
        fluidRow(column(3, sliderInput("Swifi", h3("Inflight Wifi"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Stime", h3("Departure & Arrival Time Convenience"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Sease", h3("Ease of Online Booking"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Sgate", h3("Gate Location"),
                                       min = 1, max = 5, value = 3))),
        
        fluidRow(column(3, sliderInput("Sfood", h3("Food and Drink"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Sboarding", h3("Online Boarding"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Sentertainment", h3("Inflight Entertainment"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Sonboard", h3("Onboard Service"),
                                       min = 1, max = 5, value = 3))),
        
        fluidRow(column(3, sliderInput("SLegroom", h3("Legroom Service"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("SBaggage", h3("Baggage Handling"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Scheckin", h3("Check in Service"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Sinflight", h3("Inflight Service"),
                                       min = 1, max = 5, value = 3))),
        
        fluidRow(column(3, sliderInput("Sclean", h3("Cleaniness"),
                                       min = 1, max = 5, value = 3)),
                 column(3, sliderInput("Sseat", h3("Seat Comfort"),
                                       min = 1, max = 5, value = 3))),
        tabBox(
          id = "tabset1",
          height = "1000px",
          width = 14,
          
          tabPanel(h4("Prediction"),box(
            width = 14, div(h4('Based on the model, we predict that this customer would most likely be:')),
            verbatimTextOutput("Result", placeholder = TRUE),
            actionButton('subm','Submit'))),
          
          tabPanel(h4("Summary of Training Data used"),box(withSpinner(DT::dataTableOutput('tbl')),
                                                           width = 14)),
          
          
          tabPanel(h4("Logistic Regression Model"),box(withSpinner(verbatimTextOutput("summary")),
                                                       width = 14))
        )
        
      )
    )
  )
)

server <- function(input, output) {
  
  # Data Overview
  output$pie_gender <- renderPlot({
    ggplot(airline_cleaned, aes(x = "", fill = factor(Gender))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5),
            plot.margin = margin(1,1,1,1,"cm"),
            panel.background = element_rect(colour = "black")) + 
      labs(fill="Gender", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart - Gender", 
           caption="Source: mpg") + 
      coord_polar(theta = "y", start=0)
  })
  
  output$pie_customer_type <- renderPlot({
    ggplot(airline_cleaned, aes(x = "", fill = factor(CustomerType))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5),
            panel.background = element_rect(colour = "black")) + 
      labs(fill="CustomerType", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart - Customer Type", 
           caption="Source: mpg") + 
      coord_polar(theta = "y", start=0)
    
  })
  
  output$pie_travel_type <- renderPlot({
    ggplot(airline_cleaned, aes(x = "", fill = factor(TypeOfTravel))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5),
            plot.margin = margin(0.3,0.3,0.3,0.3,"cm"),
            panel.background = element_rect(colour = "black")) + 
      labs(fill="TypeOfTravel", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart - Type Of Travel", 
           caption="Source: mpg") + 
      coord_polar(theta = "y", start=0)
  })
  
  output$pie_class_type <- renderPlot({
    ggplot(airline_cleaned, aes(x = "", fill = factor(Class))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5),
            plot.margin = margin(0.75,0.75,0.75,0.75,"cm"),
            panel.background = element_rect(colour = "black")) + 
      labs(fill="Class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart - Type of Class", 
           caption="Source: mpg") + 
      coord_polar(theta = "y", start=0)
  })
  
  output$overall_satisfaction <- renderPlot({
    ggplot(airline_cleaned, aes(x = "", fill = factor(Satisfaction))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="Satisfaction", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart - Overall Satisfaction", 
           caption="Source: mpg") + 
      coord_polar(theta = "y", start=0)
  })
  
  
  output$overall_boxplot <- renderPlot({
    
    alldat <- c(airline_cleaned$DepartureArrivalTimeConvenient, 
                airline_cleaned$EaseOfOnlineBooking, 
                airline_cleaned$OnlineBoarding, 
                airline_cleaned$CheckInService, 
                airline_cleaned$BaggageHandling, 
                airline_cleaned$GateLocation,
                airline_cleaned$OnBoardService,
                airline_cleaned$InflightService,
                airline_cleaned$SeatComfort,
                airline_cleaned$LegRoomService,
                airline_cleaned$InflightWifi,
                airline_cleaned$InflightEntertainment,
                airline_cleaned$FoodAndDrink,
                airline_cleaned$Cleanliness)
    
    my_data <- data.frame("Level" = alldat, 
                          "Item" = c(rep("DepartureArrivalTimeConvenient",95415),
                                     rep("EaseOfOnlineBooking",95415),
                                     rep("OnlineBoarding",95415),
                                     rep("CheckInService",95415),
                                     rep("BaggageHandling",95415),
                                     rep("GateLocation",95415),
                                     rep("OnBoardService",95415),
                                     rep("InflightService",95415),
                                     rep("SeatComfort",95415),
                                     rep("LegRoomService",95415),
                                     rep("InflightWifi",95415),
                                     rep("InflightEntertainment",95415),
                                     rep("FoodAndDrink",95415),
                                     rep("Cleanliness",95415)
                          )
    )
    
    get_box_stats <- function(y, upper_limit = 5*1.15){
      return(data.frame(
        y = 0.95 * upper_limit,
        label = paste(
          "Max=",round(max(y), 2), "\n",
          "Min =", round(min(y), 2), "\n",
          "Median =", round(median(y), 2), "\n",
          "Mean=",round(mean(y), 2), "\n"
        )
      ))
    }
    
    library(ggpubr)
    ggboxplot(my_data, x = "Item", y = "Level",color = "Item", palette = c("#FF0000","#FF7F00","#00FF00","#0000FF","#4B0082","#9400D3","#00FFFF","#FF00FF","#000080","#DC143C","#FFD700","#228B22","#A0522D","#8B4513"),
              #order = c("DepartureArrivalTimeConvenient", "EaseOfOnlineBooking","OnlineBoarding","CheckInService","BaggageHandling","GateLocation","OnBoardService","InflightService","SeatComfort","LegRoomService","InflightWifi","InflightEntertainment","FoodAndDrink","Cleanliness"),
              ylab = "Satisfaction Level", xlab = "Factors") +
      theme(text = element_text(size=8),axis.text.x = element_text(angle=90, hjust=1)) +
      ggtitle("Satisfaction Levels") +
      stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9,size=2)
    
  })
  
  
  
  
  # Descriptive Statistics
  
  chart = reactive({
    if (input$factor == "Gender") {
      dt <- airline_cleaned%>%
        dplyr::group_by(Gender, Satisfaction)%>%
        dplyr::tally()%>%
        dplyr::mutate(percent=n/sum(n))
      
      ggplot(data = dt, aes(x = Gender, y = n,fill = Satisfaction))+
        geom_bar(stat="identity")+ geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
                                             position=position_stack(vjust=0.5), colour="white", size = 2)+
        ggtitle("Satisfaction by Gender") + labs(x="Gender",y="Satisfaction Count")
      
    } else if (input$factor == "Age"){
      airline_cleaned %>%
        select(Age, Satisfaction) %>%
        ggplot(aes(x = Age, fill = Satisfaction)) +
        geom_histogram(position = 'identity', alpha = 0.3)+
        labs(x="Age of Passengers",y="Satisfaction Count")+
        ggtitle("Satisfaction by Age")
      
    } else if (input$factor == "CustomerType") {
      dt2 <- airline_cleaned%>%
        dplyr::group_by(CustomerType, Satisfaction)%>%
        dplyr::tally()%>%
        dplyr::mutate(percent=n/sum(n))
      
      ggplot(dt2, aes(x = CustomerType, y = n, fill = Satisfaction))+
        geom_bar(stat="identity")+ geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
                                             position=position_stack(vjust=0.5), colour="white", size = 2)+
        ggtitle("Satisfaction by Customer Type") + labs(x="Customer Type",y="Satisfaction Count")
      
    } else if (input$factor == "TypeOfTravel"){
      dt3 <- airline_cleaned%>%
        dplyr::group_by(TypeOfTravel, Satisfaction)%>%
        dplyr::tally()%>%
        dplyr::mutate(percent=n/sum(n))
      
      ggplot(dt3,aes(x= TypeOfTravel, y = n,fill = Satisfaction))+
        geom_bar(stat="identity")+ geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
                                             position=position_stack(vjust=0.5), colour="white", size = 2)+
        ggtitle("Satisfaction by Type of Travel") + labs(x="Type of Travel",y="Satisfaction Count")
      
    } else if (input$factor == "Class") {
      dt4 <- airline_cleaned%>%
        dplyr::group_by(Class, Satisfaction)%>%
        dplyr::tally()%>%
        dplyr::mutate(percent=n/sum(n))
      
      ggplot(dt4, aes(x = Class, y = n,fill = Satisfaction))+
        geom_bar(stat="identity")+ geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
                                             position=position_stack(vjust=0.5), colour="white", size = 2)+
        ggtitle("Satisfaction by Class") + labs(x="Class",y="Satisfaction Count")
      
    } else {
      airline_cleaned %>%
        select(FlightDistance, Satisfaction) %>%
        ggplot(aes(x = FlightDistance, fill = Satisfaction)) +
        geom_histogram(position = 'identity', alpha = 0.3)+
        labs(x="Flight Distance",y="Satisfaction Count")+
        ggtitle("Satisfaction by Flight Distance")
    }
  })
  
  output$factor_satisfaction <- 
    renderPlot({
      chart()
    })
  
  output$bar_inflightwifi_class <- renderPlot({
    ggplot(airline_cleaned, aes(InflightWifi))+geom_bar(aes(fill=factor(InflightWifi)))+facet_grid(.~Class)
  })
  
  output$bar_foodanddrink_class <- renderPlot({
    ggplot(airline_cleaned, aes(FoodAndDrink))+geom_bar(aes(fill=factor(FoodAndDrink)))+facet_grid(.~Class)
  })
  
  output$bar_cleanliness_class <- renderPlot({
    ggplot(airline_cleaned, aes(Cleanliness))+geom_bar(aes(fill=factor(Cleanliness)))+facet_grid(.~Class)
  })
  
  output$bar_inflightwifi_customertype <- renderPlot({
    ggplot(airline_cleaned, aes(InflightWifi))+geom_bar(aes(fill=factor(InflightWifi)))+facet_grid(.~CustomerType)
  })
  
  output$bar_foodanddrink_customertype <- renderPlot({
    ggplot(airline_cleaned, aes(FoodAndDrink))+geom_bar(aes(fill=factor(FoodAndDrink)))+facet_grid(.~CustomerType)
  })
  
  output$bar_cleanliness_customertype <- renderPlot({
    ggplot(airline_cleaned, aes(Cleanliness))+geom_bar(aes(fill=factor(Cleanliness)))+facet_grid(.~CustomerType)
  })
  
  
  # Before Flight Factors
  output$before_flight_boxplot <- renderPlot({
    
    bffdat <- c(airline_cleaned$DepartureArrivalTimeConvenient, 
                airline_cleaned$EaseOfOnlineBooking, 
                airline_cleaned$OnlineBoarding, 
                airline_cleaned$CheckInService, 
                airline_cleaned$BaggageHandling, 
                airline_cleaned$GateLocation)
    
    my_data <- data.frame("Level" = bffdat, 
                          "Item" = c(rep("DepartureArrivalTimeConvenient",95415),
                                     rep("EaseOfOnlineBooking",95415),
                                     rep("OnlineBoarding",95415),
                                     rep("CheckInService",95415),
                                     rep("BaggageHandling",95415),
                                     rep("GateLocation",95415)
                          )
    )
    
    get_box_stats <- function(y, upper_limit = 5*1.15){
      return(data.frame(
        y = 0.95 * upper_limit,
        label = paste(
          "Max=",round(max(y), 2), "\n",
          "Min =", round(min(y), 2), "\n",
          "Median =", round(median(y), 2), "\n",
          "Mean=",round(mean(y), 2), "\n"
        )
      ))
    }
    
    library(ggpubr)
    ggboxplot(my_data, x = "Item", y = "Level", 
              color = "Item", palette = c("#FF0000","#FF7F00","#00FF00","#0000FF","#4B0082","#9400D3"),
              order = c("DepartureArrivalTimeConvenient", 
                        "EaseOfOnlineBooking",
                        "OnlineBoarding",
                        "CheckInService",
                        "BaggageHandling",
                        "GateLocation"),
              ylab = "Satisfaction Level", 
              xlab = "Before Flight Factors") +
      theme(text = element_text(size=8),axis.text.x = element_text(angle=90, hjust=1)) +
      ggtitle("Satisfaction Levels of Before Flight Factors") +
      stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9,size=2)
  })
  
  
  
  output$during_flight_boxplot <- renderPlot({
    dffdat <- c(airline_cleaned$OnBoardService,
                airline_cleaned$InflightService,
                airline_cleaned$SeatComfort,
                airline_cleaned$LegRoomService,
                airline_cleaned$InflightWifi,
                airline_cleaned$InflightEntertainment,
                airline_cleaned$FoodAndDrink,
                airline_cleaned$Cleanliness)
    
    my_data <- data.frame("Level" = dffdat, 
                          "Item" = c(rep("OnBoardService",95415),
                                     rep("InflightService",95415),
                                     rep("SeatComfort",95415),
                                     rep("LegRoomService",95415),
                                     rep("InflightWifi",95415),
                                     rep("InflightEntertainment",95415),
                                     rep("FoodAndDrink",95415),
                                     rep("Cleanliness",95415)
                          )
    )
    get_box_stats2 <- function(y, upper_limit = 5*1.15){
      return(data.frame(
        y = 0.95 * upper_limit,
        label = paste(
          "Max=",round(max(y), 2), "\n",
          "Min =", round(min(y), 2), "\n",
          "Median =", round(median(y), 2), "\n",
          "Mean=",round(mean(y), 2), "\n"
        )
      ))
    }
    
    ggboxplot(my_data, x = "Item", y = "Level", 
              color = "Item", palette = c("#FF0000","#FF7F00","#00FF00","#0000FF","#4B0082","#9400D3","#99004C","#808080"),
              order = c("OnBoardService", "InflightService","SeatComfort","LegRoomService","InflightWifi","InflightEntertainment","FoodAndDrink","Cleanliness"),
              ylab = "Satisfaction Level", xlab = "During Flight Factors")+theme(text = element_text(size=8),axis.text.x = element_text(angle=90, hjust=1))+ggtitle("Satisfaction Levels of During Flight Factors")+
      stat_summary(fun.data = get_box_stats2, geom = "text", hjust = 0.5, vjust = 0.9,size=2)
  })
  
  
  
  #Prediction model
  #React value when using the action button
  a <- reactiveValues(Result = NULL)
  
  observeEvent(input$subm, {
    
    #dataframe for the single prediction
    values = data.frame(CustomerType = input$Rcusttype,
                        Age = input$nage,
                        TypeOfTravel = input$Rtraveltype,
                        Class = input$Rclass,
                        InflightWifi = input$Swifi,
                        DepartureArrivalTimeConvenient = input$Stime,
                        EaseOfOnlineBooking = input$Sease,
                        GateLocation = input$Sgate,
                        FoodAndDrink = input$Sfood,
                        OnlineBoarding = input$Sboarding,
                        SeatComfort = input$Sseat,
                        InflightEntertainment = input$Sentertainment,
                        OnBoardService = input$Sonboard,
                        LegRoomService = input$SLegroom,
                        BaggageHandling = input$SBaggage,
                        CheckInService = input$Scheckin,
                        InflightService = input$Sinflight,
                        Cleanliness = input$Sclean,
                        DepartureDelayInMinutes = input$ndeparturedelay,
                        ArrivalDelayInMinutes = input$narrivaldelay)
    
    #Single prediction using the logistic regression model
    a$Result <- ifelse((log.model2 %>% predict(values, type= "response")) > 0.5, "satisfied", "neutral or dissatisfied")
  })
  
  output$Result <- renderText({
    #Display the prediction value
    paste(a$Result)
  })
  
  #display train data
  output$tbl = DT::renderDataTable({
    DT::datatable(airline_lm, options = list(lengthChange = FALSE))
  })
  
  
  #display log regression model's summary
  output$summary <- renderPrint({
    fit <- log.model2 <- glm(Satisfaction ~ . , family = binomial(link = 'logit'), data = airline_lm)
    summary(fit)
  })
}

shinyApp(ui = ui, server = server)
