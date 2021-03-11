suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(htmltools))

#Data Read and Transformation

#Read the flight data
flights_2018 <- readRDS("data/flights_2018.rds")

#Data frame that gathers all the delay types for delay type analysis
flights_2018 %>%
  gather("Carrier_Delay", 
         "Weather_Delay", 
         "Nas_Delay", 
         "Security_Delay", 
         "Late_Ac_Delay", 
         key = "Delay_Type", 
         value = "Delay_Type_Time") -> flights_2018_delay_types

#Delay time bins for delay type proportion over delay time analysis
delay_time_bins <- seq(min(flights_2018$Dep_Delay),max(flights_2018$Dep_Delay),30)

#Data frame that calculates the delay type proportions over total delay time
flights_2018 %>%
  mutate(Delay_Bins = cut(Dep_Delay, delay_time_bins)) %>%
  group_by(Delay_Bins) %>%
  mutate(Carrier_Delay_Prop = sum(Carrier_Delay)/sum(Carrier_Delay, 
                                                     Weather_Delay, 
                                                     Nas_Delay, 
                                                     Security_Delay, 
                                                     Late_Ac_Delay),
         Weather_Delay_Prop = sum(Weather_Delay)/sum(Carrier_Delay, 
                                                     Weather_Delay, 
                                                     Nas_Delay, 
                                                     Security_Delay, 
                                                     Late_Ac_Delay),
         Nas_Delay_Prop = sum(Nas_Delay)/sum(Carrier_Delay, 
                                             Weather_Delay, 
                                             Nas_Delay, 
                                             Security_Delay, 
                                             Late_Ac_Delay),
         Security_Delay_Prop = sum(Security_Delay)/sum(Carrier_Delay, 
                                                       Weather_Delay, 
                                                       Nas_Delay, 
                                                       Security_Delay, 
                                                       Late_Ac_Delay),
         Late_Ac_Delay_Prop = sum(Late_Ac_Delay)/sum(Carrier_Delay, 
                                                     Weather_Delay, 
                                                     Nas_Delay, 
                                                     Security_Delay, 
                                                     Late_Ac_Delay)) %>%
  gather("Carrier_Delay_Prop",
         "Weather_Delay_Prop",
         "Nas_Delay_Prop",
         "Security_Delay_Prop",
         "Late_Ac_Delay_Prop",
         key = "Delay_Prop_Type",
         value = "Delay_Prop") -> flights_2018_delay_props


#Choices for App Inputs

#Plot choices for airline delay analysis
plot_sel_choices_al_del <- c("Delay Distribution", "Delay Airline Distribution", "Delay Airline Mean", "t-Test Plot")

#Plot choices for overall delay analysis
plot_sel_choices_ov_del <- c("Overall Delay Distribution", "Delay Type Distribution", "Delay Type Proportions", "Delay Type Proportions Plot")

#Names of airlines, used for airline choices in airline delay t-test
test_choices_al_del <- unique(flights_2018$Carrier_Name)


#Plot choices for temporal delay analysis
plot_sel_choices_tm_del <- c("Histogram", "Scatterplot", "t-Test Plot")

#Local time planned departure hours, used for time choices in temporal delay t-test
test_choices_tm_del <- seq(0, 23, 1)


# Create Airlines List
flights_2018 %>% 
  select(Carrier_Code, Carrier_Name) %>% 
  unique() %>% 
  collect()  %>%
  split(.$Carrier_Name) %>%
  map(~.$Carrier_Code) -> airline_list3


# Create Airlines List
flights_2018 %>% 
  select(Carrier_Code, Carrier_Name) %>% 
  unique() %>% 
  collect()  %>%
  split(.$Carrier_Name) %>%
  map(~.$Carrier_Code) -> airline_list2

# Create Month Options
flights_2018 %>% 
  mutate(Month = month(Date)) %>%
  mutate(Month = as.integer(Month)) %>% 
  mutate(Name.Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  mutate(Day = mday(Date)) %>%
  mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) -> flights_2018



list(
  "All Year" = 99,
  "January" = 1,
  "February" = 2,
  "March" = 3,
  "April" = 4,
  "May" = 5,
  "June" = 6,
  "July" = 7,
  "August" = 8,
  "September" = 9,
  "October" = 10,
  "November" = 11,
  "December" = 12
) -> month.list

list(
  "January" = 1,
  "February" = 2,
  "March" = 3,
  "April" = 4,
  "May" = 5,
  "June" = 6,
  "July" = 7,
  "August" = 8,
  "September" = 9,
  "October" = 10,
  "November" = 11,
  "December" = 12
) -> month.list2

list(
  "Sunday" = 1,
  "Monday" = 2,
  "Tuesday" = 3,
  "Wednesday" = 4,
  "Thursday" = 5,
  "Friday" = 6,
  "Saturday" = 7
) -> day.list


# Origin Airport List
flights_2018 %>% 
  select(Origin) %>% 
  unique() %>% 
  collect()  %>%
  split(.$Origin) %>%
  map(~.$Origin) -> origin_airport

# Destination Airport List
flights_2018 %>% 
  select(Dest) %>% 
  unique() %>% 
  collect()  %>%
  split(.$Dest) %>%
  map(~.$Dest) -> dest_airport


ui <- fluidPage(theme = shinytheme("slate"),
                           titlePanel("Airports and Carriers Flight Performance 2018"),
                titlePanel(tags$h5("Janice Cessna, Rachel Regberg, Jorid Topi")),
                                       
  tabsetPanel(
    tabPanel("Overall Delays",
       sidebarLayout(
         sidebarPanel(
           sliderInput("delay_scope_ov_del", "Delay Scope", value = c(0, max(flights_2018$Dep_Delay)), min=0, max = max(flights_2018$Dep_Delay)),
           selectInput("plot_sel_ov_del", "Delay Plot", choices = plot_sel_choices_ov_del)
         ),
         mainPanel(
           plotOutput("plot_out_ov_del")
         )
      )
  
    ),
    tabPanel("Airline Delays",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("delay_scope_al_del", "Delay Scope", value = c(0, max(flights_2018$Dep_Delay)), min=0, max = max(flights_2018$Dep_Delay)),
                 varSelectInput("delay_type_al_del", "Delay Type", data=flights_2018[, c(10,19:23)], selected = "Carrier_Delay"),
                 selectInput("plot_sel_al_del", "Delay Plot", choices = plot_sel_choices_al_del),
                 checkboxInput("log_al_del", "Log"),
                 selectInput("test1_in_al_del", "t-Test Airline 1", choices = test_choices_al_del, selected = "Southwest Airlines Co."),
                 selectInput("test2_in_al_del", "t-Test Airline 2", choices = test_choices_al_del, selected = "SkyWest Airlines Inc.")
               ),
               mainPanel(
                 plotOutput("plot_out_al_del"),
                 tableOutput("test_out_al_del")
                 #textOutput("test_out_al_del_text")
               )
             )
             
    ),

    tabPanel("Temporal Delays",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("delay_scope_tm_del", "Delay Scope", value = c(0, max(flights_2018$Dep_Delay)), min=0, max = max(flights_2018$Dep_Delay)),
                 varSelectInput("delay_type_tm_del", "Delay Type", data=flights_2018[, c(10,19:23)], selected = "Carrier_Delay"),
                 selectInput("plot_sel_tm_del", "Delay Plot", choices = plot_sel_choices_tm_del),
                 checkboxInput("log_tm_del", "Log"),
                 selectInput("test1_in_tm_del", "t-Test Dep Hour 1", choices = test_choices_tm_del, selected = "5"),
                 selectInput("test2_in_tm_del", "t-Test Dep Hour 2", choices = test_choices_tm_del, selected = "15")
               ),
               mainPanel(
                 plotOutput("plot_out_tm_del"),
                 tableOutput("test_out_tm_del")
                 #textOutput("test_out_tm_del_text")
               )
             )
             
    ),

    tabPanel("Delay Regressions",
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Operations Effect"),
                 varSelectInput("delay_type_rg_del", "Delay Type", data = flights_2018[, c(10:12,19:23)]),
                 checkboxInput("log_rg_del1", "Log Delay"),
                 selectInput("plot_sel_rg_del", "Total Operations", choices = c("Tot_Ops.x")),
                 checkboxInput("log_rg_del2", "Log Operations"),
                 #textOutput("lm_out_rg_del_text"),
                 tags$hr(),
                 tags$h4("Regional Effect"),
                 checkboxInput("region_rg_del3", "By Region?"),
                 tags$img(src = "FAA regions.jpg", style = "width:300px;height:220px;"),
                 tags$hr(),
                 tags$h4("Regression Model Output"),
                 tableOutput("lm_out_rg_del")
               ),
               mainPanel(
                 plotOutput("plot_out_rg_del"),
                 plotOutput("plot_res_rg_del")
                 
               )
             )
             
    ),

    tabPanel("Airport Delay Table",
             dataTableOutput("delay_table")
    ),
    
    tabPanel("Visualizations",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "airline",
                   label = "Airline:",
                   choices = airline_list2,
                   selectize = FALSE),
                 selectInput(
                   inputId = "month",
                   label = "Month:",
                   choices = month.list),
                 tags$a("Source: Edgar Ruiz - GitHub", href = "https://gist.github.com/edgararuiz/89e771b5d1b82adaa0033c0928d1846d")
               ),
               
               mainPanel(
                 fluidRow(
                   valueBoxOutput("total_flights"),
                   valueBoxOutput("percent_cancelled"),
                   valueBoxOutput("percent_delayed")
                 ),
                 fluidRow(
                   column(width = 7,
                          p(textOutput("monthly")),
                          p("Click on a point in the plot to see the monthly/daily counts"),
                          highchartOutput("group_totals")),
                   column(width = 5,
                          p("Click on an airport code in the plot to see the details"),
                          highchartOutput("top_airports"))
                 )
               )
             ))
  )
)  


server <- function(input, output, session) {
  
  #Tab1 ---------------------------------  
  ov_del_plot1_df<-reactive({
    flights_2018 %>%
      filter(Dep_Delay >= input$delay_scope_ov_del[[1]] & Dep_Delay <= input$delay_scope_ov_del[[2]])
  })
  
  ov_del_plot2_df<-reactive({
    flights_2018_delay_types %>%
      filter(Delay_Type_Time >= input$delay_scope_ov_del[[1]] & Delay_Type_Time <= input$delay_scope_ov_del[[2]] & Delay_Type_Time != 0)
  })
  
  ov_del_plot3_df<-reactive({
    flights_2018_delay_types %>%
      filter(Delay_Type_Time >= input$delay_scope_ov_del[[1]] & Delay_Type_Time <= input$delay_scope_ov_del[[2]] & Delay_Type_Time != 0) %>%
      group_by(Delay_Type) %>%
      mutate(Del_Type_Sum = sum(Delay_Type_Time)) %>%
      ungroup() %>%
      mutate(Del_Type_Total = sum(Delay_Type_Time)) %>%
      mutate(Del_Type_Prop = Del_Type_Sum/Del_Type_Total) %>%
      select(c("Delay_Type", "Del_Type_Prop")) %>%
      unique()
  })
  
  ov_del_plot4_df<-reactive({
    flights_2018_delay_props %>%
      filter(Dep_Delay >= input$delay_scope_ov_del[[1]] & Dep_Delay <= input$delay_scope_ov_del[[2]])
  })

  output$plot_out_ov_del <- renderPlot({
    
    if(input$plot_sel_ov_del == "Overall Delay Distribution"){
      pl_ov_del <- ggplot(ov_del_plot1_df(), aes(x=Dep_Delay)) +
        geom_freqpoly() +
        theme_economist() +
        xlab("Departure Delay") +
        ggtitle("Overall Delay Distribution") 
        
    }
    
    if(input$plot_sel_ov_del == "Delay Type Distribution"){
      pl_ov_del <- ggplot(ov_del_plot2_df(), aes(x=Delay_Type_Time, group = Delay_Type, color = Delay_Type)) +
        geom_freqpoly() +
        theme_economist() +
        xlab("Delay Type - Time") +
        scale_color_discrete(name = "Delay Type",
                             labels = c("Carrier",
                                        "Late Aircraft",
                                        "National Aviation System",
                                        "Security",
                                        "Weather"),
                             position = "left") +
        ggtitle("Delay Type Distribution")
    }
    
    if(input$plot_sel_ov_del == "Delay Type Proportions"){
      pl_ov_del <- ggplot(ov_del_plot3_df(), aes(x=Delay_Type, y=Del_Type_Prop)) +
        geom_bar(stat = "identity") +
        theme_economist() +
        xlab("Delay Type") +
        ylab("Delay Type - Proportion") +
        scale_x_discrete(labels = c("Carrier",
                                    "Late Aircraft",
                                    "National Aviation System",
                                    "Security",
                                    "Weather")) +
        ggtitle("Delay Type Proportions")
    }
    
    if(input$plot_sel_ov_del == "Delay Type Proportions Plot"){
      pl_ov_del <- ggplot(ov_del_plot4_df(), aes(x=Dep_Delay, y=Delay_Prop, group = Delay_Prop_Type, color = Delay_Prop_Type)) + 
        geom_point() + 
        geom_smooth() +
        theme_economist() +
        xlab("Departure Delay") +
        ylab("Delay - Proportion") +
        scale_color_discrete(name = "Delay Type",
                             labels = c("Carrier",
                                        "Late Aircraft",
                                        "National Aviation System",
                                        "Security",
                                        "Weather")) +
        ggtitle("Delay Type Proportions")
    }
    
    pl_ov_del +
      theme_economist()
    
    })

  
  #Tab2 --------------------------------- 
  al_del_plot_df<-reactive({
    flights_2018 %>%
      filter(!!input$delay_type_al_del > input$delay_scope_al_del[[1]] & !!input$delay_type_al_del <= input$delay_scope_al_del[[2]])
  })
  
  al_del_test_plot_df<-reactive({
    flights_2018 %>%
      filter(!!input$delay_type_al_del > input$delay_scope_al_del[[1]] & !!input$delay_type_al_del <= input$delay_scope_al_del[[2]]) %>%
      filter(Carrier_Name == !!input$test1_in_al_del | Carrier_Name == !!input$test2_in_al_del)
  })
  
  
  output$plot_out_al_del <- renderPlot({
    
    
    if(input$plot_sel_al_del == "Delay Distribution"){
      pl_al_del <- ggplot(al_del_plot_df(), aes(x = !!input$delay_type_al_del)) +
        geom_bar() +
        theme_bw() +
        theme_economist()
      if(input$log_al_del){
        pl_al_del <- pl_al_del + scale_x_log10()
      }
    }
    
    if(input$plot_sel_al_del == "Delay Airline Distribution"){
      pl_al_del <- ggplot(al_del_plot_df(), aes(x = reorder(Carrier_Name, !!input$delay_type_al_del, FUN=median), y = !!input$delay_type_al_del)) +
        geom_boxplot(aes(group = Carrier_Name)) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
      if(input$log_al_del){
        pl_al_del <- pl_al_del + scale_y_log10()
        }
    }
    
    if(input$plot_sel_al_del == "Delay Airline Mean"){
      al_del_plot_df() %>%
        group_by(Carrier_Name) %>%
        summarise(Delay_Mean = mean(!!input$delay_type_al_del)) %>%
        mutate(Carrier_Name = fct_reorder(Carrier_Name,Delay_Mean)) %>%
        ggplot(aes(x = Carrier_Name, y = Delay_Mean)) +
        geom_point() + 
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) -> pl_al_del
      if(input$log_al_del){
        al_del_plot_df() %>%  
          mutate(Delay_Log = log(!!input$delay_type_al_del)) %>%
          group_by(Carrier_Name) %>%
          summarise(Delay_Mean_Log = mean(Delay_Log)) %>%
          mutate(Carrier_Name = fct_reorder(Carrier_Name,Delay_Mean_Log)) %>%
          ggplot(aes(x = Carrier_Name, y = Delay_Mean_Log)) +
          geom_point() + 
          theme_economist() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  -> pl_al_del
      }
      
    }
    
    if(input$plot_sel_al_del == "t-Test Plot"){
      al_del_test_plot_df() %>%
        ggplot(aes(x = Carrier_Name, y = !!input$delay_type_al_del)) +
        geom_boxplot(aes(group = Carrier_Name)) + 
        theme_economist() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) -> pl_al_del
      if(input$log_al_del){
        pl_al_del <- pl_al_del + scale_y_log10()
      }
    }
    
    pl_al_del
    
  })
  
  
  delay_airline_t_test<-reactive({ 
    if(input$log_al_del){
      flights_2018 %>%
        filter(!!input$delay_type_al_del > input$delay_scope_al_del[[1]] & !!input$delay_type_al_del <= input$delay_scope_al_del[[2]]) %>%
        filter(Carrier_Name == !!input$test1_in_al_del | Carrier_Name == !!input$test2_in_al_del) %>%
        mutate(Carrier_Delay_Log = log(!!input$delay_type_al_del)) %>%
        t.test(Carrier_Delay_Log~Carrier_Name, data = .) %>%
        tidy() %>%
        mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) -> al_del_tout
    } else{
      flights_2018 %>%
        filter(!!input$delay_type_al_del > input$delay_scope_al_del[[1]] & !!input$delay_type_al_del <= input$delay_scope_al_del[[2]]) %>%
        filter(Carrier_Name == !!input$test1_in_al_del | Carrier_Name == !!input$test2_in_al_del) %>%
        mutate(Carrier_Delay = !!input$delay_type_al_del) %>%
        t.test(Carrier_Delay~Carrier_Name, data = .) %>%
        tidy() -> al_del_tout
    }
    
    al_del_tout
    })
  
  
  output$test_out_al_del <- renderTable({
    select(delay_airline_t_test(), c(Estimate = estimate, `Estimate 1` = estimate1, `Estimate 2` = estimate2, Lower = conf.low, Upper = conf.high))
  })
  
  # output$test_out_al_del_text <- renderText({
  #   if (input$log_al_del == FALSE) {
  #     str_c("The difference in average delay time between Airline 1 and Airline 2 is ", 
  #           round(delay_airline_t_test()$estimate, digits = 2), " in minutes (95% CI of ", 
  #           round(delay_airline_t_test()$conf.low, digits = 2), " to ", 
  #           round(delay_airline_t_test()$conf.high, digits = 2), ").")
  #   } else {
  #     str_c("We estimate that the median ",
  #           input$delay_type_al_del, " on Airline 1 is ",
  #           round(delay_airline_t_test()$estimate, digits = 2), " times the median ",
  #           input$delay_type_al_del, " for Airline 2. The 95% confidence interval for ratio of median ",
  #           input$delay_type_al_del, " on Airline 1 to median ",
  #           input$delay_type_al_del, " for Airline 2 is ",
  #           round(delay_airline_t_test()$conf.low, digits = 2), " to ", 
  #           round(delay_airline_t_test()$conf.high, digits = 2), ".")    
  #   }
  # })
  
  
  #Tab3 --------------------------------- 
  tm_del_plot_df<-reactive({
    if(input$log_tm_del){
      flights_2018 %>%
        filter(!!input$delay_type_tm_del > input$delay_scope_tm_del[[1]] & !!input$delay_type_tm_del <= input$delay_scope_tm_del[[2]]) %>%
        mutate(Delay_log = log(!!input$delay_type_tm_del)) %>%
        group_by(Dep_Time_Planned_Local_Hour) %>%
        summarize(Hour_Delay_Mean = mean(Delay_log)) 
    }else{
      flights_2018 %>%
        filter(!!input$delay_type_tm_del > input$delay_scope_tm_del[[1]] & !!input$delay_type_tm_del <= input$delay_scope_tm_del[[2]]) %>%
        group_by(Dep_Time_Planned_Local_Hour) %>%
        summarize(Hour_Delay_Mean = mean(!!input$delay_type_tm_del))
    }
    
  })
  
  
    
  tm_del_test_plot_df<-reactive({
    flights_2018 %>%
      filter(!!input$delay_type_tm_del > input$delay_scope_tm_del[[1]] & !!input$delay_type_tm_del <= input$delay_scope_tm_del[[2]]) %>%
      filter(Dep_Time_Planned_Local_Hour == !!input$test1_in_tm_del | Dep_Time_Planned_Local_Hour == !!input$test2_in_tm_del)
  })
  
  
  output$plot_out_tm_del <- renderPlot({
    
    
    if(input$plot_sel_tm_del == "Histogram"){
      pl_tm_del <- tm_del_plot_df() %>%
          ggplot(aes(x = Dep_Time_Planned_Local_Hour, y = Hour_Delay_Mean)) +
          geom_bar(stat = "identity") +
          theme_economist()
    }
    
    if(input$plot_sel_tm_del == "Scatterplot"){
      pl_tm_del <- flights_2018 %>%
          filter(!!input$delay_type_tm_del > input$delay_scope_tm_del[[1]] & !!input$delay_type_tm_del <= input$delay_scope_tm_del[[2]]) %>%
          group_by(Dep_Time_Planned_Local_Hour) %>%
          summarise(Delay_Mean = mean(!!input$delay_type_tm_del)) %>%
          mutate(Dep_Time_Planned_Local_Hour = as.character(Dep_Time_Planned_Local_Hour)) %>%
          mutate(Dep_Time_Planned_Local_Hour = fct_reorder(Dep_Time_Planned_Local_Hour,Delay_Mean)) %>%
          ggplot(aes(x = Dep_Time_Planned_Local_Hour, y = Delay_Mean)) +
          geom_point() +
          theme_economist()
      if(input$log_tm_del){
        pl_tm_del <- flights_2018 %>%
          filter(!!input$delay_type_tm_del > input$delay_scope_tm_del[[1]] & !!input$delay_type_tm_del <= input$delay_scope_tm_del[[2]]) %>%
          mutate(Delay_Log = log(!!input$delay_type_tm_del)) %>%
          group_by(Dep_Time_Planned_Local_Hour) %>%
          summarise(Delay_Mean_Log = mean(Delay_Log)) %>%
          mutate(Dep_Time_Planned_Local_Hour = as.character(Dep_Time_Planned_Local_Hour)) %>%
          mutate(Dep_Time_Planned_Local_Hour = fct_reorder(Dep_Time_Planned_Local_Hour,Delay_Mean_Log)) %>%
          ggplot(aes(x = Dep_Time_Planned_Local_Hour, y = Delay_Mean_Log)) +
          geom_point() +
          theme_economist()
      }
    }
    
    if(input$plot_sel_tm_del == "t-Test Plot"){
      tm_del_test_plot_df() %>%
        ggplot(aes(x = Dep_Time_Planned_Local_Hour, y = !!input$delay_type_tm_del)) +
        geom_boxplot(aes(group = Dep_Time_Planned_Local_Hour)) +
        theme_economist() -> pl_tm_del
      if(input$log_tm_del){
        pl_tm_del <- pl_tm_del + scale_y_log10()
      }
    }
    
    pl_tm_del
  })
  
  
  tm_t_test<-reactive({ 
    if(input$log_tm_del){
      flights_2018 %>%
        filter(!!input$delay_type_tm_del > input$delay_scope_tm_del[[1]] & !!input$delay_type_tm_del <= input$delay_scope_tm_del[[2]]) %>%
        filter(Dep_Time_Planned_Local_Hour == !!input$test1_in_tm_del | Dep_Time_Planned_Local_Hour == !!input$test2_in_tm_del) %>%
        mutate(Delay_Log = log(!!input$delay_type_tm_del)) %>%
        t.test(Delay_Log~Dep_Time_Planned_Local_Hour, data = .) %>%
        tidy() %>%
        mutate(estimate = exp(estimate), conf.low = exp(conf.low), conf.high = exp(conf.high)) -> tm_del_tout
    } else{
      flights_2018 %>%
        filter(!!input$delay_type_tm_del > input$delay_scope_tm_del[[1]] & !!input$delay_type_tm_del <= input$delay_scope_tm_del[[2]]) %>%
        filter(Dep_Time_Planned_Local_Hour == !!input$test1_in_tm_del | Dep_Time_Planned_Local_Hour == !!input$test2_in_tm_del) %>%
        mutate(Delay = !!input$delay_type_tm_del) %>%
        t.test(Delay~Dep_Time_Planned_Local_Hour, data = .) %>%
        tidy() -> tm_del_tout
    }
    
    tm_del_tout
    
  })
  
  
  output$test_out_tm_del <- renderTable({
    select(tm_t_test(), c(Estimate = estimate, `Estimate 1` = estimate1, `Estimate 2` = estimate2,Lower = conf.low, Upper = conf.high))
  })
  
  output$test_out_al_del <- renderTable({
    select(delay_airline_t_test(), c(Estimate = estimate, `Estimate 1` = estimate1, `Estimate 2` = estimate2, Lower = conf.low, Upper = conf.high))
  })
  
  # output$test_out_tm_del_text <- renderText({
  #   if (input$log_tm_del == FALSE) {
  #     str_c("We estimate that Hour 1 tends to have about ", 
  #           round(delay_airline_t_test()$estimate, digits = 2), " more ", 
  #           input$delay_type_tm_del, " than Hour 2 (95% of ", 
  #           round(delay_airline_t_test()$conf.low, digits = 2), " to ",
  #           round(delay_airline_t_test()$conf.high, digits = 2), ").")
  #   } else {
  #     str_c("We estimate that the median ",
  #           input$delay_type_tm_del, " on Hour 1 is ",
  #           round(delay_airline_t_test()$estimate, digits = 2), " times the median ",
  #           input$delay_type_tm_del, " for Hour 2. The 95% confidence interval for ratio of median ",
  #           input$delay_type_tm_del, " on Hour 1 to median ",
  #           input$delay_type_tm_del, " for Hour 2 is ",
  #           round(delay_airline_t_test()$conf.low, digits = 2), " to ", 
  #           round(delay_airline_t_test()$conf.high, digits = 2), ".")   
  #   }
  # })  
  
  #Tab4 --------------------------------- 
   
  #generate plot output from two input variables + log checkboxes
  plot_rg_del_reactive_df <- reactive({
    flights_2018 %>%
      select(Origin, Origin_City, Origin_State, !!input$delay_type_rg_del, Tot_Ops.x, Region) %>%
      filter(!!input$delay_type_rg_del > 0) %>%  
      group_by(Origin) %>% 
      mutate(Mean = mean(!!input$delay_type_rg_del)) %>%
      mutate(Duplicate_Origin = duplicated(Origin)) %>%
      filter(Duplicate_Origin == "FALSE")
    })
  
  plot_rg_del_reactive_region <- reactive({
    if (input$region_rg_del3 == FALSE){ 
      pl <- plot_rg_del_reactive_df() %>%
        ggplot(aes(x = Tot_Ops.x, y = Mean)) +
        geom_point(na.rm = TRUE) +
        geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
        xlab("Total Airport Operations") +
        ylab("Mean Delay") +
        theme_economist()
    } else {
      pl <- plot_rg_del_reactive_df() %>%
        ggplot(aes(x = Tot_Ops.x, y = Mean, group = Region, color = Region)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
        xlab("Total Airport Operations") +
        ylab("Mean Departure Delay") +
        labs(title = "Delay trend by operations") +
        theme_economist()
      }
        
    if(input$log_rg_del1 == TRUE) {
      pl <- pl + scale_y_log10()
    } 
    if(input$log_rg_del2 == TRUE) {
      pl <- pl + scale_x_log10()
    }
    pl
  })

  output$plot_out_rg_del <- renderPlot({
    plot_rg_del_reactive_region()  
  })
  
  
  #calculate lm and generate output for bottom of sidebar
  rg_del_lmout <- reactive({ 
    if(input$log_rg_del1 == TRUE & input$log_rg_del2 == TRUE) {
      lm_delay <- lm(log2(Mean) ~ log2(Tot_Ops.x), data = plot_rg_del_reactive_df())
      tidy(lm_delay)
    } else if (input$log_rg_del1 == TRUE & input$log_rg_del2 == FALSE) {
      lm_delay <- lm(log2(Mean) ~ Tot_Ops.x, data = plot_rg_del_reactive_df())
      tidy(lm_delay)      
    } else if (input$log_rg_del1 == FALSE & input$log_rg_del2 == TRUE) {
      lm_delay <- lm(Mean ~ log2(Tot_Ops.x), data = plot_rg_del_reactive_df())
      tidy(lm_delay)
    } else {
      lm_delay <- lm(Mean ~ Tot_Ops.x, data = plot_rg_del_reactive_df())
      tidy(lm_delay)
    }
  })
  output$lm_out_rg_del <- renderTable({
    if (input$region_rg_del3 == FALSE) {
      rg_del_lmout()
    } else {
      print("Not available for regional comparison")
    }
  })
  
  
  #calculate lm residuals, calculate regional estimates, and generate lower plot (residuals or region estimates)
  rg_del_resout <- reactive({
    if(input$log_rg_del1 == TRUE & input$log_rg_del2 == TRUE) {
      lm_delay <- lm(log2(Mean) ~ log2(Tot_Ops.x), data = plot_rg_del_reactive_df())
      augment(lm_delay)
    } else if (input$log_rg_del1 == TRUE & input$log_rg_del2 == FALSE) {
      lm_delay <- lm(log2(Mean) ~ Tot_Ops.x, data = plot_rg_del_reactive_df())
      augment(lm_delay)      
    } else if (input$log_rg_del1 == FALSE & input$log_rg_del2 == TRUE) {
      lm_delay <- lm(Mean ~ log2(Tot_Ops.x), data = plot_rg_del_reactive_df())
      augment(lm_delay)
    } else {
      lm_delay <- lm(Mean ~ Tot_Ops.x, data = plot_rg_del_reactive_df())
      augment(lm_delay)
    }    
  })

  rg_del_regout <- reactive ({
    flights_nested <- plot_rg_del_reactive_df() %>%
      group_by(Region) %>%
      nest() %>%
      mutate(lmout = map(data, ~lm(Mean ~ Tot_Ops.x, data = .))) %>%
      mutate(tidyout = map(lmout, ~tidy(., conf.int = TRUE))) 
    flights_nested %>%
      unnest(tidyout) %>%
      filter(term == "Tot_Ops.x") %>%
      select(c("Region", "estimate", "p.value", "conf.low", "conf.high"))
  })
    
  output$plot_res_rg_del <- renderPlot({
    if (input$region_rg_del3 == FALSE) {
      res <- rg_del_resout() %>%
        ggplot(aes(x = .fitted, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, color = "red", linetype = 2) +
        theme_economist()
    } else {
      res <- rg_del_regout() %>%
        ggplot(aes(x = fct_reorder(Region, estimate), y = estimate)) +
        geom_point() +
        theme_economist() +
        geom_hline(yintercept = 0, color = "red", linetype = 2) +
        geom_segment(aes(x = Region, xend = Region, y = conf.low, yend = conf.high), 
                     color = "black", alpha = 0.3) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
        theme(legend.position = "none") 
    }
    res
  })
  
  #Tab5 ---------------------------------   
  output$delay_table <- renderDataTable({
    plot_rg_del_reactive_df() %>%
      arrange(desc(Mean))
  })
  
  #Tab 6 --------------------------------
  
  # output$monthly <- renderText({
  #   if(input$month == "99")"Click on a month in the plot to see the daily counts"
  # })
  
  output$total_flights <- renderValueBox({
    # The following code runs inside the database
    result <- flights_2018 %>%
      filter(Carrier_Code == input$airline)
    
    if(input$month != 99) result <- filter(result, Month == input$month)
    
    result <- result %>%
      tally() %>%
      pull() %>% 
      as.integer()
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "Number of Flights",
             icon = icon("plane"))
  })
  
  
  output$percent_cancelled <- renderValueBox({
    result <- flights_2018 %>% 
      filter(Carrier_Code == input$airline)
    
    if(input$month != 99) result <- filter(result, Month == input$month)
    result   <- result %>% 
      mutate(Cancelled = ifelse(Cancelled=="No", 0, 1)) %>% 
      summarise(cancels = sum(Cancelled),
                total = n()) %>%
      mutate(percent = cancels / total) %>%
      pull()
    
    valueBox(paste0(round(result * 100), "%"),
             subtitle = "Flights cancelled",
             icon = icon("bell-slash"))
  })
  
  
  output$percent_delayed <- renderValueBox({
    
    # The following code runs inside the database
    result <- flights_2018 %>%
      filter(Carrier_Code == input$airline)
    
    if(input$month != 99) result <- filter(result, Month == input$month)
    result <- result %>%
      filter(!is.na(Dep_Delay)) %>%
      mutate(delayed = ifelse(Dep_Delay > 0, 1, 0)) %>%
      summarise(delays = sum(delayed),
                total = n()) %>%
      mutate(percent = delays / total) %>%
      pull()
    
    valueBox(paste0(round(result * 100), "%"),
             subtitle = "Flights delayed",
             icon = icon("bell"))
  })
  
  # Events in Highcharts can be tracked using a JavaScript. For data points in a plot, the 
  # event.point.category returns the value that is used for an additional filter, in this case
  # the month that was clicked on.  A paired observeEvent() command is activated when
  # this java script is executed
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  output$group_totals <- renderHighchart({
    
    if(input$month != 99) {
      result <- flights_2018 %>%
        filter(Month == input$month,
               Carrier_Code == input$airline) %>%
        group_by(Day) %>%
        tally() %>%
        collect()
      group_name <- "Daily"
    } else {
      result <- flights_2018 %>%
        filter(Carrier_Code == input$airline) %>%
        group_by(Month) %>%
        tally() %>%
        collect()    
      group_name <- "Monthly"
    } 
    
    highchart() %>%
      hc_add_series(
        data = result$n, 
        type = "line",
        name = paste(group_name, "total flights"),
        events = list(click = js_click_line)) 
    
  })
  
  
  
  # Tracks the JavaScript event created by `js_click_line`
  observeEvent(input$line_clicked != "",
               if(input$month == 99)
                 updateSelectInput(session, "month", selected = input$line_clicked),
               ignoreInit = TRUE)
  
  js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
  
  output$top_airports <- renderHighchart({
    # The following code runs inside the database
    result <- flights_2018 %>%
      filter(Carrier_Code == input$airline) 
    
    if(input$month != 99) result <- filter(result, Month == input$month) 
    
    result <- result %>%
      group_by(Dest) %>%
      tally() %>%
      arrange(desc(n)) %>%
      collect() %>%
      head(10)
    
    highchart() %>%
      hc_add_series(
        data = result$n, 
        type = "bar",
        name = paste("No. of Flights"),
        events = list(click = js_bar_clicked)) %>%
      hc_xAxis(
        categories = result$Dest,
        tickmarkPlacement="on")
  })
  
}

shinyApp(ui, server)
