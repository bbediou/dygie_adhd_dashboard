library(shiny)
library(shinydashboard)
# library(Hmisc) - dropped: only the %nin% operator was used.
# It is defined below, after rm(list=ls()), which would otherwise erase it.
library(dplyr)
library(tidyverse)
# library(DT)
# library(data.table)
library(ggplot2)
library(GGally)
library(ggsci)
library(ggExtra)
library(ggpubr)
library(wesanderson)
library(RColorBrewer)
library(plotly)
# library(plotly)
library(shinyWidgets)
library(cowplot)
library(shinycssloaders)
library(lme4)

# library(MASS)

# clear workspace
rm(list=ls())

# `%nin%` was previously provided by Hmisc. Defined here, AFTER rm(), so it
# survives the workspace clear above.
`%nin%` <- function(x, table) !(x %in% table)

#--- files ---------------------------------------------------------------------
# files <- dir(pattern=".csv")

#--- color palettes ------------------------------------------------------------
myPalette_age <- colorRampPalette(brewer.pal(9, "OrRd"))
myPalette_ADHD <- colorRampPalette(brewer.pal(9, "GnBu"))
pjd <- position_jitterdodge(jitter.width = .1, dodge.width = .8, seed = 123)
pd <- position_dodge(width = .8)

#--- prepare data ------------------------------------------------------------
# data <-  read_csv("_Mini-jeu ile - Dataset_07-06-23.csv") %>%
data <-  read_csv("_Mini-jeu ile - Dataset_06-30-23.csv") %>%
  filter(!is.na(Age)) %>%
  mutate(age = Age) %>%
# VERSION 06-30-23.csv
  mutate(age_num = age) %>%
  mutate(ADHD_score = ADHDScore + 1) %>%
# VERSION 07-06-23.csv
  # mutate(age = sub(x=age,pattern="Moins de 6 ans", replacement="5 or less"))%>%
  # mutate(age = sub(x=age,pattern="Plus de 12 ans", replacement="13 or more"))%>%
  # mutate(age = sub(x=age,pattern="ans", replacement="years old")) %>%
  # mutate(age = factor(age, levels = c("5 or less",
  #                                   "6 years old",
  #                                   "7 years old",
  #                                   "8 years old",
  #                                   "9 years old",
  #                                   "10 years old",
  #                                   "11 years old",
  #                                   "12 years old",
  #                                   "13 or more"),
  #                   ordered = TRUE),
  #      age_num = as.numeric(gsub("\\D", "", age))) %>%
  # mutate(ADHD_score = `ADHD score` + 1) %>%
  
  mutate(ADHD_group = ifelse(ADHD_score < 4.5, "ADHD", "CTL")) %>%
  mutate(ADHD_group = factor(ADHD_group, levels =c("ADHD", "CTL"), ordered = TRUE)) %>%
  mutate(rotation = CumulativeAngle / TimeOfTheLastPosition) %>%
  mutate(date = strptime(Date, format="%m/%d/%Y %H:%M:%S")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(UID,Date) %>%
  group_by(UID) %>%
  dplyr::mutate(game_id = row_number()) %>%
  ungroup() %>%
  filter(!is.na(UID), !is.na(ADHD_group)) 
data

# table(data$age, data$age_num)
# data <- loadData()

# long (df) format
df <- data %>% 
  # filter(UID =="0brTUHAhlRXI76f8I0rwbV8nFbo1") %>%
  select(UID, age, age_num, Date, date, game_id, ADHD_score, ADHD_group, PlayerPosX0:FoundObjectivesT59) %>%
  pivot_longer(
    cols = !c(ADHD_score, ADHD_group, UID, age, age_num, Date, date, game_id),
    names_to = c("measure", "timepoint"),
    names_pattern = "([A-Za-z]+)(\\d+)",
    values_to = "value"
  ) %>% 
  mutate(timepoint = as.numeric(timepoint))
# median(dt_m$n_found)

# time series
dt <- df %>% pivot_wider(
  id_cols = c("ADHD_score", "ADHD_group", "UID", "age", "age_num", "Date", "date", "game_id", "timepoint"),
  names_from = "measure",
  values_from = "value"
)

# timeseries butterflies found
dt_filt <- dt %>%
  # na.omit() %>%
  filter(FoundObjectivesT != 9999) %>%
  arrange(UID, Date, FoundObjectivesT) %>%
  dplyr::group_by(ADHD_score, ADHD_group, UID, age, Date, date, game_id) %>%
  dplyr::mutate(time_diff = FoundObjectivesT - lag(FoundObjectivesT)) %>%
  dplyr::mutate(time_diff = ifelse(is.na(time_diff), FoundObjectivesT, time_diff))  

# means by subject
dt_m <- dt_filt %>%
  group_by(ADHD_group, ADHD_score, UID, age, age_num, Date, date, game_id) %>%
  dplyr::summarise(n_found=n(),
                   meanTime = mean(time_diff))  
# hist(dt_m$meanTime)

# add variables
dt_m <- data %>% 
  filter(!is.na(Age)) %>%
  select(UID, age, age_num, Date, date, game_id, ADHD_score, ADHD_group, 
         BoxCountings, CumulativeAngle, CumulativeDotProduct, ConvexArea) %>%
  right_join(dt_m)

#--- DATA CHECKS ---------------------------------------------------------------
# dt_m %>% group_by(Date) %>% count() %>% nrow()
# tmp %>% group_by(Date) %>% count() %>% nrow()
# toCheck <- tmp %>% filter(Date %nin% dt_m$Date)
# toCheck2 <- dt_m %>% filter(UID %in% toCheck$UID)
# toCheck3 <- data %>% filter(UID %in% toCheck$UID)
# toCheck4 <- dt %>% filter(Date %in% toCheck$Date)
#-------------------------------------------------------------------------------

UIDs <- unique(df$UID)

# group_data <- dt_m %>%
#   group_by(group, age) %>%
#   summarise(n_subj = length(unique(UID)),
#             n_sessions = length(unique(date)),
#             y = mean(n_found, na.rm = TRUE)) 
#--- filters -------------------------------------------------------------------
age_range <- range(data$age_num, na.rm = TRUE)
age_levels <- levels(data$age)
ADHD_scores <- sort(unique(data$ADHD_score, na.rm = TRUE))
ADHD_range <- range(data$ADHD_score, na.rm = TRUE)
ADHD_median <- median(data$ADHD_score, na.rm = TRUE)

num_variables <- dt_m %>%
  ungroup() %>%
  dplyr::select(where(is.numeric)) %>%
  names()

################################################################################
# TEST SECTION 
# input <- list(age_select = range(data$age_num, na.rm = TRUE),
#               ADHD_threshold = median(data$ADHD_score, na.rm = TRUE),
#               Xvar = "meanTime",
#               Yvar = "n_found")
# subdata <- data %>%
#   mutate(ADHD_group = ifelse(ADHD_score <= input$ADHD_threshold, "ADHD", "CTL")) %>%
#   filter(age_num >= input$age_select[1], age_num <= input$age_select[2])
# subdata_count <-  subdata %>%
#   group_by(ADHD_score) %>%
#   dplyr::count()
# median(subdata$ADHD_score)
################################################################################

#--- BUILD DASHBOARD INTERFACE -------------------------------------------------
header <- dashboardHeader(title = "Dygie ADHD Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Raw Data Set", tabName = "dataset", icon = icon("dashboard")),
    menuItem("Players & Correlations", tabName = "players", icon = icon("user")),
    menuItem("Data Exploration", tabName = "trajectories",icon = icon("signal")),
    hr(),# add horizontal line
    menuItem("Group data", tabName = "groupdata",icon = icon("road")),
    menuItem("Exploratory stats", tabName = "exploratory_stats",icon = icon("question")),
    # menuSubItem("Filtered dataset", tabName = "filtered_data",selected=FALSE),
    # menuSubItem("Trajectories", tabName = "trajectories"),
    hr(),# add horizontal line
    menuItem("Filter", tabName = "filter", icon = icon("filter")),
    # fileInput("dataset_select",
    #           label=HTML("<font size=3>Select File</font>"),
    #           multiple=FALSE,
    #           accept=c("text/csv",
    #                    "text/comma-separated-values,text/plain",
    #                    ".csv")),
    sliderInput("age_select",
                "Age range:",
                min = min(data$age_num, na.rm = TRUE)-1,
                max = max(data$age_num, na.rm = TRUE)+1,
                value = age_range),
    sliderInput("ADHD_threshold",
                "ADHD range:",
                min = min(data$ADHD_score, na.rm = TRUE),
                max = max(data$ADHD_score, na.rm = TRUE),
                value = ADHD_median)
  )
)

body <- dashboardBody(
  
  tabItems(
    
    # Distributions of age and ADHD scores
    tabItem("dataset",
      fluidRow(
         box(
           width = 6, solidHeader = TRUE, status = "info",
           title = "Ages", 
           plotOutput("ages_distrib_filtered") %>% withSpinner(color="#0dc5c1")
           ),
        box(
          width = 6, solidHeader = TRUE, status = "info",
          title = "ADHD scores", 
          plotOutput("ADHD_distrib_filtered") %>% withSpinner(color="#0dc5c1")
        ),
         box(
           width = 12, solidHeader = TRUE, status = "info",
           title = "Number of Games played", 
           plotOutput("games_distrib_filtered") %>% withSpinner(color="#0dc5c1")
         )
      )#fluidrow
    ),#tabItem
  
    # Player characteristics
    tabItem("players",
      fluidRow(
        box(
          width = 12, solidHeader = TRUE, status = "info",
          title = "Players",
          tableOutput("players_table") %>% withSpinner(color="#0dc5c1")
        ),
        box(
          width = 12, solidHeader = TRUE, status = "info",
          title = "Correlations between variables",
          plotOutput("correlation_plot") %>% withSpinner(color="#0dc5c1")
        )
      )
    ),
            
    # Trajectories
    tabItem("trajectories",
      fluidRow(
        box(
          width = 12, solidHeader = TRUE, status = "info",
          title = "Butterflies found - All players",
          plotlyOutput("n_found_plot") %>% withSpinner(color="#0dc5c1")     
        )
      ),
      fluidRow(
        box(
          width = 12, solidHeader = TRUE, status = "info",
          title = "Trajectories - Individual players",
    
          pickerInput(
            inputId = "select_UID",
            label = "Select/deselect participant(s) to show",
            choices = UIDs,
            selected = UIDs[1],
            options = list(
              `actions-box` = TRUE,
              size = 12,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          ),# picker
          hr(),
          plotOutput("trajectory_plot") %>% withSpinner(color="#0dc5c1"),
          br(),
          plotOutput("timeseries_plot") %>% withSpinner(color="#0dc5c1")
        )#box
      )#fluidRow
    ),#tabitem
    
    # Stats
    tabItem("groupdata",
      fluidRow(
        box(
          width = 12, solidHeader = TRUE, status = "info",
          title = "Sample analysed",    
            tableOutput("players_table_filtered") %>% withSpinner(color="#0dc5c1")
        )
      ),
        fluidRow(
          box(
            width = 12, solidHeader = TRUE, status = "info",
            title = "Group comparisons",
            plotOutput("nfound_group") %>% withSpinner(color="#0dc5c1"),  
            verbatimTextOutput("anova_nfound") %>% withSpinner(color="#0dc5c1"),  
            plotOutput("meantime_group") %>% withSpinner(color="#0dc5c1"),
            verbatimTextOutput("anova_meantime") %>% withSpinner(color="#0dc5c1")
          )#box
        ),#fluidrow
        fluidRow(
          box(
            width = 12, solidHeader = TRUE, status = "info",
            title = "Correlations",
            plotOutput("correlations_filtered") %>% withSpinner(color="#0dc5c1")
          )#box
        )#fluidRow    
    ),#tabItem

  tabItem("exploratory_stats",
      fluidRow(
        box(
          width = 6, solidHeader = TRUE, status = "info",
          title = "Select variables to analyse",

          pickerInput(
            inputId = "Xvar",
            label = "Choose X variable",
            choices = num_variables,
            selected = num_variables[5],
            options = list(
              `actions-box` = TRUE,
              size = 6,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          ),# picker
          hr(),
          pickerInput(
            inputId = "Yvar",
            label = "Choose Y variable",
            choices = num_variables,
            selected = num_variables[6],
            options = list(
              `actions-box` = TRUE,
              size = 6,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          ),# picker
          hr(),
        ),# box
        box(
          width = 6, solidHeader = TRUE, status = "info",
          title = "correlation between variables",
          plotOutput("custom_scatter") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(
            width = 6, solidHeader = TRUE, status = "info",
            title = "Group effect - Xvar",
          plotOutput("Xvar_group_plot") %>% withSpinner(color="#0dc5c1"),
        ),
        box(
          width = 6, solidHeader = TRUE, status = "info",
          title = "Stats - Xvar",
          verbatimTextOutput("anova_Xvar") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(
          width = 6, solidHeader = TRUE, status = "info",
          title = "Group effect - Yvar",
          plotOutput("Yvar_group_plot") %>% withSpinner(color="#0dc5c1"),
        ),
        box(
          width = 6, solidHeader = TRUE, status = "info",
          title = "Stats - Yvar",
          verbatimTextOutput("anova_Yvar") %>% withSpinner(color="#0dc5c1")
        )#box
      )#fluidRow
    )#tabitem
  )#tabItems
)#body





#--- User Interface ------------------------------------------------------------
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "red"
)


# input <- list(ADHD_threshold=ADHD_median,
#                     age_select=c(5,13))

server <- function(input, output) {
 
  # Filter data
  subset <- reactive({
    data %>%
      # filter(game_id<3) %>%
      # filter(TimeOfTheLastPosition == 179) %>%
      mutate(ADHD_group = ifelse(ADHD_score <= input$ADHD_threshold, "ADHD", "CTL")) %>%
      filter(age_num >= input$age_select[1], age_num <= input$age_select[2])
  })
  
  # Stats per subject  
  stats <- reactive({
    dt_stats <- dt_m %>%
      ungroup() %>%
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) 
    dt_stats <- dt_stats %>% 
      mutate(Xvar = dt_stats[[input$Xvar]],
             Yvar = dt_stats[[input$Yvar]]) 
    dt_stats
  })
  
  output$players_table <- renderTable({
    data %>%
      group_by(age) %>%
      dplyr::summarize(n_players = length(unique(UID)),
                n_games = n(),
                mean_ADHD = mean(ADHD_score, na.rm = TRUE),
                median_ADHD = median(ADHD_score, na.rm = TRUE)) 
  })
  
  output$players_table_filtered <- renderTable({
    subset() %>%
      group_by(age, ADHD_group) %>%
      dplyr::summarize(n_players = length(unique(UID)),
                n_games = n(),
                mean_ADHD = mean(ADHD_score, na.rm = TRUE),
                median_ADHD = median(ADHD_score, na.rm = TRUE)) 
  })
  
  # output$ages_distrib_raw <- renderPlot({
  #   data %>% 
  #     # mutate(ADHD_group = if_else(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
  #     # filter(age_num > input$age_select[1] & age_num < input$age_select[2]) %>%
  #     group_by(UID) %>%
  #     dplyr::summarize(nGames = n(),
  #               age = unique(age)) %>%
  #     group_by(age) %>%
  #     dplyr::count() %>%
  #     ggplot(aes(x=age, y = n, fill = age))+
  #     geom_bar(stat = "identity")+
  #     geom_text(aes(y = (n+2), label = n))+
  #     labs(x = "player age", y = "number of players")+
  #     scale_fill_manual(values = myPalette_age(15)[4:15], na.value = "gray15")+
  #     # scale_fill_gradientn(colours = myPalette_age(100), limits = c(4,15))+
  #     # scale_color_manual(values= wes_palette("Zissou1", n = length(unique(data$age))))+
  #     # scale_x_continuous(breaks = 1:max(data$age_num))+
  #     guides(fill = "none")+
  #     theme_classic()
  # })
  output$ages_distrib_filtered <- renderPlot({
    subset() %>% 
      # filter(TimeOfTheLastPosition == 179) %>%
      # mutate(ADHD_group = if_else(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      # dplyr::filter(age_num > input$age_select[1] & age_num < input$age_select[2]) %>%
      group_by(UID, ADHD_group) %>%
      dplyr::summarize(nGames = n(),
                age = unique(age)) %>%
      group_by(age, ADHD_group) %>%
      dplyr::count() %>%
      ggplot(aes(x=age, y = n, fill = ADHD_group))+
      geom_bar(stat = "identity", position = "stack")+
      geom_text(aes(y = (n), label = n), position = position_stack(vjust = .5))+
      scale_fill_manual(values = c("#7BF3AD","#7BC8F3", "gray"))+
      # scale_fill_gradientn(colours = myPalette_age(100), limits = c(4,15))+
      labs(x = "player age", y = "number of players")+
      # scale_x_continuous(breaks = 1:max(data$age_num))+
      # guides(fill = "none")+
      theme_classic()
  })
  
  # output$ADHD_distrib_raw <- renderPlot({
  #   p <- data %>% 
  #     # mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
  #     # filter(age > input$age_select[1] & age < input$age_select[2]) %>%
  #     # group_by(UID) %>%
  #     # dplyr::summarize(nGames = n(),
  #     #           ADHD_score = unique(ADHD_score)) %>%
  #     group_by(ADHD_score) %>%
  #     dplyr::count() %>%
  #     ggplot(aes(x = ADHD_score, y = n, fill = ADHD_score))+
  #     geom_point()+
  #     geom_bar(stat = "identity")+
  #     geom_vline(xintercept = input$ADHD_threshold, col = "red")+
  #     geom_text(aes(x = 6, y = 15, group = 1,
  #                   label = paste0("median = ",median(data$ADHD_score))))+
  #     geom_text(aes(y = (n/2), label = n))+
  #     scale_fill_gradientn(colours = myPalette_ADHD(100))+
  #     labs(x = "ADHD score", y = "number of players")+
  #     # scale_x_continuous(breaks = 0:(max(data$ADHD_score, na.rm = TRUE)+1))+
  #     guides(fill = "none")+
  #     theme_classic()
  #   
  #   ggMarginal(p,  type="boxplot", margins = "x")
  # })
  output$ADHD_distrib_filtered <- renderPlot({
    subdata <- subset() 
    subdata_count <- subdata %>%
      group_by(ADHD_score) %>%
      dplyr::count()
    ADHD_median = median(subdata$ADHD_score, na.rm = TRUE) 
    p <- subdata_count %>%
      ggplot(aes(x = ADHD_score, y = n, group = ADHD_score, fill = ADHD_score))+
      # geom_histogram(col = "white", stat = "count", binwidth = 1)+
      geom_point()+
      geom_bar(stat = "identity")+
      geom_vline(xintercept = ADHD_median, col = "blue")+
      geom_vline(xintercept = input$ADHD_threshold, col = "red")+
      geom_text(aes(x = 1, y = 25, group = 1,
                    label = paste0("median = ", ADHD_median)))+
      geom_text(aes(y = (n/2), label = n))+
      scale_fill_gradientn(colours = myPalette_ADHD(100))+
      labs(x = "ADHD score", y = "number of players")+
      scale_x_continuous(breaks = 1:10)+
      # scale_x_continuous(breaks = 0:(max(data$ADHD_score, na.rm = TRUE)+1))+
      guides(fill = "none")+
      theme_classic()+
      theme(axis.line.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x =  element_blank())
    # p
    b <- subdata %>%
      ggplot(aes(x="", y = ADHD_score))+
      geom_boxplot()+
      geom_point(aes(col = ADHD_score),
                 size = 3, position = position_jitter(width = .1), alpha = .6)+
      scale_color_gradientn(colours = myPalette_ADHD(100))+
      scale_y_continuous(breaks = 1:10)+
      theme_classic()+
      labs(x="")+
      guides(color = "none")+
      coord_flip()
    # b
    cowplot::plot_grid(p, b, nrow = 2, align = "v", rel_heights = c(.8,.2))
    # ggMarginal(p,  type="boxplot", margins = "x")
    
  })
  
  # output$games_distrib_raw <- renderPlot({
  #   data  %>% 
  #     # filter(TimeOfTheLastPosition == 179) %>%
  #     group_by(UID) %>%
  #     dplyr::summarize(nGames = n(),
  #               ADHD_score = unique(ADHD_score))  %>%
  #     ggplot(aes(x=nGames)) +
  #     geom_histogram(binwidth = 1, col = "white")+
  #     labs(x = "number of games played", y = "number of players")+
  #     scale_x_continuous(breaks = 1:max(subjData$nGames))+
  #     theme_classic()
  # })
  output$games_distrib_filtered <- renderPlot({
    subjData <- subset() %>% 
      # filter(age > input$age_select[1] & age < input$age_select[2]) %>%
      # dplyr::select(UID, age, age_num, Date, ADHD_score, PlayerPosX0:FoundObjectivesT59) %>%
      group_by(UID) %>%
      dplyr::summarize(nGames = n(),
                       ADHD_score = unique(ADHD_score))  
    subjData %>%
      ggplot(aes(x=nGames)) +
      geom_histogram(binwidth = 1, col = "white")+
      labs(x = "number of games played", y = "number of players")+
      scale_x_continuous(breaks = 1:max(subjData$nGames))+
      theme_classic()
  })
  
  output$trajectory_plot <- renderPlot({
    # dt <- data %>%
    #   # filter(UID =="0brTUHAhlRXI76f8I0rwbV8nFbo1") %>%
    #   dplyr::select(UID, age, age_num, Date, ADHD_score, PlayerPosX0:FoundObjectivesT59) %>%
    #   pivot_longer(
    #     cols = !c(ADHD_score, UID, age, age_num, Date),
    #     names_to = c("measure", "timepoint"),
    #     names_pattern = "([A-Za-z]+)(\\d+)",
    #     values_to = "value"
    #   ) %>% pivot_wider(
    #     id_cols = c("ADHD_score","UID", "age", "age_num", "Date","timepoint"),
    #     names_from = "measure",
    #     values_from = "value") 
    
      dt %>% 
        filter(UID %in% input$select_UID) %>%
        ggplot(aes(x=FoundObjectivesX, y=FoundObjectivesY))+
        geom_point(size = 3)+
        geom_path(aes(x=PlayerPosX, y = PlayerPosY, col = UID))+
        # geom_point(data = subset(dt,
        #                           UID==input$select_UID &
        #                           FoundObjectivesT<9999 & !is.na(FoundObjectivesT)),
        #            aes(x = FoundObjectivesX,y = FoundObjectivesY),
        #            shape = 2, size = 5, col = "red")+
        labs(title = input$select_UID)+
        guides(col = FALSE)+
        facet_wrap(UID~Date)+
        theme_minimal()
  })
  # input <- data.frame(select_UID="TEDBFD07uNhlkcwaPEXx9TeCsra2")
  output$timeseries_plot <- renderPlot({
    p1 <- dt_m %>%
      filter(UID %in% input$select_UID) %>%
      ggplot(aes(x=date, y = n_found, col = UID))+
      geom_point()+
      geom_path()+
      scale_x_date(date_labels = "%Y %b %d")+
      theme_classic()
    p2 <- dt_m %>%
      filter(UID %in% input$select_UID) %>%
      ggplot(aes(x=date, y = meanTime, col = UID))+
      geom_point()+
      geom_path()+
      scale_x_date(date_labels = "%Y %b %d")+
      theme_classic()
    cowplot::plot_grid(p1,p2, ncol = 1)
  })

  output$n_found_plot <- renderPlotly({
  p <- dt_m %>%
    ggplot(aes(x=date, y = n_found, col = UID))+
    geom_point()+
    geom_path()+
    guides(col = "none")+
    scale_x_date(date_labels = "%b %Y")+
    theme_classic() 
  ggplotly(p)  
  })

  ## Correlations between subject variables
  output$correlation_plot <- renderPlot({
  data %>%
    # filter(TimeOfTheLastPosition == 179) %>%
    dplyr::select(age_num, ADHD_score, TimeOfTheLastPosition:CumulativeAngle) %>%
    na.omit(.) %>%
    ggpairs(.,
            diag = list(continuous = wrap("barDiag", binwidth = 1)),
            lower = list(continuous = wrap("points", alpha = 0.1))#, combo = wrap("dot_no_facet", alpha = 0.4))
    )+
    theme_classic()
  })
  
  output$meantime_group <- renderPlot({
    dt_m <- dt_m %>%
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) 
    
    
    group_data <- dt_m %>%
      group_by(ADHD_group, age) %>%
      summarise(n_subj = length(unique(UID)),
                n_sessions = length(unique(date)),
                y = mean(meanTime, na.rm = TRUE)) 
    subj_data <-  dt_m %>%
      group_by(ADHD_group, age, UID) %>%
      summarise(y = mean(meanTime, na.rm = TRUE),
                n_sessions = length(unique(date))) 
    dt_m %>%
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) %>%
      ggplot(aes(x=age, y = meanTime, fill = ADHD_group))+
      stat_summary(fun.data = mean_cl_boot, geom = "bar", position = position_dodge(width = .9))+
      stat_summary(fun.data = mean_cl_boot, geom = "linerange", position = position_dodge(width = .9))+
      # geom_text(data = group_data, aes(y = y+10, col = ADHD_group,
      #                                  label = paste0("N=",n_subj, "\ngames=", n_sessions)),
      #           position = position_dodge(width = .7))+
      scale_fill_manual(values = c("darkorange","gold"))+
      scale_color_manual(values = c("black","black"))+
      labs(title = "mean collection time")+      
      stat_compare_means(label = "p.signif", label.y = 80)+
      # facet_wrap(~age, scales = "free")+
      theme_classic()
  })
  
  output$nfound_group <- renderPlot({
    dt_m <- dt_m %>%
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) 
    group_data <- dt_m %>%
      group_by(ADHD_group, age) %>%
      summarise(n_subj = length(unique(UID)),
                n_sessions = length(unique(date)),
                y = mean(n_found, na.rm = TRUE)) 
    dt_m %>%
      ggplot(aes(x=age, y = n_found, fill = ADHD_group))+
      stat_summary(fun.data = mean_cl_boot, geom = "bar", position = position_dodge(width = .9))+
      stat_summary(fun.data = mean_cl_boot, geom = "linerange", position = position_dodge(width = .9))+
      # geom_text(data = group_data, aes(y = y+10, col = ADHD_group,
      #                                  label = paste0("N=",n_subj, "\ngames=", n_sessions)),
      #           position = position_dodge(width = .7))+
      scale_fill_manual(values = c("#df5725","#f8d999"))+
      scale_color_manual(values = c("black","black"))+
      labs(title = "number of butterflies found")+
      stat_compare_means(label = "p.signif")+
      # facet_wrap(~age, scales = "free")+
      theme_classic()
  })
  
  output$anova_nfound <- renderPrint({
    mod <- dt_m %>%
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) %>%
      # group_by(ADHD_score, ADHD_group, UID, age_num) %>%
      # dplyr::summarise(n_found = mean(n_found)) %>%
      lmer(n_found ~ age_num * ADHD_group + (1|UID), data = .)
    summary(mod)
    car::Anova(mod, type = 3)
  })
  
  output$anova_meantime <- renderPrint({
    mod <- dt_m %>%
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) %>%
      # group_by(ADHD_score, ADHD_group, UID, age_num) %>%
      # dplyr::summarise(meanTime= mean(meanTime)) %>%
      lmer(meanTime ~ age_num * ADHD_group + (1|UID), data = .)
    summary(mod)
    car::Anova(mod, type = 3)
  })

  output$correlations_filtered <- renderPlot({
    p1 <- dt_m %>% 
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
      dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) %>%
      ggplot(aes(x = n_found, y = meanTime,col = ADHD_group))+
      geom_point(size = 3,
                 alpha = .6,
                 position = position_jitter(width = .1)) +
      geom_smooth(method = "glm")+
      # geom_smooth(method = nls, formula = y ~a*exp(-b*x))+
      stat_cor()+
      geom_smooth(aes(group = 1), method = "glm", col = "black")+
      stat_cor(aes(group = 1), col = "black", label.x = 5, label.y = 150)+
      scale_colour_manual(values = c("#7BF3AD","#7BC8F3", "gray"))+
      guides(col = "none")+
      theme_classic()
    
   p2 <-  dt_m %>% 
      mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
     dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) %>%
     group_by(UID, ADHD_score, age, ADHD_group) %>%
     dplyr::summarize(nGames = n(),
                      mean_found = mean(n_found),
                      sd_found = sd(n_found),
                      mean_time = mean(meanTime),
                      sd_time = sd(meanTime)) %>%
      ggplot(aes(x = ADHD_score, y = nGames, col = ADHD_group))+
      geom_point(size = 3,
                 alpha = .6,
                 position = position_jitter(width = .1)) +
      geom_smooth(method = "glm")+
      stat_cor()+
     geom_smooth(aes(group = 1), method = "glm", col = "black")+
     stat_cor(aes(group = 1), col = "black", label.x = 3, label.y = 10)+
     scale_colour_manual(values = c("#7BF3AD","#7BC8F3", "gray"))+
      guides(col = "none")+
      theme_classic()
   
   p3 <- dt_m %>% 
     mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
     dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) %>%
     group_by(UID, ADHD_score, age, ADHD_group) %>%
     dplyr::summarize(nGames = n(),
               mean_found = mean(n_found),
               sd_found = sd(n_found),
               mean_time = mean(meanTime),
               sd_time = sd(meanTime)) %>%
     ggplot(aes(x=ADHD_score, y = mean_time, col = ADHD_group))+
     geom_point(size = 3,
                alpha = .6,
                position = position_jitter(width = .1)) +
     geom_smooth(method = "glm")+
     stat_cor()+
     geom_smooth(aes(group = 1), method = "glm", col = "black")+
     stat_cor(aes(group = 1), col = "black", label.x = 5, label.y = 130)+
     scale_colour_manual(values = c("#7BF3AD","#7BC8F3", "gray"))+
     guides(col = "none")+
     theme_classic()
   
   cowplot::plot_grid(p1, p2, p3, nrow = 1)
     

  })    
  
   output$custom_scatter <- renderPlot({
     dt_stats <- dt_m %>%
       ungroup() %>%
       mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
       dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2])  
     dt_stats <- dt_stats %>% 
       mutate(Xvar = dt_stats[[input$Xvar]],
                        Yvar = dt_stats[[input$Yvar]]) 
     dt_stats %>%
       ggplot(aes(x = Xvar, y = Yvar, col = ADHD_group))+
       geom_point(size = 5, alpha = .6,
                  position = position_jitter(width = .1))+
       geom_smooth(method = "glm")+
       stat_cor()+
       labs(x = input$Xvar,
            y = input$Yvar)+
       scale_color_manual(values = c("#df5725","#f8d999"))+
       theme_classic()

   })
   
   output$Xvar_group_plot <- renderPlot({
     dt_stats <- dt_m %>%
       ungroup() %>%
       mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
       dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) 
     dt_stats <- dt_stats %>% 
       mutate(Xvar = dt_stats[[input$Xvar]],
              Yvar = dt_stats[[input$Yvar]]) 
     dt_stats %>%
       ggplot(aes(x=factor(age), y = Xvar, col = ADHD_group, fill = ADHD_group))+
       geom_boxplot(col = "black", position = pd, alpha = .3)+
       geom_point(position = pjd)+
       # stat_summary(fun.data = mean_cl_boot, geom = "bar", position = position_dodge(width = .9))+
       # stat_summary(fun.data = mean_cl_boot, geom = "linerange", position = position_dodge(width = .9))+
       # geom_text(data = group_data, aes(y = y+10, col = ADHD_group,
       #                                  label = paste0("N=",n_subj, "\ngames=", n_sessions)),
       #           position = position_dodge(width = .7))+
       scale_fill_manual(values = c("#df5725","#f8d999"))+
       scale_color_manual(values = c("#df5725","#f8d999"))+
       labs(y = input$Xvar, title = input$Xvar)+
       stat_compare_means(label = "p.signif")+
       # facet_wrap(~age, scales = "free")+
       theme_classic()
     # ggplotly(p)  
   })
   
   output$Yvar_group_plot <- renderPlot({
     dt_stats <- dt_m %>%
       ungroup() %>%
       mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
       dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) 
     dt_stats <- dt_stats %>% 
       mutate(Xvar = dt_stats[[input$Xvar]],
              Yvar = dt_stats[[input$Yvar]])
     dt_stats %>%
     ggplot(aes(x=factor(age), y = Yvar, col = ADHD_group, fill = ADHD_group))+
       geom_boxplot(col = "black", position = pd, alpha = .3)+
       geom_point(position = pjd)+
       # stat_summary(fun.data = mean_cl_boot, geom = "bar", position = position_dodge(width = .9))+
       # stat_summary(fun.data = mean_cl_boot, geom = "linerange", position = position_dodge(width = .9))+
       # geom_text(data = group_data, aes(y = y+10, col = ADHD_group,
       #                                  label = paste0("N=",n_subj, "\ngames=", n_sessions)),
       #           position = position_dodge(width = .7))+
       scale_fill_manual(values = c("#df5725","#f8d999"))+
       scale_color_manual(values = c("#df5725","#f8d999"))+
       labs(y = input$Yvar, title = input$Yvar)+
       stat_compare_means(label = "p.signif")+
       # facet_wrap(~age, scales = "free")+
       theme_classic()
     # ggplotly(p)  
   })
   
   output$anova_Xvar <- renderPrint({
     dt_stats <- dt_m %>%
       ungroup() %>%
       mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
       dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) 
     dt_stats <- dt_stats %>% 
       mutate(Xvar = dt_stats[[input$Xvar]],
              Yvar = dt_stats[[input$Yvar]]) 
     mod <- dt_stats %>%
       lmer(Xvar ~ age_num * ADHD_group + (1|UID), data = .)
     print(input$Xvar)
     summary(mod)
     car::Anova(mod, type = 3)
   })
   
   output$anova_Yvar <- renderPrint({
     dt_stats <- dt_m %>%
       ungroup() %>%
       mutate(ADHD_group = ifelse(ADHD_score < input$ADHD_threshold, "ADHD", "CTL")) %>%
       dplyr::filter(age_num >= input$age_select[1] & age_num <= input$age_select[2]) 
     dt_stats <- dt_stats %>% 
       mutate(Xvar = dt_stats[[input$Xvar]],
              Yvar = dt_stats[[input$Yvar]]) 
     mod <- dt_stats %>%
       lmer(Yvar ~ age_num * ADHD_group + (1|UID), data = .)
     print(input$Yvar)
     summary(mod)
     car::Anova(mod, type = 3)
   })
        
}

# Preview the UI in the console
shinyApp(ui = ui, server = server)
