library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(bslib)
library(shinyWidgets)
library(plotly)
library(gghalves)
library(ggforce)
library(ggdist)
library(shinycssloaders)
library(shinytitle)
library(DBI)

# Connecting to SUPA :) base
# https://www.appsilon.com/post/connecting-r-to-postgres
# Make sure to do the .env file so you don't leak this secret info
# It won't show up in the files section but it will be there

pg_conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres",
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS")
)

dbListTables(pg_conn) # Gives you a list of available tables from supabase

mindsets <- dbSendQuery(conn = pg_conn, statement = "SELECT * FROM mindsets") 

# Selecting all from mindsets table

mindsets_df <- dbFetch(mindsets) # Create dataframe from query

# select applicable columns, drop rows that have no participant id (filters out incomplete surveys)

mindsets_df = mindsets_df %>% select("participant_id", "team_id", "training_date", "time_start", "time_end", "consent",
                                     "promotecare1", "preventharm1", "failcare1",  "failharm1", "escapeharm1", 
                                     "promotecare2", "preventharm2", "failcare2",  "failharm2", "escapeharm2", 
                                     "promotecare3", "preventharm3", "failcare3",  "failharm3", "escapeharm3",
                                     "promotecare4", "preventharm4", "failcare4",  "failharm4", "escapeharm4",
                                     "promotecare5", "preventharm5", "failcare5",  "failharm5", "escapeharm5", 
                                     "prevent_qual", "promote_qual",
                                     "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8",
                                     "goal9", "goal10", "gender", "race", "education", "role", "industry", "age",
                                     "surveys") %>% drop_na("participant_id")

# make these columns numeric for calculations

mindsets_df = mindsets_df %>% mutate(across(c(
  "promotecare1", "preventharm1", "failcare1",  "failharm1", "escapeharm1", 
  "promotecare2", "preventharm2", "failcare2",  "failharm2", "escapeharm2", 
  "promotecare3", "preventharm3", "failcare3",  "failharm3", "escapeharm3",
  "promotecare4", "preventharm4", "failcare4",  "failharm4", "escapeharm4",
  "promotecare5", "preventharm5", "failcare5",  "failharm5", "escapeharm5", 
  "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8",
  "goal9", "goal10"), 
  as.integer))

# mindsets scoring

mindsets_df = mindsets_df %>% mutate(
  promotecare_all = (promotecare1 + promotecare2 + promotecare3 + promotecare4 + promotecare5) / 5,
  preventharm_all = (preventharm1 + preventharm2 + preventharm3 + preventharm4 + preventharm5) / 5,
  failcare_all = (failcare1 + failcare2 + failcare3 + failcare4 + failcare5) / 5,
  failharm_all = (failharm1 + failharm2 + failharm3 + failharm4 + failharm5) / 5,
  escapeharm_all = (escapeharm1 + escapeharm2 + escapeharm3 + escapeharm4 + escapeharm5) / 5
)

# scoring for coxcomb plot

scores_long = gather(mindsets_df, name, value, promotecare_all:escapeharm_all, factor_key=TRUE)

scores_long = scores_long %>% mutate(
  name = case_when(name == "promotecare_all" ~ "Promote Care",
                   name == "preventharm_all" ~ "Prevent Harm",
                   name == "failcare_all" ~ "Fail Care",
                   name == "failharm_all" ~ "Fail Harm",
                   name == "escapeharm_all" ~ "Escape Harm"
  ))

# need to make data long form in order to plot data for coxcomb, renaming to look nice on plots

ui <- fluidPage(
  theme = bs_theme(), # use brand.yml file theming
  setBackgroundColor("#f2f6f9"),
  windowTitle = "Mindsets Results",
  title = tags$head(
    tags$title("Mindsets Results"), # title in browser
    tags$link(rel = "icon", type = "image/png", href = "favicon.png") # use custom browser icon
  ),
    navset_card_pill(  
        nav_panel("Main",
                  h3(tags$b("Thank you for taking the survey!"), style="text-align:center"),
                  h4("Navigate using the tabs above to view your results!", style="text-align:center"),
                  h4("About the Research"),
                  p("This survey aims to examine people’s attitudes and beliefs about helping others in the place they live and work. These beliefs can be important for how people get along with others, and how they feel about themselves. These items were meant to be non-threatening and non-embarrassing. However, if you have any concerns about these items, please feel free to contact the primary investigator (Dr. Jordan Booker, bookerja@missouri.edu). If you want to talk privately about your rights or any issues related to your participation in this study, you can also contact University of Missouri Research Participant Advocacy by calling 888-280-5002 (a free call), or emailing MUResearchRPA@missouri.edu."),
                  h4("About the App"),
                  p("Shiny app v3 was developed by Zach Spiegel, Shane McCarty & Bryan Acton. The survey and app was developed using Surveydown and Posit Cloud in R."),
                  div(img(src = "logo.png", width = "20%", height = "20%"), style = "text-align: center;")
                  ), 
        
        nav_panel("Overall Results", 
                  fluidRow(
                    column(
                      width = 3,
                      card(
                        p(tags$b("Here are the results of all participants who took the survey.")),
                        p("Each segment represents one of the 5 different mindsets, with the size representing the preference of the individual.")
                      )),
                    column(
                      width = 9,
                      card(
                        plotOutput("overall_coxcomb")
                      ),
                    )),
                  br(),
                  div(img(src = "long_logo.png", width = "30%", height = "30%"), style = "text-align: center;")), 
        
        
        nav_panel("Individual Results", 
                  fluidRow(
                  column(
                        width = 3,
                    card(
                        p(tags$b("Enter your information from the survey to view your mindsets.")),
                        p("Each segment represents one of the 5 different mindsets, with the size representing your personal preferences."),
                        textInput("individual_id", "Enter your participant ID here:", placeholder = "ex. hello1234"),
                        dateInput("individual_date", "Enter your training date here:")
                      )),
                  column(
                      width = 9,
                    card(
                      plotOutput("ind_coxcomb")
                       ),
                  )),
                  br(),
                  div(img(src = "long_logo.png", width = "30%", height = "30%"), style = "text-align: center;")
                  ), 
        
        nav_panel("Team Results", 
                  fluidRow(
                    column(
                      width = 3,
                      card(
                        p(tags$b("Enter your team's information from the survey to view your team's mindsets.")),
                        p("Each segment represents one of the 5 different mindsets, with the size representing your team's personal preferences."),
                        textInput("team_id", "Enter your team ID here:", placeholder = "ex. FRI-PH-1"),
                        dateInput("team_date", "Enter your training date here:")
                      )),
                    column(
                      width = 9,
                      card(
                        plotOutput("team_coxcomb")
                      ),
                  )),
                  br(),
                  div(img(src = "long_logo.png", width = "30%", height = "30%"), style = "text-align: center;"))
                  )
                )
    
    

server <- function(input, output) {

  reactive_individual = reactive({
    scores_long %>%
      filter(participant_id %in% input$individual_id,
             training_date %in% input$individual_date)
  })
  
  output$ind_coxcomb = renderPlot({
    ggplot(reactive_individual(), aes(x = name, y = value, fill = name)) + 
      geom_bar(stat = "identity", color = "black", width = 1) +
      geom_label(aes(label = name, y = 4.5), fill = "white", color = "black") +
      scale_fill_manual(values = c("Promote Care" = "#8297cd", "Prevent Harm" = "#453b97",
                                   "Escape Harm" = "#646cb2", "Fail Harm" = "#e78666", "Fail Care" = "#e78666")) +
      coord_polar(clip = "off") +
      ylim(0, 6) +
      theme_minimal() +
      theme(
        axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_blank()
      )
  })
  
  # coxcomb plot tutorial: https://www.robinlovelace.net/post/2013-12-27-coxcomb-plots-spiecharts-r/
  
  ind_scores_summary = scores_long %>%
    group_by(name) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))
  
  output$overall_coxcomb = renderPlot({
    ggplot(ind_scores_summary, aes(x = name, y = mean_value, fill = name)) + 
      geom_bar(stat = "identity", color = "black", width = 1) +
      geom_label(aes(label = name, y = 4.5), fill = "white", color = "black") +
      scale_fill_manual(values = c("Promote Care" = "#8297cd", "Prevent Harm" = "#453b97",
                                   "Escape Harm" = "#646cb2", "Fail Harm" = "#e78666", "Fail Care" = "#e78666")) +
      coord_polar(clip = "off") +
      ylim(0, 6) +
      theme_minimal() +
      theme(
        axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_blank()
      )
  })
  
  reactive_team = reactive({
    scores_long %>%
      filter(team_id %in% input$team_id,
             training_date %in% input$team_date) %>%
      group_by(name) %>%
      summarise(mean_value = mean(value, na.rm = TRUE))
  })
  
  
  output$team_coxcomb = renderPlot({
    ggplot(reactive_team(), aes(x = name, y = mean_value, fill = name)) + 
      geom_bar(stat = "identity", color = "black", width = 1) +
      geom_label(aes(label = name, y = 4.5), fill = "white", color = "black") +
      scale_fill_manual(values = c("Promote Care" = "#8297cd", "Prevent Harm" = "#453b97",
                                   "Escape Harm" = "#646cb2", "Fail Harm" = "#e78666", "Fail Care" = "#e78666")) +
      coord_polar(clip = "off") +
      ylim(0, 6) +
      theme_minimal() +
      theme(
        axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "none", 
        axis.text.x = element_blank()
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
