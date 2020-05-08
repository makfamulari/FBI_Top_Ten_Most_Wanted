#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(janitor)
library(lubridate)
library(shinythemes)
library(ggplot2)
library(tidyverse)

FBI <- read_csv("FBI_full - Sheet1.csv") %>% 
  clean_names() 


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("sandstone"),
                 "The FBI's Top Ten Most Wanted List",
                 tabPanel("About",
                          column(6,
                                 h1("Background"),
                                 p("The Federal Bureau of Investigation introduced its now infamous Top Ten Most 
                                   Wanted List on March 14th, 1950. The list was first designed to bring publicity 
                                   to the most dangeous fugitives that otherwise did not merit media attention. 
                                   Of the 523 fugitives that have been placed on the list, 488 have been apprehended 
                                   or located."),
                                 p("There are two criteria a criminal must meet to merit placement on the list. 
                                   First, the criminal must either be considered particularily dangerous due to 
                                   current charges OR the criminal must have a meaningful record involving
                                   serious crimes. Second, the publicity resulting from placement on the 
                                   list must be of assistance in apprehending the criminal. In other words,
                                   if the criminal is already garnering meaningful media coverage, their placement
                                   would be considered unnecessary."),
                                 p("Removal from the list is only possible under one of the following conditions:"),
                                 p("- They are captured"),
                                 p("- The case of the criminal is dismissed"),
                                 p("- The criminal no longer fits the criteria"),
                                 h1("Purpose"),
                                 p("The purpose of this project is to investigate the members of the FBI's Top Ten Most
                                   Wanted list from the years 1950-2020. Specifically, we aim to discover the types of
                                   criminals that merit placement based upon biographical information and the nature of their
                                   crimes. The list's members beginning in the year 1950 and to the year 2020 will be
                                   studied."),
                                 h1("Data"),
                                 p("There are no current existing databases aggregating information on members of the 
                                   FBI's Most Wanted List. Data scrubbing was conducted by myself using information from
                                   the ",
                                   a("FBI's archived most wanted lists.",
                                     href = "https://www.fbi.gov/wanted/topten/ten-most-wanted-fugitives-faq"),
                                   "These lists contained information on dates of placement, 
                                   dates of removal, and biographical information such as race, gender, and nationality. For a significant 
                                   number of past criminals, the nature of crimes/profiles were not available through the FBI. To determine 
                                   the reasons for placement (including suspected charges
                                   and convicted charges, if apprehended), archived court files were utilized."),
                                 h1("About me"),
                                 p("I am a first-year student at Harvard College studying neuroscience on a computer science track.
                                   For information about this project or to request my full dataset, please email me at ",
                                   a("mfamulari@college.harvard.edu",
                                   href = "mailto: mfamulari@college.harvard.edu"),
                                   "."),
                                 p("To inspect my code or review any of the methods used in this project, please see
                                   my ",
                                   a("Github Account",
                                     href = "https://github.com/makfamulari/FBI_Top_Ten_Most_Wanted"),
                                   "."))),
                 tabPanel("The Crimes",
                          column(9,
                                 h1("What crimes end up on the list?"),
                                 h1("Total:"),
                                 p("The following chart lists the frequency of crimes on the FBI's Most Wanted Lists 
                                   for the years 1950-2019.")),
                          column(12,
                                 mainPanel(plotOutput("crime_breakdown"))),
                          column(12,
                                 h1("By Selected Decade:"),
                                 sidebarPanel(
                                   selectInput("decade", "Choose a time period:",
                                               choices = c("1950s" = "1950",
                                                           "1960s" = "1960",
                                                           "1970s" = "1970",
                                                           "1980s" = "1980",
                                                           "1990s" = "1990",
                                                           "2000s" = "2000",
                                                           "2010s" = "2010"),
                                               selected = "1950"))),
                          column(12,
                                 mainPanel(
                                   plotOutput("crime_year"))),
                          column(12,
                                 h1("Frequency of Specific Crimes:"),
                                 sidebarPanel(
                                 selectInput("crime", "Select a crime category:",
                                             choices = c("Crimes Against Children" = "crimes_against_children",
                                                         "Murder" = "murder",
                                                         "Sexual Crimes" = "sexual_crimes",
                                                         "Other Violent Crime" = "additional_violent_crime",
                                                         "Politically Motivated Crime" = "political_group",
                                                         "Terrorism" = "terrorism",
                                                         "White Collar Crime" = "white_collar_crime",
                                                         "Escapee" = "escapee"),
                                             selected = "murder"))),
                          column(12,
                                 mainPanel(
                                   plotOutput("over_time_crime")
                                 ))),
                 tabPanel("The Criminals",
                          column(4,
                                 h1("Demographics of Criminals"),
                                 p("To view the demographics of the criminals placed on the FBI's Most Wanted List, 
                                   please select a demographic characteristic.")),
                          column(7,
                                 sidebarPanel(
                                   selectInput("demographics", "Choose a demographic characteristic:",
                                               choices = c("Race" = "race", 
                                                           "Gender" = "gender",
                                                           "Nationality" = "nationality"),
                                               selected = "race")),
                                 mainPanel(
                                   plotOutput("distPlot")))),
                 tabPanel("Special Cases",
                 column(4,
                        h1("Do law enforcement victims effect outcomes?"),
                        p("The relationship between length of time on list (filtered to exclude removals) and law enforcement victims
                          is plotted below. Note: law enforcement victims include police officers, fish and game wardens, detectives,
                          air marshalls, sheriffs, highway patrol officers, and any other uniformed officer. Victimization includes
                          attempted assault/murder/kidnapping, as well as actualized assault/murder/kidnapping."),
                        p("To interpret graph, note that x = 1 means YES, there is a police victim and x = 0 means NO, there is
                          not a police victim.")),
                 column(12,
                        mainPanel(plotOutput("police_effect"))),
                 column(4,
                        h1("Does criminal organization affiliation effect outcomes?"),
                        p("Criminal organizations are defined as gangs, mob, or other affiliations with criminal groups. The 
                          relationship between criminal organization affiliation and days on the list is graphed below."),
                        p("To interpret graph, note that x = 1 means YES, there is a group affiliation and x = 0 means NO, there is
                          not a group affiliation.")),
                 column(12,
                        mainPanel(plotOutput("gang_effect")))),
                 tabPanel("Findings",
                          column(5,
                          h1("Efficacy"),
                          p("Of the observations in this dataset, 87.83% of the fugitives were apprehended. An additional
                            4.6% surrendered."),
                          p("- 5 fugitives died during their placement on the list."),
                          p("- 4 fugitives were killed during arrest."),
                          p(" - 1 fugitive commited suicide during his time on the list"),
                          p("- 13 fugitives were removed from the list for no longer qualifying"),
                          h1("Crimes"),
                          p("For the two decades following the creation of the FBI's Most Wanted Fugitives List,
                            the most common crime fell under the category of Other Violent Crimes, which included assault,
                            robbery, and bank heists (armed or otherwise). The other most common categories included
                            murder, personal crimes, and escapees."),
                          h1("The Criminals"),
                          p("For the years 1950-1969, the average fugitive on the FBI's Most Wanted List was white,
                            male, and american."),
                          h1("Special Cases"),
                          p("Method: to determine connection between special cases and outcomes, a generalized linear
                            model was used."),
                          p("Findings: The difference in duration on the list was not significant between those fugitives who had
                            law enforcement victims and those who did not. Those fugitives that had affiliations to organized
                            crime remained on the lists slightly longer compared to fugitives without said affiliation.")
                          )))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$crime_breakdown <- renderPlot({
    crime_breakdown <- FBI %>% 
      select(-number,
             -name,
             -date_added,
             -date_removed,
             -decade,
             -reason_for_removal,
             -race,
             -nationality,
             -gender,
             -police_victim) %>% 
      pivot_longer(everything(),
                   names_to = "crime",
                   values_to = "count") %>% 
      filter(count == 1) %>% 
      select(crime) %>% 
      count(crime) %>% 
      filter(n > 1)
    
    ggplot(crime_breakdown, aes(x = crime, y = n, fill = crime)) +
      geom_col(stat = "identity") + 
      scale_fill_discrete(
                        breaks=c("additional_violent_crime",
                                 "murder",
                                 "personal_crimes", 
                                 "escapee",
                                 "political_group",
                                 "terrorism",
                                 "criminal_enterprise",
                                 "white_collar_crime",
                                 "sexual_crimes",
                                 "crimes_against_children"),
                        labels=c("Other Violent Crime", 
                                 "Murder", 
                                 "Personal Crimes",
                                 "Escape",
                                 "Politically Motivated Crime",
                                 "Terrorism",
                                 "Criminal Enterprise",
                                 "White Collar Crime",
                                 "Sexual Crime",
                                 "Crimes Against Children")) +
      scale_x_discrete(
        breaks=c("additional_violent_crime",
                 "murder",
                 "personal_crimes", 
                 "escapee",
                 "political_group",
                 "terrorism",
                 "criminal_enterprise",
                 "white_collar_crime",
                 "sexual_crimes",
                 "crimes_against_children"),
        labels=c("Other Violent
    Crime", 
                 "Murder", 
                 "Personal",
                 "Escape",
                 "Political",
                 "Terrorism",
                 "Criminal Enterprise",
                 "White Collar",
                 "Sexual",
                 "Crimes Against
    Children")) +
      labs(
        title = "Frequency of Crimes on the List (Total)",
        fill = "Type of Crime",
        x = "Crime",
        y = "Count",
        caption = "*Personal Crimes: assault, kidnapping, attempted kidnapping, attempted murder.
        *Additional violent crime: robbery, bank robbery, robbery with a deadly weapon.
        *Politically motivated: crimes committed in the name of political movements/groups."
      ) +
      theme_dark()
    
  })
  
  output$distPlot <- renderPlot({
    
    pie_chart <- FBI %>% 
      filter(! (.data[[input$demographics]] == 0)) %>% 
      group_by(.data[[input$demographics]]) %>% 
      count() %>% 
      mutate(prop = (n)/(sum(n))) 
    
    pie_chart %>% 
      ggplot(aes(x = "", y = n, fill = .data[[input$demographics]])) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + 
      scale_fill_brewer(palette = "Spectral") +
      theme_dark() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank()) +
      labs(
        x = NULL,
        y = NULL, 
        fill = NULL
      )
  })
  
  output$crime_year <- renderPlot({
    
    FBI_year<- FBI %>% 
        mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
        mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
        mutate(days = (date_removed - date_added)) %>% 
        mutate(year = format(date_added, "%Y")) 
    
    crime <- FBI %>% 
      filter(decade == input$decade) %>% 
      pivot_longer(cols = c("additional_violent_crime",
                           "murder",
                           "personal_crimes", 
                           "escapee",
                           "political_group",
                           "terrorism",
                           "criminal_enterprise",
                           "white_collar_crime",
                           "sexual_crimes",
                           "crimes_against_children"),
                  names_to = "crime",
                  values_to = "count") %>% 
      filter(count == 1) %>% 
      select(crime) %>% 
      count(crime) %>% 
      filter(n > 1)
    
    ggplot(crime, aes(x = crime, y = n, fill = crime)) +
      geom_col(stat = "identity") + 
      scale_fill_discrete(
        breaks=c("additional_violent_crime",
                 "murder",
                 "personal_crimes", 
                 "escapee",
                 "political_group",
                 "terrorism",
                 "criminal_enterprise",
                 "white_collar_crime",
                 "sexual_crimes",
                 "crimes_against_children"),
        labels=c("Other Violent Crime", 
                 "Murder", 
                 "Personal Crimes",
                 "Escape",
                 "Politically Motivated",
                 "Terrorism",
                 "Criminal Enterprise",
                 "White Collar Crime",
                 "Sexual Crime",
                 "Crimes Against Children")) +
      scale_x_discrete(
        breaks=c("additional_violent_crime",
                 "murder",
                 "personal_crimes", 
                 "escapee",
                 "political_group",
                 "terrorism",
                 "criminal_enterprise",
                 "white_collar_crime",
                 "sexual_crimes",
                 "crimes_against_children"),
        labels=c("Other Violent
      Crime", 
                 "Murder", 
                 "Personal",
                 "Escape",
                 "Political",
                 "Terror",
                 "Criminal Enterprise",
                 "White Collar",
                 "Sexual",
                 "Crimes Against
      Children")) +
      labs(
        fill = "Type of Crime",
        x = "Crime",
        y = "Count",
        caption = "*Personal Crimes: assault, kidnapping, attempted kidnapping, attempted murder.
        *Additional violent crime: robbery, bank robbery, robbery with a deadly weapon."
      ) +
      theme_dark()
    
  })
  
  output$over_time_crime <- renderPlot({
    
    FBI_over_year<- FBI %>% 
      mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
      mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
      mutate(days = (date_removed - date_added)) %>% 
      mutate(year = format(as.Date(date_added, format = "%m/%d/%Y"), "%Y"))
    
    crime_over_time <- FBI_over_year %>% 
      filter(! (.data[[input$crime]] == 0)) %>% 
      group_by(decade) %>% 
      count()
    
    ggplot(crime_over_time, aes(x = decade, y = n)) +
      geom_line(group = 1, color = "pink", size = .75)+
      ylim(0, 30) +
      geom_point(col = "pink") +
      labs(
        x = "Year",
        y = "Count",
        caption = "*Personal Crimes: assault, kidnapping, attempted kidnapping, attempted murder.
        *Additional violent crime: robbery, bank robbery, robbery with a deadly weapon."
      )  +
      theme_dark()
    
  })
  
  output$police_effect <- renderPlot({
    
    crimes <- FBI %>% 
      select(-number,
             -name,
             -date_added,
             -date_removed,
             -decade,
             -reason_for_removal,
             -race,
             -nationality,
             -gender,
             - police_victim) %>% 
      pivot_longer(everything(),
                   names_to = "crime",
                   values_to = "count") 
    
    FBI1 <- FBI %>% 
      mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
      mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
      mutate(days = (date_removed - date_added))
    
    FBI1 %>%
      filter(! reason_for_removal == "removal") %>% 
      ggplot(aes(x = police_victim, y = days)) +
      geom_point() +
      labs(x = "Police Victim?", 
           y = "Days on List", 
           title = "The Effect of Police Victims on Length on List",
           subtitle = "Conclusion: There is Virtually No Effect on Outcomes by Police Victims") +
      geom_smooth(method = "glm", se = FALSE) +
      theme_classic()
      
  })
  
  
  output$gang_effect <- renderPlot({
    
    crimes <- FBI %>% 
      select(-number,
             -name,
             -date_added,
             -date_removed,
             -decade,
             -reason_for_removal,
             -race,
             -nationality,
             -gender,
             - police_victim) %>% 
      pivot_longer(everything(),
                   names_to = "crime",
                   values_to = "count") 
    
    FBI1 <- FBI %>% 
      mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
      mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
      mutate(days = (date_removed - date_added))
    
    FBI1 %>%
      filter(! reason_for_removal == "removal") %>% 
      ggplot(aes(x = criminal_enterprise, y = days)) +
      geom_point() +
      labs(x = "Criminal Enterprise Affiliation?", y = "Days on List", 
           title = "The Effect of Criminal Enterprise Affliliation on Length on List",
           subtitle = "Conclusion: Criminal Affliliation is Assosiated with Longer Lengths on the List") +
      geom_smooth(method = "glm", se = FALSE) +
      theme_classic()
    
  })
    
}



# Run the application 
shinyApp(ui = ui, server = server)

