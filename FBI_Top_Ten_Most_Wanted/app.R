#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Loading libraries

library(shiny)
library(janitor)
library(lubridate)
library(pdftools)
library(shinythemes)
library(broom)
library(ggplot2)
library(tidyverse)
library(slickR)

# Data read in of FBI's Most Wanted

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
                                   Wanted list from the year it was created (1950) to present day(2020). Specifically, we aim to discover the types of
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
                                                         "Ideologically Motivated Crime" = "political_group",
                                                         "Criminal Enterprise" = "criminal_enterprise",
                                                         "Terrorism" = "terrorism",
                                                         "White Collar Crime" = "white_collar_crime",
                                                         "Escapee" = "escapee"),
                                             selected = "crimes_against_children"))),
                          column(12,
                                 mainPanel(
                                   plotOutput("over_time_crime")
                                 ))),
                 tabPanel("The Criminals",
                          tabsetPanel(
                          tabPanel("Demographics",
                                 h1("Demographics of Criminals"),
                                 p("To view the demographics of the criminals placed on the FBI's Most Wanted List, 
                                   please select a demographic characteristic."),
                                 sidebarPanel(
                                  selectInput("demographics", "Choose a demographic characteristic:",
                                               choices = c("Race" = "race", 
                                                           "Gender" = "gender",
                                                           "Nationality" = "nationality"),
                                               selected = "race")),
                                 mainPanel(
                                   plotOutput("distPlot"))),
                          tabPanel("Current List",
                                 h1("Current Top Ten Most Wanted Fugitives"),
                                 p("To view the current members of the Top Ten Most Wanted Fugitive
                                   List, please click on the arrows below."),
                          mainPanel(
                            slickROutput("slick_pics"))),
                          tabPanel("Historical List",
                                 h1("Historical Top Ten Most Wanted Fugitives:"),
                                 p("Below are the pictures of every fugitive placed on the FBI's
                                   Most Wanted Fugitive List since its inception in 1950 to 2020."),
                                 mainPanel(
                                   slickROutput("historical",
                                                width='100%')
                                 )))),
                 tabPanel("Special Cases",
                 column(4,
                        h1("Do law enforcement victims effect outcomes?"),
                        p("The relationship between length of time on list (filtered to exclude removals) and law enforcement victims
                          is plotted below. Note: law enforcement victims include police officers, fish and game wardens, detectives,
                          air marshalls, sheriffs, highway patrol officers, and any other uniformed officer. Victimization includes
                          attempted assault/murder/kidnapping, as well as actualized assault/murder/kidnapping.")),
                 column(10,
                        mainPanel(plotOutput("police_effect"))),
                 column(4,
                        h1("Does criminal organization affiliation effect outcomes?"),
                        p("Criminal organizations are defined as gangs, mob, or other affiliations with criminal groups. The 
                          relationship between criminal organization affiliation and days on the list is graphed below.")),
                 column(10,
                        mainPanel(plotOutput("gang_effect"))),
                 column(4,
                        h1("Does other group organization crime effect outcome?"),
                        p("Other group organization crime is defined as a criminal offense relating to political/religious/ideological 
                          groups. Examples include Warren Jeffs, whose crimes (child sexual assault) were commited under the Fundamentalist
                          Church of Jesus Christ of Latter-Day Saints. Another example would be Clayton Lee Waagner, placed on the list for
                          a conviction of bank robbery and anti-abortion terrorism.")),
                 column(10,
                        mainPanel(plotOutput("other_groups")))),
                 tabPanel("Findings",
                          column(7,
                          h1("Efficacy"),
                          p("Of the observations in this dataset, 83.56% of the fugitives were apprehended. An additional
                            4.78% surrendered."),
                          p("- 10 fugitives died during their placement on the list."),
                          p("- 12 fugitives were killed during arrest."),
                          p("- 5 fugitives commited suicide during their time on the list"),
                          p("- 24 fugitives were removed from the list for no longer meeting the qualifying criteria or 
                            due to their cases being dismissed"),
                          h1("The Crimes"),
                          p("For entirety of the FBI's Most Wanted Fugitives List,
                            the most common crime fell under the category of Other Violent Crimes, which includes assault,
                            robbery, bank heists (armed or otherwise), and other crime. Murder was also consistently
                            high in each given decade of the data."),
                          p("Crimes that remained mostly static, but were not particularily prevalent, included
                            Crimes Against Children and Sexual Crimes."),
                          p("A few types of crime peaked in the earlier years of the FBI's Most Wanted List.
                            These included escapees and white collar crime."),
                          p("Crimes relating to ideological groups emerged and peaked in the 1970s. 
                            Terrorism first emerged in the 1990s and quickly dissipated."),
                          h1("The Criminals"),
                          p("For all years of the FBI's Most Wanted List, the average fugitive on the FBI's Most Wanted List was white,
                            male, and american. Nationality and race of the fugitives differed from this average slightly in later years.
                            However, female fugitives remained continuously rare (with a total of ten women on the list)."),
                          h1("Special Cases"),
                          h3("Method:"),
                          p("To determine connection between special cases and outcomes, a generalized linear
                            model was used which mapped the upper and lower confidence intervals for estimated time on list
                            according to special case."),
                          h3("Findings:"),
                          p("When law enforcement victims were involved in the case of the fugitive, the average length
                            on the list was lower. This suggests that law enforcement victims are tied to faster capture.
                            It is important to note that the confidence intervals are large, likely due to outliers that skew 
                            the data."),
                          p("When fugitives were tied to criminal organizations, the average length on the list was longer.
                            This may be due to the fact that early criminal organizations in the dataset, such as mafia leaders,
                            have access to more resources to evade capture. Again, the confidence intervals are rather sprawling,
                            so a clear conclusion cannot be made."),
                          p("When fugitives were tied to ideological organizations, the average length on the list was significantly
                            higher. The same confidence interval pattern is observed. However, the difference in length may
                            indicate that ideological affiliations are correlated with longer amounts of time evading capture.")
                          )))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$crime_breakdown <- renderPlot({
    
    # Pivot crime longer to count by crime
    
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
    
    # Plot so that crime is on x and count is on y with fill = crime.
    
    ggplot(crime_breakdown, aes(x = crime, y = n, fill = crime)) +
      geom_col(stat = "identity") + 
      
      # Modify legend labels for clarification.
      
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
                                 "Ideologically Motivated Crime",
                                 "Terrorism",
                                 "Criminal Enterprise",
                                 "White Collar Crime",
                                 "Sexual Crime",
                                 "Crimes Against Children")) +
      
      # Shorten x-axis labels for reader clarity.
      
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
                 "Ideological",
                 "Terrorism",
                 "Criminal Enterprise",
                 "White Collar",
                 "Sexual",
                 "Crimes Against
    Children")) +
      
      # Add title, axis-labels, and modified legend title.
      
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
    
    # Filter out where the input equals 0.
    
    pie_chart <- FBI %>% 
      filter(! (.data[[input$demographics]] == 0)) %>% 
      group_by(.data[[input$demographics]]) %>% 
      count() %>% 
      mutate(prop = (n)/(sum(n))) 
    
    # Create bar plot to add coord_polar.
    
    pie_chart %>% 
      ggplot(aes(x = "", y = n, fill = .data[[input$demographics]])) +
      geom_bar(width = 1, stat = "identity", col = "grey") +
      coord_polar("y", start = 0) + 
    
      # Add theme for consistency.
      
      theme_dark() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank()) +
      
      # Eliminate unnecessary borders/labels.
      
      labs(
        x = NULL,
        y = NULL, 
        fill = NULL
      )
  })
  
  output$slick_pics <- renderSlickR({
    imgs <- list.files("png_pics", pattern = ".png", full.names = TRUE)
    slickR(imgs, slideId = "sld1")
    
  })
  
  output$historical <- renderSlickR({
    imgs1 <- list.files("historical", pattern = ".png", full.names = TRUE)
    slickR(imgs1, slideId = "sld2") 
    
  })
  
  output$crime_year <- renderPlot({
    
    # Mutate columns to act as effective dates.
    
    FBI_year<- FBI %>% 
        mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
        mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
      
    # Subtract columns to get total days on list.
      
        mutate(days = (date_removed - date_added)) %>% 
        mutate(year = format(date_added, "%Y")) 
    
    # Filter for decade input.
    
    crime <- FBI %>% 
      filter(decade == input$decade) %>% 
    
    # Pivot crime longer for counts.
      
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
    
    # Follow same formatting as previous output.
    
    ggplot(crime, aes(x = crime, y = n, fill = crime)) +
      geom_col(stat = "identity") + 
      
    # Modify legend objects.
      
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
                 "Ideologically Motivated",
                 "Terrorism",
                 "Criminal Enterprise",
                 "White Collar Crime",
                 "Sexual Crime",
                 "Crimes Against Children")) +
    
    # Modify x-axis labels.
      
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
                 "Ideological",
                 "Terror",
                 "Criminal Enterprise",
                 "White Collar",
                 "Sexual",
                 "Crimes Against
      Children")) +
      
    # Add title, axis-labels, and captions.
      
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
    
    # Filter out where input = 0 (OR NO).
    
    crime_over_time <- FBI %>% 
      filter(! (.data[[input$crime]] == 0)) %>% 
      group_by(decade) %>% 
      select(input$crime) %>% 
      count(input$crime)
    
    # Map count by decade. 
    
    ggplot(crime_over_time, aes(x = decade, y = n)) +
    
      # Style element to enhance visibility.
      
      geom_line(group = 1, color = "pink", size = .75) +
      
      # Add ylim to maintain consistent axis length
      # For better comparison
      
      ylim(0, 150) +
      
      # Add points with consistent coloring.
      
      geom_point(col = "pink") +
      
      # Add relevant details for viewer clarity.
      
      labs(
        x = "Year",
        y = "Count",
        caption = "*Personal Crimes: assault, kidnapping, attempted kidnapping, attempted murder.
        *Additional violent crime: robbery, bank robbery, robbery with a deadly weapon."
      )  +
      theme_dark()
    
  })
  
  output$police_effect <- renderPlot({
    
    # Edit date columns to be dates for total day 
    # calculation.
    
    FBI1 <- FBI %>% 
      mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
      mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
      mutate(days = (date_removed - date_added)) %>% 
      mutate(days = as.numeric(days))
    
    # Make police_victim a factor, instead of numeric.
    # This will aid in the linear regression output.
    
    Edit <- FBI1 %>% 
      mutate(police_victim = as.factor(police_victim))
    
    # Perform linear regression tying police victims to
    # days on list. Select relevant columns after tidying.
    
    lm <- lm(days ~ police_victim, data = Edit) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high, std.error) 
    
    # Graph object lm with x = term and y = estimate.
    
    ggplot(lm, aes(x = term, y = estimate)) +
      
      # Use the estimate minus standard error and plus SE to 
      # map the ranges of the estimates.
      
      geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error), width = 0.1) +
      geom_line() +
      geom_point() +
      
      # Modify x-axis to be self-explanatory.
      
      scale_x_discrete(
        breaks = c("(Intercept)", "police_victim1"),
        labels = c("No", "Yes")
      ) +
      
      # Add relevant information for interpretation.
      
      labs(
        title = "Estimated Length on List (in Days) as Related to Police Victims",
        subtitle = "Using a Linear Regression Showing Upper and Lower Confidence Intervals",
        x = "Police Victim?",
        y = "Length on List (Days)"
      ) +
      theme_dark() 
      
  })
  
  
  output$gang_effect <- renderPlot({
    
    # Modify dates for subtraction to get total
    # days on list.
    
    FBI1 <- FBI %>% 
      mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
      mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
      mutate(days = (date_removed - date_added)) %>% 
      mutate(days = as.numeric(days))
    
    # Mutate as factor for clarification.
    
    enterprise <- FBI1 %>% 
      mutate(criminal_enterprise = as.factor(criminal_enterprise))
    
    # Linear model between length of placement
    # and connections to criminal enterprises.
    # Select relevant columns.
    
    lm2 <- lm(days ~ criminal_enterprise, data = enterprise) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high, std.error) 
    
    # Map with the term and estimate.
    
    ggplot(lm2, aes(x = term, y = estimate)) +
      
      # Use the SE to get a lower and upper bound
      # for the estimate.
      
      geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error), width = 0.1) +
      geom_line() +
      geom_point() +
      
      # Clarify x-axis labels.
      
      scale_x_discrete(
        breaks = c("(Intercept)", "criminal_enterprise1"),
        labels = c("No", "Yes")
      ) +
      
      # Adding info for viewer about the 
      # regression.
      
      labs(
        title = "Estimated Length on List (in Days) as Related to Criminal Enterprise Affiliation",
        subtitle = "Using a Linear Regression Showing Upper and Lower Confidence Intervals",
        x = "Criminal Enterprise?",
        y = "Length on List (Days)"
      ) +
      theme_dark()
    
  })
  
  output$other_groups <- renderPlot({
    
    # Modify dates for subtraction to get total
    # days on list.
    
    FBI1 <- FBI %>% 
      mutate(date_added = as.Date(date_added, format="%m/%d/%Y")) %>% 
      mutate(date_removed = as.Date(date_removed, format="%m/%d/%Y")) %>% 
      mutate(days = (date_removed - date_added)) %>% 
      mutate(days = as.numeric(days))
    
    # Mutate as factor for clarification.
    
    political <- FBI1 %>% 
      mutate(political_group = as.factor(political_group))
    
    # Linear model between length of placement
    # and connections to ideological groups.
    # Select relevant columns.
    
    lm3 <- lm(days ~ political_group, data = political) %>% 
      tidy(conf.int = TRUE) %>% 
      select(term, estimate, conf.low, conf.high, std.error) 
    
    # Map with the term and estimate.
    
    ggplot(lm3, aes(x = term, y = estimate)) +
      
      # Use the SE to get a lower and upper bound
      # for the estimate.
      
      geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error), width = 0.1) +
      geom_line() +
      geom_point() +
      
      # Clarify x-axis labels.
      
      scale_x_discrete(
        breaks = c("(Intercept)", "political_group1"),
        labels = c("No", "Yes")
      ) +
      
      # Adding info for viewer about the 
      # regression.
      
      labs(
        title = "Estimated Length on List (in Days) as Related to Ideologically Motivated Crime",
        subtitle = "Using a Linear Regression Showing Upper and Lower Confidence Intervals",
        x = "Ideological Motivation?",
        y = "Length on List (Days)"
      ) +
      theme_dark()
    
  })  
}



# Run the application 
shinyApp(ui = ui, server = server)

