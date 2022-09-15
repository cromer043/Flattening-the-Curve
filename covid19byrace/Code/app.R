##### Load Pacakges #####
library(shiny)
library(shinythemes)
library(plotly)
library(readr)
library(dplyr)
library(ggplot2)
library(tools)


##### Import Data #####
# setwd("./covid19byrace") # For debugging only
covid_acs <- read_csv("./data/covid_acs.csv")
acs_map_data_for_ratios <- read_csv("./data/acs_map_data_for_ratios.csv")
acs_map_population_percents <- read_csv("./data/acs_map_population_percents.csv")


data_1cr <- read.csv("./data/datacr.csv")
data_2cr <-read.csv("./data/data_2cr.csv")
datacr <- inner_join(data_1cr,data_2cr) %>%
  mutate(State.Name = toTitleCase(tolower(as.character(State.Name)))) %>% 
  mutate(Ratio = round(Ratio, digits = 2)) %>% 
  mutate(Percent = round(Percent, digits =2 ))



# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


##### SHINY UI #####
ui <- navbarPage(theme = shinytheme("flatly"),

    # Title
    "COVID-19 by Race",
    
    ##### Situation Tab #####
    tabPanel(
      "Project Description",
      titlePanel(h1("Mapping Racial and Ethnic Differences with COVID-19", align = "center")),
      p(h3("By Shawn Meepagala and Carl Romer", align = "center")),
      fluidRow(column(1),
               column(10,
                      p(h5("Under normal circumstances, the inequities of American society are seen as a long-term problem that may be dealt with in the next administration. However, as COVID-19 has spread, it has not spread amongst the population evenly. These pre-existing inequalities have given rise to a distinct impact in COVID-19 cases and deaths on racial minorities. In order to prevent future pandemics from having particular impact on Black, Hispanic, and Native American communities, policy makers must tackle the systemic poverty, lack of healthcare, and food poverty from which these disparities arise. COVID-19 has made it clear that policy makers must not wait to address these issues.",
                           align = "justify"),
                        h5("This project attempts to map where COVID-19 related cases and deaths have hit underserved minority communities particularly hard while showing where preexisting conditions for those communities have been allowed to grow. Hopefully, mapping these inequalities will allow policy makers to build a more paritable society. 
", align = "justify"))))
    ),
    
    
    ##### Cases/Deaths Map Tab #####
    tabPanel(
        # Tab name and page title
        "Cases/Deaths Map",
        titlePanel("COVID-19 Map by Race/Ethnicity"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                # Select race/ethnicity
                selectInput(
                    "race_tabcases", h5("Race/Ethnicity"), 
                    choices = acs_map_data_for_ratios %>% 
                           select(Category) %>% unique() %>% as.list(),
                    multiple = FALSE
                    ),
                
                # Select cases/deaths
                selectInput(
                    "cases_or_deaths_tabcases", h5("Cases or Deaths"), 
                    choices = c("Cases", "Deaths"), 
                    selected = "Cases", multiple = FALSE
                    )#,
                
                # # Select colorbar max ratio
                # uiOutput("colorbarmax_tabcases")
                ),

        # Show a plot of the generated distribution
            mainPanel(
                htmlOutput("map_tabcases_title"),
                plotlyOutput("usmap_tabcases")
                )
        ),
        fluidRow(
          column(5),
          column(6, p("Sources:",
                      a("Census American Community Survey",
                        href ="https://data.census.gov/cedsci/"), "2018 5-Year Estimates ,", 
                      a("The COVID Tracking Project", 
                        href = "https://covidtracking.com/race"), "(Racial Data Tracker Dataset)", 
                      align = "right")#,
                 )
          ),
        fluidRow(column(4),
                 column(7,
                        p("Notes:"),
        
        p(h5("1. The ratio shown in the map is the percent of the cases or deaths of the selected race divided by the percent of the population of the race. For example, the ratio in Alabama for white cases is the percent of COVID-19 cases that have been identified as white divided by the percent of the Alabama population that identifies as white.", align = 'justify')),
        p(h5("2. The color scale adapts based upon the selected ratio",align = "justify")),
        p(h5("3. The percent populations of a particular race/ethnicity category for each state are calculated from the percent of all people in each state who fit that particular race/ethnicity category. For example, the percent white in Alabama is calculated as the percentage of the people in Alabama who would be able to categorize themselves as white (including people with two or more races).", align = "justify")),
       p(h5("4. Missing case/death data may be a result of inconsistencies in state reporting or cases/deaths with unkown race/ethnicity.")),
         p(h5("5. COVID-19 case and death data is as current as ", strong("April 29, 2020")))))
        
    ),
    
    ##### State Cases/Deaths Tab #####
    tabPanel(
      # Tab name and page title
      "State Cases/Deaths",
      titlePanel("Cases/Deaths by State"),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          # Select race/ethnicity
          selectInput(
            "state_tabcasesbar", h5("State"), 
            choices = acs_map_data_for_ratios %>% 
              select(NAME) %>% unique() %>% as.list(),
            multiple = FALSE
          ),
          
          # Select cases/deaths
          selectInput(
            "cases_or_deaths_tabcasesbar", h5("Cases or Deaths"), 
            choices = c("Cases", "Deaths"), 
            selected = "Cases", multiple = FALSE
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          htmlOutput("barchart_tabcases_title"),
          plotlyOutput("barchart_tabcases")
        )
      ),
      fluidRow(
        column(5),
        column(6, 
               p("Sources:",
                 a("Census American Community Survey",
                   href ="https://data.census.gov/cedsci/"), "2018 5-Year Estimates ,", 
                 a("The COVID Tracking Project", 
                   href = "https://covidtracking.com/race"), "(Racial Data Tracker Dataset)", 
                 align = "right")
               )
      ),
      fluidRow(column(4),
               column(7,
                      p("Notes:"),
                      p(h5("1. The ratios shown in the graph is the percent of the cases or deaths of the displayed race divided by the percent of the population of the race. For example, the ratio in Alabama for white cases is the percent of COVID-19 cases that have been identified as white divided by the percent of the Alabama population that identifies as white.", align = 'justify')),
                      p(h5("2. The percent populations of a particular race/ethnicity category for each state are calculated from the percent of all people in each state who fit that particular race/ethnicity category. For example, the percent white in Alabama is calculated as the percentage of the people in Alabama who would be able to categorize themselves as white (including people with two or more races).", align = "justify")),
                      p(h5("3. The dotted line signifies proportional case/death distribution, any bar above the line signifies overproportion and any bar below the line signfies underproportion.")),
                      p(h5("4. Missing case/death data may be a result of inconsistencies in state reporting or cases/deaths with unkown race/ethnicity.")),
                      p(h5("5. COVID-19 case and death data is as current as ", strong("April 29, 2020")))))
      
      
    ),
    
    ##### Risk Factors Map Tab #####
    tabPanel(
      # Tab name and page title
      "Risk Factors Map",
      titlePanel("Preexisting Conditions by Race"),
      # Show a plot of the generated distribution
      sidebarLayout(
        sidebarPanel(
          selectInput("condition_selected", h5("Preexisting Condition"), 
                      # Set 
                      choices = datacr %>% 
                        select(Condition) %>% 
                        arrange(Condition) %>% 
                        unique() %>% 
                        as.list(), 
                      # Set default value
                      selected = "Asthma",
                      # One value at a time
                      multiple = FALSE),
          selectInput("race_selected_map", h5("Race/Ethnicity"), 
                      # Set 
                      choices = datacr %>% 
                        select(Race) %>% 
                        arrange(Race) %>% 
                        unique() %>% 
                        as.list(), 
                      # Set default value
                      selected = "American Indian or Alaska Native",
                      # One value at a time
                      multiple = FALSE)
        ),
        
        mainPanel(
          htmlOutput("current_race"),
          plotlyOutput("raceMap")
          
        )
        ),
      fluidRow(
        column(5), 
        column(6, 
               p(h5("Sources: Census American Community Survey 2018 5-Year Estimates via",
                    a("IPUMS",href = "https://ipums.org/"),", 2018 Behavioral Risk Factor Surveillance System Survey via the", 
                    a("CDC", href="https://www.cdc.gov/brfss/annual_data/annual_2018.html", align = "right")
                    )#,
                 )
      ),
      fluidRow(column(4),
               column(7,
               p("Notes:"),
               p(h5("1. The ratio is the percent of the selected condition of the selected race divided by the percent of the population of the race. For example, the white asthma ratio in Alabama is the percent of Alabamians with asthma that identify as white divided by the percent of the population that identifies as white")),
               p(h5("2. The color scale adapts based upon the selected ratio.",align = "justify"))
               )
               ))
   ),

##### State Risk Factors Tab #####
tabPanel("State Risk Factors", 
         titlePanel("Risk Factors by State"),
         sidebarLayout(
           sidebarPanel(
             selectInput("state_selected", h5("State"), 
                         # Set 
                         choices = datacr %>% 
                           select(State.Name) %>% 
                           arrange(State.Name) %>% 
                           unique() %>% 
                           as.list(), 
                         # Set default value
                         selected = "Alabama",
                         # One value at a time
                         multiple = FALSE),
             selectInput("race_selected", h5("Race/Ethnicity"), 
                         # Set 
                         choices = datacr %>% 
                           select(Race) %>% 
                           arrange(Race) %>% 
                           unique() %>% 
                           as.list(), 
                         # Set default value
                         selected = "American Indian or Alaska Native",
                         # One value at a time
                         multiple = FALSE)),
           mainPanel(
             htmlOutput("state_selected"),
             plotlyOutput("raceBars")
           )
         ),
         fluidRow(
           column(5),
           column(6, 
                  p(h5("Sources: Census American Community Survey 2018 5-Year Estimates via",  
                       a("IPUMS",href = "https://ipums.org/"),", 2018 Behavioral Risk Factor Surveillance System Survey via the", 
                       a("CDC", href="https://www.cdc.gov/brfss/annual_data/annual_2018.html", align = "right"
                         )
                       )#,
         )),
         fluidRow(column(4),
                  column(7,
                         p("Notes:"),
                         p(h5('1. The percent population by race and condition is the percent of the population with the condition that identifies as that race, for example, the "percent white in Alabama with asthma" is calculated as the percentage of the people in Alabama with asthma who identify themselves as white')),
                         p(h5("2. The dotted line is the percent of the state's population that identify as the selected race/ethnicity."))
                          )
                  )
         )
         ),
##### Interpretation Tab #####
tabPanel(
    "Interpretation",
    fluidRow(column(1),
             
             column(10,
                    h3("Some Final Thoughts:", align = "left"),
                    h5( "Racial and ethnic minorities are facing disproportionate cases and deaths due to COVID-19. However, there are inconsistencies in state COVID-19 reporting, such as the multitude of cases with unknown racial/ethnic classification and differences in racial categorization schemes, along with inconsistencies in the beta-released Racial Data Tracker. These inconsistencies make it difficult to make strong claims about effects on specific communities (primarily the American Indian/Alaska Native and Native Hawaiian/Pacific Islander communities). As we continue to fight this pandemic, Congress must demand that states consistently report COVID-19 demographic data.", align = "justify"), 
                    h5("There are a wide range of outcomes for preexisting conditions between states. For states attempting to prevent COVID-19 or future pandemics from taking a heavier toll on vulnerable communities, they must address the inequitable social structures that lead to imbalanced health outcomes within their states. This cannot be a one size fits all model as each state combats different health disparities.", 
                       align = "justify")
                    )
             )
    )
)
 


##### SHINY SERVER #####
server <- function(input, output) {
  ##### Situation Tab #####
    
    
  ##### Cases/Deaths Barchart Tab #####
  output$barchart_tabcases_title <- renderText(
  HTML((paste0("<h3>",input$state_tabcasesbar, " ", input$cases_or_deaths_tabcasesbar, " Ratios","</h3>"))))
        
  togglecategory_bar <- reactive({
    switch(input$cases_or_deaths_tabcasesbar,
           "Cases" = "CasesRatio",
           "Deaths" = "DeathsRatio")
  })
  
  # Reactive ratio map dataset
  ratio_barchart_data <- reactive({
    covid_acs %>%
      filter(NAME == input$state_tabcasesbar) %>% 
      mutate(Category = case_when(
        Category == "American Indian or Alaska Native" ~ "AIAN",
        Category == "Native Hawaiian or Pacific Islander" ~ "NHPI",
        TRUE ~ Category
      )) %>% 
      mutate(display = .[[togglecategory_bar()]]) %>% 
      mutate(flavortext_ratio = case_when(
        togglecategory_bar() == "CasesRatio" ~ paste0('<b>', NAME, '</b><br>',
                                                'Race/Ethnicity: ', Category, '<br>',
                                                'Case Ratio: ', CasesRatio, '<br>'),
        togglecategory_bar() == "DeathsRatio" ~ paste0('<b>', NAME, '</b><br>',
                                                'Race/Ethnicity: ', Category, '<br>',
                                                'Death Ratio: ', DeathsRatio, '<br>')
        )
      )
  })
  
  # Barchart of state ratios
  output$barchart_tabcases <- renderPlotly({
    (
      ggplot(data = ratio_barchart_data(),
             aes(x = Category, y = display,
                 text = flavortext_ratio)) +
        geom_col(aes(fill = Category)) +
        geom_hline(yintercept = 1,
                   linetype="dotted") +
        labs(x = "Race/Ethnicity",
             y = paste("Ratio of %", 
                               input$cases_or_deaths_tabcasesbar,
                               "to % State Population<sup>1</sup>" ),
             caption = "<a href=\"https://data.census.gov/cedsci/\">Census American Community Survey</a> 2018 5-Year Estimates, <a href=\"https://covidtracking.com/race\">TheCOVID Tracking Project</a>")+
        theme_classic()
      ) %>% 
      ggplotly(tooltip = "text") %>% 
      layout(showlegend = FALSE)
      
    
  })
  
  
  
  ##### Cases/Deaths Tab #####
  # Reactive ratio to display
  togglecategory <- reactive({
    switch(input$cases_or_deaths_tabcases,
           "Cases" = "CasesRatio",
           "Deaths" = "DeathsRatio")
  })
    
  # Reactive ratio map dataset
  ratio_map_data <- reactive({
    covid_acs %>%
      filter(Category == input$race_tabcases) %>% 
      mutate(display = .[[togglecategory()]]) %>% 
      mutate(flavortext_ratio = case_when(
        togglecategory() == "CasesRatio" ~ paste0('<b>', NAME, '</b><br>',
                                                'Race/Ethnicity: ', Category, '<br>',
                                                'Case Ratio: ', CasesRatio, '<br>'),
        togglecategory() == "DeathsRatio" ~ paste0('<b>', NAME, '</b><br>',
                                                'Race/Ethnicity: ', Category, '<br>',
                                                'Death Ratio: ', DeathsRatio, '<br>')
      )
               )
  })
    
  output$map_tabcases_title <-  renderText(
    HTML(paste0("<h3>", input$cases_or_deaths_tabcases, 
                " Ratio Map", "</h3>", 
                "<h4>", input$race_tabcases,"</h4>"))
  )
  
  # Cases/deaths ratio map
  output$usmap_tabcases <- renderPlotly({
        fig <-
        plot_geo(
          ratio_map_data(),
          locationmode = 'USA-states') %>%
        add_trace(
          z = ~display,
          color = ~display,
          locations = ~abb,
          text = ~flavortext_ratio,
          colorscale="Reds",
          hovertemplate = paste0(
            '%{text}', '<extra></extra>')
          ) %>%
        layout(geo = g) %>%
        colorbar(title = paste("Ratio of %", 
                               input$cases_or_deaths_tabcases,
                               "\nto % State Population<sup>1</sup>"))
    })
    
    
  ##### Risk Factors Tabs #####
  barchart_state_risk_data <- reactive({
    datacr %>% 
      filter(State.Name == input$state_selected) %>% 
      filter(Race == input$race_selected)
  })
  
  overall_state_risk <- reactive ({
    (datacr %>% 
     filter(State.Name == input$state_selected) %>% 
     filter(Race == input$race_selected) %>% 
     select(Overall) %>% distinct())[[1]]
      
    })
  
  # Display text to check state selected
  output$state_selected <- renderText({
    HTML(paste0("<h3>",input$state_selected, " Risk Factors", "</h3>","<h4>", "(", input$race_selected,")","</h4>"))
  })
  
  output$raceBars <- renderPlotly({
    p <-
      (   # Make ggplot graph
        ggplot(data = barchart_state_risk_data()) +
          # geom_col
          geom_col(aes(x = Condition, y = Percent,
                       fill = Condition,
                       text = paste0(
                                 '<b>', State.Name, '</b><br>',
                                'Condition: ', Condition, '<br>',
                                'Percent: ', Percent, '%', '<br>'))) +
          geom_hline(yintercept = overall_state_risk(),
                     linetype="dotted",
                     text = paste0("State Percent")) +
          annotate("text",x = 1:7,  y = overall_state_risk(), 
                   label = paste("Percent of", input$state_selected,
                                 "Population\nwho",
                                 "identify themselves as", input$race_selected), 
                   color = NA) +
          labs(y = paste0("Percent ", input$race_selected,"<sup>1</sup>")) +
          theme_classic()
      ) %>% 
      # Convert to plolty and display hovertext
      ggplotly(tooltip = "text") %>%
      layout(showlegend = FALSE)          
  })
  
  output$current_race <- renderText({
    HTML(paste0("<h3>", "Ratio of ", 
                input$condition_selected, " Population to State Population", "</h3>","<h4>", "(",input$race_selected_map,")","</h4>"))
    
  })
  
  # Reactive  dataset
  map_race_data <- reactive({
    datacr %>%
      filter(Race == input$race_selected_map) %>% 
      filter(Condition == input$condition_selected)
  })
  
  # Set colorbar title
  colorbar_title <- reactive({
    paste(input$condition_selected, "Ratio<sup>1</sup>")
  })
  
  # Create map
  output$raceMap <- renderPlotly({
    fig <-
      plot_geo(
        map_race_data(),
        locationmode = 'USA-states') %>%
      add_trace(
        z = ~Ratio,
        color = ~Ratio,
        locations = ~`STATE.`,
        text = ~paste0(
                    '<b>', State.Name, '</b><br>',
                    'Condition: ', Condition, '<br>',
                    'Ratio: ', Ratio, '<br>'),
        colorscale="Reds",
        hovertemplate = paste0(
          '%{text}', '<extra></extra>')
      ) %>%
      layout(geo = g) %>%
      colorbar(title = colorbar_title())
  })
    
}

##### SHINY RUN APP #####
shinyApp(ui = ui, server = server)
