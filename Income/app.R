#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

tidy_all <- read_rds("tidy_all.rds")

axis_vars <- c(
  "Mean Household Income" = "hhinc_mean2000",
  "Mean Commute Time" = "mean_commutetime2000",
  "Share of College Graduates in 2010" = "frac_coll_plus2010",
  "Share of College Graduates in 2000" = "frac_coll_plus2000",
  "Share of Foreign Born Residents" = "foreign_share2010",
  "Median Household Income in 2016" = "med_hhinc2016",
  "Median Household Income in 1990" = "med_hhinc1990",
  "Share of Individuals Below Poverty Line in 2010" = "poor_share2010",
  "Share of Individuals Below Poverty Line in 2000" = "poor_share2000",
  "Share of Individuals Below Poverty Line in 1990" = "poor_share1990",
  "Share of Black Residents in 2010" = "share_black2010",
  "Share of Hispanic Residents in 2010" = "share_hisp2010",
  "Share of Asian Residents in 2010" = "share_asian2010",
  "Share of Black Residents in 2000" = "share_black2000",
  "Share of White Residents in 2000" = "share_white2000",
  "Share of Hispanic Residents in 2000" = "share_hisp2000",
  "Share of Asian Residents in 2000" = "share_asian2000",
  "Mean 3rd Grade Math Test Scores" = "gsmn_math_g3_2013",
  "Median Rent for Two Bedroom Housing" = "rent_twobed2015",
  "Share of Single Parents in 2010" = "singleparent_share2010",
  "Share of Single Parents in 1990" = "singleparent_share1990",
  "Share of Single Parents in 2000" = "singleparent_share2000",
  "Share of Workers with a Travel Time Below 15 Minutes" = "traveltime15_2010",
  "Rate of Employment" = "emp2000",
  "Rate of Responses to 2010 Census" = "mail_return_rate2010",
  "Share of Nonwhite Residents" = "nonwhite_share2010",
  "Number of Residents/Square Mile in 2010" = "popdensity2010",
  "Number of Residents/Square Mile in 2000" = "popdensity2000",
  "Annual Job Growth Rate" = "ann_avg_job_growth_2004_2013",
  "Number of Jobs/Square Mile" = "job_density_2013"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  titlePanel("Income Explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             selectInput("xvar", "X-axis:", axis_vars),
             checkboxGroupInput("race", "Race:", c("Black" = "black", 
                                                   "White" = "white", 
                                                   "Hispanic" = "hispanic"), 
                                selected = c("black", "white", "hisp")),
             checkboxGroupInput("gender", "Gender:", c("Male" = "male", 
                                                       "Female" = "female"),
                                selected = c("male", "female")),
             sliderInput("alpha", "Opacity of points:",
                         0, 1, 0.5, step = .05)
           )
    ),
    column(9,
           h4(
             span(textOutput("title"), 
                  "vs. Mean Income Rank of Children Whose Parents Were at the 25th Percentile")
           ),
           plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    results <- tidy_all %>%
      filter(metric_type == "income") %>%
      filter(race %in% input$race) %>%
      filter(gender %in% input$gender) %>%
      mutate(value = value * 100)
    
    ggplot(results, aes_string(x = input$xvar, y = "value", color = "race", shape = "gender")) +
    geom_point(alpha = input$alpha) +
    labs(x = names(axis_vars)[axis_vars == input$xvar], y = "Mean Income Rank for Children Whose Parents Were at 25th Percentile")
  })
  
  output$title <- renderText({ names(axis_vars)[axis_vars == input$xvar] })
}

# Run the application 
shinyApp(ui = ui, server = server)

