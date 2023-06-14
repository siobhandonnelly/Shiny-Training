# create app for FVA
# Installing the required packages
#install.packages(c("tidyverse", "readxl", "janitor", "plotly", "shiny"))

# Loading the required libraries
library(shiny)
library(tidyverse)
library(plotly)
library(usethis)
source("Data Manipulation.R")
source("Routing.R")
# Define UI
ui <- fluidPage(
  titlePanel("Design and Nature of Work Score"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
  ),  sidebarLayout(
    sidebarPanel(
      selectInput("provider", "Provider Name", choices = unique(nature_of_work$f_providername)),#creating a filter that will filter the dashboard by all of the providers currently in the dataset, it is set to only allow one selection at a time, it does not allow users to view all providers at once which is something I want to code
      actionButton("update", "Update") , # this creates an update button, not sure if this is needed
      selectInput("Year", "Academic Year", choices = unique(nature_of_work$f_zcohort)), # similar to the provider filter, this will filter the charts by the academic year
      actionButton("update2", "Update") # this creates an update button, not sure if this is needed
      ),
    mainPanel(
      
      tags$ul( # creating a series of links to the other pages that I want to hold on my app
        tags$li(a(href = route_link("/"), "Dashboard")),
        tags$li(a(href = route_link("Work"), "Work")),
        tags$li(a(href = route_link("Study"), "Study")),
        tags$li(a(href = route_link("Personal_Charecteristics"), "Personal_Charecteristics"))
      ),
      router$ui
  )
  )
)

# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    nature_of_work %>%
      filter(f_providername %in% input$provider) %>% #Allowing the data to be filtered by university provider
      filter(f_zcohort %in% input$Year) # Allowing the data to be filtered by academic year
      })
  #Creating Plot 1, which is the fairwork score by graduates SOC group
  output$chart1 <- renderPlotly({
    ndf_soc <- filtered_data() %>% #creating a subset of the dataframe that is grouped by SOC major group
      group_by( f_xwrk2020soc1) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    
    plot_ly(
      data = ndf_soc,
      x = ~ f_xwrk2020soc1,
      y = ~ mean_danow,
      color = ~ f_xwrk2020soc1,
      type = "bar",
      showlegend = FALSE,
      marker = list(color = '#1F4388')
    ) %>%
      layout(
        yaxis = list(title = "Mean Fairwork score"),
        xaxis = list(title = 'SOC group', tickvals = list("Managers, directors and senior officials", "Professional occupations", "Associate professional occupations", "Administrative and secretarial occupations", "Skilled trades occupations", "Caring, leisure and other service occupations", "Sales and customer service occupations", "Process, plant and machine operatives", "Elementary occupations")),
        title = "Mean Fairwork score by graduates SOC major group"
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  
  #Creating Plot 2, which is the fairwork score by graduates SIC group
  output$chart2 <- renderPlotly({
    ndf_sic <- filtered_data() %>% #creating a subset of the dataframe that is grouped by SIC major group
      group_by(f_xwrk2007sic1) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    
    plot_ly(
      data = ndf_sic,
      x = ~ f_xwrk2007sic1,
      y = ~ mean_danow,
      color = ~ f_xwrk2007sic1,
      type = "bar",
      showlegend = FALSE,
      marker = list(color = '#1F4388')
    ) %>%
      layout(
        yaxis = list(title = "Mean Fairwork score"),
        xaxis = list(title = 'SIC Group',tickvals = list("Agriculture, forestry and fishing", "Mining and quarrying", "Manufacturing", "Electricity, gas, steam and air conditioning supply" , "Water supply; sewerage, waste management and remediation activities", "Construction" , "Wholesale and retail trade; repair of motor vehicles and motorcycles" , "Transportation and storage", "Accommodation and food service activities" , "Information and communication" , "Financial and insurance activities", "Real estate activities", "Professional, scientific and technical activities", "Administrative and support service activities", "Public administration and defence; compulsory social security", "Education", "Human health and social work activities", "Arts, entertainment and recreation", "Other service activities", "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use", "Activities of extraterritorial organisations and bodies")),
        title = "Mean Fairwork score by graduates SIC major group"
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  #Creating Plot 3, which is the fairwork score by graduates employment basis
  output$chart3 <- renderPlotly({
    ndf_f_empbasis <- filtered_data() %>% #creating a subset of the dataframe that is grouped by SIC major group
      group_by( f_xempbasis) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    
    plot_ly(
      data = ndf_f_empbasis,
      x = ~ f_xempbasis,
      y = ~ mean_danow,
      color = ~ f_xempbasis,
      type = "bar",
      showlegend = FALSE,
      marker = list(color = '#1F4388')
    ) %>%
      layout(
        yaxis = list(title = 'Mean Fairwork score'),
        xaxis = list(title = 'Employment Basis',tickvals = list("On a permanent/open ended contract", "On a fixed-term contract lasting 12 months or longer","On a fixed-term contract lasting less than 12 months","Temping (including supply teaching)","On a zero hours contract","Volunteering","On an internship","Other","Not known")),
        title = "Mean Fairwork score by graduates employment basis") %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
 {router$server(input, output, session)}
}
# Run the application
shinyApp(ui = ui, server = server)
