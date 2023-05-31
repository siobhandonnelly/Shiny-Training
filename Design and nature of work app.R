# create app for FVA
# Installing the required packages
#install.packages(c("tidyverse", "readxl", "janitor", "plotly", "shiny"))

# Loading the required libraries
library(shiny)
library(tidyverse)
library(plotly)
library(usethis)
#
# Trying to load in data with nulls dealt with
nature_of_work <- read_csv("UKHSA dataset.csv", na = c("U", "Z0 - Missing data", "NA", "Not Known", "Not applicable", "-1", "*", ".", "", "NULL"))

# Define UI
ui <- fluidPage(
  titlePanel("Design and Nature of Work Score"),
  sidebarLayout(
    sidebarPanel(
      selectInput("provider", "Provider Name", choices = unique(nature_of_work$f_providername)),
      actionButton("update", "Update") ,
      selectInput("Year", "Acedemic Year", choices = unique(nature_of_work$f_zcohort)),
      actionButton("update", "Update")
      ),
    mainPanel(
      plotlyOutput("chart1"),
      plotlyOutput("chart2"),
      plotlyOutput("chart3")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    nature_of_work %>%
      filter(f_providername %in% input$provider) %>%
      mutate(
        f_zcohort = as.factor(f_zcohort),
        f_xwrk2020soc1 = factor(f_xwrk2020soc1, levels = c(
          "Managers, directors and senior officials",
          "Professional occupations",
          "Associate professional occupations",
          "Administrative and secretarial occupations",
          "Skilled trades occupations",
          "Caring, leisure and other service occupations",
          "Sales and customer service occupations",
          "Process, plant and machine operatives",
          "Elementary occupations"
        )),
        f_xwrk2007sic1 = factor(f_xwrk2007sic1, levels = c(
          "Agriculture, forestry and fishing",
          "Mining and quarrying",
          "Manufacturing",
          "Electricity, gas, steam and air conditioning supply",
          "Water supply; sewerage, waste management and remediation activities",
          "Construction",
          "Wholesale and retail trade; repair of motor vehicles and motorcycles",
          "Transportation and storage",
          "Accommodation and food service activities",
          "Information and communication",
          "Financial and insurance activities",
          "Real estate activities",
          "Professional, scientific and technical activities",
          "Administrative and support service activities",
          "Public administration and defence; compulsory social security",
          "Education",
          "Human health and social work activities",
          "Arts, entertainment and recreation",
          "Other service activities",
          "Activities of households as employers; undifferentiated goods-and services-producing activities of households for own use",
          "Activities of extraterritorial organisations and bodies"
        )),
        f_xempbasis = factor(f_xempbasis, levels = c(
          "On a permanent/open ended contract",
          "On a fixed-term contract lasting 12 months or longer",
          "On a fixed-term contract lasting less than 12 months",
          "Temping (including supply teaching)",
          "On a zero hours contract",
          "Volunteering",
          "On an internship",
          "Other",
          "Not known"
        )),
        f_providername = factor(f_providername)
      ) %>%
      mutate(study_mode = case_when(f_xqmode01 == "Part-time" ~ 2,
                                    f_xqmode01 == "Full-time" ~ 1)) %>%
      mutate(work_mean_num = case_when(f_wrkmean == "Strongly agree" ~ 5,
                                       f_wrkmean == "Agree" ~ 4,
                                       f_wrkmean == "Neither agree nor disagree" ~ 3,
                                       f_wrkmean == "Disagree" ~ 2,
                                       f_wrkmean == "Strongly disagree" ~ 1)) %>%
      mutate(work_skills_num = case_when(f_wrkskills == "Strongly agree" ~ 5,
                                         f_wrkskills == "Agree" ~ 4,
                                         f_wrkskills == "Neither agree nor disagree" ~ 3,
                                         f_wrkskills == "Disagree" ~ 2,
                                         f_wrkskills == "Strongly disagree" ~ 1)) %>%
      mutate(work_ontrack_num = case_when(f_wrkontrack == "Strongly agree" ~ 5,
                                          f_wrkontrack == "Agree" ~ 4,
                                          f_wrkontrack == "Neither agree nor disagree" ~ 3,
                                          f_wrkontrack == "Disagree" ~ 2,
                                          f_wrkontrack == "Strongly disagree" ~ 1)) %>%
      mutate(danow = (work_ontrack_num + work_skills_num + work_mean_num) / 3) %>%
      distinct(f_zanonymous, .keep_all = TRUE) %>%
      filter(study_mode == 1 | !is.na(work_ontrack_num) | !is.na(work_skills_num) | !is.na(work_mean_num))
  })
  
  output$chart1 <- renderPlotly({
    ndf_soc <- filtered_data() %>%
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
  
  output$chart2 <- renderPlotly({
    ndf_sic <- filtered_data() %>%
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
  
  output$chart3 <- renderPlotly({
    ndf_f_empbasis <- filtered_data() %>%
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
}

#now to figure out how to upload to Git
# Run the application
shinyApp(ui = ui, server = server)
