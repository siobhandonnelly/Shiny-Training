library(shiny)
library(shiny.router)

home_page <- div(
  titlePanel("Dashboard"),
  p("This is a dashboard page")
)

work_page <- div(
  titlePanel("Work"),
  wellPanel( fluidRow(
    column( p("p creates a paragraph of text."), width = 6), # this is a placeholder for text surrounding the design and nature of work score, for the landing page this will be generic text around the measure, whilst ton the secondary pages it will be more tailored to what is being shown in the charts
    br(),
    column(plotlyOutput("chart1"), width = 6) # specifying that plot 1 should be what is held in the top right hand corner of the app
  )),
  wellPanel( fluidRow(
    column(plotlyOutput("chart2"), width = 6),
    column(plotlyOutput("chart3"), width = 6)
  ))
)

Study_page <- div(
  titlePanel("Study"),
  wellPanel( fluidRow(
    column( p("This is a study page"), width = 6), # this is a placeholder for text surrounding the design and nature of work score, for the landing page this will be generic text around the measure, whilst ton the secondary pages it will be more tailored to what is being shown in the charts
    br(),
    column(plotlyOutput("chart7"), width = 6) # specifying that plot 1 should be what is held in the top right hand corner of the app
  )),
  wellPanel( fluidRow(
    column(plotlyOutput("chart8"), width = 6),
    column(plotlyOutput("chart9"), width = 6)
  ))
)

Personal_characteristics_page <- div(
  titlePanel("Personal_characteristics"),
  wellPanel( fluidRow(
    column( p("This is a Personal characteristics"), width = 6), # this is a placeholder for text surrounding the design and nature of work score, for the landing page this will be generic text around the measure, whilst ton the secondary pages it will be more tailored to what is being shown in the charts
    br(),
    column(plotlyOutput("chart4"), width = 6) # specifying that plot 1 should be what is held in the top right hand corner of the app
  )),
  wellPanel( fluidRow(
    column(plotlyOutput("chart5"), width = 6),
    column(plotlyOutput("chart6"), width = 6)
  ))
)

router <- make_router(
  route("/", home_page),
  route("Work", work_page),
  route("Study", Study_page),
  route("Personal_characteristics", Personal_characteristics_page)
)