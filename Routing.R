

home_page <- div(
  tags$style(
    HTML(
      ".blue-box {
        background-color:  #1F4388;
        color: white;
        padding: 10px;
        border-radius: 5px;
      }"
    )
  ),
  titlePanel("Dashboard"),
  wellPanel(fluidRow(
  column(div(verbatimTextOutput("meanOutput"),style = "background-color:  #1F4388; color: white; padding: 10px; border-radius: 5px;"),width = 4),
  
  column(div(verbatimTextOutput("meansalaryOutput"), style = "background-color:  #1F4388; color: white; padding: 10px; border-radius: 5px;"), width = 4),
  column(div(verbatimTextOutput("proportionOutput"), style = "background-color:  #1F4388; color: white; padding: 10px; border-radius: 5px;"), width = 4)
)),
wellPanel(fluidRow(
  column(p("It is recognised that achieving prosperity across the globe requires the establishment of fair and decent work for all. Over the past six years in the UK, this matter has grown in prominence both nationally and within the devolved administrations, leading to increased demand for statistics relating to employment quality. The Measuring Job Quality Working Group – formed following the publication of the Taylor Review – has identified a total of eighteen measures of job quality, which span seven broad dimensions. Data on some of these measures (e.g. career progression) are now being gathered in the UK through the Labour Force Survey, which is overseen by the Office for National Statistics.

One of the seven facets relates to the design of the job and the nature of the work undertaken. This element incorporates aspects such as skill use, progression opportunities and the extent to which one’s employment provides a sense of purpose. While higher education is a devolved issue within the UK, all four nations have a shared objective in wanting graduates from all backgrounds to achieve personal fulfilment after qualifying, as well as being able to effectively utilise the skills they acquire through study in the labour market. These policy goals therefore align closely with the aforementioned element of employment quality.

However, the range of quantitative data about the quality of work undertaken by graduates is currently limited. After illustrating why HESA and the Graduate Outcomes survey are well placed to collect this information for the higher education sector, we highlight how three questions in the survey at present fit within the design/nature of work component of job quality and are also in line with the aspirations of UK higher education policy.[1] Following the recommendations of the Measuring Job Quality Working Group, we explore whether a composite variable (relating to the design/nature of work) can be formulated from these three survey questions."), width = 12)
))
)
  work_page <- div(
  titlePanel("Work"),
  wellPanel(fluidRow(
    column(selectInput("Sex", "Graduate Sex", choices = unique(nature_of_work$f_sexid)), width = 12)
  )),
  wellPanel(fluidRow(
    column(p("p creates a paragraph of text."), width = 6),
    br(),
    column(plotlyOutput("chart1"), width = 6)
  )),
  wellPanel(fluidRow(
    column(plotlyOutput("chart2"), width = 6),
    column(plotlyOutput("chart3"), width = 6)
  ))
)

Study_page <- div(
  titlePanel("Study"),
  wellPanel(fluidRow(
    column(p("This is a study page"), width = 6),
    br(),
    column(plotlyOutput("chart7"), width = 6)
  )),
  wellPanel(fluidRow(
    column(plotlyOutput("chart8"), width = 6),
    column(plotlyOutput("chart9"), width = 6)
  ))
)

Personal_Characteristics_page <- div(
  titlePanel("Personal_Characteristics"),
  wellPanel(fluidRow(
    column(p("This is a Personal characteristics"), width = 6),
    br(),
    column(plotlyOutput("chart4"), width = 6)
  )),
  wellPanel(fluidRow(
    column(plotlyOutput("chart5"), width = 6),
    column(plotlyOutput("chart6"), width = 6)
  ))
)

router <- make_router(
  route("/", home_page),
  route("Work", work_page),
  route("Study", Study_page),
  route("Personal_Characteristics", Personal_Characteristics_page)
)