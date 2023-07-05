#This R file details the routing applied to the app. The routing creates a set of three pages, which you can navigate by a set of tabs at the top of the page.  As the charts are held within these sub-pages, the code regarding the page layout is held here. 
#This next section covers the creation of the first sub-page, this will display automatically when the app is loaded
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
  titlePanel("Introduction"),
wellPanel(fluidRow(
  column(
    width = 12,
  HTML("
  <p>Creating fair and decent work opportunities is crucial for global prosperity. In the UK, there has been increased focus on the quality of employment in recent years, leading to a demand for statistics on employment quality. The Measuring Job Quality Working Group, established after the Taylor Review, has identified 18 measures of job quality across seven dimensions. Some of these measures, such as career progression, are now being collected through the Labour Force Survey overseen by the Office for National Statistics.</p>

<p>One important aspect of job quality is the design of the job and the nature of the work involved. This includes factors like skill utilization, opportunities for growth, and finding purpose in one's work. While higher education is managed independently in different parts of the UK, all four nations share the objective of helping graduates from diverse backgrounds find personal fulfillment and effectively apply their acquired skills in the job market. This aligns with the goal of improving employment quality.</p>

<p>However, there is currently limited quantitative data on the quality of work undertaken by graduates. HESA and the Graduate Outcomes survey are well positioned to collect this information for the higher education sector. We will focus on three survey questions that relate to the design and nature of work, which also align with UK higher education policy aspirations. These questions may help formulate a composite variable representing the design and nature of work.</p>

<p>For more information about our Design and Nature of Work score, please refer to our research publications available at: https://www.hesa.ac.uk/data-and-analysis/research?tid_1=876</p>"))
))
)
#This section covers the second sub-page, which contains charts that display the design and nature of fair work score by work characteristics. 
  work_page <- div(
  titlePanel("Work"),
  wellPanel(fluidRow(
    column(
      width = 6,
      HTML("<p>  </p> <p>  </p> <p>  </p> <p>  </p>
      <p>This page covers graduates' design and nature of work score, split by three work characteristics. Whether they required their qualification for their work, the standard occupational code of their job role, and their job contract length.</p>

      <p>From this, we can see that when graduates are employed in job roles that require their degree, they have a higher design and nature of work score. As agreement increases, so does their score.</p>

      <p>Graduates who are employed in SOC group 2 hold the highest design and nature of work score, with those in SOC group 1 coming in second. This may be linked to the qualification requirement mentioned above. Interestingly, graduates employed in SOC group 7 Caring, leisure and other caring activities, have the third highest score. This is likely due to the highly vocational nature of their work.</p>

      <p>Graduates who are in permanent/open-ended contracts also have the highest design and nature of work score. This likely represents the benefit that job security offers to graduates, which is then reflected in their score. In support of this, graduates who are employed on zero-hour contracts have the worst design and nature of work score, which indicates that greater job insecurity had a negative impact on their design and nature of work score.</p>")
    ),
    br(),
    column(plotlyOutput("chart2"), width = 6)
  )),
  wellPanel(fluidRow(
    column(plotlyOutput("chart1"), width = 6),
    column(plotlyOutput("chart3"), width = 6)
  ))
)
  #This section covers the third sub-page, which contains charts that display the design and nature of fair work score by graduates study characteristics. 
Study_page <- div(
  titlePanel("Study"),
  wellPanel(fluidRow(
    column( width = 6,
            HTML("<p>  </p> <p>  </p> <p>  </p> <p>  </p> <p>Graduates who achieved a first class degree show the highest design and nature of work score, with those who received a third class degree having the lowest design and nature of work score. This likely ties to our previous findings regarding whether a graduate required their qualifications for their work, as it is likely that graduates who achieved a higher class of degree, are more likely to be in employment that requires their qualification. </p>
<p>Similarly, graduates from higher level degrees, such as postgraduate research, and postgraduate taught have higher design and nature of work scores than those who graduated from first degrees or other degree types. This likely reflects those employed in more specialist employment sectors, who require specific degree attainment for employment. </p>
<p>Highly vocational courses, such as medicine & dentistry, veterinary science, and education have the highest design and nature of work score. This indicates that aspects other than salary are impacting how graduates view their work. This can be further seen, as subject such as Law and Economics, two subjects often associated with high salaries, score lower on design and nature of work score. As these are areas that often require higher education qualifications, and offer higher salaries, it indicates that another driver is impacting the design and nature of work score of graduates.</p>")
  ),
  br(),
    column(plotlyOutput("chart7"), width = 6)
  )),
  wellPanel(fluidRow(
    column(plotlyOutput("chart8"), width = 6),
    column(plotlyOutput("chart9"), width = 6)
  ))
)

#This section dictates the link between the tabs and the associated sub-page
router <- make_router(
  route("/", home_page),
  route("Work", work_page),
  route("Study", Study_page)
)