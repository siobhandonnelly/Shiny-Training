source("Data Manipulation.R") #run the Data manipulation R File
source("Routing.R") # Run the routing R  file
# Define UI
ui <- fluidPage(
  titlePanel("Dashboard for HESA's Design and Nature of Work Score"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
  ),  
  mainPanel(
    wellPanel(fluidRow( #creating the main body panel, where we show the three boxes with mean figures, as well as titles.
      column( width = 3,
        div(tags$h3(style = "font-size: 12px;",  "Design & Nature of Work" )),
        div(verbatimTextOutput("meanOutput"),  style = "background-color: #1F4388; color: white; padding: 10px; border-radius: 5px;")),
      column(  width = 3, div(  tags$h3(  style = "font-size: 12px;",   "Pay & Benefits"  )),
        div(verbatimTextOutput("meansalaryOutput"),     style = "background-color: #1F4388; color: white; padding: 10px; border-radius: 5px;")),
      column( width = 6,div(tags$h3(style = "font-size: 12px;", "Terms of Employment" )),
        div(verbatimTextOutput("proportionOutput"),style = "background-color: #1F4388; color: white; padding: 10px; border-radius: 5px;")
      )
    ))
  ),
        wellPanel(fluidRow( #Creating the academic year filter
        column(selectInput("Year", "Academic Year", choices = unique(nature_of_work$f_zcohort)), width = 12))),
      wellPanel(fluidRow( #creating the filters for Sex, Ethnicity, and Disability
        column(selectInput("Sex", "Graduate Sex", choices = unique(nature_of_work$f_sexid)), width = 4),
        column(selectInput("Ethnicity", "Graduate Ethnicity", choices = unique(nature_of_work$f_xethnic01)), width = 4),
        column(selectInput("Disability", "Graduate Disability Status", choices = unique(nature_of_work$f_zstudis_marker)), width = 4)
      )),
  
      tags$ul( # creating a series of links to the other pages that I want to hold on my app
        tags$li(a(href = route_link("/"), "Dashboard")),
        tags$li(a(href = route_link("Work"), "Work")),
        tags$li(a(href = route_link("Study"), "Study"))
      ),
      router$ui
  )
  

# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    nature_of_work %>%
    filter(f_zcohort %in% input$Year) %>%  # Allowing the data to be filtered by academic year, sex, ethnicity, and disability
    filter(f_sexid %in% input$Sex) %>% 
    filter(f_xethnic01 %in% input$Ethnicity) %>% 
    filter(f_zstudis_marker %in% input$Disability)
           })
  output$meanOutput <- renderText({ #defining the Mean fairwork score output
    mean_score <- mean(nature_of_work$danow, na.rm = TRUE)
    mean_score_formatted <- sprintf("%.2f", mean_score)
    paste("Mean Fairwork Score: ", mean_score_formatted)
  })
  
  output$meansalaryOutput <- renderText({ #defining the mean salary output 
    mean_salary <- mean(nature_of_work$salary, na.rm = TRUE)   
    mean_salary_formatted <- sprintf("%.2f", mean_salary)
    paste("Mean Salary: ", mean_salary_formatted)
  })
  
  output$proportionOutput <- renderText({ #creating an output to show the proportion of population in a  permanent/open ended contract

    category_counts <- table(nature_of_work$f_xempbasis)  
    category_A_count <- category_counts["On a permanent/open ended contract"]  
    total_count <- sum(category_counts)
    proportion_A <- category_A_count / total_count
    mean_proportion_A_formatted <- sprintf("%.2f", proportion_A)
    paste("Proportion of Graduates on a permanent/open ended contract: ", mean_proportion_A_formatted)
  })
  
  
  #Creating Plot 1, which is the fairwork score by graduates SOC group
  output$chart1 <- renderPlotly({
    ndf_soc <- filtered_data() %>% 
      group_by(f_xwrk2020soc1) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
       ndf_soc$f_xwrk2020soc1 <- factor(ndf_soc$f_xwrk2020soc1, levels = ndf_soc$f_xwrk2020soc1[order(ndf_soc$mean_danow)])
    
    plot_ly(
      data = ndf_soc,
      x = ~mean_danow,
      y = ~f_xwrk2020soc1,
      color = ~f_xwrk2020soc1,
      type = "scatter",
      mode = "lines",
      showlegend = FALSE,
      marker = list(symbol = "circle", size = 10, color = '#1F4388')
    ) %>%
      add_segments(
        x = ~4,
        xend = ~mean_danow,
        y = ~f_xwrk2020soc1,
        yend = ~f_xwrk2020soc1,
        color = ~f_xwrk2020soc1,
        mode = "lines",
        line = list(width = 1),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = 'SOC group'),
        xaxis = list(title ="Mean Fairwork score" ),
        title = "Mean Fairwork score by graduates SOC major group",
        hovermode = "closest"
      ) %>%
      hide_colorbar() %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        hovertemplate = paste(
          "<b>SOC Group:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      suppressWarnings()
  })
  
  #Creating Plot 2, which is the fairwork score by graduates qualifications required variable 
  output$chart2 <- renderPlotly({
    ndf_qualreq <- filtered_data() %>% 
      group_by(f_xwrkqualreq) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    ndf_qualreq$f_xwrkqualreq <- factor(ndf_qualreq$f_xwrkqualreq, levels = ndf_qualreq$f_xwrkqualreq[order(ndf_qualreq$mean_danow)])
   
      plot_ly(
      data = ndf_qualreq,
      x = ~mean_danow,
      y = ~f_xwrkqualreq,
      color = ~f_xwrkqualreq,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(symbol = "circle", size = 10, color = '#1F4388')
    ) %>%
      add_segments(
        x = 2,
        xend = ~mean_danow,
        y = ~f_xwrkqualreq,
        yend = ~f_xwrkqualreq,
        color = ~f_xwrkqualreq,
        mode = "lines",
        line = list(width = 1),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = 'Qualifications required',
                         ticktext = list("Yes: both the level and subject of qualification …", " Yes: the level of qualification …", " Yes: the subject of the qualification …", " Yes: Not a requirement, it did give me an advantage..", " No: the qualification was not required", " Don't know"), 
    tickvals = list("Yes: both the level and subject of qualification was a formal requirement","Yes: the level of qualification was a formal requirement", "Yes: the subject of the qualification was a formal requirement", "Yes: while the qualification was not a formal requirement it did give me an advantage","No: the qualification was not required", "Don't know"),
    tickmode = "array"),
         xaxis = list(title = "Mean Fairwork score"),
        title = "Mean Fairwork score by Qualifications required",
        hovermode = "closest"
      ) %>%
      hide_colorbar() %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        hovertemplate = paste(
          "<b>Qualifications required:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      suppressWarnings()
  })

  
 #Creating Chart 3 - Fairwork score by employment basis
  output$chart3 <- renderPlotly({
    ndf_f_empbasis <- filtered_data() %>% 
      group_by(f_xempbasis) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    ndf_f_empbasis$f_xempbasis <- factor(ndf_f_empbasis$f_xempbasis, levels = ndf_f_empbasis$f_xempbasis[order(ndf_f_empbasis$mean_danow)])
    
    plot_ly(
      data = ndf_f_empbasis,
      x = ~mean_danow,
      y = ~f_xempbasis,
      color = ~f_xempbasis,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(size = 10, color = '#1F4388')
    ) %>%
      add_segments(
        x = 2,
        xend = ~mean_danow,
        y = ~f_xempbasis,
        yend = ~f_xempbasis,
        color = ~f_xempbasis,
        mode = "lines",
        line = list(width = 1),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = 'Employment Basis'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates employment basis",
        hovermode = "closest"
      ) %>%
      hide_colorbar() %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        hovertemplate = paste(
          "<b>Employment Basis:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      suppressWarnings()
  })
  #Creating Chart 7 - Fairwork score by graduates class of degree
  output$chart7 <- renderPlotly({
    ndf_f_xclass01<- filtered_data() %>% 
      group_by(f_xclass01) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE)) %>% 
      arrange(desc(mean_danow)) 
    ndf_f_xclass01$f_xclass01 <- factor(ndf_f_xclass01$f_xclass01, levels = ndf_f_xclass01$f_xclass01[order(ndf_f_xclass01$mean_danow)])
    
    plot_ly(
      data = ndf_f_xclass01,
      x = ~mean_danow,
      y = ~f_xclass01,
      color = ~f_xclass01,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(size = 10, color = '#1F4388')
    ) %>%
      add_segments(
        x = 2,
        xend = ~mean_danow,
        y = ~f_xclass01,
        yend = ~f_xclass01,
        color = ~f_xclass01,
        mode = "lines",
        line = list(width = 1),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = 'Class of Degree'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates class of Degree",
        hovermode = "closest"
      ) %>%
      hide_colorbar() %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        hovertemplate = paste(
          "<b>Class of Degree:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )) %>%
      suppressWarnings()
  })
  #Creating Chart 8 - Fairwork score by graduates level of degree
  output$chart8 <- renderPlotly({
    ndf_f_xglev501 <- filtered_data() %>% 
      group_by(f_xglev501) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    arrange(desc(mean_danow)) 
    ndf_f_xglev501$f_xglev501 <- factor(ndf_f_xglev501$f_xglev501, levels = ndf_f_xglev501$f_xglev501[order(ndf_f_xglev501$mean_danow)])
    
    plot_ly(
      data = ndf_f_xglev501,
      x = ~mean_danow,
      y = ~f_xglev501,
      color = ~f_xglev501,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(size = 10, color = '#1F4388')
    ) %>%
      
      add_segments(
        x = 2,
        xend = ~mean_danow,
        y = ~f_xglev501,
        yend = ~f_xglev501,
        color = ~f_xglev501,
        mode = "lines",
        line = list(width = 1),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = 'Level of study'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates level of study",
        hovermode = "closest"
      ) %>%
      hide_colorbar() %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        hovertemplate = paste(
          "<b>Level of study:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )) %>%
      suppressWarnings()
  })
  #Creating Chart 9 - Fairwork score by graduates subject of degree
  output$chart9 <- renderPlotly({
    ndf_f_xjacsa01 <- filtered_data() %>% 
      group_by(f_xjacsa01) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE)) %>% 
    arrange(desc(mean_danow)) 
    ndf_f_xjacsa01$f_xjacsa01 <- factor(ndf_f_xjacsa01$f_xjacsa01, levels = ndf_f_xjacsa01$f_xjacsa01[order(ndf_f_xjacsa01$mean_danow)])
    
    plot_ly(
      data = ndf_f_xjacsa01,
      x = ~mean_danow,
      y = ~f_xjacsa01,
      color = ~f_xjacsa01,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(size = 10, color = '#1F4388')
    ) %>%
      
      add_segments(
        x = 2,
        xend = ~mean_danow,
        y = ~f_xjacsa01,
        yend = ~f_xjacsa01,
        color = ~f_xjacsa01,
        mode = "lines",
        line = list(width = 1),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = 'Subject of study'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates subject of study",
        hovermode = "closest"
      ) %>%
      hide_colorbar() %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        hovertemplate = paste(
          "<b>Subject of study:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )) %>%
      suppressWarnings()
  })
  
 {router$server(input, output, session)}
}
# Run the application
shinyApp(ui = ui, server = server)
