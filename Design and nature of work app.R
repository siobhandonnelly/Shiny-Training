
source("Data Manipulation.R")
source("Routing.R")
# Define UI
ui <- fluidPage(
  titlePanel("Design and Nature of Work Score"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css")
  ),  
    mainPanel(
      wellPanel(fluidRow(
        column(selectInput("Year", "Academic Year", choices = unique(nature_of_work$f_zcohort)), width = 12)
      )),
      tags$ul( # creating a series of links to the other pages that I want to hold on my app
        tags$li(a(href = route_link("/"), "Dashboard")),
        tags$li(a(href = route_link("Work"), "Work")),
        tags$li(a(href = route_link("Study"), "Study")),
        tags$li(a(href = route_link("Personal_Characteristics"), "Personal Characteristics"))
      ),
      router$ui
  )
  )


# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    nature_of_work %>%
     # filter(f_providername %in% input$provider) %>% #Allowing the data to be filtered by university provider
      filter(f_zcohort %in% input$Year) # Allowing the data to be filtered by academic year
      })
  output$meanOutput <- renderText({
    mean_score <- mean(nature_of_work$danow, na.rm = TRUE)
    mean_score_formatted <- sprintf("%.2f", mean_score)
    paste("Mean Fairwork Score: ", mean_score_formatted)
  })
  
  output$meansalaryOutput <- renderText({
    mean_salary <- mean(nature_of_work$salary, na.rm = TRUE)   
    mean_salary_formatted <- sprintf("%.2f", mean_salary)
    paste("Mean Salary: ", mean_salary_formatted)
  })
  
  output$proportionOutput <- renderText({
    category_counts <- table(nature_of_work$f_empbasis)  
    
    category_A_count <- category_counts["On a permanent/open ended contract"]  
    total_count <- sum(category_counts)
    
    proportion_A <- category_A_count / total_count
    
    paste("Proportion of Graduates on a permanent/open ended contract: ", proportion_A)
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
      mode = "markers",
      showlegend = FALSE,
      marker = list(symbol = "circle", size = 10, color = '#1F4388')
    ) %>%
      add_segments(
        x = 0,
        xend = ~mean_danow,
        y = ~f_xwrk2020soc1,
        yend = ~f_xwrk2020soc1,
        color = ~f_xwrk2020soc1,
        mode = "lines",
        line = list(width = 1),
        showlegend = FALSE
      ) %>%
      layout(
        yaxis = list(title = "Mean Fairwork score"),
        xaxis = list(title = 'SOC group'),
        title = "Mean Fairwork score by graduates SOC major group",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>SOC group:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  
  #Creating Plot 2, which is the fairwork score by graduates SIC group
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
      layout(
        yaxis = list(title = "Mean Fairwork score"),
        xaxis = list(title = 'Qualifications required'),
        title = "Mean Fairwork score by Qualifications required",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>Qualifications required:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })

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
      layout(
        yaxis = list(title = 'Employment Basis'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates employment basis",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>Employment Basis:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  
  output$chart4 <- renderPlotly({
    ndf_f_ethnic <- filtered_data() %>% 
      group_by(f_xethnic01) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    ndf_f_ethnic$f_xethnic01 <- factor(ndf_f_ethnic$f_xethnic01, levels = ndf_f_ethnic$f_xethnic01[order(ndf_f_ethnic$mean_danow)])
    
    plot_ly(
      data = ndf_f_ethnic,
      x = ~mean_danow,
      y = ~f_xethnic01,
      color = ~f_xethnic01,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(size = 10, color = '#1F4388')
    ) %>%
      layout(
        yaxis = list(title = 'Ethnicity'),  # y-axis is Ethnicity
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates ethnicity",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>Ethnicity:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  output$chart5 <- renderPlotly({
    ndf_f_sex <- filtered_data() %>% 
      group_by(f_sexid) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    ndf_f_sex$f_sexid <- factor(ndf_f_sex$f_sexid, levels = ndf_f_sex$f_sexid[order(ndf_f_sex$mean_danow)])
    
    
    plot_ly(
      data = ndf_f_sex,
      x = ~mean_danow,
      y = ~f_sexid,
      color = ~f_sexid,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(size = 10, color = '#1F4388')
    ) %>%
      layout(
        yaxis = list(title = 'Sex'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates Sex",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>Sex:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  output$chart6 <- renderPlotly({
    ndf_f_pared <- filtered_data() %>% 
      group_by(f_pared) %>%
      summarise(mean_danow = mean(danow, na.rm = TRUE))
    ndf_f_pared$f_pared <- factor(ndf_f_pared$f_pared, levels = ndf_f_pared$f_pared[order(ndf_f_pared$mean_danow)])
    
    
    plot_ly(
      data = ndf_f_pared,
      x = ~mean_danow,
      y = ~f_pared,
      color = ~f_pared,
      type = "scatter",
      mode = "markers",
      showlegend = FALSE,
      marker = list(size = 10, color = '#1F4388')
    ) %>%
      layout(
        yaxis = list(title = 'Parental Education'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates Parental Education",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>Parental Education:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  
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
      layout(
        yaxis = list(title = 'f_xclass01'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates f_xclass01",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>f_xclass01:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
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
      layout(
        yaxis = list(title = 'f_xglev501'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates f_xglev501",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>f_xglev501:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
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
      layout(
        yaxis = list(title = 'f_xjacsa01'),  # y-axis is Employment Basis
        xaxis = list(title = 'Mean Fairwork score'),  # x-axis is Mean Fairwork score
        title = "Mean Fairwork score by graduates f_xjacsa01",
        hovermode = "closest",
        hovertemplate = paste(
          "<b>f_xjacsa01:</b> %{y}<br>",
          "<b>Mean Fairwork score:</b> %{x}<br>"
        )
      ) %>%
      hide_colorbar() %>%
      suppressWarnings()
  })
  
 {router$server(input, output, session)}
}
# Run the application
shinyApp(ui = ui, server = server)
