server <-  function(input, output, session) {
  print(c(unique(data$Indicator_Name)))
  output$Country <- renderUI({
  data <- data
  data <-data %>% filter ( Region %in% c(unique(input$Region)))
  pickerInput("Countrys", 
              "Country", 
              choices = c(unique(data$Country_Name)),
              multiple = TRUE,
              pickerOptions(
                actionsBox = TRUE),
              selected  = c(unique(data$Country_Name))
              )
  })

  output$Country1 <- renderUI({
    data <- data
    data <-data %>% filter ( Region %in% c(unique(input$Region1))) %>%
              filter ( Income_Group %in% c(unique(input$Income_Group1)))
    selectInput("Countrys1", 
                "Country1", 
                choices = c(unique(data$Country_Name),
                       selected= "Germany"     )
    )
  })
 
  output$Country2 <- renderUI({
    data <- data
    data <-data %>% filter ( Region %in% c(unique(input$Region2))) %>%
      filter ( Income_Group %in% c(unique(input$Income_Group2)))
    selectInput("Countrys2", 
                "Country2", 
                choices = c(unique(data$Country_Name))
    )
  }) 

  output$Income_Group_1 <- renderUI({
    data <- data
    data <-data %>% filter ( Region %in% c(unique(input$Region1)))
    selectInput("Income_Group1", 
                "Income_Group1", 
                choices = c(unique(data$Income_Group))
    )
  }) 

  output$Income_Group_2 <- renderUI({
    data <- data
    data <-data %>% filter ( Region %in% c(unique(input$Region2)))
    selectInput("Income_Group2", 
                "Income_Group2", 
                choices = c(unique(data$Income_Group))
    )
  })  
  output$mytable = DT::renderDataTable({
    data <- data
    data_2 <- data %>%filter(value >0)%>%
      group_by(Country_Name,Indicator_Name) %>%
      dplyr::mutate(
        first = dplyr::first(year),
        last = dplyr::last(year)
      )
    data_2<- data_2  %>% select(Country_Name,first, last,Indicator_Name,) %>%  unique()

    vars <- data %>% filter ( Country_Name %in% c(unique(input$Countrys))) %>% 
      filter ( Indicator_Name %in% c(unique(input$Indicator_Name))) %>% 
      filter ( value >0) %>% 
      #filter ( year %in% c(1990:2014)) %>% 
      group_by(Indicator_Name,Income_Group,Country_Name) %>% 
      summarise(Observacions=  n()) 

    vars <-merge(vars, data_2, by.y=c("Country_Name", "Indicator_Name"), by.x=c("Country_Name", "Indicator_Name")) # NA's match

    vars
  })
  output$plot3 <- renderPlotly({ 
    data <- data
    vars <- data %>% filter ( Country_Name %in% c(unique(input$Countrys))) %>% 
     # filter ( Indicator_Name %in% c(unique(input$Indicator_Name))) %>% 
      filter ( Indicator_Name == "Gini index (World Bank estimate)") %>% 
      filter ( value >0) %>% 
      #filter ( year %in% c(1990:2014)) %>% 
      group_by(Indicator_Name,Income_Group,Country_Name) %>% 
      summarise(Observacions=  n()) 
    fig <- plot_ly(vars, 
                   x = ~Observacions, 
                   y = ~Country_Name, 
                   type = 'bar', 
                   color = ~Income_Group,
                   colors = "Dark2")  %>% 
      layout(title = 'Count Years with valid Gini Index')
  })
  output$plot4 <- renderPlotly({ 
    data <- data
    vars <- data %>% filter ( Country_Name %in% c(unique(input$Countrys))) %>% 
     # filter ( Indicator_Name %in% c(unique(input$Indicator_Name))) %>% 
      filter ( value >0) %>% 
      filter ( Indicator_Name == "Gini index (World Bank estimate)") %>% 
      select(Country_Name,Income_Group)%>% 
      unique()%>% 
      #filter ( year %in% c(1990:2014)) %>% 
      group_by(Income_Group) %>% 
      summarise(Observacions=  n()) 
    fig <- plot_ly(vars, 
                   labels = ~Income_Group, 
                   values = ~Observacions, 
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   showlegend = FALSE,
                   type = 'pie', 
                   colors = "Dark2")  %>% 
      layout(title = 'Distribution of Income')
  })
  output$plot5 <- renderPlotly({ 
    data <- data
    vars <- data %>% filter ( Country_Name %in% c(unique(input$Countrys))) %>% 
      #filter ( Indicator_Name %in% c(unique(input$Indicator_Name))) %>% 
      filter ( value >0) %>% 
      filter ( Indicator_Name == "Gini index (World Bank estimate)") %>% 
      select(Country_Name,year)%>% 
      unique()%>% 
      #filter ( year %in% c(1990:2014)) %>% 
      group_by(year) %>% 
      summarise(Observacions=  n()) 
    fig <- plot_ly(vars, 
                   x = ~year, 
                   y = ~Observacions, 
                   type = 'bar', 
                   colors = "Dark2") 
  }) 
  
  output$plot1 <- renderPlotly({ 

    data2 <- data %>% filter ( Country_Name %in% c(unique(input$Countrys))) %>% 
                    filter ( Indicator_Name %in% c(unique(input$Indicator_Name))) %>% 
                    filter ( value >0) %>% 
         # filter ( year %in% c(1990:2014)) %>% 
     # filter ( Country_Name %in% c(vars$Country_Name))%>% 
      group_by(year,Indicator_Name,Income_Group,.groups = 'drop') %>% 
      summarise(value_total= as.numeric(mean(value)))
    
  
    
 data2 <- as.data.frame(data2)
        f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Year",
      titlefont = f
    )
    y <- list(
      title = "Percentage",
      titlefont = f
    )
    
    
    fig <- plot_ly(data2, 
                   x = ~year, 
                   y = ~value_total, 
                   type = 'scatter', 
                   mode = 'lines',
                   color = ~Indicator_Name,
                   colors = "Dark2") 
   # fig <- fig %>% add_lines()
    fig <- fig %>% layout(xaxis = x, 
                          yaxis = y,
                          title = paste0( c(input$Indicator_Name),
                                          ' From ' ,
                                          min(data$year),
                                          ' To ' ,
                                          max(data$year))
    )
    fig
  })
  
  output$plot2 <- renderPlotly({ 
    data <- data
    data <- data %>% filter ( Country_Name %in% c(unique(input$Countrys1),unique(input$Countrys2))) %>% 
      filter ( Indicator_Name %in% c(unique(input$Indicator_Name2))) %>% 
      filter ( value >0)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Year",
      titlefont = f
    )
    y <- list(
      title = "Percentage",
      titlefont = f
    )
    fig <- plot_ly(data, 
                   x = ~year, 
                   y = ~value, 
                   type = 'scatter', 
                   mode = 'lines',
                   color = ~Country_Name,
                   colors = "Dark2") 
    #fig <- fig %>% add_lines()
    fig <- fig %>% layout(xaxis = x, 
                          yaxis = y,
                          title = paste0( c(input$Indicator_Name2),
                                          ' From ' ,
                                          min(data$year),
                                          ' To ' ,
                                          max(data$year))
    )
    fig
  })
  
  output$mytable2 = DT::renderDataTable({
    stats <-stats %>% filter ( Short_Name %in%  c(unique(input$Countrys1),unique(input$Countrys2))) 

    stats <-datatable(stats,

   extensions = c('Buttons', 'Scroller'),
   options = list(scrollY = 650,
                  scrollX = 500,
                  deferRender = TRUE,
                  scroller = TRUE,
                  # paging = TRUE,
                  # pageLength = 25,
                  buttons = list('excel',
                                 list(extend = 'colvis', targets = 0, visible = FALSE)),
                  dom = 'lBfrtip',
                  fixedColumns = TRUE), 
   rownames = FALSE)
    stats
  })
  output$plot6<- renderPlotly({ 
    final_geo2 <- geo %>%
    filter ( Indicator_Name %in% c(unique(input$Indicator_Name3)))  
  
    g <- list(
      fitbounds = "locations",
      visible = FALSE
    )
  fig <- plot_ly(final_geo2, 
                 type='choropleth', 
                 locations=final_geo2$CODE, 
                 z=final_geo2$value, 
                 colorscale="Viridis"
                 #text=final_geo2$year)
  )
 # fig <- fig %>% layout(
#    geo = g
#  )
 # fig <- fig %>% colorbar(title = paste0( c(input$Indicator_Name3)))
  fig <-fig %>% layout(title = paste0( c(input$Indicator_Name3)))
  
  fig
  })
  
  output$info <- renderText({
    
    paste0("x=", input$plot_click$locations, "\ny=", input$plot_click$z)
  })
  output$plot7<- renderPlotly({ 
    gdp<-gdp
    geo<- geo %>% select(Country_Code,Country_Name)%>% unique()
    gdp <- merge(gdp,geo ,  by.x="country", by.y="Country_Name")
    fig <- plot_ly(gdp, 
                   type='choropleth', 
                   locations=gdp$Country_Code, 
                   z=gdp$NY.GDP.PCAP.KD, 
                   colorscale="Viridis"
                   #text=final_geo2$year)
                   
    )
    fig <-fig %>% layout(title =  "GDP per capita (constant 2000 US$)" )
    fig
  })
 
  output$ibox <- renderInfoBox({
    stats <- stats 
    stats <- stats %>% filter(Short_Name %in% input$Country_Name_Filter) %>% 
      select(System_of_trade) %>% unique()
    infoBox(
      "Title",
      value = tags$p(style = "font-size: 13px;", stats),
      icon = icon("credit-card")
    )
  }) 
  output$ibox2 <- renderInfoBox({
    stats <- stats 
    stats <- stats %>% filter(Short_Name %in% input$Country_Name_Filter) %>% 
      select(Income_Group) %>% unique()
    infoBox(
      "Title",
      value = tags$p(style = "font-size: 13px;", stats),
      icon = icon("credit-card")
    )
  }) 
  
  output$ibox3 <- renderInfoBox({
    data <- data %>% filter (Country_Name %in% unique(input$Country_Name_Filter)) %>% 
      filter ( Indicator_Name %in% c(unique(input$Indicator_Name3))) %>% 
      filter ( value >0)
    min_y = min(data$year)
    max_y = max(data$year)
    min <-data %>% filter(year==min_y) %>%  select(value)
    max <-data %>% filter(year==max_y) %>%  select(value)
    
    infoBox(
      "Title",
      value = tags$p(style = "font-size: 13px;", paste0('From ' ,min , ' to ',max)),
      color = "green"
    )
  }) 
  
  output$plot8 <- renderPlotly({ 
    data <- data
    data <- data %>% filter (Country_Name %in% unique(input$Country_Name_Filter)) %>% 
      filter ( Indicator_Name %in% c(unique(input$Indicator_Name3))) %>% 
      filter ( value >0)
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Year",
      titlefont = f
    )
    y <- list(
      title = "Percentage",
      titlefont = f
    )
    fig <- plot_ly(data, 
                   x = ~year, 
                   y = ~value, 
                   type = 'scatter', 
                   mode = 'lines',
                   color = ~Country_Name,
                   colors = "Dark2") 
    #fig <- fig %>% add_lines()
    fig <- fig %>% layout(xaxis = x, 
                          yaxis = y,
                          title = paste0( c(input$Indicator_Name3),
                                          ' From ' ,
                                          min(data$year),
                                          ' To ' ,
                                          max(data$year),
                                          ' in ',
                                          c(input$Country_Name_Filter))
    )
    fig
  })
  #
}