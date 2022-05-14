ui <- dashboardPage(
  dashboardHeader(title = "Makro Data Science"),
  dashboardSidebar(
    sidebarMenu(
    id = "sidebarid",
    menuItem("Database Info", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Comparison", tabName = "comparison", icon = icon("dashboard")),
    
    conditionalPanel(
      'input.sidebarid == "dashboard"',
    pickerInput(
      inputId = "Region",
      label = "Region",
      choices = c(unique(data$Region)),
      selected= "Europe & Central Asia"
    ),
     uiOutput("Country") 
    )
    )
      ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(column(12,box(height = 120,width = 740,title ="Info" , status = "primary", background = "green",
                p("Dieses Dahboard wurde im Rahmen des Seminars Angewandte Data -Science im Bereich Makro erstellt.
                   Ziel dieses  Dashboards ist die explorative Analyse der Ungleichverteilung von Einkommen mit dem Gini-Index und anderen Indikaoren.
                   Als Datengrundlage gilt die Poverty And Equity Database der Weltbank.
                  "))))
            ,
            fluidRow(column(4,
              box(height = 520,width = 400, footer = 'This Graph is dependent on the Filter on the left', title = '  Income Class group by continent',
                  plotlyOutput("plot4", height = 450,width = 380))),
             column(8,
                              box(height = 520,width = 750, footer = 'This Graph is dependent on the on the Filter left',title = 'Number of observations by country',
                                  plotlyOutput("plot3", height = 450,width = 740)))
              
            ),
            br(),
            br(),
            fluidRow(column(12,
              box(height = 230,width = 990, footer = 'This Graph is dependent on the Filter on the left . Aggregation of Countrys by Year',title = 'Number of observations by year',
                  plotlyOutput("plot5", height = 170,width = 990)))),
           br(),
           br(),
           br(),
           fluidRow(column(12,
             box(height = 90,width = 990, title ="Info" ,status = "primary", background = "green",
                 p("Der folgende Abschnitt dient dazu, zu untersuchen wie vollständig der Datensatz ist. Dazu können einzelne Indikatoren ausgewählt werden und die Anzahl an Beobachtungen werden gezählt.")))),
           br(),
             fluidRow(box( height = 120,width = 740,title = 'Indicator Selection',
                          pickerInput(
                            inputId = "Indicator_Name",
                            label = "Indicator_Name",
                            choices = c(unique(data$Indicator_Name))
                          ),
                          DT::dataTableOutput("mytable")
            )),
          #  fluidRow(
           #   box(height = 530,width = 740, title = 'Visualization',
            #      plotlyOutput("plot1", height = 450,width = 940))

                  #        ),
            # fluidRow(
            #   box(height = 330,width = 740, title = 'Number of observations by country',
            #   DT::dataTableOutput("mytable")
            #   )
            # )
    ),
    tabItem(tabName = "comparison",
            fluidRow(
                            box(height = 90,width = 990, title ="Info" ,status = "primary", background = "green",
                                p("Der folgende Abschnitt dient dazu, einzelne Länder zu vergleichen.  Dazu sind die hierarchischen Filter von Links nach Rechts auszuwählen"))),
            br(),
            fluidRow(box( height = 280,width = 740,title = 'Indicator & Country Selection',
                          column(12,pickerInput(
                            inputId = "Indicator_Name2",
                            label = "Indicator_Name",
                            choices = c(unique(data$Indicator_Name))
                          )),
                          column(4,
                                 pickerInput(
                                   inputId = "Region1",
                                   label = "Region1",
                                   choices = c(unique(data$Region)),
                                   selected= "Europe & Central Asia"
                                 )
                          ),
                          column(4,
                                 uiOutput("Income_Group_1")) ,
                          column(4,
                                 uiOutput("Country1")
                                 
                          ),
                          column(4,
                                 pickerInput(
                                   inputId = "Region2",
                                   label = "Region2",
                                   choices = c(unique(data$Region)),
                                   selected= "North America"
                                 )
                          ),
                          column(4,
                                 uiOutput("Income_Group_2")) ,
                          column(4,
                                 uiOutput("Country2") 
                                 
                          ),
                          )),
            fluidRow(
              box(height = 530,width = 740, title = 'Visualization',
                  plotlyOutput("plot2", height = 450,width = 940))
              
            ),
           
            fluidRow(
              box(height = 330,width = 740, title = 'Infos by country',
                  DT::dataTableOutput("mytable2")
              )
            )
    ),
    tabItem(tabName = "overview",
            fluidRow(column(12,
              box(height = 90,width = 990, title ="Info" ,status = "primary", background = "green",
                  p("Der folgende Abschnitt dient dazu, sich einen Überblick mittels Weltkarte über die einzelnen Indikatoren zu bekommen. Maßgeblich ist immer das letzte Jahr in dem eine Erhebung stattgefunden hat. Auf der Rechten Seite können noch Zusatzinformationen für einzelne Länder abgefragt werden, dazu bitte den Filter benutzen")))),
            br(),
            fluidRow(column(12,box( height = 160,width = 740,title = 'Indicator Selection',
                          pickerInput(
                            inputId = "Indicator_Name3",
                            label = "Indicator_Name",
                            choices = c(unique(data$Indicator_Name))
                          ),
                          p("Choosen Index is displayed in Graph below")
            ))),
            fluidRow(column(10,
              box(height = 540,width = 910, title = 'Visualization by KPI',
                  plotlyOutput("plot6", height = 480,width = 900 )
            )),
            column(2,
                   box(
                     title = "Inputs", status = "warning",width= 300 ,solidHeader = TRUE,
                     "Country Content", br(),
                   pickerInput("Country_Name_Filter", 
                               "Country_Name_Filter", 
                               choices = c(unique(data$Country_Name)),
                               selected =c("Germany"))
                   ),
                   box(
                     title = "Trade System", status = "warning",width= 350, height =170 ,solidHeader = TRUE,
                    # "Country Income Group",
                     br(),
                     infoBoxOutput("ibox"),
                   )
                   ,
                   box(
                     title = "Income_Group", status = "warning",width= 350, height =170 ,solidHeader = TRUE,
                     # "Country Income Group",
                     br(),
                     infoBoxOutput("ibox2"),
                   )
                   )
            
            ),
            fluidRow( column(9,
                            box(height = 200,width = 800, title = 'Visualization by KPI and Country',
                                plotlyOutput("plot8", height = 127,width = 920 )
                                ,
                                p("Dependen on Picker Input's above and right"))),
            column(3,
                   box(
                     title = "Development", status = "success",width= 400, height =201 ,solidHeader = TRUE,
                     # "Country Income Group",
                     br(),
                     infoBoxOutput("ibox3"),
                   ))),
            # fluidRow(
            #   box(height = 540,width = 740, title = 'Visualization GDP',
            #       plotlyOutput("plot7", height = 460,width = 940))
            #   
            # )
    )
            
  )
)
)