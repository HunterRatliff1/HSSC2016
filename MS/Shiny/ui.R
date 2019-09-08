dashboardPage(
  dashboardHeader(title = "Middle School"),
  dashboardSidebar(sidebarMenu(
    menuItem("Charts", icon=icon("bar-chart"), tabName = "charts"),
    # menuItem("Text Output", icon=icon("user"), tabName = "text"),
    
    # Filter inputs
    menuItem("Filters", icon=icon("filter"), tabName="filters",
      menuItem("Demographics", icon=icon("info-circle"), tabName="demographics",
        selectInput("gender", "Gender ID", multiple=TRUE, selectize=TRUE, width='98%',
                    choices=getOpts(df, "Gender"), selected=getOpts(df, "Gender")),
        selectInput("language", "Languages", multiple=TRUE, selectize=TRUE, width='98%',
                    choices=getOpts(df, "Lang"), selected=getOpts(df, "Lang")),
        selectInput("race", "Race/Ethnicity ID", multiple=TRUE, selectize=TRUE, width='98%',
                    choices=getOpts(df, "Race"), selected=getOpts(df, "Race"))
      ),
      menuItem("Educational", icon=icon("university"), tabName="educational",
        selectInput("district", "District", multiple=TRUE, selectize=TRUE, width='98%',
                    choices=getOpts(df, "District"), selected=getOpts(df, "District"))
      )
    ),
    menuItem("Individual Responses", icon=icon("user"), tabName = "cards"),
    menuItem("Table", icon=icon("table"), tabName = "table"),
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem("Source code", icon = icon("github"), 
             href = "https://github.com/hunterratliff1/HSSC2016/")
  )),
  dashboardBody(
    # tags$head(tags$link(rel="stylesheet", type="text/css", href="utexas.css")),
    tabItems(
      # tabItem(tabName = "text", verbatimTextOutput("text")),
      tabItem(tabName = "charts", 
        box(title = "Pre-Scores", status = "info", solidHeader = TRUE,
            collapsible = TRUE, plotOutput("plot1", height = 300)),
        box(title = "Post-Scores", status = "success", solidHeader = TRUE,
            collapsible = TRUE, plotOutput("plot2", height = 300))
        
      ),
      tabItem(tabName = "table", DT::dataTableOutput("table")),
      tabItem(tabName = "cards", uiOutput("cards")),
      tabItem(tabName = "about", includeMarkdown("www/about.md"))
    )
  )
)