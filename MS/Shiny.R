require(rio)
require(dplyr)
require(shiny)
require(shinydashboard)
require(shinythemes)

df <- import("MS/Data/Pre/Survey.csv")

body <- dashboardBody(fluidRow(
  column(12, id="columns",
    lapply(df$UID, function(i) {
      tabBox(width=6,  
        title = h3(df$UID[df$UID==i]),
        
        ## Overview panel
        tabPanel(title="Overview", 
          tags$table(style="width: 100%",
            tags$tr(
              tags$td(strong("School: "), df$Q04[df$UID==i]),
              tags$td(strong("Gender ID: "), df$Q13[df$UID==i])
            ),
            tags$tr(tags$td(colspan="2", strong("Race/Ethnicity ID: "), df$Q14[df$UID==i])),
            tags$tr(tags$td(colspan="2", strong("Language(s): "), df$Q12[df$UID==i])),
            tags$tr(tags$td(colspan="2", strong("Parent's Max Edu: "), df$`Student name`[df$UID==i])),
            tags$tr(
              tags$td(strong("Pre-Score: ")),
              tags$td(strong("Post-Score: "))
            )
          )
        ), ## end 'At a glance' tabPanel
        
        ## School panel
        tabPanel(title="School", tags$table(style="width: 100%",
          tags$tr(
            tags$td(align="center", bgcolor='#F1F1F1', strong("Favorite subject")), 
            tags$td(align="center", bgcolor='#E1E1E1', strong("Least favorite"))
          ),
          tags$tr(
            tags$td(align="center", 
              span(style="font-variant: small-caps; color: #7f0000", df$Q15a[df$UID==i]),
              if_else(!is.na(df$Q15b[df$UID==i]), paste0(" ~ ", df$Q15b[df$UID==i]), "")
            ),
            tags$td(align="center",
                    span(style="font-variant: small-caps; color: #7f0000", df$Q16a[df$UID==i]),
                    if_else(!is.na(df$Q16b[df$UID==i]), paste0(" ~ ", df$Q16b[df$UID==i]), "")
            )
          ),
          tags$tr(tags$td(colspan="2", align="center", bgcolor="#B1B1B1",
            span(style="font-variant: small-caps; color: #FFFFFF",
                 "In high school, what are you...")
          )),
          tags$tr(
            tags$td(align='center', bgcolor='#F1F1F1', strong("Most excited for?")),
            tags$td(align='center', bgcolor='#E1E1E1', strong("Most afraid of?"))
          ),
          tags$tr(
            tags$td(align='center', df$Q03a[df$UID==i]),
            tags$td(align='center', df$Q03b[df$UID==i])
          )
        )), ## end 'School' tabPanel
        
        ## Text panel
        tabPanel(title="Text",
          strong("What do you think you might want to do when you’re older?"), "  ",
          df$Q02[df$UID==i], br(),
          strong("In your own words, why are you attending this camp?"), "  ",
          df$Q01[df$UID==i]
        )
      ) # end tabBox
    }) # end lapply
  )
))

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Middle School"),
    dashboardSidebar(),
    body
  ),
  server = function(input, output) {
    # --- # --- # --- # --- # --- # --- # --- # --- #
    #                  REACTIVE FXN                 #
    #             Filter our responses              #
    # --- # --- # --- # --- # --- # --- # --- # --- #
#     Filtered <- reactive({
#       ## Reactive expression for the data subsetted based
#       ## on the user input.
#       
#       # This line of code sets the selected 'HP_type' as the 'Value' 
#       df$Value <- df[,c(input$HP_type)]
#     })
      
    
    
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })
  }
)
rm(body)