function(input, output) {
  
  ## Delete this later
  Survey <- import("Data/Survey.csv")
  # Survey <- import("MS/Shiny/Data/Survey.csv")
  
  Filtered <- reactive({
    ## Reactive expression for the data subsetted based
    ## on the user input from the sidebar
    Survey %>% 
      # semi_join(select(filter(ParseCSV(Survey, "Q14"), Q14 %in% input$race), -Q14)) %>%
      # semi_join(select(filter(ParseCSV(Survey, "Q12"), Q12 %in% input$language), -Q12)) %>%
      filter(
        str_detect(Race, str_c(input$race, collapse="|")), 
        str_detect(Lang, str_c(input$language, collapse="|")),
        Gender %in% input$gender, District %in% input$district) 
  })
    
  output$text <- renderPrint(glimpse(Filtered()))
  
  # df <- import("MS/Shiny/Data/Survey.csv")
  output$plot1 <- renderPlot({
    # Pre-Test scores histogram+density
    df <- Filtered()
    df %>%
      ggplot(aes(x=Score0)) +
      geom_histogram(aes(y=..density..), binwidth=0.06, alpha=0.75,
                     fill="grey") +
      geom_density() + 
      geom_vline(xintercept=mean(df$Score0, na.rm=T), linetype=2) + 
      annotate("text", x=mean(df$Score0, na.rm=T), y=0, label="mean", vjust=-1, hjust=0) +
      scale_x_continuous(labels=percent, limits=c(0,1)) + 
      theme_fivethirtyeight() + labs(y="", x="Percent Correct") +
      theme(axis.title=element_text(), strip.background=element_rect(fill="#DDDDDD"))
  })
  output$plot2 <- renderPlot({
    # Post-Test scores histogram+density
    df <- Filtered()
    df %>%
      ggplot(aes(x=ScoreF)) +
      geom_histogram(aes(y=..density..), binwidth=0.06, alpha=0.75,
                     fill="grey") +
      geom_density() + 
      geom_vline(xintercept=mean(df$ScoreF, na.rm=T), linetype=2) + 
      annotate("text", x=mean(df$ScoreF, na.rm=T), y=0, label="mean", vjust=-1, hjust=0) +
      scale_x_continuous(labels=percent, limits=c(0,1)) + 
      theme_fivethirtyeight() + labs(y="", x="Percent Correct") +
      theme(axis.title=element_text(), strip.background=element_rect(fill="#DDDDDD"))
  })
  
  output$table <- DT::renderDataTable(select(Filtered(), UID, Lang, Gender, Race))
  output$cards <- renderUI({
    
    df <- Filtered()
    # df <- Survey
    
    fluidRow(
      column(12, id="columns", 
        valueBox(nrow(df), subtitle="Students", icon=icon("user"), width = 6),
        lapply(df$UID, function(i) {
          tabBox(width=6, title = h3(df$UID[df$UID==i]), 
            ## Overview panel                  
            tabPanel(title="Overview", background = "#D6D2C4", tags$table(style="width: 100%", 
              tags$tr(
                tags$td(strong("School: "), df$School[df$UID==i]),
                tags$td(strong("Gender ID: "), df$Gender[df$UID==i])
              ),
              tags$tr(tags$td(colspan="2", strong("Race/Ethnicity ID: "), df$Race[df$UID==i])),
              tags$tr(tags$td(colspan="2", strong("Language(s): "), df$Lang[df$UID==i])),
              tags$tr(tags$td(colspan="2", strong("EO: "), 
                              max(c(df$Q10c[df$UID==i], df$Q10c[df$UID==i]), na.rm=T))),
              tags$tr(
                tags$td(strong("Pre-Score: "), code(
                  if_else(is.na(df$Score0[df$UID==i]), "", percent(df$Score0[df$UID==i])))),
              tags$td(strong("Post-Score: "), code(
                if_else(is.na(df$ScoreF[df$UID==i]), "", percent(df$ScoreF[df$UID==i]))))
              )
            )), # end 'Overview' tabPanel
                        
            ## School panel
            tabPanel(title="School", tags$table(style="width: 100%",
              tags$tr(
                tags$td(align="center", bgcolor='#F1F1F1', strong("Favorite subject")), 
                tags$td(align="center", bgcolor='#E1E1E1', strong("Least favorite"))
              ),
              tags$tr(
                tags$td(align="center", 
                  span(style="font-variant: small-caps; color: #7f0000", df$subLike[df$UID==i]),
                  if_else(!is.na(df$subLike.why[df$UID==i]), 
                          paste0(" ~ ", df$subLike.why[df$UID==i]), "")
                ),
                tags$td(align="center",
                  span(style="font-variant: small-caps; color: #7f0000", df$subDis[df$UID==i]),
                  if_else(!is.na(df$subDis.why[df$UID==i]), 
                          paste0(" ~ ", df$subDis.why[df$UID==i]), "")
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
                tags$td(align='center', df$HS.excited[df$UID==i]),
                tags$td(align='center', df$HS.fear[df$UID==i])
              )
            )), ## end 'School' tabPanel
                        
            ## Text panel
            tabPanel(title="Text",
              strong("What do you think you might want to do when you’re older?"), "  ",
              df$plans[df$UID==i], br(),
              strong("In your own words, why are you attending this camp?"), "  ",
              df$whyThere[df$UID==i]
            )
          ) # end tabBox
        }) # end lapply
      )
    )
  })
}