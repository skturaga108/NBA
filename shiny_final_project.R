# library(shiny)
# library(bslib)


ui = fluidPage(
  
  theme = bs_theme(bootswatch = "sandstone"),
  
  headerPanel(textOutput(outputId = "header_text")),
  
  tabsetPanel(
    
    ############### introduction code ###############
      
      tabPanel("Start Here",
               
               wellPanel(
                 textOutput(outputId = "intro_text")

                                )),
    ############### optimization code ###############      
    
      tabPanel("Optimization",
               
          headerPanel(
                 
              textOutput(outputId = "opt_header_text")),
               
          wellPanel(     
            
              numericInput(inputId = "opt_season", label = "Our dataset runs from 96' to 
                           the year of the pandemic. Select a season, any season...", min=1996, max=2020, value=1996, width="25%"),
             
              radioButtons(inputId = "opt_stat", 
                           label = "Which stat would you like to optimize for?", 
                           choiceValues = c("age", "height", "weight", "points", "rebounds", "assists", "net_rating", "true_shooting_pct"),
                           choiceNames = c("Age","Height","Weight","Points","Rebounds","Assists","Net Rating","True Shooting %"), 
                           inline=T),
             
              radioButtons(inputId = "opt_type", label = "Shall we maximize or minimize the stat? Minimizing points
                           isn't very interesting, but height may be...", 
                           choiceValues = c("max", "min"),
                           choiceNames = c("Maximize, of course", "Minimize, why not"),
                           inline=T),
              
              radioButtons(inputId = "opt_constraint", label = "For the value of usage constriant, you may select 
                           'league average' which calculates the average usage rate across the league for the selected
                           season. Or you can really have some fun by customizing.", choiceValues = c("avg", "custom"), 
                           choiceNames = c("league average", "custom"), inline=T),
              
              sliderInput(inputId = "opt_constraint_slider", "usage constraint", min=0.5, max=2.0, value=1.0, step=0.1, width="40%"),
             
              actionButton(inputId = "opt_go", label = "Let's go")),
                 
          mainPanel(
              
              textOutput(outputId = "opt_text2"),
              
              textOutput(outputId = "opt_text3"),
              
              textOutput(outputId = "opt_text4"),
            
              tableOutput(outputId = "opt_table"),
              
              plotOutput(outputId = "opt_plot"))),
    
    ############### player comparison code ###############
    
      tabPanel("Player Comparison",
               
          fluidRow(
            
          column(3,
        
              selectInput(inputId = "comp_player1",
                          label = "Select player 1",
                          data$player),
              
              numericInput(inputId = "comp_start1", label = "Select start year", min=1996, max=2020, value=2000),
              
              actionButton(inputId = "comp_go", label = "Compare Players", width="100%"),offset = 3),
          
          column(3,
            
            selectInput(inputId = "comp_player2",
                        label = "Select player 2",
                        data$player, selected = "LeBron James"),
            
            numericInput(inputId = "comp_end1", label = "Select end year", min=1996, max=2020, value=2010))),
            
#            numericInput(inputId = "comp_start2", label = "Select start year", min=1996, max=2020, value=2000),
            
#            numericInput(inputId = "comp_end2", label = "Select end year", min=1996, max=2020, value=2010),offset = 0)),
          
          
          fluidRow(
            
          column(6,
            
            tableOutput(outputId = "comp_player1_table")),
          
          column(6,
                 
            tableOutput(outputId = "comp_player2_table"))),

          
          fluidRow(
            
            column(12,
                   
                   plotOutput(outputId = "comp_plot"))
            
          )),


    ############### taboo code ###############


    tabPanel("Taboo - Don't Click",


            headerPanel(

              textOutput(outputId = "taboo_header")

            ),


            fluidRow(
              
              column(6,
                     
                     
                     radioButtons(inputId = "taboo_season", label = "Which version of Michael?",
                                  choices=c(1984:1997), inline=T),
                     
                     tableOutput(outputId = "taboo_table"),                     
                     
                     actionButton(inputId = "taboo_go", label = "Could 4 of these MJs outscore the optimal team")
                     

                     
                     ),
              
              column(6,
                     
                     radioButtons(inputId = "taboo_season2", label = "Which year to select Mike's opponents from?",
                                  choices=c(1996:2020), inline=T),
                     

                     
                     tableOutput(outputId = "taboo_table3"))
                     
                     
                     
                     ),
            
            fluidRow(
              
              column(6,
                     
                     tableOutput(outputId = "taboo_table2"),
                     textOutput(outputId = "taboo_answer")
                     
                     ),
              
              column(6,
                     tableOutput(outputId = "taboo_table4"),                    
                     )
              
            )



                     ))

        
     



)

server = function(input, output) {
  
  
  ############### header code ###############
  
  
  output$header_text = renderText({

    "Group 3 Shiny App"
    
  })  
  
  
  ############### introduction tab code ################
  
  
  output$intro_text = renderText({
    
    "Welcome, you bold NBA enthusiast.
    \n\n
    Welcome to the frontiers of speculative sports science, where we blend sound data
    analysis with controversy.
    \n
    By opening this app, you've already violated statutes in 19 US states, ran afoul of
    the FCC's code of conduct, not to mention upset your mother. You're in too deep, so might as well press on.
    \n
    Here at Group 3, Inc., we take the user into uncharted terrain, asking uncouth questions such as: what is the shortest possible NBA lineup?
    The tallest? How about heaviest? Youngest? We do all this without violating the laws of common sense, by
    constraining the line-ups by usage percentage. Exploiting cutting edge techniues in optimization and data visualization, we believe
    that with great data comes great curiosity.
    \n
    We don't just cross the taboo line, we blow right past it into sacred territory,
    asking a most dangerous question: could four Michael Jordans beat the best five players from any given season?
    \n
    I'll be your tour guide, stick with me and don't get lost. These are dangerous parts.
    \n\n
    Let's begin."
    
    
  })
  
  
  
  ############### optimization code ###############
  
  
  opt_data = eventReactive(input$opt_go, {
    top5(input$opt_season, input$opt_stat, input$opt_type)
  })
  
  opt_data2 = eventReactive(input$opt_go, {
    list(avg_usage(input$opt_season),
         input$opt_season)
  })
  
  opt_data3 = eventReactive(input$opt_go, {
    top5(input$opt_season, input$opt_stat, input$opt_type, constraint=input$opt_constraint_slider)
  })  
  
  
  output$opt_table = renderTable({
    
    if (input$opt_constraint == "custom") {
      
      opt_data3() } else {
        opt_data()
      }
    })
  
  output$opt_text2 = renderText({
    
    if (input$opt_constraint == "custom") {
    paste("Total",input$opt_stat,":",round(sum(opt_data3()[,input$opt_stat]),1))
    } else {
      paste("Total",input$opt_stat,":",round(sum(opt_data()[,input$opt_stat]),1))
    }
  })
  
  output$opt_text3 = renderText({
    
    if (input$opt_constraint == "custom") {    
    paste("Average",input$opt_stat,":",round(mean(opt_data3()[,input$opt_stat]),1))
    } else {
      paste("Average",input$opt_stat,":",round(mean(opt_data()[,input$opt_stat]),1))
    }
  })
  
  output$opt_text4 = renderText({
    if (input$opt_constraint == "custom") {
    paste("The average usage in",opt_data2()[2],"was",opt_data2()[1], 
          "the usage of this team is",round(sum(opt_data3()[,"usage_pct"]),2))
    } else {
      paste("The average usage in",opt_data2()[2],"was",opt_data2()[1], 
            "the usage of this team is",round(sum(opt_data()[,"usage_pct"]),2))
    }
  })
  
  
  ############### player comparison code ###############
 
   
  comp_data = eventReactive(input$comp_go, {
   list(player_season(input$comp_player1, input$comp_start1, input$comp_end1),
        player_season(input$comp_player2, input$comp_start1, input$comp_end1),
        as.numeric(input$comp_start1), as.numeric(input$comp_end1))
  })
  
  comp_data2 = eventReactive(input$comp_go, {
    comp_graph_data(input$comp_player1, input$comp_player2, input$comp_start1, input$comp_end1)
  })
  
  output$comp_player1_table = renderTable({
    comp_data()[1]
  })
  
  output$comp_player2_table = renderTable({
    comp_data()[2]
  })
  
  output$comp_plot = renderPlot({
    ggplot(comp_data2(), aes(x, y, col=stats)) + geom_point() + geom_smooth(se=F) + 
      ggtitle(paste(input$comp_player1," and ",input$comp_player2,"'s Stats", sep="")) + 
      xlab("Season") + ylab("Stat") + ylim(0,max(comp_data2()[,2])) +
      xlim(comp_data()[[3]], comp_data()[[4]]) + facet_grid(cols = vars(player))
  })
  
  ############### taboo code ############### 
  
  output$taboo_header = renderText({
    "You clicked. Well, since you're here let's ask the mother of all taboo sports questions:
    Could 4 MJs beat the top five players from the rest of the league? Find out."
  })
  
  taboo_data = eventReactive(input$taboo_go, {
    mike %>%
      filter(season==input$taboo_season) %>%
      select(player, points, rebounds, assists, usage_pct, season) %>%
      mutate(points = points*4, rebounds = rebounds*4, assists = assists*4, usage_pct = usage_pct*4)
  })
  
  taboo_data2 = eventReactive(input$taboo_go, {
    top5(input$taboo_season2, "points", "max", constraint = (mike %>%
           filter(season==input$taboo_season2) %>%
           select(usage_pct) %>%
           mutate(usage_pct = usage_pct*4))[1,1])
  })
  
  taboo_data3 = eventReactive(input$taboo_go, {
    
    top5(input$taboo_season2, "points", "max", constraint = (mike %>%
                                                               filter(season==input$taboo_season) %>%
                                                               select(usage_pct) %>%
                                                               mutate(usage_pct = usage_pct*4))[1,1])
    
  })
  
  taboo_data4 = eventReactive(input$taboo_go, {
    
    top5(input$taboo_season2, "points", "max", constraint = (mike %>%
                                                               filter(season==input$taboo_season) %>%
                                                               select(usage_pct) %>%
                                                               mutate(usage_pct = usage_pct*4))[1,1])
    
  })
  
  output$taboo_table = renderTable({
    mike %>%
      filter(season==input$taboo_season) %>%
      select(player, points, rebounds, assists, usage_pct, season)
  })
  
  output$taboo_table2 = renderTable({
    taboo_data()
  })
  
  output$taboo_table3 = renderTable({
    taboo_data3()
  })
  
  output$taboo_table4 = renderTable({
    a = apply(X=taboo_data4()[,c("points","rebounds","assists","usage_pct")],MARGIN=2,FUN=sum)
    a = t(a)
    a
  })
  
  output$taboo_answer = renderText({
    
    if (sum(taboo_data3()[,"points"]) < taboo_data()[,"points"]) {
      
      "Mike wins"
      
    } else {"Mike loses, but it was four on five so..."}
    
  })
  
}

shinyApp(ui = ui, server = server)
