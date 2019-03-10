library(shiny)
ui <- navbarPage(title = "Questions", inverse = TRUE, id = "nav",
                 
  
  ## Question 1: Does Greatness Hold Up? (Sam)
  tabPanel(title = "Q1", #Is music like fine cheese?,
    sidebarLayout(
      sidebarPanel(
        titlePanel("Does greatness hold up? It depends on who you trust."),
        p("As you can see in the graph to the right, Rolling Stone tends to rank older music much higher than recent music, whereas Spotify data shows that newer music is more popular."),
        selectInput(inputId = "band_choice", label = "Band", choices = popularity_comparison_data$Artist)
      ),
      mainPanel(
        plotOutput("plotQ1"),
        br(),
        br(),
        br(),
        br(),
        plotOutput("plotQ1num2")
      )
             
    )
    
  ),
  
  
  ## Question 2: What Makes an album great? (Andrew)
  tabPanel(title = "Q2"
    
  ),
  
  
  
  ## Question 3: What do fans and critics agree on? (Alex)
  tabPanel(title = "Q3"
    
  ),
  
  
  
  ## Quesiton 4: How well do sales dictate greatness? (Spencer)
  tabPanel(title = "Q4"
    
  )
  
)