library(shiny)
ui <- navbarPage(title = "Music Analysis", inverse = TRUE, id = "nav",
  
   tabPanel(title = "Overview",
      sidebarLayout(
        sidebarPanel(    
          titlePanel("The Top-500s of Music"),
          selectInput(inputId = "song_choice", label = "Songs", choices = songs$Song)
        ),
        mainPanel(
          textOutput("danceability"),
          plotOutput("dance_plot")
        )
      )
   ),
                 
  
  ## Question 1: Does Greatness Hold Up? (Sam)
  tabPanel(title = "Q1", #Is music like fine cheese?,
    sidebarLayout(
      sidebarPanel(
        titlePanel("Does music have to be old to be good? It depends on who you trust."),
        p("As you can see in the graph to the right, Rolling Stone tends to rank older music much higher than recent music, whereas Spotify data shows that newer music is more popular."),
        p("The significant dip in Rolling Stone ratings around 1970 show that in Rolling Stone's opinion, the greats really do hold up. Spotify users on the other hand
          are much more liberal with their appreciation, and lean toward liking newer music more."),
        p("For the 300 bands analyzed (300 due to the fact that artists needed to have \"great\" songs and albums) the popularity
          score for each year was calculated by averaging all the bands scores for that year. To be included in a year, a band must
          have produced an album or song that year."),
        selectInput(inputId = "band_choice", label = "Band", choices = popularity_comparison_data$Artist)
      ),
      mainPanel(
        plotOutput("plotQ1"),
        br(),
        br(),
        br(),
        br(),
        plotOutput("plotQ1num2"),
        br(),
        br(),
        br(),
        br(),
        plotOutput("plotQ1num3")
      )
             
    )
    
  ),
  
  
  ## Question 2: What Makes an album great? (Andrew)
  tabPanel(title = "Q2"
    
  ),
  
  
  
  ## Question 3: What do fans and critics agree on? (Alex)
  tabPanel(
    title = "What Do Fans & Critics Agree On?",
    sidebarLayout(
      sidebarPanel(
        titlePanel("Can Fans and Critics agree on Music?"),
        p("From this chart, fans and critics opinions can differ quite a lot depending on the genre of music they are listening to."),
        p("Critics appeared to favor all of the genres in this data set more than fans, except for Easy and R&B 1990's music."),
        p("The highest scored genre by the critics was Rock 1960's with a score of 20339, while the lowest scored genre was Easy with a score of 96."),
        p("The highest scored genre by fans was TBD with a score of 11326, while the lowest scored genre was Easy with a score of 209."),
        selectInput(inputId = "genre_choice", label = "Genre", choices = genre_ranking$Genre, selected = 1)
      ),
      mainPanel(
        plotOutput("plotQ3_num1"),
        br(),
        br(),
        br(),
        br(),
        plotOutput("plotQ3_num2")
      )
    )
  )#,
  
  
  
  ## Quesiton 4: How well do sales dictate greatness? (Spencer)
  tabPanel(title = "Q4",
           sidebarLayout(
             sidebarPanel(
               titlePanel("How well do sales dictate greatness? The answer may surprise you!"),
               p("Between Rolling Stone's 500 best albums and a dataset of the top 316 bestselling albums,
                 only 41 albums overlapped. This number itself is surprising and it is even more surprising
                 that the higher selling albums tended to be more poorly received by Rolling Stone."),
               h2("Fame and Fortune"),
               p("If you could go back in time, what genre of music could you create to maximize your wealth
                 and legacy as a great musician? Choose a year and see which genre of music would return the
                 most dough and notoriety."),
               selectInput(inputId = "year_choice", label = "Year", choices = combined_best_and_sales$Year)
               ),
             mainPanel(
               plotOutput("plotQ4"),
               br(),
               br(),
               br(),
               br(),
               plotOutput("plotQ4num2"),
               br(),
               br(),
               br(),
               br(),
               plotOutput("plotQ4num3")
             )
           )
  )
 
  
)