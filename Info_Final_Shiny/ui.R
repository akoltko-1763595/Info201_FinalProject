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
        p("The highest scored genre by fans was Rock 1960's with a score of 7655, while the lowest scored genre was Easy with a score of 209."),
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
  ),
  
  
  
  ## Quesiton 4: How well do sales dictate greatness? (Spencer)
  tabPanel(title = "Sales & Greatness",
           sidebarLayout(
             sidebarPanel(
               titlePanel("How well do sales dictate greatness? The answer may surprise you!"),
               p("Between Rolling Stone's 500 best albums and a dataset of the top 316 bestselling albums,
                 only 39 albums overlapped. The smallness of this number is surprising and it is even more surprising
                 to find that the higher selling albums (of the 39) tended to be more poorly received by Rolling Stone.
                 To supplement the conclusions of the Q1 tab, it appears that Rolling Stone likes less popular and older
                 albums."),
               p("In the second plot, it appears that while average sales for the best albums stay relatively
                 the same over time, the average greatness of music lessens according to Rolling Stone."),
               h5("Fame and Fortune"),
               p("If you wanted to be rich and famous, which genre is the best way to go? Pick a genre
                 and see its average sales and greatness as a percentage of the average of all genres (intersection of
                 lines denote average)."),
               selectInput(inputId = "genre_choice2", label = "Genre", choices = combined_best_and_sales$Genre)
               ),
             mainPanel(
               plotOutput("plotQ4"),
               br(),
               br(),
               br(),
               plotOutput("plotQ4num2"),
               br(),
               br(),
               br(),
               plotOutput("plotQ4num3")
             )
           )
  )
)