library(shiny)
source("analysis.R")

ui <- navbarPage(title = "Music Analysis", inverse = TRUE, id = "nav",
 
  ## Question 1: Does Greatness Hold Up? (Sam)
  tabPanel(title = "Q1",
    sidebarLayout(
      sidebarPanel(
        titlePanel("Does music have to be old to be good?"),
        h4("It depends on who you trust."),
        p("As you can see in the graph to the right, Rolling Stone tends to rank older music much higher than recent music, whereas Spotify data shows that newer music is more popular."),
        p("The significant dip in Rolling Stone ratings around 1970 show that in Rolling Stone's opinion, the greats really do hold up. Spotify users on the other hand
          are much more liberal with their appreciation, and lean toward liking newer music more."),
        p("For the 300 bands analyzed (300 due to the fact that artists needed to have \"great\" songs and albums) the popularity
          score for each year was calculated by averaging all the bands scores for that year. To be included in a year, a band must
          have produced an album or song that year."),
        selectInput(inputId = "band_choice", label = "Band", choices = popularity_comparison_data$Artist, selected = "  Queen")
      ),
      mainPanel(
        plotOutput("plotQ1"),
        plotOutput("plotQ1num2")
      )
    )
  ),
  
  
  ## Question 2: What Makes an album great? (Andrew)
  tabPanel(title = "Q2",
    sidebarLayout(
      sidebarPanel(
        titlePanel("What Makes an Album Great?"),
        h4("It's more than just what Rolling Stone Thinks!"),
        p("As we discovered more data, it became clear that Rolling Stone was not the final word in what was good music. By taking Rolling Stones' list and the 316 best selling albums of all time worldwide, we had a set of 61 Artists who are both successfull critically and commercially. For each of those artists, we also found their popularity on Spotify."),
        p("With that data, we could rank each artist from 1 to 61 in 3 distinct categories: critical acclaim, commercial success, and longevity of popularity. Averaging these ranks, we then have an overall rank for each artist. To the left, you can see a graph of that overallrank, relative to where each measure ranked it."),
        p("Below that, you can choose an artist and see where they ranked and how well they performed for each measure and overall!"),
        selectInput(inputId = "artist_choice_q2", label = "Artist", choices = RS_SP_AS_pop$Artist, selected = "Elton John")
      ),
      mainPanel(
        plotOutput("plotQ2Overall"),
        plotOutput("plotQ2Artist"),
        uiOutput("artistDetails")
      )
    )
  ),
  
  
  
  ## Question 3: What do fans and critics agree on? (Alex)
  tabPanel(
    title = "Q3",#"What Do Fans & Critics Agree On?",
    sidebarLayout(
      sidebarPanel(
        titlePanel("Can Fans and Critics agree on Music?"),
        h4("T be frank, 1960's Rock"),
        p("From this chart, fans and critics opinions can differ quite a lot depending on the genre of music they are listening to."),
        p("Critics appeared to favor all of the genres in this data set more than fans, except for Easy and R&B 1990's music."),
        p("The highest scored genre by the critics was Rock 1960's with a score of 20339, while the lowest scored genre was Easy with a score of 96."),
        p("Similarly, the highest scored genre by fans was Rock 1960's with a score of 7655, while the lowest scored genre was Easy with a score of 209."),
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
  tabPanel(title = "Q4",
           sidebarLayout(
             sidebarPanel(
               titlePanel("How well do sales dictate greatness?"), 
               h4("The answer may surprise you!"),
               p("Between Rolling Stone's 500 best albums and a dataset of the top 316 bestselling albums,
                 only 39 albums overlapped. This is a surprisingly small number and when we plot sales against Rolling Stone's
                 ranking, we find that as sales increases the ranking generally improves. The correlation coefficient is not very high,
                 but it can still suggest a relationship. To supplement the conclusion from Q1,
                 Rolling Stone may like older albums, but they are not deterred by commercial success."),
               p("Note: The higher the greatness ranking is the better the album did"),
               hr(),
               p("In the second plot, it appears that while average sales for the best albums stay relatively
                 the same over time, the average greatness of music lessens according to Rolling Stone. The correlation
                 coefficient of this is stronger than with sales and greatness, which aligns with our first question's
                 plot as well."),
               hr(),
               h4("Fame and Fortune"),
               p("If you wanted to be rich and famous, which genre is the best way to go? Pick a genre
                 and see its average sales and greatness expressed as a percentage of the average of all genres (intersection of
                 lines denotes average)."),
               selectInput(inputId = "genre_choice2", label = "Genre", choices = combined_best_and_sales$Genre, selected = 1)),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Sales vs. Greatness",
                                    plotOutput("plotQ4"),
                                    p(paste("Correlation coefficient:",
                                            round(cor(combined_best_and_sales$Probable, combined_best_and_sales$Place), digits = 3)))),
                           tabPanel("Time and Genre Performance",
                                    plotOutput("plotQ4num2"),
                                    verbatimTextOutput("textQ4", placeholder = FALSE),
                                    br(),
                                    hr(),
                                    plotOutput("plotQ4num3"))
               ))
           )),
  tabPanel(
    title = "Sources",
    h2("Our Sources:"),
    uiOutput("bibli")
  )
)