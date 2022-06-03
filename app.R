#install.packages("bslib")

library(shiny)
library(bslib)
library(rtweet)
library(dplyr)
library(tibble)
library(syuzhet)
library(ggplot2)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
myPalette <- brewer.pal(5, "Set2") 




twitter_token <- create_token(consumer_key = "TceFIyLzeRd1xI9VtvQzL8IpY",consumer_secret = "PIf2EMZRcThpQj3k0mr4HbGh8tKQgIiULwbOy54VrR0mqKZH5r",access_token = "326110718-G1UX0TAuRoVbnhTqlEvESjtYMJeuTIsfJyQChxoD",access_secret = "6IbW0PqjK0mbfRtJ8iabBkL9PvydeHh6uuHsG1wE5Jput",set_renv = TRUE)
name <- readline(prompt="Enter name: ")
timeline <- get_timeline(name, n=3200)

print("hello")

no_of_retweets <- sum(timeline$is_retweet=="TRUE")
no_of_quotes <- sum(timeline$is_quote=="TRUE")
x <- c(no_of_retweets, no_of_quotes, (3200-no_of_retweets+no_of_quotes))
labels <- c("Retweets", "Quotes", "Organic Tweets")


s <- get_nrc_sentiment(timeline$text)



max_lim <- max(table(timeline$created_at))
data <- table(timeline$created_at)
data2 <- as.data.frame(data)
data2$Var1 = as.Date(data2$Var1)
colnames(data2) <- c("Date", "Freq")




for_hashtag <- unlist(unique(timeline$hashtags))




account_loc <- unique(timeline$location)
no_followers <- unique(timeline$followers_count)
no_following <- unique(timeline$friends_count)
fr_fllw <- no_following/no_followers
favcount <- unique(timeline$favourites_count)
listcount <- unique(timeline$listed_count)
creator <- unique(timeline$account_created_at)

print(paste(" Account Created on ->",toString(creator)))
print(paste(" Account's Location ->",toString(account_loc)))
print(paste(" Number of Followers ->",toString(no_followers)))
print(paste(" Number of Following  ->",toString(no_following)))
print(paste(" Friends/Followers Ration ->",toString(fr_fllw)))
print(paste(" Number of Favorited Tweets ->",toString(favcount)))
print(paste(" Numbers of Lists featured in ->",toString(listcount)))




# Define UI for application that draws a histogram
ui <- fluidPage(

  
    theme = bs_theme(version = 4, bootswatch = "superhero"),
    # Application title
    titlePanel(h1("User Analytics", align = "center")),
    
    
    
   
    
    
   
      
    #The timeplot

    
    fluidRow(
      column(12, plotOutput("tweets_by_time"))
    ), 
    
    
    # The four plots
    
    fluidRow(
      column(6, plotOutput("source_breakup")),
      column(6, plotOutput("lang_breakup"))
      
      ), 
    
    fluidRow(
      column(6, plotOutput("sentimentPlot")),
      column(6, plotOutput("retweet_breakup"))
    ),

    fluidRow(
      column(12, plotOutput("hashtag"))
    ))




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Tweets by Type
  

  output$retweet_breakup <- renderPlot({pie(x,labels, main="Tweets by Type", col=myPalette, border="white", edges=15)})
  
  
  # Tweets by Source

  output$source_breakup <- renderPlot({pie(table(timeline$source), main = "Tweets by Source", border="white", edges=15, col=myPalette)})
  
  # Tweets by Sentiment
  
 
  output$sentimentPlot <- renderPlot({pie(colSums(s),  main = 'Sentiment Scores Tweets', border="white", edges = 15, col=myPalette)})
  
  

  # Tweets by Languages
  
  output$lang_breakup <- renderPlot({pie(table(timeline$lang), main = "Tweets by Language", border = "white", edges=15, col=myPalette)})
  
  
  
  # Tweets by Time 

  output$tweets_by_time <- renderPlot({data2 %>% ggplot(aes(x=Date, y=Freq))+geom_path()+geom_point()})
  
  # Hashtag wordcloud

  output$hashtag <- renderPlot({wordcloud(for_hashtag, title="Wordcloud of Hashtags")})
  
  
  
  
  
    

}
# Run the application 
shinyApp(ui = ui, server = server)
