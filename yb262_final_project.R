# Open webpage
library(RSelenium)
library(tidyverse)
library(data.table)
library(tidytext)
library(gridExtra)
library(ggrepel)
library(scales)


# Keep scrolling down page, loading new content each time. 


scrape_reddit <- function(subreddit) {
  rD <- rsDriver(browser="firefox", port=4548L, verbose=F)
  remDr = rD[["client"]]
  url = paste0("https://www.reddit.com/r/", subreddit, "/")
  remDr$navigate(url)
  last_height = 0 #
  scraped_text <- c("subreddit")
  repeat {   
    elems <- remDr$findElements(using = "class name", "_eYtD2XCVieq6emjKBH3m")
    
    for (elem in elems) {
      scraped_text <- c(scraped_text, elem$getElementText())
    }
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
    Sys.sleep(3) #delay by 3sec to give chance to load. 
    
    # Updated if statement which breaks if we can't scroll further 
    new_height = remDr$executeScript("return document.body.scrollHeight")
    if(unlist(last_height) == unlist(new_height) | length(scraped_text) > 5000) {
      break
    } else {
      last_height = new_height
    }
  }
  remDr$close()
  rD$server$stop()
  return(scraped_text)
}

worldnews_scrape <- scrape_reddit("worldnews")
science_scrape <- scrape_reddit("science")
showerthoughts_scrape <- scrape_reddit("Showerthoughts")
announcements_scrape <- scrape_reddit("announcements")

#Prepend a Title for the entries
#worldnews_scrape <- c("WorldNewsEntries", worldnews_scrape)
#science_scrape <- c("ScienceEntries", science_scrape)
#showerthoughts_scrape <- c("ShowerthoughtsEntries", showerthoughts_scrape)
#announcements_scrape <- c("AnnouncementsEntries", announcements_scrape)

#Write vector data into plain text files
fwrite(list(worldnews_scrape), file = "worldnews_scrape.txt")
fwrite(list(science_scrape), file = "science_scrape.txt")
fwrite(list(showerthoughts_scrape), file = "showerthoughts_scrape.txt")
fwrite(list(announcements_scrape), file = "announcements_scrape.txt")



worldnews_t <- data.table::fread(input = "worldnews_scrape.txt", sep = '\n') %>%
  as_tibble() %>% 
  filter(WorldNewsEntries != "\"\"") %>%
  unnest_tokens(word, 
                WorldNewsEntries, 
                token = "words") %>%
  anti_join(stop_words)

worldnews_counts <- worldnews_t %>% count(word, sort = TRUE)
  

science_t <- data.table::fread(input = "science_scrape.txt", sep = '\n') %>%
  as_tibble() %>% 
  filter(ScienceEntries != "\"\"") %>%
  unnest_tokens(word, 
                ScienceEntries, 
                token = "words") %>%
  anti_join(stop_words)


science_counts <- science_t %>% count(word, sort = TRUE)

showerthoughts_t <- data.table::fread(input = "showerthoughts_scrape.txt", sep = '\n') %>%
  as_tibble() %>% 
  filter(ShowerthoughtsEntries != "\"\"") %>%
  unnest_tokens(word, 
                ShowerthoughtsEntries, 
                token = "words") %>%
  anti_join(stop_words)

showerthoughts_counts <- showerthoughts_t %>% count(word, sort = TRUE)


announcements_t <- data.table::fread(input = "announcements_scrape.t <- t", sep = '\n') %>%
  as_tibble() %>% 
  filter(AnnouncementsEntries != "\"\"") %>%
  unnest_tokens(word, 
                AnnouncementsEntries, 
                token = "words") %>%
  anti_join(stop_words)


announcement_counts <- announcements_t %>% count(word, sort = TRUE)



frequency <- bind_rows(mutate(worldnews_t,subreddit="WorldNews"),
                       mutate(science_t,subreddit="Science"),
                       mutate(showerthoughts_t,subreddit="Showerthoughts"),
                       mutate(announcements_t, subreddit="Announcements")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  group_by(subreddit) %>%
  count(word) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = "subreddit", values_from = "proportion") %>%
  drop_na()
frequency


p1 <- ggplot(frequency, aes(x = `WorldNews`, y = `Science`)) + 
  geom_abline(color = "red", lty = 2, lwd=2) +
  geom_point(color="grey")+
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()

p2 <- ggplot(frequency, aes(x = `WorldNews`, y = `Showerthoughts`)) + 
  geom_abline(color = "red", lty = 2, lwd=2) +
  geom_point(color="grey")+
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()

p3 <- ggplot(frequency, aes(x = `WorldNews`, y = `Announcements`)) + 
  geom_abline(color = "red", lty = 2, lwd=2) +
  geom_point(color="grey")+
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()


p4 <- ggplot(frequency, aes(x = `Science`, y = `Showerthoughts`)) + 
  geom_abline(color = "red", lty = 2, lwd=2) +
  geom_point(color="grey")+
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()

p5 <- ggplot(frequency, aes(x = `Science`, y = `Announcements`)) + 
  geom_abline(color = "red", lty = 2, lwd=2) +
  geom_point(color="grey")+
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()

p6 <- ggplot(frequency, aes(x = `Showerthoughts`, y = `Announcements`)) + 
  geom_abline(color = "red", lty = 2, lwd=2) +
  geom_point(color="grey")+
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()


grid.arrange(p1, p2, p3, p4,p5, p6, nrow=2)





create_sentiment_pie <- function(data, title){
  
  sum_neg <- data %>% select(negative) %>% drop_na() %>% sum()
  sum_pos <- data %>% select(positive) %>% drop_na() %>% sum()
  x <-  c(sum_neg, sum_pos)
  labels <-  c("Negative", "Positive")
  
  piepercent<- round(100*x/sum(x), 1)
  
  pie(x, labels = paste0(piepercent, "%"), main = title, col = rainbow(length(x)))
  legend("topright", labels, cex = 0.8,
         fill = rainbow(length(x)))
}

  

worldnews_sentiment <- worldnews_t %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = "n")

write_csv(worldnews_sentiment, "worldnews_sentiment.csv")
science_sentiment <- science_t %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = "n")

write_csv(science_sentiment, "science_sentiment.csv")

showerthoughts_sentiment <- showerthoughts_t %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = "n")

write_csv(showerthoughts_sentiment, "showerthoughts_sentiment.csv")

announcements_sentiment <- announcements_t %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  pivot_wider(names_from = "sentiment", values_from = "n")

write_csv(announcements_sentiment, "announcements_sentiment.csv")

par(mfrow=c(2,2))
create_sentiment_pie(worldnews_sentiment, "World News")
create_sentiment_pie(science_sentiment, "Science")
create_sentiment_pie(showerthoughts_sentiment, "ShowerThoughts")
create_sentiment_pie(announcements_sentiment, "Announcements")




