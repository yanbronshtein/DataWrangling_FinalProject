# Open webpage
library(RSelenium)
library(tidyverse)
library(data.table)

# Keep scrolling down page, loading new content each time. 


scrape_reddit <- function(subreddit) {
  rD <- rsDriver(browser="firefox", port=4548L, verbose=F)
  remDr = rD[["client"]]
  url = paste0("https://www.reddit.com/r/", subreddit, "/")
  remDr$navigate(url)
  last_height = 0 #
  scraped_text <- NULL
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

#worldnews_scrape <- scrape_reddit("worldnews")
#science_scrape <- scrape_reddit("science")
#showerthoughts_scrape <- scrape_reddit("Showerthoughts")
#announcements_scrape <- scrape_reddit("announcements")

#Prepend a Title for the entries
worldnews_scrape <- c("WorldNewsEntries", worldnews_scrape)
science_scrape <- c("ScienceEntries", science_scrape)
showerthoughts_scrape <- c("ShowerthoughtsEntries", showerthoughts_scrape)
announcements_scrape <- c("AnnouncementsEntries", announcements_scrape)

#Write vector data into plain text files
fwrite(list(worldnews_scrape), file = "worldnews_scrape.txt")
fwrite(list(science_scrape), file = "science_scrape.txt")
fwrite(list(showerthoughts_scrape), file = "showerthoughts_scrape.txt")
fwrite(list(announcements_scrape), file = "announcements_scrape.txt")



worldnews_t <- data.table::fread(input = "worldnews_scrape.txt", sep = '\n') %>%
  as_tibble() %>% filter(WorldNewsEntries != "\"\"")

science_t <- data.table::fread(input = "science_scrape.txt", sep = '\n') %>%
  as_tibble() %>% filter(ScienceEntries != "\"\"")

showerthoughts_t <- data.table::fread(input = "showerthoughts_scrape.txt", sep = '\n') %>%
  as_tibble() %>% filter(ShowerthoughtsEntries != "\"\"")

announcements_t <- data.table::fread(input = "announcements_scrape.txt", sep = '\n') %>%
  as_tibble() %>% filter(AnnouncementsEntries != "\"\"")

