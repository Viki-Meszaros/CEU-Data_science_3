#####################################################################
##          DATA SCIENCE 3 - Unstructured text analysis
##
##   Get the transcripts for all "How I met your mother" episodes.
##   I will scrape them from
##   https://transcripts.foreverdreaming.org/viewforum.php?f=177
##
#####################################################################


## Load packages
library(rvest)
library(tidyverse)
library(data.table)
library(stringr)
library(tidyr)
library(pbapply)
# Get the links for the episodes ------------------------------------------

#### Links for the 9 pages where the links for episodes can be found
pages <- paste0("https://transcripts.foreverdreaming.org/viewforum.php?f=177&start=", seq(0, 200, 25))

#### Get the links for all episodes in one table
process_one_link <- function(my_link){
  t <- read_html(my_link)
  episodes <- list()
  episodes[['name']] <- t %>% html_nodes(".topictitle") %>% html_text()
  episodes[['link']] <- t %>% html_nodes(".topictitle") %>% html_attr('href')
  return(episodes)
}

episode_links <- data.table(rbindlist(lapply(pages, process_one_link)))


#### There are links to Info pages that are not episodes -> the name of them is 
####  "Please Read Updates: Take the 2021 Challenge!", lets get rid of them
episode_links <- episode_links[name != "Please Read Updates: Take the 2021 Challenge!",]

## 208 links remain which is perfect as there are exactly this amount of episodes in the series


# Get the transcript for all episodes -------------------------------------

# link <- episode_links$link[2]
get_transcript <- function(link) {
  # print(link)
  t <- read_html(paste0("https://transcripts.foreverdreaming.org", str_sub(link, start = 2) ))
  transcript <- t %>% html_nodes("#pagecontent p") %>% html_text()
  tinfo <- t %>% html_nodes('h2') %>% html_text()
  transcript <- str_subset(transcript, "^(?!\\[)")
  transcript <- str_subset(transcript, "^(?!\\()")
  transcript <- str_subset(transcript, "^(?!Scene)")
  transcript<- transcript[grepl(':', transcript, fixed = T)]
  textdf <- 
    rbindlist(
      lapply(transcript, function(x){
        t_piaces <- strsplit(x, ':')[[1]]
        data.table('actor' = t_piaces[1], 'text' = trimws(paste(t_piaces[2:length(t_piaces)], collapse = " " )) )
      })
    )
  textdf$season <- substring(tinfo, 1, 2)
  textdf$episode <- substring(tinfo, 4, 5)
  textdf$title <- substring(tinfo, 9,nchar(tinfo))
  return(textdf)
}

t_list <- pblapply(episode_links$link, get_transcript)
full_df <- rbindlist(t_list, fill = T)


saveRDS(full_df, "HIMYM_data.rds")

write.csv(full_df, "HIMYM_data.csv", row.names = F)






















