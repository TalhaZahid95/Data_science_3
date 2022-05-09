#############################
##### DS 3##################
###FinalProject############
###Scrapping Data##########
##Muhammad Talha Zahid####

##clear the environment
rm(list=ls())

## Load packages
library(rvest)
library(tidyverse)
library(data.table)
library(stringr)
library(tidyr)
#install.packages('pbapply')
library(pbapply)
library(dplyr)

#Links for the episodes

pages <- paste0("https://transcripts.foreverdreaming.org/viewforum.php?f=67&start=", seq(0, 60, 20))

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



episode_links <- episode_links %>% 
  filter(episode_links$name != "Game of Thrones Transcripts Index",
           episode_links$name != "Updates: (05/10/22) **Summer 2022 Challenge**",
           episode_links$name != "05x00 - A Day in the Life",
                          episode_links$name != "07x00 - The Story So Far",
                          episode_links$name != "08x00 - Movie - Greatest Moments ( 2018)",
         episode_links$name != "08x90 - The Last Watch")

## There are 2 duplicated episodes 

episode_links <- episode_links %>% 
  filter(episode_links$link != "./viewtopic.php?f=67&t=11127&sid=949f39436f79f5c09f119069d36957da",
         episode_links$link !="./viewtopic.php?f=67&t=11139&sid=949f39436f79f5c09f119069d36957da")

# 73 links remain which is the total number of episodes in the series

# Get the transcript for all episodes -------------------------------------

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


saveRDS(full_df, "GOT_data.rds")

write.csv(full_df, "GOT_data.csv", row.names = F)

