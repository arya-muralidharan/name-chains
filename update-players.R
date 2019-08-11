# ADD NEW PLAYERS TO THE CODES AND NAMES LISTS #
# THIS SCRIPT WILL NOT CHANGE NAMES THAT HAVE ALREADY BEEN SCRAPED #

######### PACKAGES #########
library(here)
library(rvest)
library(tidyverse)





######### CODES UPDATE #########

# import file that needs to be updated: codes
all_codes <- readRDS(here("save", "NBA_ABA_codes.rds"))

# main part of the basketball-reference url
main.url <- "https://www.basketball-reference.com/players/"

# list of relevant letters of the alphabet - add an 'x' if needed
letter.list <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
                 "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "y", "z")

#### re-scrape all codes ####
updated_codes <- list()
for(letter in letter.list){
  
  # read in letter page
  url_codes <- paste0(main.url, letter, "/")
  page_codes <- read_html(url_codes)
  
  # get player codes
  codes <- page_codes %>%
    html_nodes("table#players > tbody > tr > th") %>%
    html_attr("data-append-csv") %>%
    as.list()
  
  updated_codes <- c(updated_codes, codes)
}

# check to make sure codes haven't been removed
q_codes <- setdiff(all_codes, updated_codes)

# get list of new codes only
new_codes <- setdiff(updated_codes, all_codes)

# saveRDS(new_codes, here("save", "update_NBA_ABA_codes.rds"))

# update the local list of all codes
all_codes <- updated_codes
saveRDS(all_codes, here("save", "NBA_ABA_codes.rds"))





######### FULL NAMES UPDATE #########

# import file that needs to be updated: full names
all_names <- readRDS(here("save", "NBA_ABA_full_names.rds"))

# new_codes <- readRDS(here("save", "update_NBA_ABA_codes.rds"))
# main.url <- "https://www.basketball-reference.com/players/"

#### scrape new names ####
new_names <- list()
for(code in new_codes){
  
  letter <- substr(code, 1, 1)
  
  url <- paste0(main.url, letter, "/", code, ".html")
  page <- read_html(url)
  
  # get full name
  name <- page %>%
    html_nodes("#meta > div > p:nth-child(2) > strong > strong") %>%
    html_text()
  if(identical(name, character(0))){
    name <- page %>%
      html_nodes("#meta > div:nth-child(2) > p:nth-child(3) > strong > strong") %>%
      html_text()
  }
  if(identical(name, character(0))){
    name <- page %>%
      html_nodes("#meta > div > p:nth-child(3) > strong > strong") %>%
      html_text()
  }
  new_names <- c(new_names, name)
  
}

# saveRDS(new_names, here("save", "update_NBA_ABA_full_names.rds"))

# update the local list of all names
all_names <- c(all_names, new_names)
saveRDS(all_names, here("save", "NBA_ABA_full_names.rds"))





######### CLEAN NAMES UPDATE #########

# import file that needs to be updated: cleaned names
final_names <- readRDS(here("save", "NBA_ABA_final_names.rds"))

# new_names <- readRDS(here("save", "update_NBA_ABA_full_names.rds"))

# remove suffixes and split into first, middle, last, etc.
new_clean_names <- new_names %>%
  str_replace(" Jr.", "") %>%
  as.list() %>%
  str_replace(" Sr.", "") %>%
  as.list() %>%
  str_replace(" IV", "") %>%
  as.list() %>%
  str_replace(" III", "") %>%
  as.list() %>%
  str_replace(" II", "") %>%
  as.list() %>% 
  str_split(" ")

# turn this into a tibble
new_clean_names <- new_clean_names %>% 
  plyr::ldply(rbind) %>% 
  as_tibble()
colnames(new_clean_names) <- c("first", "two", "three", "four", "five")

# mutate: convert to character & create last name column
# mutate: fix certain names - manual inspection
# select: keep only first and last names
# distinct: remove duplicates
new_clean_names <- new_clean_names %>%
  mutate(first = as.character(first),
         two = as.character(two),
         three = as.character(three), 
         four = as.character(four),
         five = as.character(five),
         last = case_when(!is.na(five) ~ five,
                          !is.na(four) ~ four,
                          !is.na(three) ~ three,
                          !is.na(two) ~ two)) %>%
  # mutate(first = case_when(last == "" & first == "" ~ "", TRUE ~ first),
         # last = case_when(last == "" & first == "" ~ "", TRUE ~ last)) %>%
  select(first, last) %>%
  distinct()

# save locally
saveRDS(new_clean_names, here("save", "update_NBA_ABA_final_names.rds"))

# update the local tibble of clean names
final_names <- final_names %>%
  rbind(final_names, new_clean_names) %>%
  distinct()
saveRDS(final_names, here("save", "NBA_ABA_final_names.rds"))

######### NEXT #########

# now, go back to the main script and run the Name Chains section :)