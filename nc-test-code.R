#### 1: A NAMES FROM A PAGE ####

# get webpage
a.url <- "https://www.basketball-reference.com/players/a/"
a_page <- read_html(a.url)

# names without accents
a_noaccent <- a_page %>% 
  html_nodes("table#players > tbody > tr > th > a") %>% 
  html_text() %>%
  matrix(ncol = 1, byrow = TRUE)

colnames(a_noaccent) <- "firstlast"

a_noaccent <- as_tibble(a_noaccent)

# names with accents, etc.
a_accent <- a_page %>% 
  html_nodes("table#players > tbody > tr > th > strong > a") %>% 
  html_text() %>%
  matrix(ncol = 1, byrow = TRUE)

colnames(a_accent) <- "firstlast"

a_accent <- as_tibble(a_accent)

# bind together
all_as <- bind_rows(a_accent, a_noaccent)

# separate into first and last names and sort alphabetically by last name 
all_as <- all_as %>% 
  separate(firstlast, c("first", "last"), " ") %>%
  arrange(last)



##### 2: A NAMES FROM CODES ON A PAGE ####

# og webpage
a.url <- "https://www.basketball-reference.com/players/a/"
a_page <- read_html(a.url)

# list of codes
a_codes <- a_page %>%
  html_nodes ("table#players > tbody > tr > th") %>%
  html_attr("data-append-csv") %>%
  as.list()

# remove strings that don't start with 'a' to another list (to deal with later)
a_bad_codes <- a_codes[which(substr(a_codes, 1, 1) != "a")]
a_codes <- a_codes[-which(substr(a_codes, 1, 1) != "a")]

a_test_bad_codes <- c(a_bad_codes, "ulisty01")

a_full_names <- list()
for(code in a_codes){
  a.url.x <- paste0("https://www.basketball-reference.com/players/a/", code, ".html")
  a_page_x <- read_html(a.url.x)
  name <- a_page_x %>%
    html_nodes("#meta > div > p:nth-child(2) > strong > strong") %>%
    html_text()
  if(identical(name, character(0))){
    name <- a_page_x %>%
      html_nodes("#meta > div:nth-child(2) > p:nth-child(3) > strong > strong") %>%
      html_text()
  }
  if(identical(name, character(0))){
    name <- a_page_x %>%
      html_nodes("#meta > div > p:nth-child(3) > strong > strong") %>%
      html_text()
  }
  a_full_names <- c(a_full_names, name)
}

a_full_names <- as.matrix(a_full_names)

colnames(a_full_names) <- "firstlast"

a_full_names <- as_tibble(a_full_names)


# test bad code
fixed_abcodes <- list()
for(abcode in a_bad_codes){
  
  abletter <- substr(abcode, 1, 1)
  
  aburl <- paste0("https://www.basketball-reference.com/players/", abletter, "/", abcode, ".html")
  abpage <- read_html(aburl)
  
  #get full name
  abname <- abpage %>%
    html_nodes("#meta > div > p:nth-child(2) > strong > strong") %>%
    html_text()
  if(identical(abname, character(0))){
    abname <- abpage %>%
      html_nodes("#meta > div:nth-child(2) > p:nth-child(3) > strong > strong") %>%
      html_text()
  }
  if(identical(abname, character(0))){
    abname <- abpage %>%
      html_nodes("#meta > div > p:nth-child(3) > strong > strong") %>%
      html_text()
  }
  fixed_abcodes <- c(fixed_abcodes, abname)
  
  
}



#### 3: B NAMES FROM ALL CODES ####

b_codes <- all_codes[which(substr(all_codes, 1, 1) == "c")]

b_names <- list()
for(code in b_codes){
  
  letter <- substr(code, 1, 1)
  
  url <- paste0(main.url, letter, "/", code, ".html")
  page <- read_html(url)
  
  #get full name
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
  b_names <- c(b_names, name)
  
}