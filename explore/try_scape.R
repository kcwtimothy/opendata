# ============================================================================ #
# HK Open Data ===============================================================
# scrape info list of data (dataset & resource) ==============================
# ============================================================================ #

# ============================================================================ #
# remarks:
# it cost about 5 mins to kick all button of "load more" on home page
# it cost about 3 hours to nevigate and read all dataset level webpage (#1472)
# ============================================================================ #


# 0.Configuration ==============================================================
library(RSelenium)
library(rvest)
library(dplyr)
library(tidyverse)

# Selenium 
# -- terminal run ---------------------------------------- #
# docker run -d -p 4444:4444 selenium/standalone-chrome
# docker ps
# -------------------------------------------------------- #

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")
remDr$open()


# 1.open home page and get info of dataset =====================================
# the key is to get url of each resource page 

cat("--- start navigate home page at", as.character(Sys.time()), " --- \n")
# url <- "https://data.gov.hk/en-datasets"
url <- "https://data.gov.hk/en-datasets/provider/ia?order=name&file-content=no"
remDr$navigate(url)

repeat {
  # do something
  load_button <- remDr$findElement(using = 'css selector',".load-more")
  load_button$clickElement()
  Sys.sleep(5)
  x <- try(remDr$findElement(using = 'css selector', ".load-more"), silent = TRUE)
  # exit if the condition is met
  if (class(x) == "try-error") break
}

page_source <- remDr$getPageSource()
page_docu <- xml2::read_html(page_source[[1]])

dataset <- list()

dataset$dataset_name <- page_docu %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_text()

dataset$dataset_cate <- page_docu %>% 
  html_nodes(xpath = '//*[@id="meta"]') %>% 
  html_nodes(".badge-primary a") %>% 
  html_text()

dataset$dataset_provider <- page_docu %>% 
  html_nodes(".media-view") %>%
  html_text()

dataset$dataset_description <- page_docu %>%
  html_nodes(".notes") %>%
  html_text()

dataset$dataset_link <- page_docu %>%
  html_nodes(".dataset-heading a") %>%
  html_attr("href")

cat("--- finish navigate home page at", as.character(Sys.time()), " --- \n")


# 2.open dataset page get info for dataset and resource ========================
list_dataset <- list()
list_resource <- list()
n <- 0L

for(i in dataset$dataset_link){
  n <- n + 1
  cat(n, ":", "https://data.gov.hk", i, "\n", sep = "")
  cat("--- start scrapping at", as.character(Sys.time()), " --- \n")
  
  remDr$navigate(paste0("https://data.gov.hk", i))
  Sys.sleep(5)
  page_source <- remDr$getPageSource()
  page <- read_html(page_source[[1]])
  
  # @list_dataset --------------------------------------------------------------
  list_dataset[[i]] <- page %>% 
    html_nodes(xpath = '//*[@id="dataset-meta"]') %>%
    "[["(1) %>%
    xml_attrs() %>%  
    data.frame() %>%
    t() %>%
    data.frame() %>%
    mutate_if(is.factor, as.character) 
  
  # list_dataset[[i]]$data.dataset.datadict <- page %>%
  #   html_nodes(xpath = '//*[@id="data-dictionary"]') %>%
  #   html_nodes("a") %>% 
  #   html_attr("href") %>%
  #   "[["(1)
  
  list_dataset[[i]]$data.dataset.update.freq <- page %>% 
    html_nodes(".date-label") %>%
    html_nodes(".update-frequency-area") %>%
    html_text()
  
  # @list_resource -------------------------------------------------------------
  list_resource[[i]] <- page %>%
    html_nodes(".data-resource-list") %>%
    html_nodes(".row.item") %>%
    html_attrs() %>%
    data.frame() %>%
    t() %>%
    data.frame() %>%
    select(-class) %>%
    mutate_if(is.factor, as.character)
  
  list_resource[[i]]$data.resource.name.en <- page %>%
    html_nodes(".title a") %>%
    html_text()
  
  cat("--- resource count:", nrow(list_resource[[i]]), "--- \n")
  cat("--- finish scrapping at", as.character(Sys.time()), " --- \n")
}


# 3. to data frame =============================================================
dataset_df <- data.frame(dataset, stringsAsFactors = FALSE) %>%
  mutate(dataset_no = row_number()) %>%
  select(dataset_no, everything())

list_dataset_df <- data.frame()
for (i in 1:length(list_dataset)){
  dtt <- data.frame(list_dataset[[i]]) %>%
    mutate(dataset_no = i) %>%
    select(dataset_no, everything())
  list_dataset_df <- rbind(list_dataset_df, dtt)
  cat(nrow(list_dataset_df), "\n")
}

list_resource_df <- data.frame()
for (i in 1:length(list_resource)){
  dtt <- data.frame(list_resource[[i]]) %>%
    mutate(dataset_no = i) %>%
    select(dataset_no, everything())
  list_resource_df <- rbind(list_resource_df, dtt)
  cat(nrow(list_resource_df), "\n")
}



# chking... ------------------------------------------------------------------ #
names(dataset_df)
names(list_dataset_df)
names(list_resource_df)

a <- dataset_df$dataset_name
b <- list_dataset_df$data.dataset.title.en
sum(!a %in% b)
sum(!b %in% a)

# findings:
# ~ [result]
# ---------------------------------------------------------------------------- #


# 4. combine info of dataset and resource ======================================
list_df <- list_dataset_df %>%
  full_join(list_resource_df, by = "dataset_no")

list_df_ia <- list_df
# x.save data ==================================================================
save(dataset, list_dataset, list_resource,
     dataset_df, list_dataset_df, list_resource_df,
     file = "data/hk_open_data_full_list.rda")

save(list_df_ia, file = "data/list_df_ia.rda")

# ****** end ****** ============================================================
