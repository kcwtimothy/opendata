library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)

#load the combined_table =======================================================
load("data/table_list_combined/combined_table.rda")


#search tool====================================================================
search_tool <- function(df,col, keywords, matches, ignore.case = TRUE){
  require(tidyverse)
  if(length(col) == 1){
    rows <- which(grepl(keywords, df[,col],ignore.case = ignore.case))
    return(df[rows,])
  }else 
  x <- sapply(df[,col], function(x) grepl(keywords, x, ignore.case = ignore.case))
  x2 <- as.data.frame(x) %>%
    mutate(count = rowSums(.))
  rows <- which(x2$count >= matches)
  return(df[rows,])
}

#fetch tool====================================================================
fetch_tool <- function(df, rows, skip = 0){
  require(tidyverse)
  require(readxl)
  require(httr)
  require(jsonlite)
  if(grepl("fd.jsp?", df[rows, "download_links"])){
    warning("links containing fd.jsp? may result in corrupted files, may need to download manually")
    if(df[rows, "data_format"] == "CSV"){
      if(grepl("zip", df[rows, "download_links"])){
      download.file(df[rows, "download_links"], tf <- tempfile(fileext = ".zip"), mode = "wb")
      unzip_tf <- unzip(tf)
      x <- lapply(unzip_tf, function(x) read_csv(x))
      return(x)
      } else
        download.file(df[rows, "download_links"], tf <- tempfile(fileext = ".csv"), mode = "wb")
      x <- read_csv(tf, skip = skip)
      return(x)
    } else if(df[rows, "data_format"] == "XLSX"){
      download.file(df[rows, "download_links"], tf <- tempfile(fileext = ".xlsx"), mode = "wb")
      sheets <- excel_sheets(tf)
      x <- lapply(sheets, function(x) read_excel(tf, sheet = x, skip = skip))
      names(x) <- sheets
      return(x)
    } else if(df[rows, "data_format"] == "XLS"){
      download.file(df[rows, "download_links"], tf <- tempfile(fileext = ".xls"), mode = "wb")
      sheets <- excel_sheets(tf)
      x <- lapply(sheets, function(x) read_excel(tf, sheet = x, skip = skip))
      names(x) <- sheets
      return(x)
    } else
      print("file format not available yet")
  } else {
  if(df[rows, "data_format"] == "CSV"){
    x <- read_csv(df[rows, "download_links"], skip = skip)
    return(x)
  } else if(df[rows, "data_format"] == "XLSX"){
      GET(df[rows, "download_links"], write_disk(tf <- tempfile(fileext = ".xlsx")))
      sheets <- excel_sheets(tf)
      x <- lapply(sheets, function(x) read_excel(tf, sheet = x, skip = skip))
      names(x) <- sheets
      return(x)
    } else if(df[rows, "data_format"] == "XLS"){
      GET(df[rows, "download_links"], write_disk(tf <- tempfile(fileext = ".xls")))
      sheets <- excel_sheets(tf)
      x <- lapply(sheets, function(x) read_excel(tf, sheet = x, skip = skip))
      names(x) <- sheets
      return(x)
    } else if(df[rows, "data_format"] == "JSON"){
      x <- fromJSON(df[rows, "download_links"])
      return(x)  
    } else
    print("file format not available yet")
  }
}


#functions to call official API ================================================
list_categories <- function() {
  require(rvest)
  require(httr)
  require(dplyr)
  url <- "https://data.gov.hk/en/help/api-spec"
  tabs <- url %>% GET() %>% content() %>% html_table(fill = TRUE)
  tabs[
    sapply(tabs, function(x) grepl("Category", names(x[1]), ignore.case = TRUE))
    ][[1]]
}

list_providers <- function() {
  require(rvest)
  require(httr)
  require(dplyr)
  url <- "https://data.gov.hk/en/help/api-spec"
  tabs <- url %>% GET() %>% content() %>% html_table(fill = TRUE)
  tabs[
    sapply(tabs, function(x) grepl("provider", names(x[1]), ignore.case = TRUE))
    ][[1]]
}

list_hist_file <- function(start = Sys.Date() - 1, end = Sys.Date() - 1,
                           category = NULL, provider = NULL,
                           format = NULL, search = NULL, order = NULL,
                           skip = NULL) {
  require(httr)
  require(jsonlite)
  api_url <- "https://api.data.gov.hk/v1/historical-archive/list-files"
  start <- format(as.Date(start), "%Y%m%d")
  end <- format(as.Date(end), "%Y%m%d")
  req <- list(
    start = start,
    end = end,
    category = category,
    provider = provider,
    format = format,
    search = search,
    order = order,
    skip = skip
  )
  res <- GET(api_url, query = req)
  fromJSON(content(res, "text", encoding = "UTF-8"))$files
}

list_hist_file_nomax <- function(...) {
  max <- 500
  result <- list()
  i <- 0
  while (TRUE) {
    data <- list_hist_file(..., skip = i * max)
    if (length(data) == 0 || is.null(data) || nrow(data) == 0) break
    result <- rbind(result, data)
    if (nrow(data) < max) break
    i <- i + 1
  }
  result
}

hist_file_versions <- function(url, start, end = NULL) {
  require(httr)
  require(jsonlite)
  api_url <- "https://api.data.gov.hk/v1/historical-archive/list-file-versions"
  if (is.null(end)) end <- start
  start <- format(as.Date(start), "%Y%m%d")
  end <- format(as.Date(end), "%Y%m%d")
  req <- list(
    url = url,
    start = start,
    end = end
  )
  res <- GET(api_url, query = req)
  fromJSON(content(res, "text", encoding = "UTF-8"))
}

hist_file_url <- function(url, timestamp) {
  api_url <- "https://api.data.gov.hk/v1/historical-archive/get-file"
  sprintf(
    "%s?url=%s&time=%s",
    api_url,
    URLencode(url, reserved = TRUE),
    URLencode(timestamp, reserved = TRUE)
  )
}

get_historical <- function(df, rows, start, end){
  require(tidyverse)
  if(df[rows, "has_historical"] == "N" ){
    print("Historical files not available for this data resource")
  } else
    hist <- hist_file_versions(
      url = df[rows, "download_links"],
      start = start,
      end = end)
  dl_url <- sapply(hist$timestamps, function(x)
    hist_file_url(
    url = df[rows, "download_links"],
    timestamp = x)
  )
  table <- data.frame(df[rows, "data_dataset"], df[rows, "data_resource"],
                      start, end, hist$`version-count`,hist$timestamps, 
                      df[rows, "data_format"], dl_url, 
                      stringsAsFactors = FALSE, row.names = NULL) %>%
    rename(data_dataset = 1,
           data_resource = 2,
           version_count = 5,
           time_stamps = 6,
           data_format = 7,
           download_links = 8) %>%
    mutate(id = row.names(.)) %>%
    select(id, 1:8)
  return(table)
}

# example of using the above functions

# 1- get the full list of all files having historical data
# full_list <- list_hist_file_nomax()

# 2 - get the timestamp of each historical data
# example_url <- "http://applications.edb.gov.hk/datagovhk/data/SchoolBasicInfo.xml"
# timestamp <- hist_file_versions(example_url, "2019-01-01", "2020-01-01")

# 3 - get the download url for each historical data
# historical_url <- hist_file_url(example_url, "20190928-0850")

# function demo
# search "health" in no.2-4 column:data_dataset,data_description,data_resource
# hence, you can also specify the number of match in the result,in this case, i set
# all "3" columns should contain "health"
x <- search_tool(combined_table, 2:4, "health", 3)
# download the table from row 4756
x1 <- fetch_tool(combined_table, 4756)
# check historical files from row 4756 and specify the date you wish to find
x2 <- get_historical(combined_table, 4756, "2016-01-01", "2020-04-01") #return a table
# download 2nd historical files with timestamps "20190703-1842"
x3 <- fetch_tool(x2, 2)
