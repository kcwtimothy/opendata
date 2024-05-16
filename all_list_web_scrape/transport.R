library(RSelenium)
library(rvest)
library(dplyr)
library(gsubfn)
#set up RSelenium server========================================================
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")
remDr$open()


#get info on  data==============================================================
remDr$navigate("https://data.gov.hk/en-datasets/category/transport?order=name&file-content=no")

#dynamic webpage,needs to scroll down 5 times to reveal all the results=========
replicate(3,
          {
            load_button <- remDr$findElement(using = 'css selector',
                                             ".load-more")
            load_button$clickElement()
            Sys.sleep(5)
          })

#get the page html and extract names and data providers=========================
page_source <- remDr$getPageSource()

stat <- xml2::read_html(page_source[[1]]) %>% 
  html_nodes(".dataset-heading") %>%
  html_text()
data_provider <- xml2::read_html(page_source[[1]]) %>% 
  html_nodes(".media-view") %>%
  html_text()
data_description <- xml2::read_html(page_source[[1]]) %>%
  html_nodes(".notes") %>%
  html_text()

#get all  links for the data_tables=============================================
link <-  xml2::read_html(page_source[[1]]) %>% 
  html_nodes(".dataset-heading a") %>%
  html_attr("href")

file_no <- list()
dataset_file_size <- list()
update_freq <- c()
table_name <- list()
api <- list()
ext <- list()
url <- list()
download_links <- list()
last_update <- list()
file_size <- list()
has_historical <- list()

for(i in link){
  remDr$navigate(paste0("https://data.gov.hk", i))
  data_source <- remDr$getPageSource()
  Sys.sleep(1)
  tryCatch({
    file_no[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".data-resources-container") %>%
      html_nodes("div") %>%
      html_attr("data-dataset-file-count")
    dataset_file_size[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".data-resources-container") %>%
      html_nodes("div") %>%
      html_attr("data-dataset-file-size")
    update_freq[i] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".update-frequency-area") %>%
      html_text()
    table_name[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".middle") %>%
      html_text()
    ext[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".exten") %>%
      html_text()
    download_links[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".data-resource-list") %>%
      html_nodes("div") %>%
      html_attr("data-resource-url")
    last_update[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".data-resource-list") %>%
      html_nodes("div") %>%
      html_attr("data-resource-last-update-date")
    file_size[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".data-resource-list") %>%
      html_nodes("div") %>%
      html_attr("data-resource-file-size") 
    has_historical[[i]] <- xml2::read_html(data_source[[1]]) %>%
      html_nodes(".data-resource-list") %>%
      html_nodes("div") %>%
      html_attr("data-resource-has-historical") 
    Sys.sleep(2)
  })
}

#join the data into a table=====================================================
cleaned_name1 <- gsub("\n", "", unlist(table_name)) %>%
  trimws(., which = "both")
stat <- trimws(stat, which = "both")
data_format <- unlist(ext)
has_api <- ifelse(grepl("API Available", cleaned_name1), "1", "0")
cleaned_name <- gsub("API Available", "", cleaned_name1) %>%
  trimws(., which = "right")
download_links <- unlist(download_links) %>%
  .[!is.na(.)]
data_dataset_size <- unlist(dataset_file_size) %>% na.omit(.)
file_count <- unlist(file_no) %>% na.omit(.)
last_update <- unlist(last_update) %>% na.omit(.)
data_resource_size <- unlist(file_size) %>% na.omit(.)
has_historical <- unlist(has_historical) %>% na.omit(.)


ref <- data.frame(stat, data_provider, update_freq, data_description,
                  file_count, data_dataset_size) %>%
  mutate(file_count = as.numeric(as.character(file_count)))

transport <- ref[rep(
  row.names(ref), ref$file_count), 1:ncol(ref)] %>%
  cbind(cleaned_name, data_format, has_api, download_links,has_historical,
        data_resource_size, last_update) %>%
  mutate(data_dataset = as.character(stat),
         data_resource = as.character(cleaned_name),
         dataset_description = as.character(data_description),
         data_category = "transport",
         data_dataset_size = as.numeric(as.character(data_dataset_size)),
         data_resource_size = as.numeric(as.character(data_resource_size)),
         last_update = as.Date(as.character(last_update), "%d/%m/%y")) %>%
  select(data_dataset, dataset_description, data_resource,
         data_category, data_provider, data_format,
         update_freq, has_api, has_historical,
         download_links, data_dataset_size,
         data_resource_size, last_update)

write.csv(transport, 
          file = "data/table_list/transport_table.csv")  







