source("utils.R")
library(DT)
library(tidyverse)
library(lubridate)

crsp <- fetch()

17270

crsp <- fetch_tool(combined_table, 17270) 
dtt <- crsp %>%
  mutate(
    url = paste0('<a href="', TC_URL, '">tc_url </a>'),
    PGM_START_TIME = as.numeric(gsub(":", "", PGM_START_TIME))
    ) %>%
  select(url, everything())

useful_info <- c("url", "ENROL_METHOD",  "BALLOT_DATE",
                 "PGM_CODE", "TC_PGM_NAME",
                 "TC_DISTRICT", "TC_VENUE", "TC_DAY", "PGM_START_TIME", "PGM_END_TIME",
                 "PGM_START_DATE", "PGM_END_DATE", "FEE", "QUOTA", 
                 "ENROL_START_DATE", "ENROL_END_DATE" )  

dtt %>% 
  group_by(TC_ACT_TYPE_NAME) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% datatable()

dtt %>% filter(TC_ACT_TYPE_NAME=="游泳") %>% 
  filter(
    as.numeric(MAX_AGE) > 25,
    as.numeric(MIN_AGE) < 30,
    grepl("一", TC_PGM_NAME)
         # & FEE != "0.00"
         ) %>% 
  select(useful_info, MIN_AGE, MAX_AGE) %>% view()

dtt %>% filter(TC_ACT_TYPE_NAME=="游泳") %>% 
  filter(
    as.numeric(MAX_AGE) > 25,
    as.numeric(MIN_AGE) < 30 
    # & FEE != "0.00"
  ) %>%
  group_by(TC_PGM_NAME) %>% summarise(n=n()) %>% view()

# squash ----
squash_weekend <- dtt %>% 
  filter(
    TC_ACT_TYPE_NAME == "壁球"
    & MAX_AGE > 25
    & TC_PGM_NAME == "壁球訓練班"
    ) %>%
  filter(grepl("Sat|Sun", EN_DAY)) %>%
  select(useful_info)

squash_weekend %>% datatable(escape = FALSE)

squash_weekday <- dtt %>% 
  filter(
    TC_ACT_TYPE_NAME == "壁球"
    & MAX_AGE > 25
    & TC_PGM_NAME == "壁球訓練班"
  ) %>%
  filter(!grepl("Sat|Sun", EN_DAY)) %>%
  filter(PGM_START_TIME > 1800) %>%
  select(useful_info)

squash_weekday %>% datatable(escape = FALSE)


# squash_weekday
c("40563315", "40563337", "40568789", "40568805")
# squash_weekend
c("40570084")  # ballot

# badminton ----
badminton_weekday <- dtt %>% 
  filter(
    TC_ACT_TYPE_NAME == "羽毛球"
    & MAX_AGE > 25 
    & FEE != "0.00"
    & PGM_START_TIME > 1800
  ) %>%
  filter(!grepl("Sat|Sun", EN_DAY)) %>%
  select(useful_info)

badminton_weekday %>% 
  datatable(escape = FALSE, options = list(pageLength = 100))

c("40564270")
c("40568794", "40568807", "40568845", "40570157")


badminton_weekend <- dtt %>% 
  filter(
    TC_ACT_TYPE_NAME == "羽毛球"
    & MAX_AGE > 25 
    & FEE != "0.00"
  ) %>%
  filter(grepl("Sat|Sun", EN_DAY)) %>%
  select(useful_info)

badminton_weekend %>% 
  datatable(escape = FALSE, options = list(pageLength = 100))

c("40569438", "40565483")

# climbing ----
"40567710"

# handle stress ----
"40568972"

# long distance run ----
"40568399"

# swimming ----
swim_weekday <- dtt %>% 
  filter(
    TC_ACT_TYPE_NAME == "游泳"
    & as.numeric(MAX_AGE) > 25
    & as.numeric(MIN_AGE) < 30
    # & grepl("一", TC_PGM_NAME)
    # & PGM_START_TIME > 1800
  ) %>%
  # filter(!grepl("Sat|Sun", EN_DAY)) %>%
  select(useful_info)

swim_weekday %>% 
  datatable(escape = FALSE, options = list(pageLength = 100))

"40570569" "40570581"

dtt %>% filter(TC_ACT_TYPE_NAME=="游泳") %>% 
  filter(
    as.numeric(MAX_AGE) > 25,
    as.numeric(MIN_AGE) < 30,
    # grepl("一", TC_PGM_NAME)
    # & FEE != "0.00"
  ) %>% 
  select(useful_info, MIN_AGE, MAX_AGE) %>% view()



# sum-up
e <- c("40568399","40568972","40567710","40569438", "40565483")

a <- c("40564270")
b <- c("40568794", "40568807", "40568845", "40570157")
c <- c("40563315", "40563337", "40568789", "40568805")
# squash_weekend
d <- c("40570084")  # ballot
f <- c("40570569","40570581")

codes <- c(a,b,c,d,e)

dtt %>% filter(PGM_CODE %in% codes) %>% select(useful_info) %>% 
  datatable(escape = FALSE, options = list(pageLength = 100))

