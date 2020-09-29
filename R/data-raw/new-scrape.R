

library(dplyr)
library(stringr)
library(rvest)
library(xml2)
library(tidyr)
library(readr)
library(httr)


url <- 'https://www.procyclingstats.com/rider/alberto-contador'

usr_agent <- 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/83.0.4103.61 Chrome/83.0.4103.61 Safari/537.36'

# get curl handle
hndl <- handle(url)
session <- GET(url, handle = hndl, user_agent(usr_agent))

# get *numerical* PCS id
rider_id2 <- content(session) %>%
  html_nodes(xpath = "//div[@class='content ']/div[@class='page-content']/div/script") %>%
  .[[1]] %>%
  html_text() %>%
  str_match("var id = ([0-9]+)") %>%
  .[[2]]

# we have the main page now
results_tbl <- content(session) %>%
  html_nodes(xpath = "//table[@class='rdrResults']")

# vector of seasons
seasons <- content(session) %>%
  html_nodes(xpath = "//ul[@class='rdrSeasonNav']/li/@data-season") %>%
  html_text()

season_cur <- content(session) %>%
  html_nodes(xpath = "//ul[@class='rdrSeasonNav']/li[@class='cur']/@data-season") %>%
  html_text()

# here are *latest* results
results <- results_tbl %>%
  .[[1]] %>%
  html_table()


out_results <- NULL

res <- POST(url = 'https://www.procyclingstats.com/rdr/start_results2.php',
                  body = list('id' = rider_id2, 'season' = 2014),
                  handle = hndl,
                  user_agent(usr_agent))

# set of orphaned TR tags (without TBODY parent)
res_rows <- content(res)[[1]]
# wrap to TBODY tag
raw_tbody <- paste0('<tbody>', res_rows, '</tbody>')
# get xml_node of TBODY
tbody <- read_html(raw_tbody) %>%
  html_nodes(xpath = "//body") %>%
  xml_child("tbody")
# find TBODY in original result table
target_tbody <- xml_child(results_tbl[[1]], "tbody")
# replace with new TBODY
xml_replace(target_tbody, tbody[[1]])

results2 <- results_tbl %>%
  .[[1]] %>%
  html_table()


handle_reset(url)
