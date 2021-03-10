# library(cah)
# library(pins)
# cah::setup_rsc()
# cah::setup_pins()

# Add other library calls here
library(tidyverse)
library(rvest)
library(RSelenium)

# advice:
# https://callumgwtaylor.github.io/post/using-rselenium-and-docker-to-webscrape-in-r-using-the-who-snake-database/
# https://github.community/t/how-to-connect-to-a-docker-container/17171/2

# https://github.com/r-lib/remotes/issues/536


#' Tidy data scraped from health.gov.au
#'
#' @param .data 
#'
#' @return
#' @export
#'
#' @examples
tidy_health <- function(.data) {
  .data %>%
    tibble::as_tibble() %>% 
    mutate(across(everything(),
                  as.character)) %>% 
    pivot_longer(2:last_col()) %>% 
    mutate(value = readr::parse_number(.data$value),
           name = stringr::str_remove_all(name,
                                          "\\^|\\*")) %>% 
    pivot_wider(names_from = Jurisdiction,
                values_from = value)
}


health <- "https://www.health.gov.au/news/health-alerts/novel-coronavirus-2019-ncov-health-alert/coronavirus-covid-19-current-situation-and-case-numbers"

scrape_dt <- Sys.time()


# orig chromever
# args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value'
remote_driver <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4444L,
                                 browserName = "chrome")
remote_driver$open()

# 
# driver <- RSelenium::rsDriver(browser = "chrome",
#                               chromever =
#                                 system2(command = "wmic",
#                                         args = 'datafile where name="C:\\\\Program Files\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
#                                         stdout = TRUE,
#                                         stderr = TRUE) %>%
#                                 stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
#                                 magrittr::extract(!is.na(.)) %>%
#                                 stringr::str_replace_all(pattern = "\\.",
#                                                          replacement = "\\\\.") %>%
#                                 paste0("^",  .) %>%
#                                 stringr::str_subset(string =
#                                                       binman::list_versions(appname = "chromedriver") %>%
#                                                       dplyr::last()) %>%
#                                 as.numeric_version() %>%
#                                 max() %>%
#                                 as.character())

Sys.sleep(25)
# remote_driver <- driver[["client"]]
# Sys.sleep(5)
remote_driver$navigate(health)
Sys.sleep(15)
remote_driver$switchToFrame(NULL)
doc = xml2::read_html(remote_driver$getPageSource()[[1]])

health_date <- doc %>% 
  rvest::html_nodes("h2") %>% 
  magrittr::extract2(15) %>% 
  rvest::html_text() %>% 
  stringr::str_extract('[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}') %>% 
  lubridate::dmy()

new <- rvest::html_table(doc)[[1]]
soi <- rvest::html_table(doc)[[2]]
tests <- rvest::html_table(doc)[[3]]

# Stop selenium
try(remote_driver$quit())
try(driver$server$stop())
try(system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE))

# Tidy data
scraped_health_data <- list(new, soi, tests) %>% 
  purrr::map_dfr(tidy_health) %>% 
  dplyr::mutate(reported_date = health_date,
                scrape_dt = .env$scrape_dt) 

date <- as.character(scrape_dt) %>% 
  stringr::str_replace_all(pattern = ":|-",
                           replacement = "_")

readr::write_csv(scraped_health_data,
                 paste0("outputs/", date, "_scraped_health_data.csv"))
