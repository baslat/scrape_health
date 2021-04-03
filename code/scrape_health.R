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

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Setup the driver and connection ----
os <- get_os()

health <- "https://www.health.gov.au/news/health-alerts/novel-coronavirus-2019-ncov-health-alert/coronavirus-covid-19-current-situation-and-case-numbers"

scrape_dt <- lubridate::now(tzone = "Australia/Sydney")

server_address = if_else(os == "windows",
                         "localhost", "selenium")

server_port = if_else(os == "windows",
                      4445L, 4444L)

remote_driver <- RSelenium::remoteDriver(remoteServerAddr = server_address, # need to set this to the name of the service in docker, ie not localhost
                                         port = server_port, # change this back to 4444L
                                         browserName = "chrome")
remote_driver$open()


Sys.sleep(35)
remote_driver$navigate(health)
Sys.sleep(30)
remote_driver$switchToFrame(NULL)
doc = xml2::read_html(remote_driver$getPageSource()[[1]])

# Extract the tables ----
health_date <- doc %>% 
  rvest::html_nodes("h2") %>% 
  magrittr::extract2(15) %>% 
  rvest::html_text() %>% 
  stringr::str_extract('[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}') %>% 
  lubridate::dmy()

new <- rvest::html_table(doc)[[1]]
soi <- rvest::html_table(doc)[[2]]
tests <- rvest::html_table(doc)[[3]]


# OCR the hospital data ----
# ybCdZWz
hosp_image_location <- remote_driver$findElement(using = "id",
                                        'ybCdZWz')

remote_driver$mouseMoveToLocation(webElement = hosp_image_location)
remote_driver$click()
remote_driver$screenshot(file = "hosp_chart_raw.png")




# Stop selenium ----
try(remote_driver$quit())
try(remote_driver$server$stop())
try(system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE))

# Tidy data ----
scraped_health_data <- list(new, soi, tests) %>% 
  purrr::map_dfr(tidy_health) %>% 
  dplyr::mutate(reported_date = health_date,
                scrape_dt = .env$scrape_dt) 

date <- as.character(scrape_dt) %>% 
  stringr::str_replace_all(pattern = ":|-",
                           replacement = "_")

readr::write_csv(scraped_health_data,
                 paste0("outputs/", date, "_scraped_health_data.csv"))
