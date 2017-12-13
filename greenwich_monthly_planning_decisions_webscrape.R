#automation webscrape of the greenwich new buildings site
#Run in 64 bit to do arcgis binding with armap pto

#clear all variables
rm(list=ls())

library(purrr, lib = "C:/Program Files/R/R-3.4.1/library")
library(httr, lib = "C:/Program Files/R/R-3.4.1/library")
library(rvest,lib = "C:/Program Files/R/R-3.4.1/library")
library(XML, lib = "C:/Program Files/R/R-3.4.1/library")
library(magrittr,lib = "C:/Program Files/R/R-3.4.1/library")
library(multiplex,lib = "C:/Program Files/R/R-3.4.1/library")
library(rgdal,lib = "C:/Program Files/R/R-3.4.1/library")
library(data.table,lib = "C:/Program Files/R/R-3.4.1/library")
library(dplyr,lib = "C:/Program Files/R/R-3.4.1/library")
library(tidyr,lib = "C:/Program Files/R/R-3.4.1/library")
library(RCurl,lib = "C:/Program Files/R/R-3.4.1/library")
library(curl,lib = "C:/Program Files/R/R-3.4.1/library")
#library(RPostgreSQL, lib = "C:/Program Files/R/R-3.4.1/library")
#library(rpostgis, lib = "C:/Program Files/R/R-3.4.1/library")
#library(arcgisbinding, lib = "C:/Program Files/R/R-3.4.1/library")
#arc.check_product()

#setwd
setwd("D:/FME Scheduled_tasks/R_tasks/")

#rvest to locates form
url<-"https://planning.royalgreenwich.gov.uk/online-applications/search.do?action=monthlyList"   ## page to spider
session <-html_session(url) 
## create session
greenwhichForm <-html_form(session)[[1]]

#which month to query
month <- "Nov 17"
parish <- "GRN"

greenwhichForm <- set_values(greenwhichForm, searchCriteria.parish = parish, month = month, dateType = "DC_Decided")
session1 <- submit_form(session,greenwhichForm)

#Row1 of table
link <- session1 %>% html_nodes(xpath = paste0('//*[@id="searchresults"]/li[',1,']/a')) %>% html_text()

table <- session1 %>% 
  follow_link(link) %>%
  read_html() %>% 
  html_table() %>%
  as.data.frame()

wideTable <- table %>%
  spread(X1,X2)

linkCount <- length(session1 %>% html_nodes(xpath = '//*[@id="searchresults"]/li/a') %>% html_text())

#page 
pageScrape <- function(hyperlink, session) {

link <- session %>% html_nodes(xpath = paste0('//*[@id="searchresults"]/li[',hyperlink,']/a')) %>% html_attr('href')

table1 <- session %>% 
            jump_to(link) %>%
            read_html() %>% 
            ####remove non unicode
            html_table() %>%
            as.data.frame()

table2 <- session %>% 
            jump_to(link) %>%
            follow_link("Further Information") %>%
            read_html() %>% 
            ####remove non unicode
            html_table() %>%
            as.data.frame()

table3 <- bind_rows(table1, table2) %>% 
          distinct()

wideTableUpdate <- table3 %>%
                    spread(X1,X2)

wideTable <<- bind_rows(wideTable, wideTableUpdate)

}


#function navigates to the next page using the button, x = 3 would take you to page 4
pager <- function() {
  
    if (session1 %>% 
        html_nodes(xpath = paste0('///*[@id="searchResultsContainer"]/p[1]')) %>% 
        html_text() %>%
        stringr::str_detect("Next")){
      
      nextCommand <- c(" %>% follow_link(\"Next\")")

      session1 <<- eval(parse(text = paste0("session1", nextCommand)))
  
      print(session1)
  
    } else {
      print("no more next buttons, end of scrape")
    }
}


Pages <- session1 %>% 
          html_nodes(xpath = paste0('//*[@id="searchResultsContainer"]/p[1]/span[1]/text()')) %>% 
          html_text() %>%
          stringr::str_extract_all("\\d+") %>%
          as.numeric()/10


pageLimit <- ceiling(Pages)


for(i in 0:pageLimit) {
  linkCount <- session1 %>% 
    html_nodes(xpath = paste0('//*[@id="searchresults"]/li/a')) %>% 
    html_text() %>%
    as.data.frame() %>%
    nrow() %>%
    seq(from = 1)
  
  lapply(linkCount, pageScrape, session = session1)
  
  pager()
}


uniqueRefWideTable <- distinct(wideTable, Reference, .keep_all = TRUE)  

write.csv(uniqueRefWideTable, "Q:/Projects/Data Science/Data Cakes/WebScrape Greenwich/november_decisions.csv")
#write.csv("N:/LDD/Web_Scrape/Planning_Decision_List/Greenwich/november_decisions.csv")

##update to test github no 12/13/2017