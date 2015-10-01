
library(rvest)
library(stringi)
library(plyr)
library(dplyr)

# MDPI
url <- "http://www.mdpi.com/search?journal=remotesensing&year_from=1996&year_to=2015&page_count=10&sort=pubdate&view=abstract"

# Articles
articles <- url %>%
  html() %>%
  html_nodes(".articleItem")

length(articles)
xml_structure(articles[[1]])

# functions to get variables from html

getTitle <- function(articles){
  title <- articles %>%
    html_node(".title") %>%
    html_text() %>%
    stri_replace(regex = "^Article:\\s*(.*)", replacement = "$1")
  title
}

getAuthor <- function(articles){
  authors <- articles %>%
    html_node(".authors a") %>%
    html_text() %>%
    paste(collapse = ",")
  authors
}

getAbstract <- function(articles){
  abstract <- articles %>%
    html_node(".abstr") %>%
    html_text() %>%
    stri_replace(regex = "^\\s*Abstract:\\s(.*)\\s*$", replacement = "$1")
  abstract
}

getLinks <- function(articles){
  links <- articles %>%
    html_node(".title a") %>%
    html_attr("href")
  links
}

auxKeywords <- function(url){
  # url: string with url to article
  keywords <- url %>%
    html() %>%
    html_nodes(".art-keywords a") %>%
    html_text() %>%
    paste(collapse = ",")
  keywords
}

getKeywords <- function(articles){
  urls <- articles %>%
    getLinks() %>%
    paste("http://www.mdpi.com", ., sep = "")
  sapply(urls, auxKeywords)
}

getYear <- function(articles){
  urls <- articles %>%
    html_node(".idnt b") %>%
    html_text()
}

articlesMDPI <- function(page){
  # pag: página a scrappear
  articles <- page %>%
    html() %>%
    html_nodes(".articleItem")
  print(page)
  datos <- data.frame(
    year = getYear(articles),
    titles = getTitle(articles),
    keywords = getKeywords(articles),
    authors = getAuthor(articles),
    links = getLinks(articles),
    abstract = getAbstract(articles)
    )
  row.names(datos) <- NULL

  datos
}

# pages to scrape
pags <- paste("http://www.mdpi.com/search?journal=remotesensing&year_from=1996&year_to=2015&page_no=",
  1:10, "&page_count=200&sort=pubdate&view=abstract", sep = "")

# data frame
mdpi <- ldply(pags, articlesMDPI)
glimpse(mdpi)

save(mdpi, file = "datos/mdpi.Rdata")
#
