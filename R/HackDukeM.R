#' ---
#' title: "HackDukeM"
#' output: html_document
#' date: "2024-03-02"
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------
if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(rvest)) {install.packages("rvest"); library(rvest)}
if(!require(RSelenium)) {install.packages("RSelenium"); library(RSelenium)}
if(!require(wdman)) {install.packages("wdman"); library(wdman)}
if(!require(netstat)) {install.packages("netstat"); library(netstat)}
if(!require(xml2)) {install.packages("xml2"); library(xml2)}
if(!require(webdriver)) {install.packages("webdriver"); library(webdriver)}
if(!require(purrr)) {install.packages("purrr"); library(purrr)}
if (!require(here)) {install.packages("here"); library(here)}
if (!require(dotenv)) {install.packages("dotenv"); library(dotenv)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}

#' 
## ----data-------------------------------------------------------------------------------------------------
#data <- read.csv("./data/responses.csv")
sample_data_responses <- read.csv("./sample_data/responses.csv")
sample_data_mediaviews <- read.csv("./sample_data/media_views.csv")
sample_data_items <- read.csv("./sample_data/items.csv")
sample_data_pageviews <- read.csv("./sample_data/page_views.csv")

#' 
## ---------------------------------------------------------------------------------------------------------
#| label: tidy-page-views
#Avg engagement per section
#facet wrap on textbook, 

avg_page_engagement <- sample_data_pageviews |>
  filter(!is.na(book), !is.na(chapter_number), !is.na(section_number), !is.na(engaged)) %>%
  group_by(book, chapter_number, section_number) |>
    summarize(mean_value = mean(engaged, na.rm = TRUE))

avg_page_engagement

avg_score <- sample_data_responses %>%
  filter(!is.na(book), !is.na(chapter_number), !is.na(section_number), !is.na(points_earned)) %>%
  group_by(book, chapter_number, section_number) %>%
  summarise(avg_score = mean(points_earned, na.rm = TRUE))

merged_data <- inner_join(avg_page_engagement, avg_score, by = c("book", "chapter_number", "section_number"))

ggplot(merged_data, aes(x = mean_value, y = avg_score, color = book)) +
  geom_point() +
  facet_wrap(~ book, ncol = 2) +
  labs(x = "Engaged", y = "Average Score", title = "Density Graph of Average Score vs. Engaged, Facet Wrapped by Book") +
  theme(legend.position = "bottom",
        legend.justification = "right",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

#legend("bottomright", legend = unique(merged_data$book))


