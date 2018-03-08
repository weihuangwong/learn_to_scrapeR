library(rvest)
library(lubridate)
# library(dplyr)
library(purrr)

## ----------------------------------------------------------------------------
## Page scraper
## ----------------------------------------------------------------------------

page_scraper <- function(url) {
  content <- read_html(url)
  body <- content %>%
    html_node("div.field.body") %>%
    html_text() %>%
    gsub("\\n", " ", x = .) %>%
    trimws()
  title <- content %>%
    html_node("h1.node-title.clearfix") %>%
    html_text()
  author <- content %>%
    html_node("span.field.field-source") %>%
    html_text() %>%
    gsub("\\n", " ", x = .) %>%
    trimws()
  dt <- content %>%
    html_node("span.date-display-single") %>%
    html_text() %>%
    dmy()
  data_frame(author = author, date = dt, title = title, body = body)
}

## ----------------------------------------------------------------------------
## Page to link
## ----------------------------------------------------------------------------

page_to_link <- function(page_num) {
  url <- sprintf("https://reliefweb.int/country/afg?format=8&page=%d", page_num)
  content <- read_html(url)
  content <- content %>%
    html_node("div.river-list.river-updates.river-sort-latest") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("/report", x = ., value = TRUE) 
  ifelse(grepl("^https", content),
         content,
         paste0("https://reliefweb.int", content))
}

## ----------------------------------------------------------------------------
## Get links and content
## ----------------------------------------------------------------------------

# Test our functions
page_scraper("https://reliefweb.int/report/iraq/reconstruction-needed-displaced-iraqis-continue-return-iom-iraq")
page_to_link(1)

# Simulate a long-running job
all_links <- unlist(map(1:5, page_to_link))
all_content <- map(all_links[10:20], function(link) {
  tryCatch({
    Sys.sleep(10)
    cat(sprintf("Scraping %s...\n", link))
    page_scraper(link)
  },
  error = function(e) {
    print(e)
    print(link)
  })
})

# Make it a dataframe
all_content <- bind_rows(all_content)

write.csv(all_content, file = "all_content.csv", row.names = FALSE)
