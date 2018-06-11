
library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)
options(digits = 4)


## 1. lectura página ea sports ####
system("./phantomjs scrape_EAsports.js")

batches <- read_html("techstars.html") %>%
     html_nodes(".batch")

class(batches)

batch_titles <- batches %>%
     html_nodes(".batch_class") %>%
     html_text()

str_extract(batch_titles, "(Fall|Spring|Winter|Summer)")


#*************************
### EA
#*************************
ut-item-list-view_details


details <- read_html("EAsports_statistics.html") %>%
     html_nodes(".ut-item-list-view_details")

class(details)
html_text(details)

batch_titles <- details %>%
     html_nodes("h2") %>%
     html_text()


