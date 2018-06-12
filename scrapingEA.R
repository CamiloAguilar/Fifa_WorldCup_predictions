
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





#Loading both the required libraries
library(rvest)
library(V8)

#URL with js-rendered content to be scraped
link <- 'https://food.list.co.uk/place/22191-brewhemia-edinburgh/'

#Read the html page content and extract all javascript codes that are inside a list

emailjs <- read_html(link) %>% html_nodes('li') %>% html_nodes('script') %>% html_text()

# Create a new v8 context
ct <- v8()

#parse the html content from the js output and print it as text
read_html(ct$eval(gsub('document.write','',emailjs))) %>% html_text()





