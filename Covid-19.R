
### TASK 1: Get a COVID-19 pandemic Wiki page using HTTP request ###

install.packages("httr")
install.packages("rvest")

library(httr)
library(rvest)

get_wiki_covid19_page <- function() {
  
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
  # 1) base URL `https://en.wikipedia.org/w/index.php  
  # 2) URL parameter: `title=Template:COVID-19_testing_by_country`, seperated by question mark ?
  
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
  # in our case, it will be `Template:COVID-19_testing_by_country`
  query_param <- list(title = "Template:COVID-19_testing_by_country")
  
  # - Use the `GET` function in httr library with a `url` argument and a `query` arugment to get a HTTP response
  response <- GET(wiki_base_url, query = query_param)
  # Use the `return` function to return the response
  return(response)
}

get_wiki_covid19_page()


### TASK 2: Extract COVID-19 testing data table from the wiki HTML page ###

library(rvest) # load 'rvest' library

# Get the root html node from the http response in task 1 
# Get the first table in the HTML root node using html_node function
root_node <- read_html(get_wiki_covid19_page())
root_node

# Get the table node from the root html node
table_node <- html_node(root_node, "table")

# Read the table node as a data frame using html_table function
# Read the table node and convert it into a data frame, and print the data frame for review
data_frame <- html_table(table_node)
data_frame

### TASK 3: Pre-process and export the extracted data frame ###

# Print the summary of the data frame
summary(data_frame)  
str(data_frame)
names(data_frame)
tail(data_frame)

preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}


# call `preprocess_covid_data_frame` function and assign it to a new data frame
new_data_frame <- preprocess_covid_data_frame(data_frame)
new_data_frame

# Print the summary of the processed data frame again
summary(new_data_frame)

# Export the data frame to a csv file
write.csv(new_data_frame, "covid.csv")

# Get working directory
wd <- getwd()
# Get exported 
file_path <- paste(wd, sep="", "/covid.csv")
# File path
print(file_path)
file.exists(file_path)


### TASK 4: Get a subset of the extracted data frame ###

# Read covid_data_frame_csv from the csv file
new_data_frame <- read.csv("covid.csv")
head(new_data_frame)

# Get the 5th to 10th rows, with two "country" "confirmed" columns
new_data_frame[5:10,c("country","confirmed")]


### TASK 5: Calculate worldwide COVID testing positive ratio ###

# Get the total confirmed cases worldwide
total_confirmed_cases <- sum(new_data_frame$confirmed)
total_confirmed_cases

# Get the total tested cases worldwide
total_tested_cases <- sum(new_data_frame$tested)
total_tested_cases

# Get the positive ratio (confirmed / tested)
positive_ratio <- (total_confirmed_cases/total_tested_cases)*100
positive_ratio
percentage <- paste0(positive_ratio, '%')
percentage


### TASK 6: Get a country list which reported their testing data

# Get the `country` column
new_data_frame[,"country"]
# Check its class (should be Factor)
class(new_data_frame$"country")
# Convert the country column into character so that you can easily sort them
country_char <- as.character(new_data_frame$"country")
class(country_char)

# Sort the countries AtoZ
order(country_char)
# Sort the countries ZtoA
order(country_char, decreasing = TRUE)
# Print the sorted ZtoA list
country_char[order(country_char, decreasing = TRUE)]

# EXTRA: Sorting the whole dataframe based on the country column
# Sort the countries AtoZ
new_data_frame[order(country_char),]
# Sort the countries ZtoA
new_data_frame[order(-country_char),]
# Print the sorted ZtoA list
new_data_frame[order(country_char, decreasing = TRUE),]

### TASK 7: Identify countries names with a specific pattern ###
library(stringr)
str_detect(country_char, "^United")
str_subset(country_char, "^United")
str_extract(country_char, "United.+")
grep(pattern = "United.+",x = country_char,value = T)


### TASK 8: Pick two countries you are interested, and then review their testing data ###

# Select a subset (should be only one row) of data frame based on a selected country name and columns (country, confirmed, confirmed-population-ratio)
germany <- subset(new_data_frame, country == "Germany", c(confirmed, confirmed.population.ratio)) 
germany

spain <- subset(new_data_frame, country == "Spain", c(confirmed, confirmed.population.ratio)) 
spain

### TASK 9: Compare which one of the selected countries has a larger ratio of confirmed cases to population ###
if (spain$confirmed.population.ratio > germany$confirmed.population.ratio) {
  print("Spain has higher COVID-19 infection risk")
} else {
  print("Germany has higher COVID-19 infection risk")
}

### TASK 10: Find countries with confirmed to population ratio rate less than a threshold ###
less_than_1 <- new_data_frame[new_data_frame$confirmed.population.ratio < 1, ]
less_than_1
