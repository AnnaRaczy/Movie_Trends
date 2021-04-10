library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(leaflet)
library(utils)
library(plotly)
library(dplyr)
library(base)
library(data.table)
library(expss)
library(readxl)
library(rvest)
library(janitor)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(shinyjs)
library(readr)
library(DT)
hrbrthemes::import_roboto_condensed()


setwd('C:/Users/Hp/Desktop/R/Movie_Trends')     

App_directors <- read.csv("App_directors.csv")
Lat_Long <- read_excel('Lat_Long.xlsx')


#### Web Scrapping - Gross Revenue Data (Manual bit) ####



#### Gross Revenue 2010 ####
url10 <- 'https://www.imdb.com/search/title/?title_type=feature&release_date=2010-01-01,2010-12-31&num_votes=100000,&sort=num_votes,desc'
webpage10 <- read_html(url10)

#Using CSS selectors to scrape the gross revenue section
gross_data_html10 <- html_nodes(webpage10,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data10 <- html_text(gross_data_html10)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data10 <-gsub("M","",gross_data10)

gross_data10 <-substring(gross_data10 ,2,6)

#Data-Preprocessing: converting gross to numerical
gross_data10 <-as.numeric(gross_data10)



#### Gross Revenue 2011 ####
url11 <- 'https://www.imdb.com/search/title/?title_type=feature&release_date=2011-01-01,2011-12-31&num_votes=100000,&sort=num_votes,desc'
webpage11 <- read_html(url11)

#Using CSS selectors to scrape the gross revenue section
gross_data_html11 <- html_nodes(webpage11,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data11 <- html_text(gross_data_html11)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data11 <-gsub("M","",gross_data11)

gross_data11 <-substring(gross_data11 ,2,6)

#Data-Preprocessing: converting gross to numerical
gross_data11 <-as.numeric(gross_data11)



#### Gross Revenue 2012 ####
url12 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2012-01-01,2012-12-31&sort=num_votes,desc'
webpage12 <- read_html(url12)

#Using CSS selectors to scrape the gross revenue section
gross_data_html12 <- html_nodes(webpage12,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data12 <- html_text(gross_data_html12)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data12 <-gsub("M","",gross_data12)

gross_data12 <-substring(gross_data12 ,2,6)

#Data-Preprocessing: converting gross to numerical
gross_data12 <-as.numeric(gross_data12)



#### Gross Revenue 2013 ####
url13 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2013-01-01,2013-12-31&sort=num_votes,desc'
webpage13 <- read_html(url13)

#Using CSS selectors to scrape the gross revenue section
gross_data_html13 <- html_nodes(webpage13,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data13 <- html_text(gross_data_html13)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data13 <-gsub("M","",gross_data13)

gross_data13 <-substring(gross_data13 ,2,6)

#Data-Preprocessing: converting gross to numerical
gross_data13 <-as.numeric(gross_data13)



#### Gross Revenue 2014 ####
url14 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2014-01-01,2014-12-31&sort=num_votes,desc'
webpage14 <- read_html(url14)

#Using CSS selectors to scrape the gross revenue section
gross_data_html14 <- html_nodes(webpage14,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data14 <- html_text(gross_data_html14)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data14 <-gsub("M","",gross_data14)

gross_data14 <-substring(gross_data14 ,2,6)

#Data-Preprocessing: converting gross to numerical
gross_data14 <-as.numeric(gross_data14)



#### Gross Revenue 2015 ####
url15 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2015-01-01,2015-12-31&sort=num_votes,desc'
webpage15 <- read_html(url15)

#Using CSS selectors to scrape the gross revenue section
gross_data_html15 <- html_nodes(webpage15,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data15 <- html_text(gross_data_html15)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data15 <-gsub("M","",gross_data15)

gross_data15 <-substring(gross_data15 ,2,6)

#Data-Preprocessing: converting gross to numerical
gross_data15 <-as.numeric(gross_data15)



#### Gross Revenue 2016 ####
url16 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2016-01-01,2016-12-31&sort=num_votes,desc'
webpage16 <- read_html(url16)

#Using CSS selectors to scrape the gross revenue section
gross_data_html16 <- html_nodes(webpage16,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data16 <- html_text(gross_data_html16)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data16 <-gsub("M","",gross_data16)

gross_data16 <-substring(gross_data16 ,2,6)


#### Data missing for Gross Revenue ####
#Filling missing entries with NA
for (m in c(43)){
  
  a16.1<-gross_data16[1:(m-1)]
  
  b16.1<-gross_data16[m:length(gross_data16)]
  
  gross_data16<-append(a16.1,list("NA"))
  
  gross_data16<-append(gross_data16,b16.1)
  
}

#Data-Preprocessing: converting gross to numerical
gross_data16<-as.numeric(gross_data16)



#### Gross Revenue 2017 ####
url17 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2017-01-01,2017-12-31&sort=num_votes,desc'
webpage17 <- read_html(url17)

#Using CSS selectors to scrape the gross revenue section
gross_data_html17 <- html_nodes(webpage17,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data17 <- html_text(gross_data_html17)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data17 <-gsub("M","",gross_data17)

gross_data17 <-substring(gross_data17 ,2,6)


#### Data missing for Gross Revenue ####
#Filling missing entries with NA
for (k in c(39)){
  
  a17<-gross_data17[1:(k-1)]
  
  b17<-gross_data17[k:length(gross_data17)]
  
  gross_data17<-append(a17,list("NA"))
  
  gross_data17<-append(gross_data17,b17)
  
}

#Data-Preprocessing: converting gross to numerical
gross_data17 <-as.numeric(gross_data17)



#### Gross Revenue 2018 ####
url18 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2018-01-01,2018-12-31&sort=num_votes,desc'
webpage18 <- read_html(url18)

#Using CSS selectors to scrape the gross revenue section
gross_data_html18 <- html_nodes(webpage18,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data18 <- html_text(gross_data_html18)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data18 <-gsub("M","",gross_data18)

gross_data18 <-substring(gross_data18 ,2,6)


#### Data missing for Gross Revenue ####
#Filling missing entries with NA
for (j in c(16, 33, 42)){
  
  a18<-gross_data18[1:(j-1)]
  
  b18<-gross_data18[j:length(gross_data18)]
  
  gross_data18<-append(a18,list("NA"))
  
  gross_data18<-append(gross_data18,b18)
  
}

#Data-Preprocessing: converting gross to numerical
gross_data18 <-as.numeric(gross_data18)



#### Gross Revenue 2019 ####
url19 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2019-01-01,2019-12-31&sort=num_votes,desc'
webpage19 <- read_html(url19)

#Using CSS selectors to scrape the gross revenue section
gross_data_html19 <- html_nodes(webpage19,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data19 <- html_text(gross_data_html19)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data19 <-gsub("M","",gross_data19)

gross_data19 <-substring(gross_data19 ,2,6)


#### Data missing for Gross Revenue ####
#Filling missing entries with NA
for (o in c(16, 20, 28, 30, 35, 37, 45, 46, 47, 49)){
  
  a19<-gross_data19[1:(o-1)]
  
  b19<-gross_data19[o:length(gross_data19)]
  
  gross_data19<-append(a19,list("NA"))
  
  gross_data19<-append(gross_data19,b19)
  
}

#Data-Preprocessing: converting gross to numerical
gross_data19 <-as.numeric(gross_data19)



#### Gross Revenue 2020 ####
url20 <- 'https://www.imdb.com/search/title/?title_type=feature&year=2020-01-01,2020-12-31&sort=num_votes,desc'
webpage20 <- read_html(url20)

#Using CSS selectors to scrape the gross revenue section
gross_data_html20 <- html_nodes(webpage20,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data20 <- html_text(gross_data_html20)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data20<-gsub("M","",gross_data20)

gross_data20<-substring(gross_data20,2,6)


#Filling missing entries with NA
for (p in c(2, 3, 6, 8, 9, 10, 11, 12, 13, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
            34, 35, 37, 38, 39, 40, 41, 42, 43, 44, 46, 47, 48, 49, 50)){
  
  a20<-gross_data20[1:(p-1)]
  
  b20<-gross_data20[p:length(gross_data20)]
  
  gross_data20<-append(a20,list("NA"))
  
  gross_data20<-append(gross_data20,b20)
  
  
}

#Data-Preprocessing: converting gross to numerical
gross_data20<-as.numeric(unlist(gross_data20))
gross_data20 <- head(gross_data20 , -1) #51 elements on a list, should be 50



gross_data_combined <- c(gross_data10, gross_data11, gross_data12, gross_data13, gross_data14, gross_data15,
                         gross_data16, gross_data17, gross_data18, gross_data19, gross_data20)








#### Web Scrapping - The Rest of the data ####


### Specifying the url for desired website to be scraped ###
year <- seq(from = 2010, to = 2020, 1)
base_url <- paste0('https://www.imdb.com/search/title/?title_type=feature&release_date=', 
                   year,'-01-01,', year, '-12-31&sort=num_votes,desc')


### Reading the HTML code from the website ###
year <- seq(from = 2010, to = 2020, 1)
webpage <- lapply(base_url, read_html)



#### Rank ####
#Using CSS selectors to scrape the rankings section
rank_data_html <- lapply(webpage, html_nodes, '.text-primary')

#Converting the ranking data to text
rank_data <- lapply(rank_data_html, html_text)

#Data-Preprocessing: Converting rankings to numerical
rank_data <-  as.numeric(unlist(rank_data)) 




#### Title ####
#Using CSS selectors to scrape the title section
title_data_html <- lapply(webpage, html_nodes, '.lister-item-header a')

#Converting the title data to text
title_data <- lapply(title_data_html, html_text)




#### Description ####
#Using CSS selectors to scrape the description section
description_data_html <- lapply(webpage, html_nodes, '.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- lapply(description_data_html, html_text)

#Data-Preprocessing: removing '\n'
description_data[] <- lapply(description_data,function(x) gsub('\n','',as.character(x)))




#### Runtime ####
#Using CSS selectors to scrape the Movie runtime section
runtime_data_html <- lapply(webpage, html_nodes, '.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- lapply(runtime_data_html, html_text)

#Data-Preprocessing: removing mins and converting it to numerical
runtime_data <- lapply(runtime_data, function(x) gsub(' min','', as.character(x)))
runtime_data <- as.numeric(unlist(runtime_data))




#### Genre ####
#Using CSS selectors to scrape the Movie genre section
genre_data_html <- lapply(webpage, html_nodes, '.text-muted .genre')

#Converting the genre data to text
genre_data <- lapply(genre_data_html, html_text)

#Data-Preprocessing: removing \n
genre_data <- lapply(genre_data, function(x) gsub('\n', '', as.character(x)))

#Data-Preprocessing: removing excess spaces
genre_data <- lapply(genre_data, function(x) gsub(' ','', as.character(x)))

#Taking only the first genre of each movie
genre_data <- lapply(genre_data, function(x) gsub(',.*', '', as.character(x)))

#Converting each genre from text to factor
genre_data <-  as.factor(unlist(genre_data))




#### IMDB Rating ####
#Using CSS selectors to scrape the IMDB rating section
rating_data_html <- lapply(webpage, html_nodes, '.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- lapply(rating_data_html, html_text)

#Data-Preprocessing: converting ratings to numerical
rating_data <- as.numeric(unlist(rating_data))




#### Votes ####
#Using CSS selectors to scrape the votes section
votes_data_html <- lapply(webpage, html_nodes, '.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- lapply(votes_data_html, html_text)

#Data-Preprocessing: removing commas
votes_data <- lapply(votes_data, function(x) gsub(',', '', as.character(x)))

#Data-Preprocessing: converting votes to numerical
votes_data <- as.numeric(unlist(votes_data))




#### Directors ####
#Using CSS selectors to scrape the directors section
directors_data_html <- lapply(webpage, html_nodes, '.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- lapply(directors_data_html, html_text)

#Data-Preprocessing: converting directors data into factors
directors_data <-as.factor(unlist(directors_data))




#### Actors ####
#Using CSS selectors to scrape the directors section
actors_data_html <- lapply(webpage, html_nodes, '.lister-item-content .ghost+ a')

#Converting the directors data to text
actors_data <- lapply(actors_data_html, html_text)

#Data-Preprocessing: converting directors data into factors
actors_data <-as.factor(unlist(actors_data))



### Data Frame ###
#Combining all the lists to form a data frame

years_combined <- rep(year, each=50)

movies_df <-data.frame(Rank = unlist(rank_data), 
                       Title = unlist(title_data),
                       Description = unlist(description_data), 
                       Runtime = unlist(runtime_data),
                       Genre = unlist(genre_data), 
                       Rating = unlist(rating_data), 
                       Votes = unlist(votes_data),
                       Director = unlist(directors_data), 
                       Actor = unlist(actors_data)) %>% 
  mutate(Gross_Earning_in_Mil = gross_data_combined) %>% 
  mutate(Year= years_combined) 




### Mapping ###

movies_df <- as.data.frame(movies_df)

directors <- as.character(movies_df$Director)

directors_unique <- as.data.frame(unique(directors))


### Mapped Director ###
directors_country <- App_directors



#### Country mapping - Map tab ####


### for the map ###

movies_df2 <- movies_df %>% 
  left_join(directors_country) %>% 
  left_join(Lat_Long) %>% 
  dplyr::select(Director, Country, Latitude, Longitude)

mapped_country <- movies_df2 %>% 
  unique()

mapped_country2 <-  mapped_country %>% 
  rowwise %>%
  mutate(Density=count_if(Country, mapped_country$Country)) %>%
  group_by(Country, Latitude, Longitude) %>% 
  dplyr::select(-Director) %>% 
  unique()

mapped_country2 <- subset(mapped_country2, Density!= 23)

movies_df3 <- movies_df2 %>% 
  unique()




#### Functions ####

notation <- function(x) {
  format(x, 
         scientific =F, 
         trim= FALSE, 
         digits = NULL, 
         big.mark=",", 
         nsmall= 0L,
         justify = "r")
}


linebreaks <- function(n){HTML(strrep(br(), n))}






#### Interactive Bubble charts data - Plots Tab ####


### Genre Count per Year ###
genre_count <- movies_df %>% 
  dplyr::select(Genre, Year)
genre_count2 <- tabyl(genre_count, Genre, Year)
genre_count3 <- gather(genre_count2, Year, Count, '2010':'2020', factor_key=TRUE)

genre_count3<-genre_count3[!(genre_count3$Count==0),]

genre_count3 <- genre_count3 %>% 
  dplyr::select(Year, Genre, Count)

genre_count3 <-  as_tibble(genre_count3)

genre_count3$Year <- as.character(genre_count3$Year)
genre_count3$Year <- as.numeric(genre_count3$Year)


### Box Office ValueBox ###

box_office_valueBox <- movies_df %>% 
  dplyr::select(Gross_Earning_in_Mil, Director, Title) %>%
  filter(Gross_Earning_in_Mil > 600) %>% 
  dplyr::select(Title, Gross_Earning_in_Mil) %>% 
  arrange(-Gross_Earning_in_Mil)

box_office_valueBox$Gross_Earning_in_Mil <- round(box_office_valueBox$Gross_Earning_in_Mil, 0)

box_office_valueBox$Gross_Earning_in_Mil <- paste0("$",box_office_valueBox$Gross_Earning_in_Mil, ' M')


top_box_office <- box_office_valueBox[1,1]
top_box_office_money <- box_office_valueBox[1,2]

### Box Office Movies more than 600M ###

box_office_movies <- movies_df %>% 
  dplyr::select(Gross_Earning_in_Mil, Director) %>%
  filter(Gross_Earning_in_Mil > 600) %>% 
  group_by(Director) %>% 
  summarise(Gross_Earning_in_Mil = sum(Gross_Earning_in_Mil)) %>% 
  arrange(-Gross_Earning_in_Mil) 

box_office_movies$Director <- as.character(box_office_movies$Director)
box_office_movies$Director[which(box_office_movies$Director == 'Anthony Russo')] <- 'Anthony Russo (2 Movies)'



#### Plots ####


### Earnings per Genre ###
genre_earnings <- movies_df %>% 
  dplyr::select(Genre, Gross_Earning_in_Mil, Year)

genre_earnings2 <- genre_earnings %>% 
  group_by(Year, Genre) %>% 
  summarise(Gross_Earning_in_Mil = sum(Gross_Earning_in_Mil)) %>% 
  ungroup()  


### Runtime per Genre ###

genre_runtime <- movies_df %>% 
  dplyr::select(Year, Genre, Runtime) %>% 
  group_by(Year, Genre) %>% 
  summarise(Runtime = mean(Runtime)) %>% 
  ungroup()

genre_runtime$Year <- as.factor(genre_runtime$Year)

genre_runtime$Runtime <- round(genre_runtime$Runtime, 0)




### Most rated movies ###

genre_votes <- movies_df %>% 
  dplyr::select(Year, Genre, Votes) %>% 
  group_by(Year, Genre) %>% 
  summarise(Votes = mean(Votes)) %>% 
  arrange(desc(Votes)) %>% 
  ungroup()


#### Table tab ####
movies_df_app <- movies_df %>% 
  dplyr::select(Description, Rank, Title, Genre, Year, Director, Actor, Runtime, Rating, Gross_Earning_in_Mil, Votes) %>% 
  dplyr::rename('Gross Earning in Mil' = Gross_Earning_in_Mil)


movies_df_app$Votes <- notation(movies_df_app$Votes)





