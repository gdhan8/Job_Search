#Booz Allen Job Search

#load required libraries
library(rvest)
library(dplyr)
library(DescTools)
library(stringr)
library(tidyr)
library(devtools)
library(dplyr)
library(rgdal)
library(leaflet)
library(sf)
library(htmlwidgets)
library(leaflegend)
library(leafem)


#Establish working directory and 
setwd("~/Desktop/R Projects/Booz Allen Job Search")

#Initialize variables and vectors
baseurl <- "https://careers.boozallen.com/jobs/search/?jobOffset="
i_job_data <- data.frame()
all_job_data <- data.frame()
addendum = ""
i=0

#logic to cycle through pages /initialize variables
if(i==0){ addendum = ""} else {addendum=as.character(i)}

url = paste0(baseurl,addendum)
page <- read_html(url)

totali = page %>% html_nodes(".pagination__legend") %>% html_text()
totali = as.numeric(substr(totali, nchar(totali)-49, nchar(totali)-46))
totali = 2000

#extract data required / Build a dataframe
for( i in seq(from = i, to = totali-21, by =20)){
  i_job_data <- data.frame()
  if(i==0){ addendum = ""} else {addendum=as.character(i)}
  
  url = paste0(baseurl,addendum)
  page <- read_html(url)
  
  job_title = page %>% html_nodes(".cell-title") %>% html_text()
  location = page %>% html_nodes(".cell-title+ td") %>% html_text()
  job_link = page %>% html_nodes(".link") %>% html_attr("href")
  Date = Sys.Date()
  
  i_job_data = data.frame(Date,job_title,location, job_link, stringsAsFactors = FALSE)
  
  all_job_data = rbind.data.frame(all_job_data,i_job_data)
  
}

#Clean data
location_split <-data.frame(str_split_fixed(all_job_data$location, ",", 3))
colnames(location_split) <- c("City", "State", "Country")
location_split$City = StrTrim(location_split$City)
location_split$State = StrTrim(location_split$State)
location_split$Country = StrTrim(location_split$Country)

#Split  data 
job_title_split <- data.frame(str_split_fixed(all_job_data$job_title, ",", 2))
colnames(job_title_split) <- c("Title", "Experience")
df2<- cbind.data.frame(job_title_split, location_split) 

#rebuild data frame / save file
all_job_data_final <- cbind.data.frame(all_job_data$Date, job_title_split, location_split, all_job_data$job_link) 
colnames(all_job_data_final) <- c("Date", "Title", "Experience","City", "State", "Country","URL" )
write.csv(all_job_data_final, file=paste0("BoozAllenJobs.csv", Sys.Date()))

#Reload file
all_job_data_final <- read.csv(paste0("BoozAllenJobs.csv", Sys.Date()))

#Filter jobs to just include terms data and intelligence
datajobs <- all_job_data_final %>% filter(grepl('Data', Title ))
datajobs <- datajobs %>% filter(Experience!=" Senior")
datajobs <- datajobs %>% filter(grepl('Data', Title ))
intjobs <- all_job_data_final %>% filter(grepl('Tech Ex', Title )) 
#intjobs <- all_job_data_final %>% filter(grepl('Intelligence', Title )) 
#intjobs <- intjobs %>% filter(Experience!=" Senior")
#intjobs<- data.frame()

all_job_data_final <- rbind(datajobs, intjobs)

#load external data on US Cities
uscities <- read.csv("~/Desktop/R Projects/uscities.csv")
uscities$City <- uscities$city
uscities$State <- uscities$state_name

# join external data and sourced data
job_location_data <- left_join(all_job_data_final, uscities, by = c('City', 'State'))

#clean N/A data
job_location_data <- job_location_data %>% filter(is.na(job_location_data$city)==FALSE)

#
labels <- sprintf(
  "<strong>Job Title: %s </strong><br/> Experience: %s <br/> Location: %s, %s",
  as.character(job_location_data$Title),
  as.character(job_location_data$Experience),
  as.character(job_location_data$City),
  as.character(job_location_data$state_id),
  paste0('<a href=',as.character(job_location_data$URL),'>Job Listing </a>')
) %>% lapply(htmltools::HTML)

#initialize/customize popups to link to job listing 
popup <- sprintf(
paste0('<a href=',as.character(job_location_data$URL),'>Job Listing </a>')
) %>% lapply(htmltools::HTML)

#create the tool/visualization
booz_jobs_map <-  leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(#####
                   providers$Esri, 
                   options = providerTileOptions(
                     updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                     updateWhenIdle = TRUE ) # map won't load new tiles when panning)) 
  )%>%
  
  addTiles(#####
  )%>%
  
  addMarkers(#####
    lng = job_location_data$lng,
    lat = job_location_data$lat,
    layerId = NULL,
    group = NULL,
    icon = NULL,
    popup = popup,
    popupOptions = NULL,
    label = labels,
    labelOptions = NULL,
    options = markerOptions(),
    clusterOptions = markerClusterOptions(),
    clusterId = NULL,
  ) %>%

  addMiniMap(#####
           position = "bottomleft",
           width = 200,
           height = 200,
           collapsedWidth = 19,
           collapsedHeight = 19,
           zoomLevelOffset = -5,
           zoomLevelFixed = FALSE,
           centerFixed = FALSE,
           zoomAnimation = FALSE,
           toggleDisplay = FALSE,
           autoToggleDisplay = FALSE,
           minimized = FALSE,
           aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE),
           shadowRectOptions = list(color = "#000000", weight = 1, clickable = FALSE, opacity =
                                      0, fillOpacity = 0),
           strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
           tiles = NULL,
           mapOptions = list()
)
  #addLogo(#####
  #        "https://boozallen.com/content/dam/boozallen_site/homepage/booz-allen-logo.svg" , 
  #        url = "https://boozallen.com/content/dam/boozallen_site/homepage/",  
  #        width = 275,
  #        height = 32)

#####
#Publish solution
booz_jobs_map





