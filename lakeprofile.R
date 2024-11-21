
setwd("P:/R/lakeprofilermd")

library(dplyr)
library(rmarkdown)
library(ggplot2)
#library(plotly)
library(lubridate)
#library(htmlwidgets)
library(sf)

#####graph portion#############################################################

results <- read.csv("https://www.waterqualitydata.us/data/Result/search?organization=CT_DEP01_WQX&project=lakesABM&project=lakesNLA&mimeType=csv&zip=no&dataProfile=resultPhysChem&providers=NWIS&providers=STORET")

characteristics <- c("Dissolved oxygen (DO)", "Temperature", 
                     "Chlorophyll", "Phycocyanin (probe)") #profile data we want

units_mapping <- c(
  "Dissolved oxygen (DO)" = "mg/L",
  "Temperature" = "°C",
  "Chlorophyll" = "rfu",
  "Phycocyanin (probe)" = "rfu"
) #for graph labels

results <- results %>%
  filter(CharacteristicName %in% characteristics) #subset results

#tidy up the df
results$datetime <- paste(results$ActivityStartDate, results$ActivityStartTime.Time, sep = " ") #i like datetime columns
results$datetime <- as.POSIXct(results$datetime, format = "%Y-%m-%d %H:%M:%S") 
results$ResultMeasureValue <- as.numeric(results$ResultMeasureValue)
results$MonitoringLocationName <- gsub("\\(|\\)", "", results$MonitoringLocationName) #breaks my filename loops
results$MonitoringLocationName <- gsub("/", "-", results$MonitoringLocationName) #breaks my filename loops
results$MonitoringLocationName <- gsub('"', "", results$MonitoringLocationName) #breaks my filename loops
results$MonitoringLocationIdentifier <- gsub(".*-", "", results$MonitoringLocationIdentifier) #keep just station id
colnames(results)[colnames(results) == "ResultMeasureValue"] <- "Value" #to be nicer on the graph
colnames(results)[colnames(results) == "ActivityDepthHeightMeasure.MeasureValue"] <- "Depth" 

#do some filtering on the dupe data.. both YSI and EXO data, only want EXO
results$ActivityStartDate <- as.Date(results$ActivityStartDate)
EXO_sites <- c("15085", "16150", "18154", "16008", "18201", "15631", "18929", "16184")
start_date <- as.Date("2014-07-29")
end_date <- as.Date("2014-08-20")
f_results <- results %>% #only the results we want to keep at those times and locations
  filter(
    MonitoringLocationIdentifier %in% EXO_sites,                              
    ActivityStartDate >= start_date & ActivityStartDate <= end_date,
    grepl("EXO", ActivityCommentText, ignore.case = TRUE))

e_results <- results %>% #results minus all the sites at those dates & locations
  filter(
    !(MonitoringLocationIdentifier %in% EXO_sites 
      & ActivityStartDate >= start_date & ActivityStartDate <= end_date)
  )

#sticking them together
results <- rbind(f_results, e_results)

#fix the issue with non unique crystal site names
crystal_site <- c("18154", "20764")
c_results <- results %>% #only the results we want to keep at those times and locations
  filter(
    MonitoringLocationIdentifier %in% crystal_site)
c_results$MonitoringLocationName <- paste(c_results$MonitoringLocationName, c_results$MonitoringLocationIdentifier, sep = " " )
c_results$MonitoringLocationName <- gsub("18154", "Ellington", c_results$MonitoringLocationName)
c_results$MonitoringLocationName <- gsub("20764", "Winchester", c_results$MonitoringLocationName)
results <- results %>% 
  filter(
  !(MonitoringLocationIdentifier %in% crystal_site)
  ) #remove the crystal lake rows
results <- rbind(results, c_results) #add the new ones in
  
#for each site sampling day,  generate plots
output_dir <- "P:/R/lakeprofilermd/plots"

#one graph for each combo
unique_combinations <- unique(results %>% select(MonitoringLocationName, MonitoringLocationIdentifier,
                                                 ActivityStartDate, CharacteristicName))

for (i in 1:nrow(unique_combinations)) {
  combination <- unique_combinations[i, ]
  
  #subset the data for the current combination
  char_data <- results %>%
    filter(MonitoringLocationName == combination$MonitoringLocationName &
             MonitoringLocationIdentifier == combination$MonitoringLocationIdentifier &
             ActivityStartDate == combination$ActivityStartDate &
             CharacteristicName == combination$CharacteristicName) %>%
    distinct(Depth, .keep_all = TRUE) %>% #dupe surface samples
    arrange(Depth) # this doesnt do anything lol

  units <- units_mapping[combination$CharacteristicName] #pair units with char

  #plot time
  p <- ggplot(char_data, aes(y = Value, x = Depth, group = 1)) +
    geom_line() +
    scale_x_reverse() +
    coord_flip() +
    labs(title = paste(combination$MonitoringLocationName, " - ", 
                       combination$MonitoringLocationIdentifier, sep = ""),
         subtitle = paste("Date: ", combination$ActivityStartDate, sep = ""),
         y = paste(combination$CharacteristicName, " ",  "(",  units, ")", sep = ""),
         x = "Depth (m)") +
    theme_minimal()
  
  #p_plotly <- ggplotly(p) #the plotly graphs are too powerful
  
  #filename
  lake <- combination$MonitoringLocationName
  site <- combination$MonitoringLocationIdentifier
  date <- combination$ActivityStartDate
  characteristic <- combination$CharacteristicName
  filename <- paste(lake, "_", site, "_", date, "_", characteristic, ".png", sep = "")
  
  #save each file
  ggsave(filename, path = "P:/R/lakeprofilermd/plots", width = 400, height = 400,
         units = "px", dpi = 100) #smol
}

######spatial stuff to add to generic description at top of file################
setwd("P:/R/lakeprofilermd")

depth <- read.csv("historical_lakes_max_depth.csv")
colnames(depth)[colnames(depth) == "staSeq"] <- "MonitoringLocationIdentifier"
lakes <- read_sf(dsn = ".", layer = "NHDWaterbody")
lakes <- st_transform(lakes, crs = 4326)
towns <- read_sf(dsn = ".", layer = "Town_Polygon")
towns <- st_transform(towns, crs = 4326)
towns <- st_make_valid(towns)
# landcover <- st_read("lakes_landcover_analysis_111823.geojson")
# landcover <- st_transform(landcover, crs = 4326)
stations <- read.csv("https://www.waterqualitydata.us/data/Station/search?organization=CT_DEP01_WQX&project=lakesABM&project=lakesNLA&mimeType=csv&zip=no&providers=NWIS&providers=STORET")

#make it same format as rmd stuff
stations$MonitoringLocationName <- gsub("\\(|\\)", "", stations$MonitoringLocationName) 
stations$MonitoringLocationName <- gsub("/", "-", stations$MonitoringLocationName) 
stations$MonitoringLocationName <- gsub('"', "", stations$MonitoringLocationName) 

#fix awx format
#formatting awx id
stations$MonitoringLocationIdentifier <- gsub(".*-", "", stations$MonitoringLocationIdentifier) #keep just station id

#fix format of crystal lake
crystal_site <- c("18154", "20764")
c_stations <- stations %>% #only the results we want to keep at those times and locations
  filter(
    MonitoringLocationIdentifier %in% crystal_site)
c_stations$MonitoringLocationName <- paste(c_stations$MonitoringLocationName, c_stations$MonitoringLocationIdentifier, sep = " " )
c_stations$MonitoringLocationName <- gsub("18154", "Ellington", c_stations$MonitoringLocationName)
c_stations$MonitoringLocationName <- gsub("20764", "Winchester", c_stations$MonitoringLocationName)
stations <- stations %>% 
  filter(
    !(MonitoringLocationIdentifier %in% crystal_site)
  ) #remove the crystal lake rows
stations <- rbind(stations, c_stations) #add the new ones in

#add depth
stations <- left_join(stations, depth, by = "MonitoringLocationIdentifier")

#convert to sf
stations_sf <- st_as_sf(stations, coords=c("LongitudeMeasure","LatitudeMeasure"), crs =4326)

#smush to lakes
stations_lakes <- st_join(stations_sf, lakes)
stations_lakes<- as.data.frame(stations_lakes)
stations_lakes <- stations_lakes[c("MonitoringLocationName", "MonitoringLocationIdentifier", "ComID", "maxD", "areasqkm")]

#smush to towns
stations_towns <- st_join(stations_sf, towns)
stations_towns <- as.data.frame(stations_towns)
stations_towns <- stations_towns[c("MonitoringLocationName","TOWN", "COUNTY")]

#hadron collider
stations_joined <- merge(stations_lakes, stations_towns, by ="MonitoringLocationName")
#stations_joined <- merge(stations_joined, stations_com, by ="MonitoringLocationName")
stations_joined$acres <- stations_joined$areasqkm * 247.105
stations_joined$acres <- round(stations_joined$acres, 1)
stations_joined <- stations_joined[stations_joined$MonitoringLocationIdentifier 
                                   %in% results$MonitoringLocationIdentifier, ]
stations_joined <- stations_joined[!duplicated(stations_joined[c("MonitoringLocationName")]), ]
stations_joined$maxD <- round(stations_joined$maxD, 1)

write.csv(stations_joined, "locationinfo.csv", row.names = FALSE)

####making all da reports######################################################

setwd("P:/R/lakeprofilermd/reports")

locationcsv <- read.csv("locationinfo_f.csv") #df to filter on each iteration
locations <- unique(locationcsv$MonitoringLocationName) #loop variable

for (i in locations) {
  # filter stations to current iteration
  location <- filter(locationcsv, MonitoringLocationName == i)
  name <- unique(location$MonitoringLocationName)
  comID <- unique(location$ComID)
  # base file to modify
  base <- readLines("lakeprofile.Rmd")
  # replace the location
  base <- gsub("location: .*", paste("location:", name), base)
  # write files to reports subfolder
  outputFile <- file.path("P:/R/lakeprofilermd/reports", paste0(name, "_", comID, ".Rmd"))
  writeLines(base, outputFile)
  # render rmd file
  render(input = outputFile, 
         output_file = file.path("P:/R/lakeprofilermd/reports", paste0(name, "_", comID, ".html")),
         output_format = "html_document")  
}


