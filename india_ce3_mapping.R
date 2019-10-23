#India CE3 Geopoint Mapping
#David Kennedy 

setwd("C:\\Users\\IDCVDKEN\\Dropbox (LSoHaTM)\\DeWorm3 India - Data Management")

require(googlesheets)
#Import file from google sheets
ce3_gps <- gs_title("India CE3 Geopoints")
#Read data as data frame
df <- gs_read(ce3_gps)
#Select columns to keep from village registry
colstokeep <- c("level5", "level4", "level2_id", "level2", "level1_id", "level1")
#Path to village registry
csvfile <- file.path("Registries\\India Village Registry 20180102.csv")
#Read in village registry
library(data.table)
village_reg <- fread(csvfile, header=TRUE, select=colstokeep)
#Merge data frame to village registry on level1_id
df_vilreg <- merge(df, village_reg, by.x='level1_id_upd', by.y='level1_id')
#Other structure dataset
oth_structure <-
  fread(
    "Geopoint Mapping\\CE2 powerpoint files\\html\\india_other_structure_lookup.csv"
  )
#Merge other structure variables
df_vilreg_label <- merge(df_vilreg, oth_structure, by='type_place', all.x=TRUE)
#Read in India map 
library(rgdal)
india_map <-
  readOGR(
    "Geopoint Mapping\\Shapefiles\\IndiaClusters_Final_20180226.shp"
  )
#Make list of levels
level5_names <- unique(df_vilreg_label$level5)
library(sp)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
#Make colour palette
palette <-
  colorFactor(palette = rev("Paired"),
              domain = NULL,
              na.color = NA, ordered=FALSE)
#Loop to subset dataset, build map
ce3_maps <-
  lapply(setNames(level5_names, level5_names), function(k) {
    y <- subset(df_vilreg_label, level5 == k)
    projects.df <- sp::merge(india_map,y,
                             by = 'level2_id',
                             duplicateGeoms = TRUE)
    leaflet() %>%
      addBingTiles(apikey = "AnaeBcYpazAbocaXxVvcxxKJZj9w9nRH0UGTuix0FZ8HQjE6R-31VwpZlGAD6Fei") %>%
      addPolygons(
        data = india_map,
        stroke = TRUE,
        color = "black" ,
        opacity = 1,
        label = india_map$Name,
        group = "level2_id"
      ) %>%
      addCircleMarkers(
        data = projects.df,
        lng = ~ new_longitude,
        lat = ~ new_latitude,
        color = ~ palette(level1_id_upd),
        radius = 8,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = paste(
          "HH Entry:",projects.df$monitoring_hh_entry,"<br>",
          "HH KEY:",projects.df$monitoring_key, "<br>",
          "Village:", projects.df$level1,"<br>",
          "Type:",projects.df$type_place,"<br>",
          "Type of place:",projects.df$other_structure,"<br>",
          "Date:",projects.df$date,"<br>",
          "Co-ordinates:",projects.df$new_latitude, projects.df$new_longitude
        )
      )
  })
#Save maps as html files
saveWidget(ce3_maps$TIMIRI, 
           file="Geopoint Mapping//CE3_maps//html//india_ce3_THIMIRI.html")
saveWidget(ce3_maps$`JAWADHU HILLS`, 
           file="Geopoint Mapping//CE3_maps//html//india_ce3_JAWADHU.html")

