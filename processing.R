portal_raw <- readRDS("GISresults.rds")
sites <- readRDS("GISsites.rds")

portal_raw$ResultMeasure.MeasureUnitCode <- trimws(portal_raw$ResultMeasure.MeasureUnitCode)
portal_raw$Date <- as.Date(portal_raw$ActivityStartDate, format = "%m/%d/%Y")
portal_raw$ResultMeasureValue <- as.character(portal_raw$ResultMeasureValue)
portal_raw$ResultMeasureValue[portal_raw$ResultMeasureValue == "<10"] <- as.character(runif(length(portal_raw$ResultMeasureValue[portal_raw$ResultMeasureValue == "<10"]), 0, 9))
portal_raw$ResultMeasureValue[portal_raw$ResultMeasureValue == "U"]<-""
portal_raw$ResultMeasureValue <- as.numeric(portal_raw$ResultMeasureValue)

min(as.numeric(portal_raw$ResultMeasureValue[portal_raw$CharactersitcName == "Cadmium" 
                                  & portal_raw$ResultMeasure.MeasureUnitCode == "mg/l"]))

#result of spatial join in arc between mining footprints and monitoring site catchment basins.
#contains all combinations of mining footprints and sampling site basins

join_data <- read.csv("Join_Data.csv", header = TRUE, sep = ",")
#erroneously assigned GRIDCODE == 0 due to duplicated site basins from watershed trials
join_data$GRIDCODE[join_data$FID %in% c(31959, 35869, 39753, 43368, 47150, 51059, 54844, 54849)] <- 639
join_data$DATE <- as.Date(as.character(join_data$YEAR), format = "%Y")
site_areas <- group_by(join_data, YEAR, GRIDCODE, STATION_ID)%>%
  summarise(DATE = first(DATE), MINED = sum(SUM), ORG = first(ORG), NAME = first(NAME))

temp <- group_by(join_data, YEAR, GRIDCODE, STATION_ID)%>%
  summarise(MINED_ADJ = sum(ADJ_SUM))

#match the area of mining activities within a sampling site's basin for the appropriate year, 
#based on when the sample was taken
portal_raw$Mined <- sapply(1:nrow(portal_raw), function(i){
  a <- site_areas$MINED[as.character(site_areas$STATION_ID) == as.character(portal_raw$MonitoringLocationIdentifier[i]) &
                   lubridate::year(portal_raw$Date[i]) - site_areas$YEAR == 1]
  if(length(a) == 0){a <- 0}
  return(a)
}, USE.NAMES = FALSE
)

#going back to join total area of site drainage basins
temp <- read.table("Footprint_Sites_Join.txt", header = TRUE)
#erroneous duplicates from trials of watershed analysis
temp <- temp[-c(3,4054),]
#some basins are multipart if connected by corners
temp <- group_by(temp, GRIDCODE)%>%summarise(AREA = sum(AREA))
site_areas <- left_join(site_areas, temp, by = "GRIDCODE")
rm(temp)

portal_raw$Area <- sapply(1:nrow(portal_raw), function(i){
  a <- site_areas$AREA[as.character(site_areas$STATION_ID) == as.character(portal_raw$MonitoringLocationIdentifier[i]) &
                         lubridate::year(portal_raw$Date[i]) - site_areas$YEAR == 1]
  if(length(a) == 0){a <- 0}
  return(a)
}, USE.NAMES = FALSE
)

portal_raw$pMine <- portal_raw$Mined/portal_raw$Area

#convert all concentration values to ug/l
portal_raw$Values <- sapply(1:nrow(portal_raw), function(i){
  if(portal_raw$ResultMeasure.MeasureUnitCode[i] == "mg/l"){
    ug <- portal_raw$ResultMeasureValue[i] * 1000
  }else if (portal_raw$ResultMeasure.MeasureUnitCode[i] == "ng/l"){
    ug <- portal_raw$ResultMeasureValue[i] * 0.001
  }else {
    ug <- portal_raw$ResultMeasureValue[i]
  }
  return(ug)
}, USE.NAMES = FALSE)

portal_raw$Values <- sapply(1:nrow(portal_raw), function(i){
  if(portal_raw$ResultMeasure.MeasureUnitCode[i] == "mg/l"){
    ug <- portal_raw$ResultMeasureValue[i] * 1000
  }else if (portal_raw$ResultMeasure.MeasureUnitCode[i] == "ng/l"){
    ug <- portal_raw$ResultMeasureValue[i] * 0.001
  }else if (portal_raw$ResultMeasure.MeasureUnitCode[i] == "ppm"){
    ug <- portal_raw$ResultMeasureValue[i] * 1000
  }else if (portal_raw$ResultMeasure.MeasureUnitCode[i] == "mg/kg"){
    ug <- portal_raw$ResultMeasureValue[i] * 1000
  }else {
    ug <- portal_raw$ResultMeasureValue[i]
  }
  return(ug)
}, USE.NAMES = FALSE)



#create counts of stations
site_counts <- group_by(portal_raw, MonitoringLocationIdentifier, ActivityStartDate)%>%
  group_by(MonitoringLocationIdentifier)%>%summarise(count = n())

sites <- merge(sites, site_counts, by = "MonitoringLocationIdentifier", all = TRUE)

distances <- read.csv("distances.csv", header = TRUE)
join_data <- merge(join_data, distances[,2:4], by.x = c("FID", "GRIDCODE"), by.y = c("NEAR_FID", "IN_FID"))
#replace distances below ten with 10
join_data$ADJ_DIST_H <- 857*(join_data$NEAR_DIST ^(-0.760*(1^-0.079)))
join_data$ADJ_DIST_L <- 857*(join_data$NEAR_DIST ^(-0.760*(0.2^-0.079)))
join_data$ADJ_DIST <- ifelse(join_data$NEAR_DIST < 10, (0.01^-0.887)/60, ((join_data$NEAR_DIST/1000)^-0.887)/60)
join_data$ADJ_SUM <- join_data$SUM * join_data$ADJ_DIST