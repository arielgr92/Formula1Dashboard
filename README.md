# Formula 1 Dashboard (R Script for Data Aggregation)
```R

# ============================================================================== LIBRARIES
library(DataExplorer)
library(readxl)
library(dplyr)
library(tidyr)
library("tibble")

# ============================================================================== IMPORT RAW DATA
path= ('C:/Users/ricoari/Pictures/VizContest/Data')
# Kaggle Original ----
df_raw_circuits                = read.csv(paste(path,"/circuits.csv",sep=""))
df_raw_constructor_results     = read.csv(paste(path,"/constructor_results.csv",sep=""))
df_raw_constructor_standings   = read.csv(paste(path,"/constructor_standings.csv",sep=""))
df_raw_constructors            = read.csv(paste(path,"/constructors.csv",sep=""))
df_raw_driver_standings        = read.csv(paste(path,"/driver_standings.csv",sep=""))
df_raw_lap_times               = read.csv(paste(path,"/lap_times.csv",sep=""))
df_raw_pit_stops               = read.csv(paste(path,"/pit_stops.csv",sep=""))
df_raw_qualifying              = read.csv(paste(path,"/qualifying.csv",sep=""))
df_raw_results                 = read.csv(paste(path,"/results.csv",sep=""))
df_raw_sprint_results          = read.csv(paste(path,"/sprint_results.csv",sep=""))
df_raw_status                  = read.csv(paste(path,"/status.csv",sep=""))
df_raw_drivers                 = read.csv(paste(path,"/drivers.csv",sep=""))
df_raw_races                   = read.csv(paste(path,"/races.csv",sep=""))
df_fav_circuits                = read.csv(paste(path,"/fav_circuits.csv",sep=""))

# Other Dataset ----

# ============================================================================== CLEANING


# ============================================================================== INITIAL AGGREGATION 
# SECTION I --------------------------------------------------------------------
df_drivers =  merge(df_raw_drivers,df_raw_results,by="driverId") %>% 
              merge(df_raw_status,by="statusId")

df_total_races_by_driver     <-  df_drivers %>% 
                                 group_by(driverId, forename, surname) %>%
                                 summarise(total_races = n())    

df_drivers_top3_count        <-  df_drivers %>% 
                                 filter(positionText %in% c('1','2','3'))  %>% 
                                 group_by(driverId,positionText, forename, surname) %>% 
                                 summarise(PositionCount = n()) %>% 
                                 spread(positionText, PositionCount, fill = NA, convert = FALSE) %>% 
                                 mutate(podiums = `1`+`2`+`3`)
  
df_drivers_total_points      <- df_drivers %>% 
                                group_by(driverId, forename, surname) %>% 
                                summarise(total_points = sum(points))    


df_drivers_Numb_fastest_laps <- df_drivers %>% 
                                filter(rank == '1' )  %>% 
                                group_by(driverId, forename, surname) %>% 
                                summarise(fastest_laps = n())

df_drivers_status            <- df_drivers %>% 
                                group_by(driverId, forename, surname, status) %>%
                                summarise(count_status = n()) %>% 
                                mutate(FinishRace = ifelse(status=='Finished',1, 0)) %>% 
                                merge(df_total_races_by_driver[,c("driverId","total_races")],by='driverId',keep = FALSE)
            
df_driver_completion_ratio   <- df_drivers_status %>% 
                                filter(status=='Finished') %>% 
                                mutate(CR= count_status/total_races)

df_shortest_pit_stop         <- merge(df_drivers,df_raw_pit_stops,by="raceId") %>% 
                                group_by(driverId.x) %>% 
                                summarise(shortest_pit = min(milliseconds.y))
colnames(df_shortest_pit_stop)<- c('driverId','Shortest_pit_stop','C3')

df_longest_pit_stop          <- merge(df_drivers,df_raw_pit_stops,by="raceId") %>% 
                                group_by(driverId.x) %>% 
                                summarise(shortest_pit = max(milliseconds.y))
colnames(df_shortest_pit_stop)<- c('driverId','Longest_pit_stop','C3')

DF_DRIVERS_official = merge(df_raw_drivers,df_total_races_by_driver[,c('driverId','total_races')], by='driverId', all.x = TRUE) %>% 
                      merge(.,df_drivers_top3_count[,c('driverId','1','2','3','podiums')], by='driverId', all.x = TRUE) %>% 
                      merge(.,df_drivers_total_points[,c('driverId','total_points')], by='driverId', all.x = TRUE) %>% 
                      merge(.,df_drivers_Numb_fastest_laps[,c('driverId','fastest_laps')], by='driverId', all.x = TRUE) %>% 
                      merge(.,df_driver_completion_ratio[,c('driverId','CR')], by='driverId', all.x = TRUE) %>% 
                      merge(.,df_shortest_pit_stop[,c('driverId','Longest_pit_stop')], by='driverId', all.x = TRUE)
                      merge(.,df_fav_circuits[,c('driverId','circuitId')], by='driverId', all.x = TRUE)        

write.csv(DF_DRIVERS_official,"C:/Users/ricoari/Pictures/VizContest/Data/DF_DRIVER_Official.csv")
              
# SECTION II -------------------------------------------------------------------

DF_CONSTR1                <- merge(df_raw_races[,c('raceId','year')], df_raw_results[,c('driverId','raceId','constructorId','position')], by='raceId', all.x = TRUE) %>% 
                            merge(.,df_raw_drivers[,c('driverId','surname','forename')], by='driverId', all.x = TRUE) %>% 
                            merge(.,df_raw_constructors[,c('name','constructorId')],by='constructorId', all.x = TRUE) %>% 
                            filter(position == '1')  %>% 
                            group_by(name,surname,forename) %>% 
                            summarise(Count_of_1st_Place = n())

DF_CONSTR2                <- merge(df_raw_races[,c('raceId','year')], df_raw_results[,c('driverId','raceId','constructorId','position')], by='raceId', all.x = TRUE) %>% 
                             merge(.,df_raw_drivers[,c('driverId','surname','forename')], by='driverId', all.x = TRUE) %>% 
                             merge(.,df_raw_constructors[,c('name','constructorId')],by='constructorId', all.x = TRUE) %>% 
                             filter(position == '1')  %>% 
                             group_by(name) %>% 
                             summarise(Count_of_1st_Place = n()) %>% 
                             add_column(surname = NA) %>% add_column(forename = NA)

DF_CONSTR_unionall = rbind(DF_CONSTR1,DF_CONSTR2)

DF_CONSTR_unionall       <- DF_CONSTR_unionall %>% 
                            mutate(Level=ifelse(is.na(surname)==TRUE,1,2))

DF_CONSTR_unionall1       <- DF_CONSTR_unionall %>%   add_column(ToPad = 1)
DF_CONSTR_unionall2       <- DF_CONSTR_unionall %>%   add_column(ToPad = 203)

DF_CONSTR_FINAL = rbind(DF_CONSTR_unionall1,DF_CONSTR_unionall2)

write.csv(DF_CONSTR_FINAL,"C:/Users/ricoari/Pictures/VizContest/Data/DF_CONSTRUCT_.csv")

```
