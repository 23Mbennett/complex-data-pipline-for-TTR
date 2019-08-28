
##### TABLE c5 ######

library(tidyverse)
library(lubridate)
library(data.table)

setwd("C:/Users/bam5/Desktop/bluetoad")


##### Travel Time Reliability Table C5 #####

system.time({

THEA1 <- fread("Bluetooth_TravelTime.csv")%>% 
  mutate(Timestamp = as.POSIXct(Timestamp, format ="%m/%d/%Y %H:%M"))#%>%
  #group_by(PairID)%>%
  #arrange(Timestamp)
          

THEA2 <- THEA1 %>%
  group_by(PairID)%>%
  arrange(Timestamp)%>%
  summarise( DOW = DOW[1],  Timestamp = as.POSIXct(paste0((str_sub(Timestamp[1], end = -10))," ", "00:00:00")), `Travel time (sec)` = NA, `Speed (mph)` = NA)


#nrow(THEA2) == length(unique(THEA2$PairID)) #Data Summary Check

###NEED FREE FLOW TRAVEL TIME FOR EACH PAIR to calcualte PTI ####

pairs_fftt1 <- tibble(PairID =c("TEA-57885", "TEA-57877", "TEA-57879", "TEA-73306", "TEA-57875", "TEA-57861", "TEA-73317", "TEA-73313", "TEA-57855", "TEA-57863"), 
                      FFTSpeed = c(43, 35, 35, 41, 35, 45, 45, 35, 40, 45),
                      Dist = c(0.32, 0.25, 0.48, 0.45, 0.11, 0.52, 0.82, 0.50, 0.28, 0.52))

pairs_fftt2 <- tibble(PairID =c("TEA-57859", "TEA-73315", "TEA-57857", "TEA-57869", "TEA-57887", "TEA-73295", "TEA-73299", "TEA-73310", "TEA-57883", "TEA-57865"),
                      FFTSpeed = c(35, 45, 35, 35, 45, 41, 43, 43, 40, 35),
                      Dist = c(0.08, 0.33, 0.44, 0.11, 0.32, 0.45, 0.38, 0.32, 0.33, 0.23))

pairs_fftt3 <- tibble(PairID = c("TEA-57867","TEA-57881", "TEA-57871", "TEA-57873", "TEA-73303", "TEA-83128", "TEA-83221"),
                      FFTSpeed = c(35, 40, 35, 35, 45, 42, 41),
                      Dist = c(0.23, 0.33, 0.12, 0.11, 0.82, 0.42, 0.74))

pairs <- rbind(pairs_fftt1, pairs_fftt2, pairs_fftt3)%>%
  mutate(miles_per_sec = FFTSpeed/(60*60),
         ff_travel_time_sec = Dist/miles_per_sec,
         ff_travel_time_min = ff_travel_time_sec/60)


THEA3 <- bind_rows(THEA1, THEA2) %>%
  group_by(PairID)%>%
  arrange(Timestamp)%>%
  mutate(tday = cut(Timestamp, breaks = "30 min"))%>% #marker
  group_by(PairID, tday)%>%
  summarize(ss = length(tday), 
            sd = sd(`Travel time (sec)`, na.rm = T)/60, 
            avg = mean(`Travel time (sec)`, na.rm = T)/60,
            cv = sd/avg,
            per95 = quantile(`Travel time (sec)`, probs = 0.95, na.rm = T)/60)%>%
  mutate(tday2 = as.POSIXct(tday),
         DOW = lubridate::wday(tday2, label = TRUE, abbr = FALSE),
         buffer_index = (per95 - avg)/avg)%>%
  arrange(PairID, tday)%>%
  inner_join(pairs, by = "PairID") %>% 
  mutate(plan_index = per95/ff_travel_time_min)%>%
  select(PairID, DOW, tday, ss, sd, cv, buffer_index, plan_index)%>%
  rename(`Time of day` = tday,
         `Sample size` = ss,
         `Standard deviation` = sd,
         `Coefficient of variation` = cv,
         `Buffer time index` = buffer_index,
         `Planning time index` = plan_index)
THEA3$`Buffer time index`[as.logical(THEA3$`Sample size` == 1)] <- NA
THEA3$`Planning time index`[as.logical(THEA3$`Sample size` == 1)] <- NA


fwrite(THEA3, 'Bluetooth_TTR.csv')

})


###### DATA CHECK and  Testing STuff #########

#length(unique(THEA3$PairID))

#unique(THEA4$PairID) %>% filter(PairID != unique(pairs_all$PairID))

#unique(THEA4$PairID)[ !unique(THEA4$PairID) %in%  unique(pairs_all$PairID)] ### need these pairs 


#### DATA CHECK STUFFF#####
#THEA4 %>% filter(PairID == unique(THEA4$PairID)[10]) %>% head()
#THEA4 %>% filter(PairID == unique(THEA4$PairID)[10]) %>% tail()

#THEA4 %>% filter(PairID == unique(THEA4$PairID)[4]) %>% nrow()
#THEA4 %>% filter(PairID == unique(THEA4$PairID)[4]) %>% tail()

#THEA4 %>% filter(PairID == unique(THEA4$PairID)[9]) %>% nrow()

#paste0((str_sub(THEA2$Timestamp[1], end = -10))," ", "00:00:00")


# mutate(tday = cut(Timestamp, breaks = "30 min"))




### SO cutting the date works as long as we include 1 observation from the start of 1 st interval we want included ####
#p57855 <- THEA %>% filter(PairID ==  "TEA-57855") %>%
#rbind(c("TEA-57855", "Monday", "11/26/2018 00:00:00", NA, NA))%>%
#mutate(Timestamp = as.POSIXct(Timestamp, format ="%m/%d/%Y %H:%M:%OS")) %>%
#arrange(Timestamp)%>%
#mutate(tday = cut(Timestamp, breaks = "30 min"))


