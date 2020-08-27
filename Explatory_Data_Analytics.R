####################################################################
####################################################################
############ Hildesheim Accident Data Analysis######################
####################################################################
####################################################################

###############################
###############################
############################### Load Packages
###############################
###############################
pkg = c("devtools","dplyr","plyr","lattice","tibble","rgdal","rgl","parallel",
        "tidyverse","tidyr","XML","sp","sf","spatstat","spatstat.utils",
        "stlnpp","sparr","stplanr","raster","ggplot2","ggthemes","viridis",
        "ggmap","leaflet","maptools","mapview","reshape","reshape2","plotly",
        "ggvis","gganimate","gridExtra")
install.packages(pkg)
easypackages::libraries(pkg)

###############################
###############################
############################### Load Data
###############################
###############################
#Accident data for all Germany
setwd("D:/Github/Honnover-Analysis/Unfallorte2017_LinRef_Shapefile")
DEA <- read_sf("Unfallorte2017_LinRef.shp")

#Shapefile Accident
setwd("D:/Github/Honnover-Analysis/data/Hildesheim/HI.AC")
HI_AC <- read_sf("Hildesheim.A.shp")
HI_AC <- st_zm(HI_AC)
st_crs(HI_AC) <- st_crs(DEA)
HI_AC  <- st_transform(HI_AC,3857)
plot(HI_AC[,"UTYP1"])

#Shapefile Network
setwd("D:/Github/Honnover-Analysis/data/Hildesheim/HI.LN")
HI_LN <- read_sf("Hildesheim.LN.shp")
HI_LN  <- st_transform(HI_LN,3857)
plot(HI_LN[,"osm_id"])

#Convert Data To Class ppp
HI_AC_ppp <- as.ppp(HI_AC)
HI_AC_ppp <- unmark(HI_AC_ppp)
save("HI_AC_ppp",file="HI_AC_ppp.RData")

#Convert final Dataset To Class lpp
HI_LN_AC <- as.lpp(HI_AC_ppp,L=nw)
unique(unmark(HI_LN_AC))
save("HI_LN_AC",file="HI_LN_AC.RData")

#Convert Data To Class ppp
HI_AC_unique_ppp <- as.ppp(HI_AC_unique)
marks(HI_AC_unique_ppp) <- HI_AC_unique[,1:19]
#HI_AC_unique_ppp <- unmark(HI_AC_unique_ppp)
save("HI_AC_unique_ppp",file="HI_AC_unique_ppp.RData")

#Convert final Dataset To Class lpp
HI_LN_AC_unique <- as.lpp(HI_AC_unique_ppp,L=nw)
save("HI_LN_AC_unique",file="HI_LN_AC_unique.RData")

#########Unique
#Summ<- HI_AC %>% group_by(UIDENTSTLA) %>% summarise()
#HI_AC_unique$n.Vehicle<-Summ$`sum(New)`
HI_AC_unique<-unique(HI_AC[,c(1:21,44)])
HI_AC_unique<-HI_AC_unique[,c(1,6:22)]
NAMES<-c("ID","Year","Month","Time","Weekday","Accident categories","Type of accident",
         "Type of accident_1","Accident with a bike","Accident with a car","Accident with a pedestrian","Accident with a motorcycle",
         "Accident with others","Lighting conditions","Road condition","LINREFX","LINREFY","geometry")
names(HI_AC_unique)<-NAMES

#################################################
#################################################
################################################# Change name
#################################################
#################################################
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident categories")] <- "Accident_categories"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Type of accident")] <- "Accident_Type"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Type of accident_1")] <- "Accident_Class"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident with a bike")] <- "Accident_Bike"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident with a car")] <- "Accident_Car"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident with a pedestrian")] <- "Accident_Pedestrian"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident with a motorcycle")] <- "Accident_Motorcycle"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident with others")] <- "Accident_Others"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Lighting conditions")] <- "Light_Conditions"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Road condition")] <- "Road_Conditions"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "N_Vehicle")] <- "Count"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "hour_group")] <- "Time_Period"

#################################################
#################################################
################################################# Recode
#################################################
#################################################
HI_AC_unique <- HI_AC_unique %>% mutate(Time_Period = case_when(as.numeric(Time) >= 00  & as.numeric(Time) <03 ~  "1",
                                                                as.numeric(Time) >= 03  & as.numeric(Time) <06 ~  "2",
                                                                as.numeric(Time) >= 06  & as.numeric(Time) <09 ~  "3",
                                                                as.numeric(Time) >= 09  & as.numeric(Time) <12 ~  "4",
                                                                as.numeric(Time) >= 12  & as.numeric(Time) <15 ~  "5",
                                                                as.numeric(Time) >= 15  & as.numeric(Time) <18 ~  "6",
                                                                as.numeric(Time) >= 18  & as.numeric(Time) <21 ~  "7",
                                                                as.numeric(Time) >= 21  & as.numeric(Time) <24 ~  "8"))

#################################################
#################################################
################################################# Exploratory Data Analysis (All types of Accident)
#################################################
#################################################


#################################################
#################################################
################################################# Accident by Month
#################################################
#################################################

png("Accident_Month_Count_bar.png",height = 600,width = 800)
par(mar=c(0,0,0,1))
HI_AC_unique %>% 
  group_by(Month) %>% 
  summarize(Accident_count=n_distinct(ID)) %>%
  ggplot(aes(x=Month, y=Accident_count)) +
  geom_bar(stat="identity", fill=c("red","blue"))+
  geom_line(size = 1.5, alpha = 0.7, color = "red", group=1) +
  geom_point(size = 1) + 
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                           'Aug','Sep','Oct','Nov','Dec'))+
  ylab('Number of Accident')+
  theme(axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),axis.title = element_text(size = 21))
dev.off()
    
### weekday vs Month Plot
png("Accident_by_month_weekday.png",height = 350,width = 700)
HI_AC_unique %>%
  group_by(Month, Weekday) %>%
  summarise("n"=n()) %>%
  ggplot(data=., aes(Month, Weekday, fill=n)) +
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_y_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  theme(text = element_text(size = 20))+
  geom_tile() +
  scale_fill_viridis(option="plasma") +
  coord_fixed() +
  labs(x="Month",y="Week Day",fill="Number of Accident")
dev.off()

####Accident Distribution by Weekday and month
png("Accident_month_weekday_bar.png",height = 600,width = 800)
ggplot(aes(x = Month, fill = Weekday), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Weekday') +
  ylab('Number of Accidents') +
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  ggtitle('')
dev.off()

  ####Accident Distribution by category and month-Bar
png("Accident_month_Category_bar.png",height = 600,width = 800)
ggplot(aes(x = Month ,fill = Accident_Category), data = HI_AC_unique) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  xlab('Month') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels = c("Fatal","Severe","Slight"))+
  ggtitle('')
dev.off()

### Accident Category vs Month Plot
png("Accident_by_category_month.png",height = 230,width = 800)
HI_AC_unique %>%
  group_by(Accident_Category, Month) %>%
  summarise("n"=n()) %>%
  ggplot(data=., aes(Month,Accident_Category, fill=n)) +
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_y_discrete(labels = c("Fatal","Severe","Slight"))+
  geom_tile() +
  theme(text = element_text(size = 20))+
  scale_fill_viridis(option="plasma") +
  coord_fixed() +
  labs(x="Month",y="Accident Category",fill="Numer of Accident")
dev.off()

####Accident Distribution by bike and month
png("Accident_month_Bike.png",height = 800,width = 800)
ggplot(aes(x = Month, fill = Accident_Bike), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Weekday') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels=c('Not Included','Included'))+
  ggtitle('')
dev.off()

####Accident Distribution by light and month
png("Accident_month_light.png",height = 600,width = 800)
ggplot(aes(x = Month, fill = Light_Conditions), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Month') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_fill_discrete(labels = c("Daylight","Twilight","Darkness"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()

  ####Accident Distribution by road and month
png("Accident_month_road.png",height = 600,width = 800)
ggplot(aes(x = Month, fill = Road_Conditions), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Month') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_fill_discrete(labels = c("Dry","Slippery","Winter Smooth"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()


#################################################
#################################################
################################################# Accident by Weekday
#################################################
#################################################

####Count
png("Accident_Weekday_Count_bar.png",height = 600,width = 800)
par(mar=c(0,0,0,1))
HI_AC_unique %>% 
  group_by(Weekday) %>% 
  summarize(Accident_count=n_distinct(ID)) %>%
  ggplot(aes(x=Weekday, y=Accident_count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_line(size = 1, alpha = 0.7, color = "red", group=1) +
  geom_point(size = 1) + 
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  ylab('Number of Accident')+
  ylim(c(0,70))+
  theme(text = element_text(size = 20))
dev.off()


####Accident Distribution by Time of the Day and Day of the week
png("Accident_time_period_weekday.png",height = 600,width = 800)
ggplot(aes(x = Weekday, fill = Time_Period), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Weekday') +
  ylab('Number of Accident') +
  scale_fill_discrete(labels=c("Midnight to 2:59 am","3 am to 5:59 am","6 am to 8:59 am","9 am to Noon","Noon to 2:59 pm","3 pm to 5:59 pm","6 pm to 8:59 pm","9 pm to 11:59 pm"))+
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()
#The most accidents occurred between 03:00 am and 05:59 am on Sundays.
#Between 03:00 am and 05:59 am on Sundays is the time many people leave the bars. 
#How many times do we still have to say, don't drink and drive?



####Accident Distribution by Category and Day of the week
png("Accident_weekday_Category_bar.png",height = 400,width = 600)
ggplot(aes(x = Weekday, fill = Accident_Category), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  scale_fill_discrete(labels = c("Fatal","Severe","Slight"))+
  xlab('Weekday') +
  ylab('Number of Accident') +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()

### Accident Category vs Weekday Plot
png("Accident_by_category_weekday.png",height = 250,width = 500)
HI_AC_unique %>%
  group_by(Accident_Category, Weekday) %>%
  summarise("n"=n()) %>%
  ggplot(data=., aes(Weekday,Accident_Category, fill=n)) +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_y_discrete(labels = c("Fatal","Severe","Slight"))+
  geom_tile() +
  scale_fill_viridis(option="plasma") +
  theme(text = element_text(size = 20))+
  coord_fixed() +
  labs(x="Weekday",y="Accident Category",fill="Number of Accident")
dev.off()

####Accident Distribution by Weekday and Pedestrian
png("Accident_weekday_Pedestrian.png",height = 600,width = 800)
ggplot(aes(x = Weekday, fill = Accident_Pedestrian), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Weekday') +
  ylab('Number of Accident') +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_fill_discrete(labels=c("Not Included","Included"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()

####Accident Distribution by Weekday and Car
png("Accident_weekday_Car.png",height = 800,width = 800)
ggplot(aes(x = Weekday, fill = Accident_Car), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Weekday') +
  ylab('Number of Accident') +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_fill_discrete(labels=c("Not Included","Included"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()




############################
png("Accident_Bike_Category.png",height = 600,width = 800)
ggplot(aes(x = Accident_Category ,fill = Accident_Bike), data = HI_AC_unique) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  xlab('Category of Accident') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c("Fatal","Severe","Slight"))+
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels = c("Not Included","Included"))+
  ggtitle('')
dev.off()


png("Accident_Type_Light.png",height = 600,width = 800)
ggplot(aes(x = Light_Conditions ,fill = Accident_Type), data = HI_AC_unique) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  xlab('Light Conditions') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c("Daylight","Twilight","Darkness"))+
  scale_fill_discrete(labels = c('Collision with moving/stopping stationary vehicle'
  ,'Collision with the vehicle in front/waiting'
  ,'Collision with the vehicle moving sideways in the same direction'
  ,'Collision with an oncoming vehicle'
  ,'Collision with turning/crossing'
  ,'Collision between vehicle and pedestrian'
  ,'Impact on roadway obstacles'
  ,'Turning right'
  ,'Turning left'
  ,'Others'))+
  theme(text = element_text(size = 20))+
  guides(fill=guide_legend(title="Type of Accident"))+
  ggtitle('')
dev.off()


# . Single vehicle accidents
# . Road accidents of vehicles driving in the same direction on the road section
# . Road accidents of oncoming vehicles on the road section
# . Road accidents of vehicles entering a junction from the same direction
# . Road accidents of vehicles entering a junction from opposite directions
# . Road accidents of vehicles entering a junction from neighbouring lanes
# . Road accidents of vehicles and pedestrians
# . Road accidents with standing or parked vehicles
# . Road accidents with animals and rail vehicles
# . Other road accidents
