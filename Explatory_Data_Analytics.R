################################################################################
################################################################################
########################### Descriptive Data Analysis###########################
################################################################################
################################################################################

####################################################################
####################################################################
#########################Load Packages##############################
####################################################################
####################################################################

pkg = c("devtools","dplyr","plyr","lattice","tibble","rgdal","rgl","parallel",
        "tidyverse","tidyr","XML","sp","sf","spatstat","spatstat.utils",
        "stlnpp","sparr","stplanr","raster","ggplot2","ggthemes","viridis",
        "ggmap","leaflet","maptools","mapview","reshape","reshape2","plotly",
        "ggvis","gganimate","gridExtra")
install.packages(pkg)
easypackages::libraries(pkg)

####################################################################
####################################################################
#########################Load data##################################
####################################################################
####################################################################
load("D:/Github/Honnover-Analysis/HI_AC_LN_ppp/HI_AC_unique.RData")

# #Accident data for all Germany
# 
# setwd("D:/Github/Honnover-Analysis/Unfallorte2017_LinRef_Shapefile")
# DEA <- read_sf("Unfallorte2017_LinRef.shp")

# #Shapefile Accident
# 
# setwd("D:/Github/Honnover-Analysis/data/Hildesheim/HI.AC")
# HI_AC <- read_sf("Hildesheim.A.shp")
# HI_AC <- st_zm(HI_AC)
# st_crs(HI_AC) <- st_crs(DEA)
# HI_AC  <- st_transform(HI_AC,3857)
# plot(HI_AC[,"UTYP1"])
# 
# #Shapefile Network
# 
# setwd("D:/Github/Honnover-Analysis/data/Hildesheim/HI.LN")
# HI_LN <- read_sf("Hildesheim.LN.shp")
# HI_LN  <- st_transform(HI_LN,3857)
# plot(HI_LN[,"osm_id"])
# 
# #Convert Data To Class ppp
# 
# HI_AC_ppp <- as.ppp(HI_AC)
# HI_AC_ppp <- unmark(HI_AC_ppp)
# save("HI_AC_ppp",file="HI_AC_ppp.RData")
# 
# #Convert final Dataset To Class lpp
# 
# HI_LN_AC <- as.lpp(HI_AC_ppp,L=nw)
# unique(unmark(HI_LN_AC))
# save("HI_LN_AC",file="HI_LN_AC.RData")
# 
# #Convert Data To Class ppp
# 
# HI_AC_unique_ppp <- as.ppp(HI_AC_unique)
# marks(HI_AC_unique_ppp) <- HI_AC_unique[,1:19]
# #HI_AC_unique_ppp <- unmark(HI_AC_unique_ppp)
# save("HI_AC_unique_ppp",file="HI_AC_unique_ppp.RData")
# 
# #Convert final Dataset To Class lpp
# 
# HI_LN_AC_unique <- as.lpp(HI_AC_unique_ppp,L=nw)
# save("HI_LN_AC_unique",file="HI_LN_AC_unique.RData")
# 
# #########Unique
# setwd("D:/Github/Honnover-Analysis/HI_AC_unique")
# #Summ<- HI_AC %>% group_by(UIDENTSTLA) %>% summarise()
# #HI_AC_unique$n.Vehicle<-Summ$`sum(New)`
# HI_AC_unique<-unique(HI_AC[,c(1:21,44)])
# HI_AC_unique<-HI_AC_unique[,c(1,6:22)]
# NAMES<-c("ID","Year","Month","Time","Weekday","Accident categories","Type of accident",
#          "Type of accident_1","Accident with a bike","Accident with a car","Accident with a pedestrian","Accident with a motorcycle",
#          "Accident with others","Lighting conditions","Road condition","LINREFX","LINREFY","geometry")
# names(HI_AC_unique)<-NAMES

####################################################################
####################################################################
##########################Change names##############################
####################################################################
####################################################################

colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Category")] <- "Severity"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Type")] <- "Type"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Class")] <- "Class"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Bike")] <- "Bike"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Car")] <- "Car"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Pedestrian")] <- "Pedestrian"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Motorcycle")] <- "Motorcycle"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Accident_Others")] <- "Others"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Light_Conditions")] <- "Lighting"
colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "Road_Conditions")] <- "Road"
# colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "N_Vehicle")] <- "Count"
# colnames(HI_AC_unique)[which(colnames(HI_AC_unique) == "hour_group")] <- "Time_Period"
####################################################################
####################################################################
#############################Rescaling##############################
####################################################################
####################################################################
library(tidyverse) # mutate
HI_AC_unique <- HI_AC_unique %>% mutate(Time_Period = case_when(as.numeric(Time) >= 00  & as.numeric(Time) <03 ~  "1",
                                                                as.numeric(Time) >= 03  & as.numeric(Time) <06 ~  "2",
                                                                as.numeric(Time) >= 06  & as.numeric(Time) <09 ~  "3",
                                                                as.numeric(Time) >= 09  & as.numeric(Time) <12 ~  "4",
                                                                as.numeric(Time) >= 12  & as.numeric(Time) <15 ~  "5",
                                                                as.numeric(Time) >= 15  & as.numeric(Time) <18 ~  "6",
                                                                as.numeric(Time) >= 18  & as.numeric(Time) <21 ~  "7",
                                                                as.numeric(Time) >= 21  & as.numeric(Time) <24 ~  "8"))
st_write(HI_AC_unique,"HI_AC_unique.shp",driver = 'ESRI Shapefile')

HI_AC_unique_1 <- HI_AC_unique
HI_AC_unique_1$Weekday <- factor(HI_AC_unique_1$Weekday, labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))
HI_AC_unique_1$Bike <- factor(HI_AC_unique_1$Bike, labels = c("Not Included","Included"))
HI_AC_unique_1$Severity <- ifelse(HI_AC_unique_1$Severity=="1","2",HI_AC_unique_1$Severity)
HI_AC_unique_1$Severity <- factor(HI_AC_unique_1$Severity, labels = c("Severe","Slight"))
HI_AC_unique_1$Pedestrian <- factor(HI_AC_unique_1$Pedestrian, labels = c("Not Included","Included"))


HI_AC_unique$Month <- as.numeric(HI_AC_unique$Month)
HI_AC_unique$Weekday <- as.numeric(HI_AC_unique$Weekday)
# HI_AC_unique$Time <- as.numeric(HI_AC_unique$Time)
# HI_AC_unique$Severity <- as.numeric(HI_AC_unique$Severity)
# HI_AC_unique$Type <- as.numeric(HI_AC_unique$Type)
# HI_AC_unique$Class <- as.numeric(HI_AC_unique$Class)
# HI_AC_unique$Car <- as.numeric(HI_AC_unique$Car)
# HI_AC_unique$Bike <- as.numeric(HI_AC_unique$Bike)
# HI_AC_unique$Pedestrian <- as.numeric(HI_AC_unique$Pedestrian)
# HI_AC_unique$Motorcycle <- as.numeric(HI_AC_unique$Motorcycle)


# library(directlabels)
# library(transformr)
# library(grid)
# windows()
# p <- ggplot(HI_AC_unique, aes(Month, Weekday)) +
#   geom_line() +
#   transition_reveal(Month) +
#   labs(title = 'A: {frame_along}')
# 
# anim_save("basic_animation.gif", p)
# 
# animate(p, nframes=40, fps = 2)
####################################################################
####################################################################
##################Number of accdient by month#######################
####################################################################
####################################################################
HI_AC_unique_2 <- HI_AC_unique
HI_AC_unique_2$Month <- as.factor(HI_AC_unique$Month)

library(grDevices)
library(dplyr)
library(ggplot2)

png("Accident_Month_Count_bar.png",height = 600,width = 800)
par(mar=c(0,0,0,1))
ggplot(HI_AC_unique, aes(x=as.factor(Month),fill=as.factor(Month))) + 
  geom_bar(stat='count', fill="steelblue")+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  ylab('Number of Accident')+
  theme(axis.text.x = element_text(size = 21),
        axis.text.y = element_text(size = 21),axis.title = element_text(size = 21))
dev.off()

# par(mar=c(0,0,0,1))
# HI_AC_unique %>% 
#   summarize(Accident_count=n_distinct(ID)) %>%
#   ggplot(aes(x=Month, y=Accident_count)) +
#   geom_bar(stat="identity", fill="blue")+
#   geom_line(size = 1.5, alpha = 0.7, color = "red", group=1) +
#   geom_point(size = 1) + 
#   scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
#                               'Aug','Sep','Oct','Nov','Dec'))+
#   ylab('Number of Accident')+
#   theme(axis.text.x = element_text(size = 21),
#         axis.text.y = element_text(size = 21),axis.title = element_text(size = 21))
# dev.off()
    
####################################################################
####################################################################
#############Number of accdient by day and month####################
####################################################################
####################################################################

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
  labs(x="Month",y="Day",fill="Number of accident")
dev.off()

####Accident Distribution by Weekday and month

png("Accident_month_weekday_bar.png",height = 600,width = 800)
ggplot(aes(x = Month, fill = Weekday), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  labs(x="Month",y="Number of Accident",fill="Day")+
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  ggtitle('')
dev.off()

####################################################################
####################################################################
#############Number of accdient by month and category###############
####################################################################
####################################################################
#(one of them should be used)

png("Accident_month_Category_bar.png",height = 500,width = 800)
ggplot(aes(x = Month ,fill = Severity), data = HI_AC_unique_1) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  labs(x="Month",y='Number of Accident',fill="Severity of Accident")+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  theme(text = element_text(size = 20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  scale_fill_discrete(labels = c("Severe","Slight"))+
  ggtitle('')
dev.off()

png("Accident_month_Category_bar.png",height = 500,width = 800)
ggplot(aes(x = Month ,fill = Severity), data = HI_AC_unique_1) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  labs(x="Month",y='Number of Accident',fill="Severity of Accident")+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  theme(text = element_text(size = 20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  scale_fill_discrete(labels = c("Fatal","Severe","Slight"))+
  ggtitle('')
dev.off()

### Accident Category vs Month Plot (one of them should be used)

png("Accident_by_category_month.png",height = 230,width = 800)
HI_AC_unique %>%
  group_by(Severity, Month) %>%
  summarise("n"=n()) %>%
  ggplot(data=., aes(Month,Severity, fill=n)) +
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_y_discrete(labels = c("Fatal","Severe","Slight"))+
  geom_tile() +
  theme(text = element_text(size = 20))+
  scale_fill_viridis(option="plasma") +
  coord_fixed() +
  labs(x="Month",y="Severity of accident",fill="Numer of Accident")
dev.off()


png("Accident_by_category_month.png",height = 230,width = 800)
HI_AC_unique_1 %>%
  group_by(Severity, Month) %>%
  summarise("n"=n()) %>%
  ggplot(data=., aes(Month,Severity, fill=n)) +
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_y_discrete(labels = c("Severe","Slight"))+
  geom_tile() +
  theme(text = element_text(size = 20))+
  scale_fill_viridis(option="plasma") +
  coord_fixed() +
  labs(x="Month",y="Severity of accident",fill="Numer of Accident")
dev.off()

####################################################################
####################################################################
#############Number of accdient by bike and month###################
####################################################################
####################################################################

png("Accident_month_Bike.png",height = 800,width = 800)
ggplot(aes(x = Month, fill = Bike), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels=c('Not Included','Included'))+
  labs(x="Month",y="Number of Accident",fill="Bike")+
  ggtitle('')
dev.off()

####################################################################
####################################################################
#############Number of accdient by light and month####################
####################################################################
####################################################################

png("Accident_month_light.png",height = 600,width = 800)
ggplot(aes(x = Month, fill = Lighting), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  labs(x="Month",y="Number of Accident",fill="Light Conditions")+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_fill_discrete(labels = c("Daylight","Twilight","Darkness"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()

####################################################################
####################################################################
#############Number of accdient by road and month###################
####################################################################
####################################################################

png("Accident_month_road.png",height = 600,width = 800)
ggplot(aes(x = Month, fill = Road), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  labs(x="Month",y="Number of Accident",fill="Road Conditions")+
  scale_x_discrete(labels = c('Jan','Feb','Mar','Apr','May','June','July',
                              'Aug','Sep','Oct','Nov','Dec'))+
  scale_fill_discrete(labels = c("Dry","Slippery","Winter Smooth"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()


####################################################################
####################################################################
##################Number of accdient by day#########################
####################################################################
####################################################################

####Count
png("Accident_Weekday_Count_bar.png",height = 600,width = 800)
par(mar=c(0,0,0,1))
HI_AC_unique %>% 
  group_by(Weekday) %>% 
  summarize(Accident_count=n_distinct(ID)) %>%
  ggplot(aes(x=Weekday, y=Accident_count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_line(size = 1.5, alpha = 0.7, color = "red", group=1) +
  geom_point(size = 1) + 
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  ylab('Number of Accident')+
  xlab('Day')+
  ylim(c(0,70))+
  theme(text = element_text(size = 20))
dev.off()

####################################################################
####################################################################
#############Number of accdient by day and time period##############
####################################################################
####################################################################

png("Accident_time_period_weekday.png",height = 600,width = 800)
ggplot(aes(x = Weekday, fill = Time_Period), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single"))+
  labs(x="Day",y="Number of Accident",fill="Period of Time")+
  scale_fill_discrete(labels=c("Midnight to 2:59 am","3 am to 5:59 am","6 am to 8:59 am","9 am to Noon","Noon to 2:59 pm","3 pm to 5:59 pm","6 pm to 8:59 pm","9 pm to 11:59 pm"))+
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()


####################################################################
####################################################################
#############Number of accdient by day and category#################
####################################################################
####################################################################

png("Accident_weekday_Category_bar.png",height = 400,width = 600)
ggplot(aes(x = Weekday, fill = Severity), data = HI_AC_unique_1) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  scale_fill_discrete(labels = c("Fatal","Severe","Slight"))+
  labs(x="Day",y="Number of Accident",fill="Severity of Accident")+
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  theme(text = element_text(size = 17),
        legend.title=element_text(size=17), 
        legend.text=element_text(size=17))+
  ggtitle('')
dev.off()


png("Accident_weekday_Category_bar.png",height = 400,width = 600)
ggplot(aes(x = Weekday, fill = Severity), data = HI_AC_unique_1) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  scale_fill_discrete(labels = c("Severe","Slight"))+
  labs(x="Day",y="Number of Accident",fill="Severity of Accident")+
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  theme(text = element_text(size = 17),
        legend.title=element_text(size=17), 
        legend.text=element_text(size=17))+
  ggtitle('')
dev.off()

### Accident Category vs Weekday Plot

png("Accident_by_category_weekday.png",height = 300,width = 500)
HI_AC_unique %>%
  group_by(Severity, Weekday) %>%
  summarise("n"=n()) %>%
  ggplot(data=., aes(Weekday,Severity, fill=n)) +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_y_discrete(labels = c("Fatal","Severe","Slight"))+
  geom_tile() +
  scale_fill_viridis(option="plasma") +
  theme(text = element_text(size = 20))+
  coord_fixed() +
  labs(x="Day",y="Severity of Accident",fill="Number of Accident")
dev.off()


png("Accident_by_category_weekday.png",height = 300,width = 500)
HI_AC_unique_1 %>%
  group_by(Severity, Weekday) %>%
  summarise("n"=n()) %>%
  ggplot(data=., aes(Weekday,Severity, fill=n)) +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_y_discrete(labels = c("Severe","Slight"))+
  geom_tile() +
  scale_fill_viridis(option="plasma") +
  theme(text = element_text(size = 20))+
  coord_fixed() +
  labs(x="Day",y="Severity of Accident",fill="Number of Accident")
dev.off()
####################################################################
####################################################################
#############Number of accdient by day and pedestrian###############
####################################################################
####################################################################

png("Accident_weekday_Pedestrian.png",height = 600,width = 800)
ggplot(aes(x = Weekday, fill = Pedestrian), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Day') +
  ylab('Number of Accident') +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_fill_discrete(labels=c("Not Included","Included"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()

####################################################################
####################################################################
#######Number of accdient by pedestrian and severity and week#######
####################################################################
####################################################################

png("Accident_Pedestrian_Category_Week.png",height = 600,width = 800)
ggplot(data = HI_AC_unique_1, aes(Weekday)) +
  geom_bar(fill = "steelblue",width = 0.5, size = 1) + 
  facet_grid(Pedestrian~ Severity)+
  labs(y = "Number of Accident", x = "")+
  theme(text = element_text(size = 25))
dev.off()
####################################################################
####################################################################
#############Number of accdient by day and car######################
####################################################################
####################################################################

png("Accident_weekday_Car.png",height = 800,width = 800)
ggplot(aes(x = Weekday, fill = Car), data = HI_AC_unique) +
  geom_bar(stat = 'count', position = position_dodge(preserve="single")) +
  xlab('Day') +
  ylab('Number of Accident') +
  scale_x_discrete(labels = c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.'))+
  scale_fill_discrete(labels=c("Not Included","Included"))+
  theme(text = element_text(size = 20))+
  ggtitle('')
dev.off()

####################################################################
####################################################################
#############Number of accdient by bike and category################
####################################################################
####################################################################

png("Accident_Bike_Category.png",height = 600,width = 800)
ggplot(aes(x = Weekday ,fill = Bike), data = HI_AC_unique) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  xlab('Severity of Accident') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c("Fatal","Severe","Slight"))+
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels = c("Not Included","Included"))+
  ggtitle('')
dev.off()


png("Accident_Bike_Category.png",height = 600,width = 800)
ggplot(aes(x = Weekday ,fill = Bike), data = HI_AC_unique_1) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  xlab('Severity of Accident') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c("Severe","Slight"))+
  theme(text = element_text(size = 20))+
  scale_fill_discrete(labels = c("Not Included","Included"))+
  ggtitle('')
dev.off()

####################################################################
####################################################################
#########Number of accdient by bike and Severity and week###########
####################################################################
####################################################################

png("Accident_Bike_Category_Week.png",height = 600,width = 800)
ggplot(data = HI_AC_unique_1, aes(Weekday)) +
  geom_bar(fill = "steelblue",width = 0.5, size = 1) + 
  facet_grid(Bike~ Severity)+
  labs(y = "Number of Accident", x = "")+
theme(text = element_text(size = 25))
dev.off()

####################################################################
####################################################################
#############Number of accdient by bike and type####################
####################################################################
####################################################################

png("Accident_Type_Light.png",height = 600,width = 800)
ggplot(aes(x = Lighting ,fill = Type), data = HI_AC_unique) +
  geom_bar(stat = 'count', position=position_dodge(preserve="single"))+   
  xlab('Light Conditions') +
  ylab('Number of Accident')+
  scale_x_discrete(labels = c("Daylight","Twilight","Darkness"))+
  scale_fill_discrete(labels = c('Collision with starting/stopping vehicle'
  ,'Collision with starting/stopping/stationary vehicle'
  ,'Collision with the vehicle moving sideways'
  ,'Collision with an oncoming vehicle'
  ,'Collision with turning/crossing'
  ,'Collision between vehicle and pedestrian'
  ,'Impact on road obstacles'
  ,'Off road to the right'
  ,'Leaving the lane to the left'
  ,'Accident of another kind'))+
  theme(text = element_text(size = 20))+
  guides(fill=guide_legend(title="Type of Accident"))+
  ggtitle('')
dev.off()

