################################################################################
################################################################################
################################Relative Risk-Images############################
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

load("D:/Github/Honnover-Analysis/HI_AC_LN_ppp/HI_LN_AC_unique.RData")
HI_LN_AC_unique

####################################################################
####################################################################
##########################Change names##############################
####################################################################
####################################################################

X <- HI_LN_AC_unique
X_unmark <- unmark(X)
npoints(X_unmark)

####################################################################
####################################################################
#######################Category of accidents########################
####################################################################
####################################################################

X$data$Type.of.accident
X$data$Accident.categories
X_Accident.categories <- X
marks(X_Accident.categories) <- as.numeric(X$data$Accident.categories)
X_Accident.categories$data$marks

X_Accident.categories_1 <- X_Accident.categories[X_Accident.categories$data$marks==1]
X_Accident.categories_2 <- X_Accident.categories[X_Accident.categories$data$marks==2]
X_Accident.categories_3 <- X_Accident.categories[X_Accident.categories$data$marks==3]

npoints(X_Accident.categories_1)
#[1] 3
npoints(X_Accident.categories_2)
#[1] 26
npoints(X_Accident.categories_3)
#[1] 285

################################################
################################################
####################Kernel######################
################################################
################################################
library(sparr)

common_bw_cat23 <- LSCV.risk(as.ppp(X_Accident.categories_2),
                             as.ppp(X_Accident.categories_3),
                             method = "hazelton",hlim = c(0,3000)) # hlim depends on the pattern, usually not given, but if it detects the largest bw in the automatic range, then we need to give a large one to make sure about the selectedd bw.


d_X_Accident.categories_2 <- densityQuick.lpp(unmark(X_Accident.categories_2), sigma = common_bw_cat23,
                                              positive = TRUE,dimyx=256)

d_X_Accident.categories_3 <- densityQuick.lpp(unmark(X_Accident.categories_3), sigma = common_bw_cat23,
                                              positive = TRUE,dimyx=256)

npoints(X_Accident.categories_2)/npoints(X_Accident.categories_3)
# [1] 0.09122807

################################################
################################################
####################Plot########################
################################################
################################################

png("RR_categories_23.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.categories_2/d_X_Accident.categories_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

####################################################################
####################################################################
################################Car#################################
####################################################################
####################################################################

table(X$data$Accident.with.a.car)
X_Accident.with.a.car <- X
marks(X_Accident.with.a.car) <- as.numeric(X$data$Accident.with.a.car)
X_Accident.with.a.car$data$marks

X_Accident.with.a.car_0 <- X_Accident.with.a.car[X_Accident.with.a.car$data$marks==0]
X_Accident.with.a.car_1 <- X_Accident.with.a.car[X_Accident.with.a.car$data$marks==1]

plot(unmark(X_Accident.with.a.car_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.car_1),pch=20,cols=2,col=4,main="",cex=2.5)

npoints(X_Accident.with.a.car_0)
#[1] 31
npoints(X_Accident.with.a.car_1)
#[1] 283

################################################
################################################
####################Kernel######################
################################################
################################################

common_bw_car01 <- LSCV.risk(as.ppp(X_Accident.with.a.car_0),
                             as.ppp(X_Accident.with.a.car_1),
                             method = "hazelton",hlim = c(0,3000))

d_X_Accident.with.a.car_0 <- densityQuick.lpp(unmark(X_Accident.with.a.car_0), sigma = common_bw_car01,
                                              positive = TRUE,dimyx=256)

d_X_Accident.with.a.car_1 <- densityQuick.lpp(unmark(X_Accident.with.a.car_1), sigma = common_bw_car01,
                                              positive = TRUE,dimyx=256)

npoints(X_Accident.with.a.car_0)/npoints(X_Accident.with.a.car_1)
# [1] 0.1095406

################################################
################################################
####################Plot########################
################################################
################################################

png("RR_car_01.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.car_0/d_X_Accident.with.a.car_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

####################################################################
####################################################################
#############################Pedestrian#############################
####################################################################
####################################################################

table(X$data$Accident.with.a.pedestrian)
X_Accident.with.a.pedestrian <- X
marks(X_Accident.with.a.pedestrian) <- as.numeric(X$data$Accident.with.a.pedestrian)
X_Accident.with.a.pedestrian$data$marks

X_Accident.with.a.pedestrian_0 <- X_Accident.with.a.pedestrian[X_Accident.with.a.pedestrian$data$marks==0]
X_Accident.with.a.pedestrian_1 <- X_Accident.with.a.pedestrian[X_Accident.with.a.pedestrian$data$marks==1]

plot(unmark(X_Accident.with.a.pedestrian_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.pedestrian_1),pch=20,cols=2,col=4,main="",cex=2.5)


npoints(X_Accident.with.a.pedestrian_0)
#[1] 275
npoints(X_Accident.with.a.pedestrian_1)
#[1] 39

################################################
################################################
####################Kernel######################
################################################
################################################

common_bw_pedestrian10 <- LSCV.risk(as.ppp(X_Accident.with.a.pedestrian_1),
                                    as.ppp(X_Accident.with.a.pedestrian_0),
                                    method = "hazelton",hlim = c(0,3000)) 
#[1] 1147.83

d_X_Accident.with.a.pedestrian_0 <- densityQuick.lpp(unmark(X_Accident.with.a.pedestrian_0), sigma = common_bw_pedestrian10,
                                                     positive = TRUE,dimyx=256)

d_X_Accident.with.a.pedestrian_1 <- densityQuick.lpp(unmark(X_Accident.with.a.pedestrian_1), sigma = common_bw_pedestrian10,
                                                    positive = TRUE,dimyx=256)

npoints(X_Accident.with.a.pedestrian_1)/npoints(X_Accident.with.a.pedestrian_0)
# [1] 0.1418182

################################################
################################################
####################Plot########################
################################################
################################################

png("RR_pedestrian_10.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.pedestrian_1/d_X_Accident.with.a.pedestrian_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()


####################################################################
####################################################################
#######################Road conditions##############################
####################################################################
####################################################################

table(X$data$Road.condition)
X_Road.condition <- X
marks(X_Road.condition) <- as.numeric(X$data$Road.condition)
X_Road.condition$data$marks

X_Road.condition_0 <- X_Road.condition[X_Road.condition$data$marks==0]
X_Road.condition_1 <- X_Road.condition[X_Road.condition$data$marks==1]

plot(unmark(X_Road.condition_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Road.condition_1),pch=20,cols=2,col=4,main="",cex=2.5)

npoints(X_Road.condition_0)
#[1] 224
npoints(X_Road.condition_1)
#[1] 87

################################################
################################################
####################Kernel######################
################################################
################################################

common_bw_road10 <- LSCV.risk(as.ppp(X_Road.condition_1),
                               as.ppp(X_Road.condition_0),
                               method = "hazelton",hlim = c(0,3000)) # hlim depends on the pattern, usually not given, but if it detects the largest bw in the automatic range, then we need to give a large one to make sure about the selected bw.

# 2291.845

d_X_Road.condition_0 <- densityQuick.lpp(unmark(X_Road.condition_0), sigma = common_bw_road10,
                                         positive = TRUE,dimyx=256)

d_X_Road.condition_1 <- densityQuick.lpp(unmark(X_Road.condition_1), sigma = common_bw_road10,
                                         positive = TRUE,dimyx=256)

npoints(X_Road.condition_1)/npoints(X_Road.condition_0)
# [1] 0.3883929

################################################
################################################
####################Plot########################
################################################
################################################

png("RR_Road_canditions_10.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Road.condition_1/d_X_Road.condition_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

####################################################################
####################################################################
##################################week##############################
####################################################################
####################################################################

X$data$Weekday
X_week <- X
marks(X_week) <- as.numeric(X$data$Weekday)
X_week$data$marks

X_weekday <- X_week[X_week$data$marks==2 | X_week$data$marks==3 | X_week$data$marks==4 |
                      X_week$data$marks==5 | X_week$data$marks==6]
X_weekend <- X_week[X_week$data$marks==1 | X_week$data$marks==7]

npoints(X_weekday)
# [1] 251

npoints(X_weekend)
# [1] 63

common_bw_week <- LSCV.risk(as.ppp(X_weekend),
                            as.ppp(X_weekday),
                            method = "davies",hlim = c(0,10000),type = "fixed") # hlim depends on the pattern, usually not given, but if it detects the largest bw in the automatic range, then we need to give a large one to make sure about the selected bw.
# [1] 1154.927

d_X_weekend <- densityQuick.lpp(unmark(X_weekend), sigma = common_bw_week,
                                positive = TRUE,dimyx=256)

d_X_weekday <- densityQuick.lpp(unmark(X_weekday), sigma = common_bw_week,
                                positive = TRUE,dimyx=256)

npoints(X_weekend)/npoints(X_weekday)
# [1] 0.250996

################################################
################################################
####################Plot########################
################################################
################################################

png("RR_week.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_weekend/d_X_weekday,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

####################################################################
####################################################################
####################Image_car_pedestrian_cat_road###################
####################################################################
####################################################################

p_cat <- plot(d_X_Accident.categories_2/d_X_Accident.categories_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
p_car <- plot(d_X_Accident.with.a.car_0/d_X_Accident.with.a.car_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
p_pedestrian <- plot(d_X_Accident.with.a.pedestrian_1/d_X_Accident.with.a.pedestrian_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
p_road <- plot(d_X_Road.condition_1/d_X_Road.condition_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)

RR_car  <- d_X_Accident.with.a.car_0/d_X_Accident.with.a.car_1
RR_pede <- d_X_Accident.with.a.pedestrian_1/d_X_Accident.with.a.pedestrian_0
RR_cat  <- d_X_Accident.categories_2/d_X_Accident.categories_3
RR_road <- d_X_Road.condition_1/d_X_Road.condition_0

im_RR_car <- raster(as.im(RR_car))
im_RR_pede <- raster(as.im(RR_pede))
im_RR_cat <- raster(as.im(RR_cat))
im_RR_road <- raster(as.im(RR_road))

sp_im_RR_car <- spplot(im_RR_car,at=as.vector(quantile(im_RR_car,seq(0,1,len=40))),col.regions=c(viridis(40),"#f03b20"),
                       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      

sp_im_RR_pede <- spplot(im_RR_pede,at=as.vector(quantile(im_RR_pede,seq(0,1,len=40))),col.regions=c(viridis(40),"#f03b20"),
                        par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

sp_im_RR_cat <- spplot(im_RR_cat,at=as.vector(quantile(im_RR_cat,seq(0,1,len=40))),col.regions=c(viridis(40),"#f03b20"),
                       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      

sp_im_RR_road <- spplot(im_RR_road,at=as.vector(quantile(im_RR_road,seq(0,1,len=40))),col.regions=c(viridis(40),"#f03b20"),
                        par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      


png("im_rr_car.png",height = 400,width = 500)
par(mar=c(0,0,0,1))
sp_im_RR_car
dev.off()

png("im_rr_pedestrian.png",height = 400,width = 500)
par(mar=c(0,0,0,1))
sp_im_RR_pede
dev.off()

png("im_rr_category.png",height = 400,width = 500)
par(mar=c(0,0,0,1))
sp_im_RR_cat
dev.off()

png("im_rr_road.png",height = 400,width = 500)
par(mar=c(0,0,0,1))
sp_im_RR_road
dev.off()

png("RR_marks.png",height = 500,width = 800)
grid.arrange(sp_im_RR_car,sp_im_RR_pede,sp_im_RR_cat,sp_im_RR_road)
dev.off()

