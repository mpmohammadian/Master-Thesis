################################################################################
################################################################################
#########################Intensity Estimation-Images############################
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
library(spatstat)
X <- HI_LN_AC_unique
X_unmark <- unmark(X)
npoints(X_unmark)

####################################################################
####################################################################
#################Plot of all traffic accidents######################
####################################################################
####################################################################

####### Plot of all traffic accidents

png("accidents.png",height = 800,width = 800)
par(mar=rep(0,4))
plot(X_unmark,pch=20,cols=2,col=4,main="",cex=2.5)
dev.off()

#########################leaflet
merc = CRS("+init=epsg:3857")
WGS84 = CRS("+init=epsg:4326")
meuse.ll = st_transform(HI_AC_unique, WGS84)
bggMap = get_map(as.vector(st_bbox(meuse.ll)), source = "google", zoom = 13,maptype= "hybrid" )

png("accidents_count.png",height = 800,width = 800)
par(mar=rep(0,4))
plot(st_transform(meuse.ll[,"Count"], merc), bgMap = bggMap,lwd = 4, cex = .5)
dev.off()

png("accidents_ID.png",height = 800,width = 800)
par(mar=rep(0,4))
plot(st_transform(meuse.ll[,"ID"], merc), bgMap = bggMap,col="red",lwd = 4, cex = .5,main="")
dev.off()


####################################################################
####################################################################
####################Intensity of unmarked data######################
####################################################################
####################################################################

################################################
################################################
####################Kernel######################
################################################
################################################
library(spatstat)

# References
# Rakshit, S., Davies, T., Moradi, M., McSwiggan, G., Nair, G., Mateu, J. and Baddeley, A. (2019) 
# Fast kernel smoothing of point patterns on a large network using 2D convolution. 
# International Statistical Review. In press. Published online 06 June 2019. 
# DOI: 10.1111/insr.12327.

d_X_unmark <- densityQuick.lpp(X_unmark, sigma = bw.scott.iso(X_unmark),
                               positive = TRUE,dimyx=256,diggle = TRUE,edge2D = TRUE)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_unmark_color.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_unmark*1000,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################
library(raster)
im_d_ker <- raster(as.im(d_X_unmark))
qts_ker <- as.vector(sort(c(quantile(im_d_ker*1000,seq(0,1,len=40)))))

library(viridis)

png("im_d_ker.png",height = 400,width = 600)
spplot(im_d_ker*1000,at=qts_ker,col.regions=c(viridis(40),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))
dev.off()

####################################################################
####################################################################
##########################Intensity of marks########################
####################################################################
####################################################################

####################################################################
####################################################################
###############################Count################################
####################################################################
####################################################################

X$data$N_Vehicle
X_count <- X
marks(X_count) <- as.numeric(X$data$N_Vehicle)
X_count$data$marks

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_count <- densityQuick.lpp(unmark(X_count), sigma = bw.scott.iso(unmark(X_count)),
                              positive = TRUE,dimyx=256)
png("d_X_Accident.categories_3.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_count*1000,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

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

plot(unmark(X_Accident.categories_1),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.categories_2),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.categories_3),pch=20,cols=2,col=4,main="",cex=2.5)

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Accident.categories_1 <- densityQuick.lpp(unmark(X_Accident.categories_1), sigma = bw.scott.iso(unmark(X_Accident.categories_1)),
                                              positive = TRUE,dimyx=256)

d_X_Accident.categories_2 <- densityQuick.lpp(unmark(X_Accident.categories_2), sigma = bw.scott.iso(unmark(X_Accident.categories_2)),
                                              positive = TRUE,dimyx=256)


d_X_Accident.categories_3 <- densityQuick.lpp(unmark(X_Accident.categories_3), sigma = bw.scott.iso(unmark(X_Accident.categories_3)),
                                              positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Accident.categories_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.categories_1,main="",
     ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Accident.categories_2.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.categories_2,main="",
     ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Accident.categories_3.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.categories_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_cat1 <- raster(as.im(d_X_Accident.categories_1))
im_d_cat2 <- raster(as.im(d_X_Accident.categories_2))
im_d_cat3 <- raster(as.im(d_X_Accident.categories_3))
im_d_cat <- stack(im_d_cat2,im_d_cat3)
names(im_d_cat) <- c("Severe","Slight")
qts_cat <- as.vector(sort(c(quantile(im_d_cat2*1000,seq(0,1,len=40)),quantile(im_d_cat3*1000,seq(0,1,len=50)))))

png("im_d_cat.png",height = 400,width = 800)
spplot(im_d_cat*1000,at=qts_cat,col.regions=c(viridis(90),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))
dev.off()


####################################################################
####################################################################
#########################Class of accidents#########################
####################################################################
####################################################################

X$data$Type.of.accident_1
X_Type.of.accident_1 <- X
marks(X_Type.of.accident_1) <- as.numeric(X$data$Type.of.accident_1)
X_Type.of.accident_1$data$marks

X_Type.of.accident_1_1 <- X_Type.of.accident_1[X_Type.of.accident_1$data$marks==1]
X_Type.of.accident_1_2 <- X_Type.of.accident_1[X_Type.of.accident_1$data$marks==2]
X_Type.of.accident_1_3 <- X_Type.of.accident_1[X_Type.of.accident_1$data$marks==3]
X_Type.of.accident_1_4 <- X_Type.of.accident_1[X_Type.of.accident_1$data$marks==4]
X_Type.of.accident_1_5 <- X_Type.of.accident_1[X_Type.of.accident_1$data$marks==5]
X_Type.of.accident_1_6 <- X_Type.of.accident_1[X_Type.of.accident_1$data$marks==6]
X_Type.of.accident_1_7 <- X_Type.of.accident_1[X_Type.of.accident_1$data$marks==7]


plot(unmark(X_Type.of.accident_1_1),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_1_2),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_1_3),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_1_4),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_1_5),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_1_6),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_1_7),pch=20,cols=2,col=4,main="",cex=2.5)

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Type.of.accident_1_1 <- densityQuick.lpp(unmark(X_Type.of.accident_1_1), sigma = bw.scott.iso(unmark(X_Type.of.accident_1_1)),
                                             positive = TRUE,dimyx=256)
d_X_Type.of.accident_1_2 <- densityQuick.lpp(unmark(X_Type.of.accident_1_2), sigma = bw.scott.iso(unmark(X_Type.of.accident_1_2)),
                                             positive = TRUE,dimyx=256)
d_X_Type.of.accident_1_3 <- densityQuick.lpp(unmark(X_Type.of.accident_1_3), sigma = bw.scott.iso(unmark(X_Type.of.accident_1_3)),
                                             positive = TRUE,dimyx=256)
d_X_Type.of.accident_1_4 <- densityQuick.lpp(unmark(X_Type.of.accident_1_4), sigma = bw.scott.iso(unmark(X_Type.of.accident_1_4)),
                                             positive = TRUE,dimyx=256)
d_X_Type.of.accident_1_5 <- densityQuick.lpp(unmark(X_Type.of.accident_1_5), sigma = bw.scott.iso(unmark(X_Type.of.accident_1_5)),
                                             positive = TRUE,dimyx=256)
d_X_Type.of.accident_1_6 <- densityQuick.lpp(unmark(X_Type.of.accident_1_6), sigma = bw.scott.iso(unmark(X_Type.of.accident_1_6)),
                                             positive = TRUE,dimyx=256)
d_X_Type.of.accident_1_7 <- densityQuick.lpp(unmark(X_Type.of.accident_1_7), sigma = bw.scott.iso(unmark(X_Type.of.accident_1_7)),
                                             positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Class.of.accident_1_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Class.of.accident_1_2.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1_2,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Class.of.accident_1_3.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Class.of.accident_1_4.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1_4,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Class.of.accident_1_5.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1_5,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Class.of.accident_1_6.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1_6,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Class.of.accident_1_7.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1_7,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_class_1 <- raster(as.im(d_X_Type.of.accident_1_1))
im_d_class_2 <- raster(as.im(d_X_Type.of.accident_1_2))
im_d_class_3 <- raster(as.im(d_X_Type.of.accident_1_3))
im_d_class_4 <- raster(as.im(d_X_Type.of.accident_1_4))
im_d_class_5 <- raster(as.im(d_X_Type.of.accident_1_5))
im_d_class_6 <- raster(as.im(d_X_Type.of.accident_1_6))
im_d_class_7 <- raster(as.im(d_X_Type.of.accident_1_7))

im_d_class <- stack(im_d_class_1,im_d_class_2,im_d_class_3,im_d_class_4,im_d_class_5,
                    im_d_class_6,im_d_class_7)
names(im_d_class) <- c("Driving","Turning","Turning_Crossing",
                       "Passing","Stationary","Longitudinal","Others")
qtss_class <- as.vector(sort(c(quantile(im_d_class_1*1000,seq(0,1,len=20)),
                               quantile(im_d_class_2*1000,seq(0,1,len=20)),
                               quantile(im_d_class_3*1000,seq(0,1,len=20)),
                               quantile(im_d_class_4*1000,seq(0,1,len=20)),
                               quantile(im_d_class_5*1000,seq(0,1,len=20)),
                               quantile(im_d_class_6*1000,seq(0,1,len=20)),
                               quantile(im_d_class_7*1000,seq(0,1,len=20)))))


png("im_d_class.png",height = 400,width = 800)
spplot(im_d_class*1000,at=qtss_class,col.regions=c(viridis(140),rep("#f03b20",2)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
dev.off()

####################################################################
####################################################################
##########################Type of accidents#########################
####################################################################
####################################################################

X$data$Type.of.accident
X_Type.of.accident <- X
marks(X_Type.of.accident) <- as.numeric(X$data$Type.of.accident)
X_Type.of.accident$data$marks

X_Type.of.accident_0 <- X_Type.of.accident[X_Type.of.accident$data$marks==0]
X_Type.of.accident_1 <- X_Type.of.accident[X_Type.of.accident$data$marks==1]
X_Type.of.accident_2 <- X_Type.of.accident[X_Type.of.accident$data$marks==2]
X_Type.of.accident_3 <- X_Type.of.accident[X_Type.of.accident$data$marks==3]
X_Type.of.accident_4 <- X_Type.of.accident[X_Type.of.accident$data$marks==4]
X_Type.of.accident_5 <- X_Type.of.accident[X_Type.of.accident$data$marks==5]
X_Type.of.accident_6 <- X_Type.of.accident[X_Type.of.accident$data$marks==6]
X_Type.of.accident_7 <- X_Type.of.accident[X_Type.of.accident$data$marks==7]
X_Type.of.accident_8 <- X_Type.of.accident[X_Type.of.accident$data$marks==8]
X_Type.of.accident_9 <- X_Type.of.accident[X_Type.of.accident$data$marks==9]

plot(unmark(X_Type.of.accident_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_1),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_2),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_3),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_4),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_5),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_6),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_7),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_8),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Type.of.accident_9),pch=20,cols=2,col=4,main="",cex=2.5)

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Type.of.accident_0 <- densityQuick.lpp(unmark(X_Type.of.accident_0), sigma = bw.scott.iso(unmark(X_Type.of.accident_0)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_1 <- densityQuick.lpp(unmark(X_Type.of.accident_1), sigma = bw.scott.iso(unmark(X_Type.of.accident_1)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_2 <- densityQuick.lpp(unmark(X_Type.of.accident_2), sigma = bw.scott.iso(unmark(X_Type.of.accident_2)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_3 <- densityQuick.lpp(unmark(X_Type.of.accident_3), sigma = bw.scott.iso(unmark(X_Type.of.accident_3)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_4 <- densityQuick.lpp(unmark(X_Type.of.accident_4), sigma = bw.scott.iso(unmark(X_Type.of.accident_4)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_5 <- densityQuick.lpp(unmark(X_Type.of.accident_5), sigma = bw.scott.iso(unmark(X_Type.of.accident_5)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_6 <- densityQuick.lpp(unmark(X_Type.of.accident_6), sigma = bw.scott.iso(unmark(X_Type.of.accident_6)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_7 <- densityQuick.lpp(unmark(X_Type.of.accident_7), sigma = bw.scott.iso(unmark(X_Type.of.accident_7)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_8 <- densityQuick.lpp(unmark(X_Type.of.accident_8), sigma = bw.scott.iso(unmark(X_Type.of.accident_8)),
                                           positive = TRUE,dimyx=256)
d_X_Type.of.accident_9 <- densityQuick.lpp(unmark(X_Type.of.accident_9), sigma = bw.scott.iso(unmark(X_Type.of.accident_9)),
                                           positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Type.of.accident_0.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_2.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_2,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_3.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_4.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_4,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_5.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_5,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_6.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_6,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_7.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_7,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_8.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_8,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Type.of.accident_9.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Type.of.accident_9,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_type_0 <- raster(as.im(d_X_Type.of.accident_0))
im_d_type_1 <- raster(as.im(d_X_Type.of.accident_1))
im_d_type_2 <- raster(as.im(d_X_Type.of.accident_2))
im_d_type_3 <- raster(as.im(d_X_Type.of.accident_3))
im_d_type_4 <- raster(as.im(d_X_Type.of.accident_4))
im_d_type_5 <- raster(as.im(d_X_Type.of.accident_5))
im_d_type_6 <- raster(as.im(d_X_Type.of.accident_6))
im_d_type_7 <- raster(as.im(d_X_Type.of.accident_7))
im_d_type_8 <- raster(as.im(d_X_Type.of.accident_8))
im_d_type_9 <- raster(as.im(d_X_Type.of.accident_9))

im_d_type<- stack(im_d_type_0,im_d_type_1,im_d_type_2,im_d_type_3,im_d_type_4,im_d_type_5,
                  im_d_type_6,im_d_type_7,im_d_type_8,im_d_type_9)


qtss_type <- as.vector(sort(c(quantile(im_d_type_0*1000,seq(0,1,len=20)),
                              quantile(im_d_type_1*1000,seq(0,1,len=20)),
                              quantile(im_d_type_2*1000,seq(0,1,len=20)),
                              quantile(im_d_type_3*1000,seq(0,1,len=20)),
                              quantile(im_d_type_4*1000,seq(0,1,len=20)),
                              quantile(im_d_type_5*1000,seq(0,1,len=20)),
                              quantile(im_d_type_6*1000,seq(0,1,len=20)),
                              quantile(im_d_type_7*1000,seq(0,1,len=20)),
                              quantile(im_d_type_8*1000,seq(0,1,len=20)),
                              quantile(im_d_type_9*1000,seq(0,1,len=20)))))

png("im_d_type.png",height = 400,width = 800)
spplot(im_d_type*1000,at=unique(qtss_type),col.regions=c(viridis(200),rep("#f03b20",2)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
dev.off()

####################################################################
####################################################################
#######################Light conditions#############################
####################################################################
####################################################################

X$data$Lighting.conditions
X_Lighting.conditions <- X
marks(X_Lighting.conditions) <- as.numeric(X$data$Lighting.conditions)
X_Lighting.conditions$data$marks

X_Lighting.conditions_0 <- X_Lighting.conditions[X_Lighting.conditions$data$marks==0]
X_Lighting.conditions_1 <- X_Lighting.conditions[X_Lighting.conditions$data$marks==1]
X_Lighting.conditions_2 <- X_Lighting.conditions[X_Lighting.conditions$data$marks==2]

plot(unmark(X_Lighting.conditions_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Lighting.conditions_1),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Lighting.conditions_2),pch=20,cols=2,col=4,main="",cex=2.5)

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Lighting.conditions_0 <- densityQuick.lpp(unmark(X_Lighting.conditions_0), sigma = bw.scott.iso(unmark(X_Lighting.conditions_0)),
                                              positive = TRUE,dimyx=256)

d_X_Lighting.conditions_1 <- densityQuick.lpp(unmark(X_Lighting.conditions_1), sigma = bw.scott.iso(unmark(X_Lighting.conditions_1)),
                                              positive = TRUE,dimyx=256)

d_X_Lighting.conditions_2 <- densityQuick.lpp(unmark(X_Lighting.conditions_2), sigma = bw.scott.iso(unmark(X_Lighting.conditions_2)),
                                              positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Lighting.conditions_0.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Lighting.conditions_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Lighting.conditions_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Lighting.conditions_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()


png("d_X_Lighting.conditions_2.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Lighting.conditions_2,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################
        
im_d_light1 <- raster(as.im(d_X_Lighting.conditions_1))
im_d_light2 <- raster(as.im(d_X_Lighting.conditions_2))
im_d_light0 <- raster(as.im(d_X_Lighting.conditions_0))
im_d_light <- stack(im_d_light0,im_d_light1,im_d_light2)
names(im_d_light) <- c("Daylight","Twilight","Darkness")
qts_light <- as.vector(sort(c(quantile(im_d_light0*1000,seq(0,1,len=40)),
                       quantile(im_d_light1*1000,seq(0,1,len=30)),
                       quantile(im_d_light2*1000,seq(0,1,len=30)))))

png("im_d_light.png",height = 300,width = 800)
spplot(im_d_light*1000,at=qts_light,col.regions=c(viridis(100),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))
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

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Road.condition_0 <- densityQuick.lpp(unmark(X_Road.condition_0), sigma = bw.scott.iso(unmark(X_Road.condition_0)),
                                         positive = TRUE,dimyx=256)

d_X_Road.condition_1 <- densityQuick.lpp(unmark(X_Road.condition_1), sigma = bw.scott.iso(unmark(X_Road.condition_1)),
                                         positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Road.conditions_0.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Road.condition_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Road.conditions_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Road.condition_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_road0 <- raster(as.im(d_X_Road.condition_0))
im_d_road1 <- raster(as.im(d_X_Road.condition_1))

im_d_road <- stack(im_d_road0,im_d_road1)
names(im_d_road) <- c("Dry","Slippery")
qts_road <- as.vector(sort(c(quantile(im_d_road0*1000,seq(0,1,len=40)),
                             quantile(im_d_road1*1000,seq(0,1,len=30)))))

png("im_d_road.png",height = 400,width = 800)
spplot(im_d_road*1000,at=qts_road,col.regions=c(viridis(100),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))
dev.off()


####################################################################
####################################################################
#############################bike###################################
####################################################################
####################################################################

table(X$data$Accident.with.a.bike)
X_Accident.with.a.bike <- X
marks(X_Accident.with.a.bike) <- as.numeric(X$data$Accident.with.a.bike)
X_Accident.with.a.bike$data$marks

X_Accident.with.a.bike_0 <- X_Accident.with.a.bike[X_Accident.with.a.bike$data$marks==0]
X_Accident.with.a.bike_1 <- X_Accident.with.a.bike[X_Accident.with.a.bike$data$marks==1]

plot(unmark(X_Accident.with.a.bike_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.bike_1),pch=20,cols=2,col=4,main="",cex=2.5)


################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Accident.with.a.bike_0 <- densityQuick.lpp(unmark(X_Accident.with.a.bike_0), sigma = bw.scott.iso(unmark(X_Accident.with.a.bike_0)),
                                               positive = TRUE,dimyx=256)
d_X_Accident.with.a.bike_1 <- densityQuick.lpp(unmark(X_Accident.with.a.bike_1), sigma = bw.scott.iso(unmark(X_Accident.with.a.bike_1)),
                                               positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Accident.with.a.bike_0.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.bike_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Accident.with.a.bike_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.bike_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_bike1 <- raster(as.im(d_X_Accident.with.a.bike_1))
im_d_bike0 <- raster(as.im(d_X_Accident.with.a.bike_0))
im_d_bike <- stack(im_d_bike0,im_d_bike1)
names(im_d_bike) <- c("Not_Included","Included")
qts_bike <- as.vector(sort(c(quantile(im_d_bike0*1000,seq(0,1,len=40)),
                         quantile(im_d_bike1*1000,seq(0,1,len=20)))))
spplot(im_d_bike*1000,at=qts_bike,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

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

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Accident.with.a.car_0 <- densityQuick.lpp(unmark(X_Accident.with.a.car_0), sigma = bw.scott.iso(unmark(X_Accident.with.a.car_0)),
                                              positive = TRUE,dimyx=256)
d_X_Accident.with.a.car_1 <- densityQuick.lpp(unmark(X_Accident.with.a.car_1), sigma = bw.scott.iso(unmark(X_Accident.with.a.car_1)),
                                              positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Accident.with.a.car_0.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.car_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Accident.with.a.car_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.car_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_car1 <- raster(as.im(d_X_Accident.with.a.car_1))
im_d_car0 <- raster(as.im(d_X_Accident.with.a.car_0))
im_d_car <- stack(im_d_car0,im_d_car1)
names(im_d_car) <- c("Not_Included","Included")
qts_car <- as.vector(sort(c(quantile(im_d_car0*1000,seq(0,1,len=20)),
                         quantile(im_d_car1*1000,seq(0,1,len=40)))))
spplot(im_d_car*1000,at=qts_car,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

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

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Accident.with.a.pedestrian_0 <- densityQuick.lpp(unmark(X_Accident.with.a.pedestrian_0), sigma = bw.scott.iso(unmark(X_Accident.with.a.pedestrian_0)),
                                                     positive = TRUE,dimyx=256)
d_X_Accident.with.a.pedestrian_1 <- densityQuick.lpp(unmark(X_Accident.with.a.pedestrian_1), sigma = bw.scott.iso(unmark(X_Accident.with.a.pedestrian_1)),
                                                     positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Accident.with.a.pedestrian_0.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.pedestrian_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Accident.with.a.pedestrian_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.pedestrian_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_pedestrian1 <- raster(as.im(d_X_Accident.with.a.pedestrian_1))
im_d_pedestrian0 <- raster(as.im(d_X_Accident.with.a.pedestrian_0))
im_d_pedestrian <- stack(im_d_pedestrian0,im_d_pedestrian1)
names(im_d_pedestrian) <- c("Not_Included","Included")
qts5 <- as.vector(sort(c(quantile(im_d_pedestrian0*1000,seq(0,1,len=40)),
                         quantile(im_d_pedestrian1*1000,seq(0,1,len=20)))))
p3 <- spplot(im_d_pedestrian*1000,at=qts5,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

####################################################################
####################################################################
############################Motorcycle##############################
####################################################################
####################################################################

table(X$data$Accident.with.a.motorcycle)
X_Accident.with.a.motorcycle <- X
marks(X_Accident.with.a.motorcycle) <- as.numeric(X$data$Accident.with.a.motorcycle)
X_Accident.with.a.motorcycle$data$marks

X_Accident.with.a.motorcycle_0 <- X_Accident.with.a.motorcycle[X_Accident.with.a.motorcycle$data$marks==0]
X_Accident.with.a.motorcycle_1 <- X_Accident.with.a.motorcycle[X_Accident.with.a.motorcycle$data$marks==1]

plot(unmark(X_Accident.with.a.motorcycle_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.motorcycle_1),pch=20,cols=2,col=4,main="",cex=2.5)

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_Accident.with.a.motorcycle_0 <- densityQuick.lpp(unmark(X_Accident.with.a.motorcycle_0), sigma = bw.scott.iso(unmark(X_Accident.with.a.motorcycle_0)),
                                                     positive = TRUE,dimyx=256)
d_X_Accident.with.a.motorcycle_1 <- densityQuick.lpp(unmark(X_Accident.with.a.motorcycle_1), sigma = bw.scott.iso(unmark(X_Accident.with.a.motorcycle_1)),
                                                     positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_Accident.with.a.motorcycle_0.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.motorcycle_0,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_Accident.with.a.motorcycle_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.motorcycle_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_motorcycle1 <- raster(as.im(d_X_Accident.with.a.motorcycle_1))
im_d_motorcycle0 <- raster(as.im(d_X_Accident.with.a.motorcycle_0))
im_d_motorcycle <- stack(im_d_motorcycle0,im_d_motorcycle1)
names(im_d_motorcycle) <- c("Not_Included","Included")
qts_motorcycle <- as.vector(sort(c(quantile(im_d_motorcycle0*1000,seq(0,1,len=40)),
                         quantile(im_d_motorcycle1*1000,seq(0,1,len=20)))))
spplot(im_d_motorcycle*1000,at=qts_motorcycle,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

####################################################################
####################################################################
################Image_car_bike_pedestrian_motorcycle################
####################################################################
####################################################################

im_d_bcmp <- stack(im_d_bike1,im_d_car1,im_d_pedestrian1,im_d_motorcycle1,
              im_d_bike0,im_d_car0,im_d_pedestrian0,im_d_motorcycle0)
names(im_d_bcmp) <- c("Bike","Car","Pedestrian","Motorcycle",
                 "No_Bike","No_Car","No_Pedestrian","No_Motorcycle")
qtss_bcpm <- as.vector(sort(c(quantile(im_d_motorcycle0*1000,seq(0,1,len=20)),
                         quantile(im_d_motorcycle1*1000,seq(0,1,len=20)),
                         quantile(im_d_bike0*1000,seq(0,1,len=20)),
                         quantile(im_d_bike1*1000,seq(0,1,len=20)),
                         quantile(im_d_pedestrian1*1000,seq(0,1,len=20)),
                         quantile(im_d_pedestrian0*1000,seq(0,1,len=20)),
                         quantile(im_d_car1*1000,seq(0,1,len=90)),
                         quantile(im_d_car0*1000,seq(0,1,len=20)))))

spplot(im_d_bcmp*1000,at=qtss_bcpm,col.regions=c(viridis(230),rep("#f03b20",5)),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      

png("im_d_marks.png",height = 400,width = 800)
spplot(im_d_bcmp*1000,at=qtss_bcpm,col.regions=c(viridis(230),rep("#f03b20",5)),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))    
dev.off()


####################################################################
####################################################################
################################Week################################
####################################################################
####################################################################

X$data$Weekday
X_week <- X
marks(X_week) <- as.numeric(X$data$Weekday)
X_week$data$marks

X_week_1 <- X_week[X_week$data$marks==1]
X_week_2 <- X_week[X_week$data$marks==2]
X_week_3 <- X_week[X_week$data$marks==3]
X_week_4 <- X_week[X_week$data$marks==4]
X_week_5 <- X_week[X_week$data$marks==5]
X_week_6 <- X_week[X_week$data$marks==6]
X_week_7 <- X_week[X_week$data$marks==7]


plot(unmark(X_week_1),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_week_2),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_week_3),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_week_4),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_week_5),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_week_6),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_week_7),pch=20,cols=2,col=4,main="",cex=2.5)

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_week_1 <- densityQuick.lpp(unmark(X_week_1), sigma = bw.scott.iso(unmark(X_week_1)),
                               positive = TRUE,dimyx=256)
d_X_week_2 <- densityQuick.lpp(unmark(X_week_2), sigma = bw.scott.iso(unmark(X_week_2)),
                               positive = TRUE,dimyx=256)
d_X_week_3 <- densityQuick.lpp(unmark(X_week_3), sigma = bw.scott.iso(unmark(X_week_3)),
                               positive = TRUE,dimyx=256)
d_X_week_4 <- densityQuick.lpp(unmark(X_week_4), sigma = bw.scott.iso(unmark(X_week_4)),
                               positive = TRUE,dimyx=256)
d_X_week_5 <- densityQuick.lpp(unmark(X_week_5), sigma = bw.scott.iso(unmark(X_week_5)),
                               positive = TRUE,dimyx=256)
d_X_week_6 <- densityQuick.lpp(unmark(X_week_6), sigma = bw.scott.iso(unmark(X_week_6)),
                               positive = TRUE,dimyx=256)
d_X_week_7 <- densityQuick.lpp(unmark(X_week_7), sigma = bw.scott.iso(unmark(X_week_7)),
                               positive = TRUE,dimyx=256)

################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_week_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_week_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_week_2.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_week_2,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_week_3.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_week_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_week_4.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_week_4,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_week_5.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_week_5,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_week_6.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_week_6,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_week_7.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_week_7,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

################################################
################################################
####################Image#######################
################################################
################################################

im_d_week_1 <- raster(as.im(d_X_week_1))
im_d_week_2 <- raster(as.im(d_X_week_2))
im_d_week_3 <- raster(as.im(d_X_week_3))
im_d_week_4 <- raster(as.im(d_X_week_4))
im_d_week_5 <- raster(as.im(d_X_week_5))
im_d_week_6 <- raster(as.im(d_X_week_6))
im_d_week_7 <- raster(as.im(d_X_week_7))

im_d_week <- stack(im_d_week_1,im_d_week_2,im_d_week_3,im_d_week_4,im_d_week_5,
                   im_d_week_6,im_d_week_7)
names(im_d_week) <- c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.')
qtss_week <- as.vector(sort(c(quantile(im_d_week_1*1000,seq(0,1,len=20)),
                              quantile(im_d_week_2*1000,seq(0,1,len=20)),
                              quantile(im_d_week_3*1000,seq(0,1,len=20)),
                              quantile(im_d_week_4*1000,seq(0,1,len=20)),
                              quantile(im_d_week_5*1000,seq(0,1,len=20)),
                              quantile(im_d_week_6*1000,seq(0,1,len=20)),
                              quantile(im_d_week_7*1000,seq(0,1,len=20)))))


png("im_d_week.png",height = 400,width = 800)
spplot(im_d_week*1000,at=qtss_week,col.regions=c(viridis(140),rep("#f03b20",4)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
dev.off()


####################################################################
####################################################################
################################month################################
####################################################################
####################################################################
X$data$Month
X_month <- X
marks(X_month) <- as.numeric(X$data$Month)
X_month$data$marks

X_month_1 <- X_month[X_month$data$marks==1]
X_month_2 <- X_month[X_month$data$marks==2]
X_month_3 <- X_month[X_month$data$marks==3]
X_month_4 <- X_month[X_month$data$marks==4]
X_month_5 <- X_month[X_month$data$marks==5]
X_month_6 <- X_month[X_month$data$marks==6]
X_month_7 <- X_month[X_month$data$marks==7]
X_month_8 <- X_month[X_month$data$marks==8]
X_month_9 <- X_month[X_month$data$marks==9]
X_month_10 <- X_month[X_month$data$marks==10]
X_month_11 <- X_month[X_month$data$marks==11]
X_month_12 <- X_month[X_month$data$marks==12]

plot(unmark(X_month_1),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_2),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_3),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_4),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_5),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_6),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_7),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_8),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_9),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_10),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_11),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_month_12),pch=20,cols=2,col=4,main="",cex=2.5)

################################################
################################################
####################Kernel######################
################################################
################################################

d_X_month_1 <- densityQuick.lpp(unmark(X_month_1), sigma = bw.scott.iso(unmark(X_month_1)),
                                positive = TRUE,dimyx=256)
d_X_month_2 <- densityQuick.lpp(unmark(X_month_2), sigma = bw.scott.iso(unmark(X_month_2)),
                                positive = TRUE,dimyx=256)
d_X_month_3 <- densityQuick.lpp(unmark(X_month_3), sigma = bw.scott.iso(unmark(X_month_3)),
                                positive = TRUE,dimyx=256)
d_X_month_4 <- densityQuick.lpp(unmark(X_month_4), sigma = bw.scott.iso(unmark(X_month_4)),
                                positive = TRUE,dimyx=256)
d_X_month_5 <- densityQuick.lpp(unmark(X_month_5), sigma = bw.scott.iso(unmark(X_month_5)),
                                positive = TRUE,dimyx=256)
d_X_month_6 <- densityQuick.lpp(unmark(X_month_6), sigma = bw.scott.iso(unmark(X_month_6)),
                                positive = TRUE,dimyx=256)
d_X_month_7 <- densityQuick.lpp(unmark(X_month_7), sigma = bw.scott.iso(unmark(X_month_7)),
                                positive = TRUE,dimyx=256)
d_X_month_8 <- densityQuick.lpp(unmark(X_month_8), sigma = bw.scott.iso(unmark(X_month_8)),
                                positive = TRUE,dimyx=256)
d_X_month_9 <- densityQuick.lpp(unmark(X_month_9), sigma = bw.scott.iso(unmark(X_month_9)),
                                positive = TRUE,dimyx=256)
d_X_month_10 <- densityQuick.lpp(unmark(X_month_10), sigma = bw.scott.iso(unmark(X_month_10)),
                                 positive = TRUE,dimyx=256)
d_X_month_11 <- densityQuick.lpp(unmark(X_month_11), sigma = bw.scott.iso(unmark(X_month_11)),
                                 positive = TRUE,dimyx=256)
d_X_month_12 <- densityQuick.lpp(unmark(X_month_12), sigma = bw.scott.iso(unmark(X_month_12)),
                                 positive = TRUE,dimyx=256)
################################################
################################################
####################Plot########################
################################################
################################################

png("d_X_month_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_2.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_2,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_3.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_4.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_4,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_5.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_5,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_6.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_6,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_7.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_7,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_8.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_9.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_4,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_10.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_5,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_11.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_6,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

png("d_X_month_12.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_month_7,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()
################################################
################################################
####################Image#######################
################################################
################################################

im_d_month_1 <- raster(as.im(d_X_month_1))
im_d_month_2 <- raster(as.im(d_X_month_2))
im_d_month_3 <- raster(as.im(d_X_month_3))
im_d_month_4 <- raster(as.im(d_X_month_4))
im_d_month_5 <- raster(as.im(d_X_month_5))
im_d_month_6 <- raster(as.im(d_X_month_6))
im_d_month_7 <- raster(as.im(d_X_month_7))
im_d_month_8 <- raster(as.im(d_X_month_8))
im_d_month_9 <- raster(as.im(d_X_month_9))
im_d_month_10 <- raster(as.im(d_X_month_10))
im_d_month_11 <- raster(as.im(d_X_month_11))
im_d_month_12 <- raster(as.im(d_X_month_12))

im_d_month <- stack(im_d_month_1,im_d_month_4,im_d_month_7,im_d_month_10,
                    im_d_month_2,im_d_month_5,im_d_month_8,im_d_month_11,
                    im_d_month_3,im_d_month_6,im_d_month_9,im_d_month_12)
names(im_d_month) <- c('Jan','Apr','Jul','Oct',
                       'Feb','May','Aug','NOV',
                       'Mar','Jun','Sep','Dec')
qtss_month <- as.vector(sort(c(quantile(im_d_month_1*1000,seq(0,1,len=20)),
                               quantile(im_d_month_2*1000,seq(0,1,len=20)),
                               quantile(im_d_month_3*1000,seq(0,1,len=20)),
                               quantile(im_d_month_4*1000,seq(0,1,len=20)),
                               quantile(im_d_month_5*1000,seq(0,1,len=20)),
                               quantile(im_d_month_6*1000,seq(0,1,len=20)),
                               quantile(im_d_month_7*1000,seq(0,1,len=20)),
                               quantile(im_d_month_8*1000,seq(0,1,len=20)),
                               quantile(im_d_month_9*1000,seq(0,1,len=20)),
                               quantile(im_d_month_10*1000,seq(0,1,len=20)),
                               quantile(im_d_month_11*1000,seq(0,1,len=20)),
                               quantile(im_d_month_12*1000,seq(0,1,len=20)))))


png("im_d_month.png",height = 500,width = 700)
spplot(im_d_month*1000,at=qtss_month,col.regions=c(viridis(240),rep("#f03b20",6)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)),layout=c(4,3))                      
dev.off()



# X_season_1 <- X_month[X_month$data$marks==1 | X_month$data$marks==2 | X_month$data$marks==3]
# X_season_2 <- X_month[X_month$data$marks==4 | X_month$data$marks==5 | X_month$data$marks==6]
# X_season_3 <- X_month[X_month$data$marks==7 | X_month$data$marks==8 | X_month$data$marks==9]
# X_season_4 <- X_month[X_month$data$marks==10 | X_month$data$marks==10 | X_month$data$marks==12]
# 
# plot(unmark(X_season_1),pch=20,cols=2,col=4,main="",cex=2.5)
# plot(unmark(X_season_2),pch=20,cols=2,col=4,main="",cex=2.5)
# plot(unmark(X_season_3),pch=20,cols=2,col=4,main="",cex=2.5)
# plot(unmark(X_season_4),pch=20,cols=2,col=4,main="",cex=2.5)
# 
# ################################################
# ################################################
# ####################Kernel######################
# ################################################
# ################################################
# 
# d_X_season_1 <- densityQuick.lpp(unmark(X_season_1), sigma = bw.scott.iso(unmark(X_season_1)),
#                                 positive = TRUE,dimyx=256)
# d_X_season_2 <- densityQuick.lpp(unmark(X_season_2), sigma = bw.scott.iso(unmark(X_season_2)),
#                                 positive = TRUE,dimyx=256)
# d_X_season_3 <- densityQuick.lpp(unmark(X_season_3), sigma = bw.scott.iso(unmark(X_season_3)),
#                                 positive = TRUE,dimyx=256)
# d_X_season_4 <- densityQuick.lpp(unmark(X_season_4), sigma = bw.scott.iso(unmark(X_season_4)),
#                                 positive = TRUE,dimyx=256)
# ################################################
# ################################################
# ####################Plot########################
# ################################################
# ################################################
# 
# png("d_X_season_1.png",height = 800,width = 800)
# par(mar=c(0,0,0,1))
# plot(d_X_season_1,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
# dev.off()
# 
# png("d_X_season_2.png",height = 800,width = 800)
# par(mar=c(0,0,0,1))
# plot(d_X_season_2,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
# dev.off()
# 
# png("d_X_season_3.png",height = 800,width = 800)
# par(mar=c(0,0,0,1))
# plot(d_X_season_3,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
# dev.off()
# 
# png("d_X_season_4.png",height = 800,width = 800)
# par(mar=c(0,0,0,1))
# plot(d_X_season_4,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
# dev.off()
# 
# ################################################
# ################################################
# ####################Image#######################
# ################################################
# ################################################
# 
# im_d_season_1 <- raster(as.im(d_X_season_1))
# im_d_season_2 <- raster(as.im(d_X_season_2))
# im_d_season_3 <- raster(as.im(d_X_season_3))
# im_d_season_4 <- raster(as.im(d_X_season_4))
# 
# im_d_season <- stack(im_d_season_1,im_d_season_2,im_d_season_3,im_d_season_4)
# names(im_d_season) <- c('Jan._March.','Apr_Jun.','Jul._Sep.','Oct._Dec.')
# qtss_season <- as.vector(sort(c(quantile(im_d_season_1*1000,seq(0,1,len=20)),
#                                quantile(im_d_season_2*1000,seq(0,1,len=20)),
#                                quantile(im_d_season_3*1000,seq(0,1,len=20)),
#                                quantile(im_d_season_4*1000,seq(0,1,len=20)))))
# 
# 
# png("im_d_season.png",height = 800,width = 800)
# spplot(im_d_season*1000,at=qtss_season,col.regions=c(viridis(240),rep("#f03b20",3)),
#        par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
# dev.off()








