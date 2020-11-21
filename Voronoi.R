################################################################################
################################################################################
##############################Intensity Estimation##############################
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

#################################################
#################################################
################################################# change of name
#################################################
#################################################

X <- HI_LN_AC_unique
X_unmark <- unmark(X)
npoints(X_unmark)


#################################################
#################################################
################################################# Intensity of Unmarked Data
#################################################
#################################################

### Voronoi
d_X_unmark_Voronoi <- densityVoronoi.lpp(X_unmark,f=0.01,nrep = 400,dimyx=256)
plot(d_X_unmark_Voronoi,style = "w",adjust = 3)
plot(d_X_unmark_Voronoi)

png("d_X_unmark_Vor_width_1.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_unmark_Voronoi*1000,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()
#################################################
#################################################
################################################# Intensity- count
#################################################
#################################################
X$data$N_Vehicle
X_count <- X
marks(X_count) <- as.numeric(X$data$N_Vehicle)
X_count$data$marks

### Kernel
d_X_count <- densityQuick.lpp(unmark(X_count), sigma = bw.scott.iso(unmark(X_count)),
                                              positive = TRUE,dimyx=256)
png("d_X_Accident.categories_3.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_count*1000,main="",ribwid=0.04,ribsep=0.02,cex.axis=2)
dev.off()

#################################################
#################################################
################################################# Intensity- Categories of Injury
#################################################
#################################################

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


### Voronoi
d_X_Accident.categories_2_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.categories_2),f=0.1,nrep = 400,dimyx=256)
d_X_Accident.categories_3_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.categories_3),f=0.1,nrep = 400,dimyx=256)

png("d_X_Accident.categories_2_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.categories_2_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

plot(d_X_Accident.categories_3_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))

png("d_X_Accident.categories_3_Voronoi.png",height = 800,width = 800)
par(mar=c(1,1,1,1))
plot(d_X_Accident.categories_3_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()


#################################################
#################################################
################################################# Intensity- Ligthing Conditions
#################################################
#################################################

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

#Voronoi

d_X_Lighting.conditions_0_Voronoi <- densityVoronoi.lpp(unmark(X_Lighting.conditions_0),f=0.1,nrep = 400,dimyx=256)
#d_X_Lighting.conditions_1_Voronoi <- densityVoronoi.lpp(unmark(X_Lighting.conditions_1),f=0.1,nrep = 400,dimyx=256)
#Error in rowSums(x) : 'x' must be an array of at least two dimensions
d_X_Lighting.conditions_2_Voronoi <- densityVoronoi.lpp(unmark(X_Lighting.conditions_2),f=0.1,nrep = 400,dimyx=256)

png("d_X_Lighting.conditions_0_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Lighting.conditions_0_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

png("d_X_Lighting.conditions_2_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Lighting.conditions_2_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

#################################################
#################################################
################################################# Intensity-Bike
#################################################
#################################################

table(X$data$Accident.with.a.bike)
X_Accident.with.a.bike <- X
marks(X_Accident.with.a.bike) <- as.numeric(X$data$Accident.with.a.bike)
X_Accident.with.a.bike$data$marks

X_Accident.with.a.bike_0 <- X_Accident.with.a.bike[X_Accident.with.a.bike$data$marks==0]
X_Accident.with.a.bike_1 <- X_Accident.with.a.bike[X_Accident.with.a.bike$data$marks==1]

plot(unmark(X_Accident.with.a.bike_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.bike_1),pch=20,cols=2,col=4,main="",cex=2.5)

###Voronoi
d_X_Accident.with.a.bike_0_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.bike_0),f=0.1,nrep = 400,dimyx=256)
d_X_Accident.with.a.bike_1_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.bike_1),f=0.1,nrep = 400,dimyx=256)


png("d_X_Accident.with.a.bike_0_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.bike_0_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

png("d_X_Accident.with.a.bike_1_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.bike_1_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

#################################################
#################################################
################################################# Intensity-Car
#################################################
#################################################

table(X$data$Accident.with.a.car)
X_Accident.with.a.car <- X
marks(X_Accident.with.a.car) <- as.numeric(X$data$Accident.with.a.car)
X_Accident.with.a.car$data$marks

X_Accident.with.a.car_0 <- X_Accident.with.a.car[X_Accident.with.a.car$data$marks==0]
X_Accident.with.a.car_1 <- X_Accident.with.a.car[X_Accident.with.a.car$data$marks==1]

plot(unmark(X_Accident.with.a.car_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.car_1),pch=20,cols=2,col=4,main="",cex=2.5)

###Voronoi
d_X_Accident.with.a.car_0_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.car_0),f=0.1,nrep = 400,dimyx=256)
d_X_Accident.with.a.car_1_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.car_1),f=0.1,nrep = 400,dimyx=256)


png("d_X_Accident.with.a.car_0_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.car_0_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

png("d_X_Accident.with.a.car_1_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.car_1_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

#################################################
#################################################
################################################# Intensity-Pedestrian
#################################################
#################################################

table(X$data$Accident.with.a.pedestrian)
X_Accident.with.a.pedestrian <- X
marks(X_Accident.with.a.pedestrian) <- as.numeric(X$data$Accident.with.a.pedestrian)
X_Accident.with.a.pedestrian$data$marks

X_Accident.with.a.pedestrian_0 <- X_Accident.with.a.pedestrian[X_Accident.with.a.pedestrian$data$marks==0]
X_Accident.with.a.pedestrian_1 <- X_Accident.with.a.pedestrian[X_Accident.with.a.pedestrian$data$marks==1]

plot(unmark(X_Accident.with.a.pedestrian_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.pedestrian_1),pch=20,cols=2,col=4,main="",cex=2.5)

###Voronoi
d_X_Accident.with.a.pedestrian_0_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.pedestrian_0),f=0.1,nrep = 400,dimyx=256)
d_X_Accident.with.a.pedestrian_1_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.pedestrian_1),f=0.1,nrep = 400,dimyx=256)


png("d_X_Accident.with.a.pedestrian_0_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.pedestrian_0_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()


png("d_X_Accident.with.a.pedestrian_1_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.pedestrian_1_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

#################################################
#################################################
################################################# Intensity-Motorcycle
#################################################
#################################################

table(X$data$Accident.with.a.motorcycle)
X_Accident.with.a.motorcycle <- X
marks(X_Accident.with.a.motorcycle) <- as.numeric(X$data$Accident.with.a.motorcycle)
X_Accident.with.a.motorcycle$data$marks

X_Accident.with.a.motorcycle_0 <- X_Accident.with.a.motorcycle[X_Accident.with.a.motorcycle$data$marks==0]
X_Accident.with.a.motorcycle_1 <- X_Accident.with.a.motorcycle[X_Accident.with.a.motorcycle$data$marks==1]

plot(unmark(X_Accident.with.a.motorcycle_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Accident.with.a.motorcycle_1),pch=20,cols=2,col=4,main="",cex=2.5)

###Voronoi
d_X_Accident.with.a.motorcycle_0_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.motorcycle_0),f=0.1,nrep = 400,dimyx=256)
d_X_Accident.with.a.motorcycle_1_Voronoi <- densityVoronoi.lpp(unmark(X_Accident.with.a.motorcycle_1),f=0.1,nrep = 400,dimyx=256)


png("d_X_Accident.with.a.motorcycle_0_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.motorcycle_0_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

png("d_X_Accident.with.a.motorcycle_1_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Accident.with.a.motorcycle_1_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

#################################################
#################################################
################################################# Intensity-Road Conditions
#################################################
#################################################

table(X$data$Road.condition)
X_Road.condition <- X
marks(X_Road.condition) <- as.numeric(X$data$Road.condition)
X_Road.condition$data$marks

X_Road.condition_0 <- X_Road.condition[X_Road.condition$data$marks==0]
X_Road.condition_1 <- X_Road.condition[X_Road.condition$data$marks==1]

plot(unmark(X_Road.condition_0),pch=20,cols=2,col=4,main="",cex=2.5)
plot(unmark(X_Road.condition_1),pch=20,cols=2,col=4,main="",cex=2.5)


#Voronoi
d_X_Road.condition_0_Voronoi <- densityVoronoi.lpp(unmark(X_Road.condition_0),f=0.1,nrep = 400,dimyx=256)
d_X_Road.condition_1_Voronoi <- densityVoronoi.lpp(unmark(X_Road.condition_1),f=0.1,nrep = 400,dimyx=256)


png("d_X_Road.conditions_0_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Road.condition_0_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()

png("d_X_Road.conditions_1_Voronoi.png",height = 800,width = 800)
par(mar=c(0,0,0,1))
plot(d_X_Road.condition_1_Voronoi,style = "w", main="",
     adjust = 3,ribwid=0.04,ribsep=0.02,leg.args = list(cex=2))
dev.off()




