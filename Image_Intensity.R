#################################################
#################################################
################################################# load packages
#################################################
#################################################
pkg = c("devtools","dplyr","plyr","lattice","tibble","rgdal","rgl","parallel",
        "tidyverse","tidyr","XML","sp","sf","spatstat","spatstat.utils",
        "stlnpp","sparr","stplanr","raster","ggplot2","ggthemes","viridis",
        "ggmap","leaflet","maptools","mapview","reshape","reshape2","plotly",
        "ggvis","gganimate","gridExtra")
install.packages(pkg)
easypackages::libraries(pkg)
#####################################
#####################################
##################################### Images_Intensities
#####################################
#####################################

#####################################Images_kernel all data
im_d_ker <- raster(as.im(d_X_unmark))
qts_ker <- as.vector(sort(c(quantile(im_d_ker*1000,seq(0,1,len=40)))))

png("im_d_ker.png",height = 400,width = 600)
spplot(im_d_ker*1000,at=qts_ker,col.regions=c(viridis(40),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))
dev.off()
#####################################Images_Voronoi all data

im_vor <- raster(as.im(d_X_unmark_Voronoi))
spplot(im_vor,at=c(seq(0,2,length.out=50),seq(2.001,20,length.out=50),summary(im_vor)[5]),col.regions=c(viridis(100),rep("#fc9272",50),rep("#f03b20",50)))

png("im_d_vor.png",height = 400,width = 600)
spplot(im_vor*1000,at=c(seq(0,2,length.out=50),seq(2.001,20,length.out=50),summary(im_vor*1000)[5]),col.regions=c(viridis(100),rep("#fc9272",50),rep("#f03b20",50)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
dev.off()


####################################Image_categories

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

####################################Image_class

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

#####################################Image_Type

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
        

#####################################Image_Light
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

#####################################Image_road
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


#####################################Image_bike
im_d_bike1 <- raster(as.im(d_X_Accident.with.a.bike_1))
im_d_bike0 <- raster(as.im(d_X_Accident.with.a.bike_0))
im_d_bike <- stack(im_d_bike0,im_d_bike1)
names(im_d_bike) <- c("Not_Included","Included")
qts_bike <- as.vector(sort(c(quantile(im_d_bike0*1000,seq(0,1,len=40)),
                         quantile(im_d_bike1*1000,seq(0,1,len=20)))))
spplot(im_d_bike*1000,at=qts_bike,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

#####################################Image_car
im_d_car1 <- raster(as.im(d_X_Accident.with.a.car_1))
im_d_car0 <- raster(as.im(d_X_Accident.with.a.car_0))
im_d_car <- stack(im_d_car0,im_d_car1)
names(im_d_car) <- c("Not_Included","Included")
qts_car <- as.vector(sort(c(quantile(im_d_car0*1000,seq(0,1,len=20)),
                         quantile(im_d_car1*1000,seq(0,1,len=40)))))
spplot(im_d_car*1000,at=qts_car,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

#####################################Image_pedestrian
im_d_pedestrian1 <- raster(as.im(d_X_Accident.with.a.pedestrian_1))
im_d_pedestrian0 <- raster(as.im(d_X_Accident.with.a.pedestrian_0))
im_d_pedestrian <- stack(im_d_pedestrian0,im_d_pedestrian1)
names(im_d_pedestrian) <- c("Not_Included","Included")
qts5 <- as.vector(sort(c(quantile(im_d_pedestrian0*1000,seq(0,1,len=40)),
                         quantile(im_d_pedestrian1*1000,seq(0,1,len=20)))))
p3 <- spplot(im_d_pedestrian*1000,at=qts5,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

#####################################Image_motorcycle
im_d_motorcycle1 <- raster(as.im(d_X_Accident.with.a.motorcycle_1))
im_d_motorcycle0 <- raster(as.im(d_X_Accident.with.a.motorcycle_0))
im_d_motorcycle <- stack(im_d_motorcycle0,im_d_motorcycle1)
names(im_d_motorcycle) <- c("Not_Included","Included")
qts_motorcycle <- as.vector(sort(c(quantile(im_d_motorcycle0*1000,seq(0,1,len=40)),
                         quantile(im_d_motorcycle1*1000,seq(0,1,len=20)))))
spplot(im_d_motorcycle*1000,at=qts_motorcycle,col.regions=c(viridis(60),"#f03b20"),
       par.settings=list(fontsize=list(text=16)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))

#####################################Image_car_bike_pedestrian_motorcycle
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


#########################leaflet
merc = CRS("+init=epsg:3857")
WGS84 = CRS("+init=epsg:4326")
meuse.ll = st_transform(HI_AC_unique, WGS84)
bggMap = get_map(as.vector(st_bbox(meuse.ll)), source = "osm", zoom = 13,maptype="toner-lines")
par(mar = rep(0,4))
plot(st_transform(meuse.ll[,"Count"], merc), bgMap = bggMap, pch = 16, cex = .5)

bggMap = get_map(as.vector(st_bbox(meuse.ll)), source = "osm", zoom = 13,maptype="satellite")







