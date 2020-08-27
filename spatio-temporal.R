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
#################################################
#################################################
################################################# load data
#################################################
#################################################

#load("D:/GEO-C/Github/Honnover-Analysis/HI_AC_LN_ppp/HI_LN_AC_unique.RData")
load("D:/Github/Honnover-Analysis/HI_AC_LN_ppp/HI_LN_AC_unique.RData") ###  main

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
################################################# spatio-temporal by hour
#################################################
#################################################

hour <- as.numeric(X$data$Time) # extract time

X_hour <- as.stlpp(X_unmark,t=hour) # create a space-time pattern

d_X_hour <- density(X_hour,dimyx=256)

plot(attr(d_X_hour,"tempden")) # density plot

tgrid <- attr(d_X_hour,"tempden")$x

h1 <- which(tgrid<3)
h2 <- which(tgrid>=3 & tgrid<6)
h3 <- which(tgrid>=6 & tgrid<9)
h4 <- which(tgrid>=9 & tgrid<12)
h5 <- which(tgrid>=12 & tgrid<15)
h6 <- which(tgrid>=15 & tgrid<18)
h7 <- which(tgrid>=18 & tgrid<21)
h8 <- which(tgrid>=21 )

out_h_1 <- d_X_hour[[1]] 
for (j in h1) { 
  out_h_1 <- out_h_1+d_X_hour[[j]]
}
out_h_1 <- out_h_1 -  d_X_hour[[1]]
plot(out_h_1)  

out_h_2 <- d_X_hour[[1]] 
for (j in h2) { 
  out_h_2 <- out_h_2+d_X_hour[[j]]
}
out_h_2 <- out_h_2 -  d_X_hour[[1]]
plot(out_h_2)  

out_h_3 <- d_X_hour[[1]] 
for (j in h3) { 
  out_h_3 <- out_h_3+d_X_hour[[j]]
}
out_h_3 <- out_h_3 -  d_X_hour[[1]]
plot(out_h_3)  

out_h_4 <- d_X_hour[[1]] 
for (j in h4) { 
  out_h_4 <- out_h_4+d_X_hour[[j]]
}
out_h_4 <- out_h_4 -  d_X_hour[[1]]
plot(out_h_4)  

out_h_5 <- d_X_hour[[1]] 
for (j in h5) { 
  out_h_5 <- out_h_5+d_X_hour[[j]]
}
out_h_5 <- out_h_5 -  d_X_hour[[1]]
plot(out_h_5)  

out_h_6 <- d_X_hour[[1]] 
for (j in h6) { 
  out_h_6 <- out_h_6+d_X_hour[[j]]
}
out_h_6 <- out_h_6 -  d_X_hour[[1]]
plot(out_h_6)  

out_h_7 <- d_X_hour[[1]] 
for (j in h7) { 
  out_h_7 <- out_h_7+d_X_hour[[j]]
}
out_h_7 <- out_h_7 -  d_X_hour[[1]]
plot(out_h_7)  

out_h_8 <- d_X_hour[[1]] 
for (j in h8) { 
  out_h_8 <- out_h_8+d_X_hour[[j]]
}
out_h_8 <- out_h_8 -  d_X_hour[[1]]
plot(out_h_8) 

im_d_h_1 <- raster(as.im(out_h_1))
im_d_h_2 <- raster(as.im(out_h_2))
im_d_h_3 <- raster(as.im(out_h_3))
im_d_h_4 <- raster(as.im(out_h_4))
im_d_h_5 <- raster(as.im(out_h_5))
im_d_h_6 <- raster(as.im(out_h_6))
im_d_h_7 <- raster(as.im(out_h_7))
im_d_h_8 <- raster(as.im(out_h_8))
out_h_stack <- stack(im_d_h_1,im_d_h_2,im_d_h_3,im_d_h_4,im_d_h_5,im_d_h_6,im_d_h_7,im_d_h_8)
names(out_h_stack)=c("Midnight_h3am","h3am_h6am)","h6am_h9am","h9am_Noon",
                     "Noon_h15pm","h15pm_h18pm","h18pm_h21pm","h21pm_h24pm")

qtss_h <- as.vector(sort(c(quantile(im_d_h_1*1000,seq(0,1,len=20)),
                               quantile(im_d_h_2*1000,seq(0,1,len=20)),
                               quantile(im_d_h_3*1000,seq(0,1,len=20)),
                               quantile(im_d_h_4*1000,seq(0,1,len=20)),
                               quantile(im_d_h_5*1000,seq(0,1,len=20)),
                               quantile(im_d_h_6*1000,seq(0,1,len=20)),
                               quantile(im_d_h_7*1000,seq(0,1,len=20)),
                               quantile(im_d_h_8*1000,seq(0,1,len=20)))))

png("im_d_h.png",height = 500,width = 800)
spplot(out_h_stack*1000,at=qtss_h,col.regions=c(viridis(160),rep("#f03b20",2)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
dev.off()

#################################################
#################################################
################################################# spatio-temporal by week
#################################################
#################################################
week <- as.numeric(X$data$Weekday)
X_week <- as.stlpp(X_unmark,t=week)

as.tpp.stlpp(X_week)
plot(.Last.value)
plot(X_week)

d_X_week <- density(X_week,dimyx=256)

plot(attr(d_X_week,"tempden")) # density plot

wgrid <- attr(d_X_week,"tempden")$x
w1 <- which(wgrid <1.5)
w2 <- which(wgrid>=1.5 & wgrid<2.5)
w3 <- which(wgrid>=2.5 & wgrid<3.5)
w4 <- which(wgrid>=3.5 & wgrid<4.5)
w5 <- which(wgrid>=4.5 & wgrid<5.5)
w6 <- which(wgrid>=5.5 & wgrid<6.5)
w7 <- which(wgrid>=6.5)

out_w_1 <- d_X_week[[1]] 
for (j in w1) { 
  out_w_1 <- out_w_1+d_X_week[[j]]
}
out_w_1 <- out_w_1 -  d_X_week[[1]]
plot(out_w_1)

out_w_2 <- d_X_week[[2]] 
for (j in w2) { 
  out_w_2 <- out_w_2+d_X_week[[j]]
}
out_w_2 <- out_w_2 -  d_X_week[[2]]
plot(out_w_2)

out_w_3 <- d_X_week[[3]] 
for (j in w3) { 
  out_w_3 <- out_w_3+d_X_week[[j]]
}
out_w_3 <- out_w_3 -  d_X_week[[3]]
plot(out_w_3)

out_w_4 <- d_X_week[[4]] 
for (j in w4) { 
  out_w_4 <- out_w_4+d_X_week[[j]]
}
out_w_4 <- out_w_4 -  d_X_week[[4]]
plot(out_w_4)

out_w_5 <- d_X_week[[5]] 
for (j in w5) { 
  out_w_5 <- out_w_5+d_X_week[[j]]
}
out_w_5 <- out_w_5 -  d_X_week[[5]]
plot(out_w_5)

out_w_6 <- d_X_week[[6]] 
for (j in w6) { 
  out_w_6 <- out_w_6+d_X_week[[j]]
}
out_w_6 <- out_w_6 -  d_X_week[[6]]
plot(out_w_6)

out_w_7 <- d_X_week[[7]] 
for (j in w7) { 
  out_w_7 <- out_w_7+d_X_week[[j]]
}
out_w_7 <- out_w_7 -  d_X_week[[7]]
plot(out_w_7)

im_d_w_1 <- raster(as.im(out_w_1))
im_d_w_2 <- raster(as.im(out_w_2))
im_d_w_3 <- raster(as.im(out_w_3))
im_d_w_4 <- raster(as.im(out_w_4))
im_d_w_5 <- raster(as.im(out_w_5))
im_d_w_6 <- raster(as.im(out_w_6))
im_d_w_7 <- raster(as.im(out_w_7))

out_w_stack <- stack(im_d_w_1,im_d_w_2,im_d_w_3,im_d_w_4,im_d_w_5,im_d_w_6,im_d_w_7)
names(out_w_stack)=c('Sun.','Mon.','Tue.','Wed.','Thu.','Fri.','Sat.')

qtss_w <- as.vector(sort(c(quantile(im_d_w_1*1000,seq(0,1,len=20)),
                           quantile(im_d_w_2*1000,seq(0,1,len=20)),
                           quantile(im_d_w_3*1000,seq(0,1,len=20)),
                           quantile(im_d_w_4*1000,seq(0,1,len=20)),
                           quantile(im_d_w_5*1000,seq(0,1,len=20)),
                           quantile(im_d_w_6*1000,seq(0,1,len=20)),
                           quantile(im_d_w_7*1000,seq(0,1,len=20)))))

png("im_d_w.png",height = 500,width = 800)
spplot(out_w_stack*1000,at=qtss_w,col.regions=c(viridis(140),rep("#f03b20",6)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
dev.off()

#################################################
#################################################
################################################# spatio-temporal by season
#################################################
#################################################

month <- as.numeric(X$data$Month)
X_month <- as.stlpp(X_unmark,t=month)
d_X_month <- density(X_month,dimyx=256)

plot(attr(d_X_month,"tempden")) # density plot

mgrid <- attr(d_X_month,"tempden")$x
m1 <- which(mgrid<3.5)
m2 <- which(mgrid>=3.5 & mgrid<6.5)
m3 <- which(mgrid>=6.5 & mgrid<9.5)
m4 <- which(mgrid>=9.5 )


out_m_1 <- d_X_month[[1]] 
for (j in m1) { 
  out_m_1 <- out_m_1+d_X_month[[j]]
}
out_m_1 <- out_m_1 -  d_X_month[[1]]
plot(out_m_1)

out_m_2 <- d_X_month[[2]] 
for (j in m2) { 
  out_m_2 <- out_m_2+d_X_month[[j]]
}
out_m_2 <- out_m_2 -  d_X_month[[2]]
plot(out_m_2)

out_m_3 <- d_X_month[[3]] 
for (j in m3) { 
  out_m_3 <- out_m_3+d_X_month[[j]]
}
out_m_3 <- out_m_3 -  d_X_month[[3]]
plot(out_m_3)

out_m_4 <- d_X_month[[4]] 
for (j in m4) { 
  out_m_4 <- out_m_4+d_X_month[[j]]
}
out_m_4 <- out_m_4 -  d_X_month[[4]]
plot(out_m_4)

im_d_m_1 <- raster(as.im(out_m_1))
im_d_m_2 <- raster(as.im(out_m_2))
im_d_m_3 <- raster(as.im(out_m_3))
im_d_m_4 <- raster(as.im(out_m_4))

out_m_stack <- stack(im_d_m_1,im_d_m_2,im_d_m_3,im_d_m_4)
names(out_m_stack)<- c("Jan_March","May_Jun","Jul_Sep","Oct_Dec")
qtss_m <- as.vector(sort(c(quantile(im_d_m_1*1000,seq(0,1,len=20)),
                           quantile(im_d_m_2*1000,seq(0,1,len=20)),
                           quantile(im_d_m_3*1000,seq(0,1,len=20)),
                           quantile(im_d_m_4*1000,seq(0,1,len=20)))))


png("im_d_m.png",height = 800,width = 800)
spplot(out_m_stack*1000,at=qtss_m,col.regions=c(viridis(80),rep("#f03b20",5)),
       par.settings=list(fontsize=list(text=18)),colorkey=list(lables=list(cex=1.5,cex.main=1.5)))                      
dev.off()



################################################
################################################
################################################ 1d patterns
################################################
################################################

hp <- tpp(hour)
d_hp <- density(hp,at="pixels")
plot(d_hp,xlim=c(0,23))


dp <- tpp(as.numeric(X$data$Weekday))
d_dp <- density(dp,at="pixels")
plot(d_dp,xlim=c(1,7))
 
mp <- tpp(as.numeric(X$data$Month))
d_mp <- density(mp,at="pixels")
plot(d_mp,xlim=c(1,12))



#################################################
#################################################
################################################# K-function
#################################################
#################################################
hour <- as.numeric(X$data$Time) # extract time

X_hour <- as.stlpp(X_unmark,t=hour) # create a space-time pattern

com <- connected(X_unmark)
X_com_main <- unmark(com[[1]])

X_sup <- as.data.frame(X_com_main$data[,1:2])
x_old <- as.data.frame(X_unmark$data[,1:2])

library(plyr)
id <- as.numeric(rownames(match_df(x_old,X_sup)))

X_hour_new <- stlpp(X_com_main,T=X_hour$data$t[id])

d_X_hour_new <- density(X_hour_new,at="points")

K <- STLKinhom(X_hour_new,lambda = as.numeric(d_X_hour_new),
               normalize = TRUE)
# library(plot3D)
# persp3D(K$r,K$t,K$Kinhom)

d_X_hour_new_linim <- density(X_hour_new)
set.seed(1234)
Ysim <- rpoistlpp(lambda = d_X_hour_new_linim,nsim=99)

R <- mclapply(X=1:length(Ysim), function(i){
  
  if(i<length(Ysim)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  }
  
  d <- density(Ysim[[i]],positive = TRUE,at="points")
  K <- STLKinhom(Ysim[[i]],lambda = as.numeric(d),r=K$r,t=K$t,normalize = TRUE)$Kinhom
  return(K)
  
},mc.cores = detectCores()-2)

save.image("Kresa_st.RData")

Rmat <- array(as.numeric(unlist(R)), dim=c(10, 10, 99))
R_low <- apply(Rmat, 1:2,function(i){quantile(i,0.025)})
R_up <- apply(Rmat, 1:2,function(i){quantile(i,0.975)})
minEnv <- R_low
maxEnv <- R_up

################################ plot
library(plot3D)
################################# Black&white
png("Second_Order_st.png",height = 800,width = 800)
plot3D::persp3D(x= K$r/1000,y= K$t,z= K$Kinhom/1000,theta=40,phi=10,
        facets=FALSE,col="red",ticktype= "detailed",bty = "g",
        xlab="r = distance",ylab="t = time",zlab="",
        main=expression(italic({hat(K)[LI]^{ST}}(r,t))),
        nticks=6,cex.axis=1.5,cex.lab=2, cex.main=2,lwd=2,
        zlim= range(c(min(minEnv,K$Kinhom),max(maxEnv,K$Kinhom)))/1000,
        resfac=4)

plot3D::persp3D(K$r/1000,K$t,minEnv/1000,col = "gray80",add=T,facets=T,
                ticktype= "detailed",
                nticks=6,cex.axis=1.3,cex.lab=1.7,resfac=4,border = 1)

plot3D::persp3D(K$r/1000,K$t,maxEnv/1000,col = "gray80",add=T,facets=T,
                ticktype= "detailed",
                nticks=6,cex.axis=1.3,cex.lab=1.7,resfac=4,border = 1)

dev.off()

########## For Presentation <- Annimation

plotrgl()

################################### P_Value for null hypothesis :=Clustering

Envkinvec <- lapply(1:length(R), function(i){
  as.vector(R[[i]])
})
Envkinmat <- do.call(rbind,Envkinvec)

minEnv <- apply(Envkinmat,2,quantile,prob=0.025)
maxEnv <- apply(Envkinmat,2,quantile,prob=0.975)

minEnv <- matrix(minEnv,byrow = F,nrow = 10)
maxEnv  <- matrix(maxEnv ,byrow = F,nrow = 10)

meansurf <- apply(Envkinmat,2,mean)
sdsurf <- apply(Envkinmat,2,sd)

val <- t((t(Envkinmat)-meansurf)/sdsurf)
valint <- rowSums(val)

valK <- t((t(as.vector(K$Kinhom))-meansurf)/sdsurf)
valkint <- sum(valK)

(sum(valint>valkint)+1)/(length(valint)+1)

# [1] 0.69
