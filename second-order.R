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

load("D:/Github/Honnover-Analysis/HI_AC_LN_ppp/HI_LN_AC_unique.RData")
HI_LN_AC_unique
#################################################
#################################################
################################################# change of name
#################################################
#################################################

X <- HI_LN_AC_unique
X_unmark <- unmark(X)
X_com <- connected(X_unmark,what="component")
X_com_main <- X_com[[1]]
npoints(X_unmark)

#################################################
#################################################
################################################# K-function
#################################################
#################################################

d_X_unmark <- densityQuick.lpp(X_com_main,sigma = bw.scott.iso(X_com_main),positive = TRUE,at="points")
K_X_unmark <- linearKinhom(X_com_main,lambda = as.vector(d_X_unmark),normalise = TRUE,normpower = 2)
# plot(K_X_unmark,lwd=2)

d_X_unmark_linim <- densityQuick.lpp(X_com_main,sigma = bw.scott.iso(X_com_main),dimyx=512,positive = TRUE)
set.seed(1234)
Ysim <- rpoislpp(d_X_unmark_linim,L=domain(X_com_main),nsim=199) # this should be calculated based on 99
R <- mclapply(X=1:length(Ysim), function(i){
  
  if(i<length(Ysim)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  }
  
  d <- densityQuick.lpp(Ysim[[i]],sigma = bw.scott.iso(Ysim[[i]]),positive = TRUE,at="points",leaveoneout = TRUE)
  K <- linearKinhom(Ysim[[i]],lambda = as.vector(d),r=K_X_unmark$r,normalise = TRUE,normpower = 2)$est
  return(K)
  
},mc.cores = detectCores()-2)

save.image("Kreza.RData")

Rmat <- do.call(cbind,R)
so <- lapply(X=1:513, function(i){
  # a <- sort(Rmat[i,])[195]
  # b <- sort(Rmat[i,])[5]
  a=quantile(Rmat[i,],0.975)
  b=quantile(Rmat[i,],0.025)
  return(list(a,b))
})

so <- do.call(rbind,so)
R_up <- unlist(so[,1])# we would use %95 percent intervals at the end
R_low <- unlist(so[,2])

plot(K_X_unmark$r,K_X_unmark$est,type="n",ylim = c(0,max(R_up)),xlab = "r",ylab = "K") #,xlim=c(50,150),ylim=c(0.045,0.05))
lines(K_X_unmark$r,R_up,col="white")
lines(K_X_unmark$r,R_low,col="white")
polygon(c(K_X_unmark$r, rev(K_X_unmark$r)), c(R_up, rev(R_low)),col = "grey90", border = NA)
points(K_X_unmark$r,K_X_unmark$est,type="l",col="black",lwd=2) #,xlim=c(50,150),ylim=c(0.045,0.05))
lines(K_X_unmark$r,K_X_unmark$theo,col="red",lwd=2,lty=2)

png("Second_Order.png",height = 600,width = 600)
gg <- ggplot()+
  ylim(c(0,max(R_up)))+
  geom_polygon(aes(x=c(K_X_unmark$r/1000, rev(K_X_unmark$r/1000)),y=c(R_up,rev(R_low))),fill="grey54")+
  geom_point(aes(x=K_X_unmark$r/1000,y=K_X_unmark$est),size=2)+
  geom_line(aes(x=K_X_unmark$r/1000,y=K_X_unmark$theo),colour = "red",,lwd=0.5,lty=2)+
  labs(x="Distance",y=expression(K[L-Inhom](r)))+
  theme(text = element_text(size = 20))
dev.off()