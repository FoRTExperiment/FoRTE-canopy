require(ggplot2)

pts <- read.csv("./data/nsp_points.csv")

x11()
ggplot(data = pts, aes(x = easting, y = northing)) +
  geom_point(alpha = 1)+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()


library(sp)
library(rgdal)

#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

# Example
x<-c( -94.99729,-94.99726,-94.99457,-94.99458,-94.99729)
y<-c( 29.17112, 29.17107, 29.17273, 29.17278, 29.17112)
x <- LongLatToUTM(df$Longitude, df$Latitude, 16)

df <- cbind(df, x)

x11()
ggplot(data = df, aes(x = X, y = Y)) +
  geom_point(size = 2)+
  geom_point(data = pts, aes(x = easting, y = northing), colour = "red", size = 2)+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  theme_classic()




######
pts <- read.csv("./data/nps_coords.csv")

pts$unique_id <- paste(pts$plot_id, pts$subplot_id, pts$nsp_id, sep = "" )

x <- LongLatToUTM(pts$longitude, pts$latitude, 16)

pts <- cbind(pts, x)

for(i in nrow(pts)){
if(pts$subplot_id == "E" && pts$nsp_id == 0){
  pts$X = pts$X + 22
  print("yes?")
  } else if (pts$subplot_id == "E" && pts$nsp_id == 1){
  pts$X = pts$X + 22
  pts$Y = pts$Y + 10
} else if (pts$subplot_id == "E" && pts$nsp_id == 3){
  pts$X = pts$X + 32
  pts$Y = pts$Y
} else if (pts$subplot_id == "E" && pts$nsp_id == 5){
  pts$X = pts$X + 22
  pts$Y = pts$Y - 10
} else if (pts$subplot_id == "E" && pts$nsp_id == 7){
  pts$X = pts$X + 12
  pts$Y = pts$Y
  }
}


