library(mlbench)
spirale<-mlbench.spirals(240,1.5,0.03)
data<-spirale$x
data
klasy<-as.numeric(spirale$classes)
klasy
colornames <- c("red","blue")
#plot(data,col=colornames[klasy],ask=TRUE)

#getData Start

#data <- read.csv2("C:/statystykamichalAdam/analiza_skupien/Dane_zp_2007.csv", header=TRUE, fileEncoding="latin1")
#data<- data[,c(2,3)]
#data
#getData Start


#standarisation Start
#standarisation end


#spectral start
windows()
# nc - number_of_clusters
min_nc=2
max_nc=10
res <- array(0,c(max_nc-min_nc+1,2))
res[,1] <- min_nc:max_nc
clusters <- NULL
for (nc in min_nc:max_nc)
{
  cl2 <- speccl(data,nc,distance="sEuclidean",sigma="automatic",sigma.interval="default",mod.sample=0.75,R=10,iterations=3)
  res[nc-min_nc+1,2] <- G1 <- index.G1(cl2$Ymatrix,cl2$clusters,centrotypes="centroids")
  clusters <- rbind(clusters, cl2$clusters)
}
print(paste("max G1 for",(min_nc:max_nc)[which.max(res[,2])],"clusters=",max(res[,2])))
print("clustering for max G1")
print(clusters[which.max(res[,2]),])
#write.table(res,file="G1_res.csv",sep=";",dec=",",row.names=TRUE,col.names=FALSE)
plot(res, type="p", pch=0, xlab="Number of clusters", ylab="G1", xaxt="n")
axis(1, c(min_nc:max_nc))
print(cl2$sigma)
#cRand<-comparing.Partitions(as.vector(clusters[which.max(res[,2]),]),as.vector(klasy),type="crand")
#print(cRand)

kl=clusters[which.max(res[,2]),]
plot(data, col=rainbow(2)[kl])
#spectral end