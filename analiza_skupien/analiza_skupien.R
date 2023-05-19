library(clusterSim)

# Wczytanie zbioru danych
koszty_eksploatacji_mieszkan <-read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8')
Powierzchniamieszkalna <-read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8')
wojewodztwa <- Powierzchniamieszkalna[,2]

ludnosc <-read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8')
zaleglosciWOplatach <-read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8')

powierzchniaNaMieszkanca <- Powierzchniamieszkalna[,3]/ludnosc[,3]/1000
koszty_eksploatacji_mieszkanNaM2 <- koszty_eksploatacji_mieszkan[,51]
zalgeloscWOplatachNamieszkancaWzgledemSpoldzielniMieskzniowychNaMIeszkanca <- zaleglosciWOplatach[,4]/ludnosc[,3]

x<- cbind(zalgeloscWOplatachNamieszkancaWzgledemSpoldzielniMieskzniowychNaMIeszkanca, powierzchniaNaMieszkanca, koszty_eksploatacji_mieszkanNaM2)

options(OutDec=",")

# Wyb?r formu?y normalizacji warto?ci zmiennych
z <- data.Normalization(x, type="n1")

#spectral start
# nc - number_of_clusters
min_nc=2
max_nc=12
res <- array(0,c(max_nc-min_nc+1,3))
res[,1] <- min_nc:max_nc
clusters <- NULL
for (nc in min_nc:max_nc)
{
  cl2 <- speccl(z,nc,distance="sEuclidean",sigma="automatic",sigma.interval="default",mod.sample=0.75,R=10,iterations=3)
  res[nc-min_nc+1,3] <- G1 <- index.G1(cl2$Ymatrix,cl2$clusters,centrotypes="centroids")
  clusters <- rbind(clusters, cl2$clusters)
}
plot(res, type="p", pch=0, xlab="Number of clusters", ylab="G1", xaxt="n")
axis(1, c(min_nc:max_nc))
kl=clusters[which.max(res[,3]),]
cbind(wojewodztwa,kl)
plot(x[,1:2], col=rainbow(5)[kl])
windows()
plot(x[,2:3], col=rainbow(5)[kl])
#spectral end

