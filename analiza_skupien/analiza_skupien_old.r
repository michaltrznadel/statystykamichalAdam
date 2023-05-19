

# Wczytanie zbioru danych
koszty_eksploatacji_mieszkan <-read.csv2("C:/statystykamichalAdam/analiza_skupien/koszty_eksploatacji_mieszkan2020.csv", header=TRUE, fileEncoding="UTF-8")

mieszkania <-read.csv2("C:/statystykamichalAdam/analiza_skupien/Powierzchniamieszkalna2020.csv", header=TRUE, fileEncoding="UTF-8")
wojewodztwa <- mieszkania[,2]
ludnosc <-read.csv2("C:/statystykamichalAdam/analiza_skupien/LUDN_2020.csv", header=TRUE, fileEncoding="UTF-8")
ludnosc
zaleglosciWOplatach <-read.csv2("C:/statystykamichalAdam/analiza_skupien/GOSP_zaleglosci_w_oplatach_2020.csv", header=TRUE, fileEncoding="UTF-8")
zaleglosciWOplatach
#xx <-read.csv2("C:/statystykamichalAdam/analiza_skupien/Dane_zp_2007.csv", header=TRUE, fileEncoding="latin1")
#x <- as.matrix(xx[, 2:ncol(xx)])
powierzchniaNaMieszkanca <- mieszkania[,3]/ludnosc[,3]/1000
koszty_eksploatacji_mieszkanNaM2 <- koszty_eksploatacji_mieszkan[,51]
zalgeloscWOplatachNamieszkancaWzgledemSpoldzielniMieskzniowychNaMIeszkanca <- zaleglosciWOplatach[,4]/ludnosc[,3]
x <- cbind(zalgeloscWOplatachNamieszkancaWzgledemSpoldzielniMieskzniowychNaMIeszkanca, powierzchniaNaMieszkanca, koszty_eksploatacji_mieszkanNaM2)
x
options(OutDec=",")


# Wyb?r formu?y normalizacji warto?ci zmiennych
z <- data.Normalization(x, type="n1")
z
# Wyb?r miary odleg?o?ci
z <- as.data.frame(z)
d <- dist.GDM(z, method="GDM1")
d


print("Ustalenie liczby klas z wykorzystaniem indeksu G3", quote=FALSE)
min_nc=2
max_nc=6
res <- array(0,c(max_nc-min_nc+1, 2))
res[,1] <- min_nc:max_nc
clusters <- NULL
for (nc in min_nc:max_nc)
{
hc <- hclust(d, method="centroid")
cl2 <- cutree(hc, k=nc)
res[nc-min_nc+1,2] <- G3 <- index.G3(d,cl2)
clusters <- rbind(clusters,cl2)
}
clopt<-clusters[which.min(res[,2]),]
print(paste("min G3 dla",(min_nc:max_nc)[which.min(res[,2])],"klas=",min(res[,2])))

print("Prezentacja klasyfikacji wynikowej", quote=FALSE)
cl_wyn1 <- data.frame(wojewodztwa, clopt)
colnames(cl_wyn1) <- c("wojew?dztwa", "klasa")
print(cl_wyn1)

print("Prezentacja klasyfikacji wynikowej - uporz?dkowana", quote=FALSE)
cl_wyn2 <- cl_wyn1[order(cl_wyn1[,"klasa"], decreasing=FALSE),]
cl_wyn2 <- data.frame(cl_wyn2)
print(cl_wyn2)

# Zapisanie do pliku G3_res.csv warto?ci indeksu G3
# write.table(res,file="G3_res.csv",sep=";",dec=",",row.names=TRUE,col.names=FALSE)

plot(res, type="p", pch=0, xlab="Liczba klas", ylab="G3", xaxt="n")
axis(1, c(min_nc:max_nc))

wyn<-clusters[which.min(res[,2]),]
desc <-cluster.Description(x, clopt, "population")
print("?rednie arytmetyczne", quote=FALSE)
print(desc[,,1])
print("Odchylenia standardowe", quote=FALSE)
print(desc[,,2])

windows()
plot(hc, hang=-1, labels=NULL, main=NULL, sub=NULL, ann=FALSE)
title(xlab="Numer klasy", ylab="Poziom połączenia klas")






library(clusterSim)
sbe<-shapes.bulls.eye(numObjects=c(120,60))
plot(sbe$data,col=rainbow(2)[sbe$clusters])

windows()
# nc - number_of_clusters
min_nc=2
max_nc=10
res <- array(0,c(max_nc-min_nc+1,2))
res[,1] <- min_nc:max_nc
clusters <- NULL
for (nc in min_nc:max_nc)
{
  cl2 <- speccl(sbe$data,nc,distance="sEuclidean",sigma="automatic",sigma.interval="default",mod.sample=0.75,R=10,iterations=3)
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
cRand<-comparing.Partitions(as.vector(clusters[which.max(res[,2]),]),as.vector(sbe$clusters),type="crand")
print(cRand)
windows()
kl=clusters[which.max(res[,2]),]
plot(sbe$data,col=rainbow(2)[kl])
