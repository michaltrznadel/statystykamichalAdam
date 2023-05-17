

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
data<- cbind(zalgeloscWOplatachNamieszkancaWzgledemSpoldzielniMieskzniowychNaMIeszkanca, powierzchniaNaMieszkanca, koszty_eksploatacji_mieszkanNaM2)

options(OutDec=",")


# Wyb?r formu?y normalizacji warto?ci zmiennych
z <- data.Normalization(x, type="n1")
z
# Wyb?r miary odleg?o?ci
z <- as.data.frame(z)
d <- dist.GDM(z, method="GDM1")
d



#spectral start
windows()
# nc - number_of_clusters
min_nc=3
max_nc=3
res <- array(0,c(max_nc-min_nc+1,3))
res[,1] <- min_nc:max_nc
clusters <- NULL
for (nc in min_nc:max_nc)
{
  cl2 <- speccl(data,nc,distance="sEuclidean",sigma="automatic",sigma.interval="default",mod.sample=0.75,R=10,iterations=3)
  res[nc-min_nc+1,3] <- G1 <- index.G1(cl2$Ymatrix,cl2$clusters,centrotypes="centroids")
  clusters <- rbind(clusters, cl2$clusters)
}
print(paste("max G1 for",(min_nc:max_nc)[which.max(res[,3])],"clusters=",max(res[,3])))
print("clustering for max G1")
print(clusters[which.max(res[,3]),])
#write.table(res,file="G1_res.csv",sep=";",dec=",",row.names=TRUE,col.names=FALSE)
plot(res, type="p", pch=0, xlab="Number of clusters", ylab="G1", xaxt="n")
axis(1, c(min_nc:max_nc))
print(cl2$sigma)
#cRand<-comparing.Partitions(as.vector(clusters[which.max(res[,2]),]),as.vector(klasy),type="crand")
#print(cRand)

kl=clusters[which.max(res[,3]),]
kl
#plot(data, col=rainbow(3)[kl])
plot(data[,2:3], col=rainbow(3)[kl])
#spectral end