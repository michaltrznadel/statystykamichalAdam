library(clusterSim)
set.seed(123)           # Ustawienie generatora liczb losowych

# Wczytanie zbioru danych
xx <-read.csv2("C:/statystykamichalAdam/analiza_skupien/Dane_zp_2007.csv", header=TRUE, fileEncoding="latin1")
x <- as.matrix(xx[, 2:ncol(xx)])
options(OutDec=",")
x
# Wyb?r formu?y normalizacji warto?ci zmiennych
z <- data.Normalization(x, type="n1")
z
# Wyb?r miary odleg?o?ci
z <- as.data.frame(z)
d <- dist.GDM(z, method="GDM1")

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
cl_wyn1 <- data.frame(xx[, 1], clopt)
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
title(xlab="Numer klasy", ylab="Poziom po??czenia klas")