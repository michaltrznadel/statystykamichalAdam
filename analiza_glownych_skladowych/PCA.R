#PCA

#Poszukuję zależności między stopą bezrobocia, ilością nowych miejsc pracy, liczbą przedszkoli,ilością absolwentów

#kolejność danych w tabeli 

# 2010;	2011;	2012;	2013;	2014;	2015;
# Stopa bezrobocia wysokość stopy bezrobocia 2010-2015
# Nowe miejsca pracy liczba nowoutworzonych miejsc pracy 2010-2015
# Przedszkola liczba przedszkoli 2010-2015
# Absolwenci liczba absolwentów szkół wyższych 2010-2015

dane_csv <- read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8')
dane <- t(dane_csv)
colnames(dane) <- c("Stopa bezrobocia", "Nowe miejsca pracy", "Przedszkola", "Absolwenci")
rownames(dane) <- c("2010", "2011", "2012", "2013", "2014", "2015")

dane <- head(dane)
result_dane=princomp(dane)
summary(result_dane)
plot(result_dane)
biplot(result_dane)

# > summary(result_dane)
# Importance of components:
#   Comp.1       Comp.2       Comp.3       Comp.4
# Standard deviation     5.945816e+04 3.024782e+04 1.884905e+02 4.133509e-01
# Proportion of Variance 7.944007e-01 2.055913e-01 7.983541e-06 3.839322e-11
# Cumulative Proportion  7.944007e-01 9.999920e-01 1.000000e+00 1.000000e+00
# 
# > loadings(result_dane)
# 
# Loadings:
#   Comp.1 Comp.2 Comp.3 Comp.4
# Stopa bezrobocia                         1.000
# Nowe miejsca pracy -0.943  0.334              
# Przedszkola                       1.000       
# Absolwenci          0.334  0.943              
# 
# Comp.1 Comp.2 Comp.3 Comp.4
# SS loadings      1.00   1.00   1.00   1.00
# Proportion Var   0.25   0.25   0.25   0.25
# Cumulative Var   0.25   0.50   0.75   1.00

