#upload data

read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8') -> wynagrodzenia_csv
read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8') -> ludnosc20_24lata_csv
read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8') -> praca_csv

#read data

wynagrodzenia <- c(wynagrodzenia_csv$ogółem)
ludnosc_20_24 <- c(ludnosc20_24lata_csv$ogółem.)
praca <- c(praca_csv$liczba.nowo.utworzonych.miejsc.pracy.)

#regresja liniowa
reg_lin <- lm(ludnosc_20_24 ~ wynagrodzenia + praca)
summary(reg_lin)
##
# Call:
#   lm(formula = ludnosc_20_24 ~ wynagrodzenia + praca)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -20132.5 -10456.8   -310.6  12428.7  20234.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   200321.14   72999.79   2.744   0.0167 *  
#   wynagrodzenia    -30.91      14.29  -2.163   0.0498 *  
#   praca           2516.10     248.00  10.145 1.52e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 14440 on 13 degrees of freedom
# Multiple R-squared:  0.9547,	Adjusted R-squared:  0.9477 
# F-statistic: 136.9 on 2 and 13 DF,  p-value: 1.85e-09