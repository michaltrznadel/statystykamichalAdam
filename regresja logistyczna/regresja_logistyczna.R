read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8') -> liczbazgonow_K_csv
read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8') -> liczbazgonow_M_csv
read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8') -> ludnosc_K_csv
read.csv2(file.choose(), header=T, sep = ";", dec=",", fileEncoding = 'UTF-8') -> ludnosc_M_csv


liczbazgonow_K <- c(liczbazgonow_K_csv$kobiety.)
liczbazgonow_M <- c(liczbazgonow_M_csv$mężczyźni.)
ludnosc_K <- c(ludnosc_K_csv$ogółem.)
ludnosc_M <- c(ludnosc_M_csv$ogółem.)
liczbazgonow <- append(liczbazgonow_K, liczbazgonow_M)
ludnosc <- append(ludnosc_K, ludnosc_M)
plec <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

reg_log <- glm(plec ~ liczbazgonow + ludnosc, family = "binomial")
summary(reg_log)
exp(reg_log$coefficients)

# Call:
#   glm(formula = plec ~ liczbazgonow + ludnosc, family = "binomial")
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.91347  -0.92844  -0.05878   0.84712   1.82207  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  -3.964e-01  9.249e-01  -0.429   0.6682  
# liczbazgonow  9.010e-04  3.637e-04   2.478   0.0132 *
#   ludnosc      -1.201e-05e  4.848e-06  -2.477   0.0132 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 44.361  on 31  degrees of freedom
# Residual deviance: 32.687  on 29  degrees of freedom
# AIC: 38.687
# 
# Number of Fisher Scoring iterations: 5