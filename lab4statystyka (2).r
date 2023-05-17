#lab4
#zrobi?? analiz?? skupie?? np. klasyfikacja spektralna z plik??w  bull eye, lub spirals
#regresja dwumianowa (logistyczna) np podzieli?? kraj na p???? weg??ug pkb
#nie robimy analizy czynnikowwej
#dlaczego taka metoda a nie inna - bo winiki s?? dla niej dobre
#??rednia ci????ko??ci klay t o??rednia arytmetyczna
#nie bra?? czasu jako zmienn??
print('a')
data("mtcars")
?mtcars
attach(mtcars)
res=glm(am~hp+qsec+mpg, binomial(link = "logit"))
res
summary(res)
