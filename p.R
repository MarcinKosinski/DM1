library("foreign")
library("MASS")
library("rpart")
library("klaR")

se <- read.arff("http://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff")
se <- se[,-c(14:16)]
head(se)

# podzial na probe testowa i treningowa:

dim(se)
ile <- floor((2/3)*dim(se)[1])
s <- sample(1:dim(se)[1],ile)
tren <- se[s,]
test <- se[-s,]

# bedziemy budowac model na treningowej, a testowac na testowej :D

# model buduje bez zmiennych maxenergy i nbumps, bo sa skorelowane z innymi
# i bez zmiennych nbumps6, 7 i 89, bo byly cale zerowe

# lda

mod_lda <- lda(class ~ seismic +seismoacoustic +shift +genergy +gpuls +gdenergy +
               gdpuls +ghazard +nbumps2 +nbumps3 +nbumps4 +nbumps5 +energy,
               data=tren)
pred <- predict(mod_lda, newdata=test)$class
pred
table(pred)






