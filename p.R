library("foreign")
library("MASS")
library("rpart")
library("klaR")

se <- read.arff("http://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff")
se <- se[,-c(14:16,18,9)]
head(se,2)
dim(se)

# specjalnie dla qda, dane przerabiam z faktorowych na ilosciowe:

se_r <- se
levels(se_r$seismic) <- c(0,1,2,3)
levels(se_r$seismoacoustic) <- c(0,1,2,3)
levels(se_r$shift) <- c(0,1)
levels(se_r$ghazard) <- c(0,1,2,3)
se_r[,c(1,2,3,8)] <- apply(se_r[,c(1,2,3,8)],2,as.numeric)

head(se_r,2)

# podzial na probe testowa i treningowa:

dim(se)
ile <- floor((2/3)*dim(se)[1])
s <- sample(1:dim(se)[1],ile)
tren <- se[s,]
test <- se[-s,]

tren_r <- se_r[s,]
test_r <- se_r[-s,]

# bedziemy budowac model na treningowej, a testowac na testowej :D

# lda

mod_lda <- lda(class~., data=tren)
pred <- predict(mod_lda, newdata=test)$class
pred
t <- table(pred,test$class)
100*sum(diag(t))/sum(t)
t
table(test$class)

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja

# czyli raczej slabo :(

# qda

mod_qda <- qda(class~., data=tren_r)

pred <- predict(mod_qda, newdata=test_r)$class
t <- table(pred,test_r$class)
100*sum(diag(t))/sum(t)
t

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja

