library("foreign")
library("MASS")
library("rpart")
library("klaR")

se <- read.arff("http://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff")
head(se)
any(is.na(se))

dim(se)

mod <- lda(class~.-nbumps-nbumps6-nbumps7-nbumps89,data=se)
tree <- rpart(class~.-nbumps-nbumps6-nbumps7-nbumps89,data=se)
par(oma=c(rep(0.1,4)))
plot(tree)
text(tree)

ncol(se)
stepclass(se[,c(1:8,10:18)],se[,19],method="qda",direction="forward")
warnings()

apply(se[9:17],2,sum)

install.GitHub()
