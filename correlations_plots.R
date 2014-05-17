col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))

#install.packages("corplot")
   par(mfrow=c(1,2))
library(corrplot)


se <- read.arff("http://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff")
se <- se[,-c(14:16)]
corrplot(cor(se, method="kendall"),method="square", order="AOE", col=col2(200),diag=FALSE, tl.pos="d")
corrplot(cor(se, method="kendall"),method="pie", order="AOE", col=col2(200),diag=FALSE, tl.pos="d")