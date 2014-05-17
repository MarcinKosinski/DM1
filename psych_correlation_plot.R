install.packages("psych")
library(psych)
pairs.panels(attitude)   #see the graphics window
data(iris)
pairs.panels(iris[1:4],bg=c("red","yellow","blue")[iris$Species],
             pch=21,main="Fisher Iris data by Species") #to show color grouping
pairs.panels(iris[1:4],bg=c("red","yellow","blue")[iris$Species],
             pch=21,main="Fisher Iris data by Species",hist.col="red") 
#to show changing the diagonal
#demonstrate not showing the data points
data(sat.act)
pairs.panels(sat.act,show.points=FALSE)
#better yet is to show the points as a period
pairs.panels(sat.act,pch=".", main="giwo")
#show many variables with 0 gap between scatterplots
# data(bfi)
# pairs.panels(bfi,show.points=FALSE,gap=0)