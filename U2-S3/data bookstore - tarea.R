
round(cor(x = bookstore, method = "pearson"), 3)

library(psych) 
multi.hist(x = bookstore, dcol = c("purple", "red"),
           dlty = c("dotted", "solid"), main = "")

library(GGally)
ggpairs(bookstore, lower = list(continuous =
                                                 "smooth"), diag = list(continuous = "barDiag"), axisLabels =
                          "none")

#cuál de todas las variables influyen en la variable dependiente
modelox = lm(bookstore$price ~ bookstore$pages+bookstore$reviews+bookstore$n_reviews
             +bookstore$star5+bookstore$star4+bookstore$star3+bookstore$star2+bookstore$star1
             +bookstore$weight)

step(object = modelox, direction = "both", trace=1)

library(car)

vif(modelox)

######### Gráfica en 3D (3 variable 1y y 2x)
library(rgl)

plot3d(bookstore$price, bookstore$star5,bookstore$star1, pch = ".", size = 0.5)