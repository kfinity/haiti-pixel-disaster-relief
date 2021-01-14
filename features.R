# Kevin Finity, kf2tg

# Feature engineering - looking at some plots and considering modifying the features.

# Visualizing the separation of the Blue Tarp class from the other classes.
plot(Blue[IsBT==1], Red[IsBT==1]+Green[IsBT==1],xlab="Blue",ylab="Red+Green", col="blue",pch = 3,main="Separation of Blue Tarps")
points(Blue[IsBT==0], Red[IsBT==0]+Green[IsBT==0], col="gray",pch = 4)

# looking at some class color profile histograms. 
hist(Blue[Class=="Blue Tarp"]) # very skewed
hist(Blue[Class=="Rooftop"])
hist(Blue[Class=="Soil"]) # pretty normal
hist(Red[Class=="Soil"]) # very skewed
hist(Blue[Class=="Various Non-Tarp"])
hist(Blue[Class=="Vegetation"])
hist(Green[Class=="Vegetation"])

plot(Blue/Red+Blue/Green, IsBT.bin)

# This is very interesting - great separation with dividing blue by the other two.
plot(Blue[IsBT==1]/Red[IsBT==1], Blue[IsBT==1]/Green[IsBT==1],xlab="B/R",ylab="B/G", col="blue",pch = 3,main="Separation of Blue Tarps")
points(Blue[IsBT==0]/Red[IsBT==0], Blue[IsBT==0]/Green[IsBT==0], col="gray",pch = 4)
points(Blue[pix.c=="Rooftop"]/Red[pix.c=="Rooftop"], Blue[pix.c=="Rooftop"]/Green[pix.c=="Rooftop"], col="black",pch = 4)
points(Blue[pix.c=="Soil"]/Red[pix.c=="Soil"], Blue[pix.c=="Soil"]/Green[pix.c=="Soil"], col="red",pch = 4)
points(Blue[pix.c=="Various Non-Tarp"]/Red[pix.c=="Various Non-Tarp"], Blue[pix.c=="Various Non-Tarp"]/Green[pix.c=="Various Non-Tarp"], col="purple",pch = 4)
points(Blue[pix.c=="Vegetation"]/Red[pix.c=="Vegetation"], Blue[pix.c=="Vegetation"]/Green[pix.c=="Vegetation"], col="green",pch = 4)

levels(pix.c)
names(pix.p)

# look at principal components for RGB data
pr.out=prcomp(pix.p[,1:3], scale=TRUE)
summary(pr.out) 
# Importance of components:
#                           PC1     PC2     PC3
# Standard deviation     1.7092 0.25738 0.11111
# Proportion of Variance 0.9738 0.02208 0.00411
# Cumulative Proportion  0.9738 0.99589 1.00000
pr.out$rotation
#             PC1        PC2        PC3
# Red   0.5765526 -0.6188816  0.5334536
# Green 0.5823318 -0.1467245 -0.7996009
# Blue  0.5731290  0.7716589  0.2758001

biplot(pr.out, scale=0) # plot the data on the PCs
pr.out$rotation=-pr.out$rotation # rotate it by multiplying everything by -1
pr.out$x=-pr.out$x
biplot(pr.out, scale=0) # and view from a different angle

# pairs(pix2)
plot(pix2$br, (pix2$Red / pix2$Green))
plot(pix2$br[IsBT==1], pix2$Red[IsBT==1]/pix2$Green[IsBT==1],xlab="B/R",ylab="R/G", col="blue",pch = 3,main="Separation of Blue Tarps")
points(pix2$br[IsBT==0], pix2$Red[IsBT==0]/pix2$Green[IsBT==0], col="gray",pch = 4)