#--------------------------------------------------------------
# Exploration of correlations between deviances explained by acoustic indices
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# Step 1: Clear R's Brain (rm = remove, ls = list all objects)

rm(list=ls())

# set working directory 

setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/Data/Temporal_correlation")
#install.packages("vegan")
#library("vegan")

# attach data

data <-read.csv("Fish_abundance_correlation.csv", header = TRUE)

##################################
# subset dataframe on which to base the correlation
data_1 <- data[,1:3]

res <- cor(data_1)
round(res,2)

##################################
# Correlation matrix with significance levels

#install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(data_1))
res2

# To extract p-values or correlation coefficients

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

###############################################
#Use corrplot() function: Draw a correlogram

#install.packages("corrplot")

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05)
# Insignificant correlations are left blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
