# Distance Based Redundancy analysis mean spectrum

# Step 1: Clear R's Brain (rm = remove, ls = list all objects)

rm(list=ls())

#install.packages("tuneR")
#install.packages("seewave")
#install.packages("soundecology")
#install.packages("audio")
#install.packages("ade4")
#install.packages("adegraphics")
#install.packages("lattice")

library(tuneR)
library(seewave)
library(soundecology)
library(audio)
library(ade4)
library(adegraphics)
library(lattice)

# Step 1: Clear R's Brain (rm = remove, ls = list all objects)

rm(list=ls())

# set working directory 
setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data")


############################# IMPORTANT USER SELECTIONS ########################
# 1. LOCATE THE DIRECTORY WITH THE AUDIO FILES FOR ANALYSIS

# setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data/Audio/100_1200Hz_Fish")#use this for all 34

# setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data/Audio/100_1200Hz_Fish_nomotor")# use for without motor (applied to this paper)

# setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data/Audio/100_1200Hz_Fish_nomotor_structure")# use for coral rugosity and vector disp

setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data/Audio/1200_11050Hz_Invert_nomotor")# use for without motor (applied to this paper)

# setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data/Audio/1200_11050Hz_Invert_nomotor_structure")# use for coral rugosity and vector disp


# 2. LOCATE THE FILE WITH EXPLAINATORY VARIABLES (Take care to match files above)


alldata <- read.csv("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data/Combined_datasets_nomotor.csv") 

# alldata <- read.csv("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/dB_RDA_Data/Combined_datasets_nomotor_structure.csv") 

files <- dir(pattern ="wav$")
n <-length(files)

#################################################
# Generating the dataframe for results of db_RDA

n2 <-2 # rows i.e. column heading, % inertia explained, p-value

df <- data.frame(matrix(nrow=n2, ncol=11))

# name columns of dataframe

colnames(df) <- c("Test", "Fish Abundance","Diversity","FIsh great 30","Motor",
                  "Coral health","Fishing pressure","Algal control", "Coral cover", 
                  "Rugosity","Vector dispersion")
#colnames(df[[1]]) # check of column names

#df # check df correct
##############################################
# generation of distance matrix based on meanspec (p507 Sound analysis and synthesis with R)

w1 <- 512 # window
mspectra <- matrix(NA, nrow=w1/2, ncol=n)
for(i in 1:n) mspectra[,i] <- meanspec(readWave(files[i]), wl=w1, plot=TRUE)[,2]

comb <- combn(1:n, 2)
ncol(comb)
comb [,1:34]


m <- matrix(NA, nrow=n, ncol=n)

for(i in 1:ncol(comb))  {
  m[comb[2,i], comb[1,i]] <- diffcumspec(mspectra [,comb[1,i]], 
                                         mspectra [,comb[2,i]],
                                         plot=FALSE)
}
colnames(m) <- rownames(m) <-unlist(strsplit(files, split=".wav"))

#check first 4 rows/cols of  m matrix filled with data

m[1:4,1:4]

#fill upper triangle with data
m[upper.tri(m)] <- t(m)[upper.tri(m)]

m[1:4,1:4]

# replace NA values on diagonal with 0 for further analysis

diag(m) <- 0
#####################################################
# distance based redundancy analysis (db-RDA)

d <- as.dist(m)
pcoa <- dudi.pco(quasieuclid(d), scannf=FALSE)
summary(pcoa)

######################################################################
# Explanatory factors examined for inertia explained and significance 

#####################################################################
# Test - comparing Manokwari and R4
#####################################################################

Test <- alldata$Location
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Test - comparing Sites
#####################################################################

Test <- alldata$Site_Name
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# fish abundance
#####################################################################

Test <- alldata$Fish_Abundance_2
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# fish>30cm
#####################################################################

Test <- alldata$Fish_great_30
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Diversity
#####################################################################

Test <- alldata$Diversity
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Coral health
#####################################################################

Test <- alldata$Coral_health
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Fishing_pressure
#####################################################################

Test <- alldata$Fishing_pressure
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Algal_control
#####################################################################

Test <- alldata$Algal_control
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Coral_cover
#####################################################################

Test <- alldata$Coral_cover
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Rugosity
#####################################################################

Test <- alldata$Rugosity
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

#####################################################################
# Vector.Dispersion
#####################################################################

Test <- alldata$Vector.Dispersion
Test <-as.factor(Test)

rda.Test_factor <- pcaiv(pcoa, df=Test, scannf=FALSE)

summary(rda.Test_factor)

s.class(rda.Test_factor$ls, fac=Test)

s.class(rda.Test_factor$ls, fac=Test, plabels.optim = TRUE, plabels.cex = 0.7, 
        plabels.alpha = 0.5, plabels.boxes.alpha =  1,
        col=1, ellipseSize = 2.5, xlim=c(-0.08, 0.1), ylim=c(-0.02, 0.03), plot = TRUE)

test.Test <- randtest(rda.Test_factor, nrepet=1000)
test.Test
plot(test.Test)

