#--------------------------------------------------------------
# Exploration of IDCF complete dataset

# Step 1: Clear R's Brain (rm = remove, ls = list all objects)

rm(list=ls())

# set working directory 

setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/Data")

list.files()
library("writexl")
library(magrittr)
library(ggplot2)
library(scatterplot3d)
library(GGally)
library(stats)
library(car)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(utils)

alldata <- read.csv("Combined_datasets_nomotor.csv", header = TRUE)

head(alldata)
str(alldata)


#######################################################################
#STAGE 1 Descriptive statistics and plots                             #
#######################################################################

#Investigating relationships between variables ?ggpairs (Figure exported 
#to supplemental material

ggpairs(alldata[,6:41])
ggpairs(alldata, columns = 24:41)


# Test whether % live coral significantly different between Raja
# Ampat and Manokwari sites

#Test homogeneity of variance for coral cover and fish abundance to guide
#statistical approaches
# Levenes test Null hypothesis: the groups we're comparing all have equal population variances.
# so p<0.05 we cannot use ANOVA but should use kruskal Wallis

# Coral cover 

leveneTest(Coral_cover ~ Location, data=alldata)
leveneTest(Coral_cover ~ Site_Name, data=alldata)

# Homogeneity of variance observed for both so ANOVA applied

plot(Coral_cover ~ Location, data=alldata)
anova_coral_location <- aov(Coral_cover ~ Location, data = alldata)
summary(anova_coral_location)

plot(Coral_cover ~ Site_Name, data=alldata)
anova_coral_Site <- aov(Coral_cover ~ Site_Name, data = alldata)
summary(anova_coral_Site)
TukeyHSD(anova_coral_Site)

# Fish abundance 2

leveneTest(Fish_great_30 ~ Location, data=alldata)
leveneTest(Fish_great_30 ~ Site_Name, data=alldata)

# Homogeneity of variance observed for both so ANOVA applied

plot(Fish_great_30 ~ Location, data=alldata)
anova_fish_location <- aov(Fish_great_30 ~ Location, data=alldata)
summary(anova_fish_location)


plot(Fish_great_30 ~ Site_Name, data=alldata)
anova_fish_Site <- aov(Fish_great_30 ~ Site_Name, data=alldata)
summary(anova_fish_Site)
TukeyHSD(anova_fish_Site)


# Fish_less_30

leveneTest(Fish_less_30 ~ Location, data=alldata)
leveneTest(Fish_less_30 ~ Site_Name, data=alldata)

# Homogeneity of variance observed for both so ANOVA applied

plot(Fish_less_30 ~ Location, data=alldata)
anova_fish_less_30_location <- aov(Fish_less_30 ~ Location, data=alldata)
summary(anova_fish_less_30_location)


plot(Fish_less_30 ~ Site_Name, data=alldata)
anova_fish_less_30_Site <- aov(Fish_less_30 ~ Site_Name, data=alldata)
summary(anova_fish_less_30_Site)
TukeyHSD(anova_fish_less_30_Site)

# Fish_great_30

leveneTest(Fish_great_30 ~ Location, data=alldata)
leveneTest(Fish_great_30 ~ Site_Name, data=alldata)

# Lack of Homogeneity of variance observed for both so Kruskal Wallis applied

plot(Fish_great_30 ~ Location, data=alldata)
kw_Fish_great_30_location <- kruskal.test(Fish_great_30 ~ Location, data=alldata)
kw_Fish_great_30_location

# p=0.06 so just not sig difference in large fish btw Manokwari and Raja

plot(Fish_great_30 ~ Site_Name, data=alldata)
kw_Fish_great_30_Site <- kruskal.test(Fish_great_30 ~ Site_Name, data=alldata)
kw_Fish_great_30_Site

# p=0.003 so significant difference between sites (Sawanare clearly influencer)

# Coral_health

leveneTest(Coral_health ~ Location, data=alldata)
leveneTest(Coral_health ~ Site_Name, data=alldata)

# Homogeneity of variance observed for Location so ANOVA applied
# Lack of Homogeneity of variance observed for Site so Kruskal Wallis applied

plot(Coral_health ~ Location, data=alldata)
anova_Coral_health_location <- aov(Coral_health ~ Location, data=alldata)
summary(anova_Coral_health_location)


plot(Coral_health ~ Site_Name, data=alldata)
kw_Coral_health_Site <- kruskal.test(Coral_health ~ Site_Name, data=alldata)
kw_Coral_health_Site

# no sig difference in Coral_Health indicator species Manok/RA

# Fishing_pressure

leveneTest( Fishing_pressure ~ Location, data=alldata)
leveneTest( Fishing_pressure ~ Site_Name, data=alldata)

# Homogeneity of variance observed for Location so ANOVA applied
# Lack of Homogeneity of variance observed for Site so Kruskal Wallis applied

plot( Fishing_pressure ~ Location, data=alldata)
anova_Fishing_pressure_location <- aov( Fishing_pressure ~ Location, data=alldata)
summary(anova_Fishing_pressure_location)


plot(Fishing_pressure ~ Site_Name, data=alldata)
kw_Fishing_pressure_Site <- kruskal.test( Fishing_pressure ~ Site_Name, data=alldata)
kw_Fishing_pressure_Site

# no sig difference in Fishing Pressure Manok/RA
# Sig site differences in fishing pressure

# Algal_control

leveneTest( Algal_control ~ Location, data=alldata)
leveneTest( Algal_control ~ Site_Name, data=alldata)

# Homogeneity of variance observed for Location & Site so ANOVA applied

plot( Algal_control ~ Location, data=alldata)
anova_Algal_control_location <- aov( Algal_control ~ Location, data=alldata)
summary(anova_Algal_control_location)


plot(Algal_control ~ Site_Name, data=alldata)
kw_Algal_control_Site <- kruskal.test( Algal_control ~ Site_Name, data=alldata)
kw_Algal_control_Site

# no sig difference in Algal Control Manok/RA
# Sig site differences in Algal Control 

# Protected_species

# no protected species recorded for either site


###########################################################
###               Habitat characteristics              ###
###########################################################

# Rugosity

leveneTest(Rugosity ~ Location, data=alldata)
leveneTest(Rugosity ~ Site_Name, data=alldata)

# Homogeneity of variance observed for both so ANOVA applied

plot(Rugosity ~ Location, data=alldata)
anova_rugosity_location <- aov(Rugosity ~ Location, data=alldata)
summary(anova_rugosity_location)


plot(Rugosity ~ Site_Name, data=alldata)
anova_rugosity_Site <- aov(Rugosity ~ Site_Name, data=alldata)
summary(anova_rugosity_Site)
TukeyHSD(anova_rugosity_Site)

# no significant difference between locations or sites for rugosity

# Vector dispersion

leveneTest(Vector.Dispersion ~ Location, data=alldata)
leveneTest(Vector.Dispersion ~ Site_Name, data=alldata)

# Homogeneity of variance observed for both so ANOVA applied

plot(Vector.Dispersion ~ Location, data=alldata)
anova_VectorDispersion_location <- aov(Vector.Dispersion ~ Location, data=alldata)
summary(anova_VectorDispersion_location)

#Significant difference between Raja ampat and Manokwari for vector dispersion p=0.02

plot(Vector.Dispersion ~ Site_Name, data=alldata)
anova_VectorDispersion_Site <- aov(Vector.Dispersion ~ Site_Name, data=alldata)
summary(anova_VectorDispersion_Site)
TukeyHSD(anova_VectorDispersion_Site)

# no significant difference between  sites for vector dispersion p=0.054.

# TEST 1 model - poisson
#######################################################################
# fish abundance ~ coral cover?

hist(alldata$Fish_Abundance_2)

# Count data, bounded 0 - infinity: Poission model

model_Fish_Abundance_2 <- glm(Fish_Abundance_2 ~ Coral_cover, family = "poisson", data = alldata)
model_fish_NULL <- glm(Fish_Abundance_2 ~ 1, family = "poisson", data = alldata)
anova(model_Fish_Abundance_2, test = "Chisq")
summary(model_Fish_Abundance_2)


with(summary(model_Fish_Abundance_2), 1-deviance/null.deviance)

# coral cover explains 47.14% of variation in fish abundance

# Generate plots using ggpubr http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
# Figure 4 Fish abundance ~ coral cover regression

ggscatter(alldata, x = "Coral_cover", y = "Fish_Abundance_2",
          add = "reg.line",                                 # Add regression line
          conf.int = TRUE,                                  # Add confidence interval
          add.params = list(color = "black",
                            fill = "lightgray"),
                    xlab = "Percentage live coral cover",
                    ylab = "Fish abundance",
                    font.label = c(20,"plain") 
)+
  stat_cor(method = "pearson", label.x = 0.1, label.y = 40)  # Add correlation coefficient

#################################################################

# Graphing a curve to the data (Prediction)

# PREDICTIONS workflow

# seq (sequence to set up numbers list) seq(from = 0.01, to = 0.99 (coral cover), length = 100)
# Set the data up using expand.grid - first set up your list of x values
# ?expand.grid

newX <- expand.grid(Coral_cover = seq(from = 0.01, to = 0.99, length = 30))

newX # look at it

# Check out limits to predict and predict.glm

#
?predict
#?predict.glm

#predict(object, newdata = NULL,
#        type = c("link", "response", "terms"),
#        se.fit = FALSE, dispersion = NULL, terms = NULL,
#        na.action = na.pass, ...)

newy <- predict(model_Fish_Abundance_2, newdata = newX, type = "response")


# Note new y is NOT a DATAFRAME

# Put x and y values together in new dataframe

pd <- data.frame(newX, fish.abun = newy) 

head(pd) # Loook it is now a new dataframe with fitness as a new column header
tail(pd)


# Plot the graph

ggplot(alldata, aes(x = Coral_cover, y = Fish_Abundance_2)) + 
  # add raw data
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  # add line of predicted values on response scale from type = 'response'
  # in predict.glm (stored in pd data frame I made)
  geom_line(data = pd, aes(x = Coral_cover, y = fish.abun))+
  theme_bw(base_size = 15)

###########################################################

# Confidence intervals 

newX <- expand.grid(Coral_cover = seq(from = 0.01, to = 0.99, length = 30))

newX # look at it

newy <- predict(model_Fish_Abundance_2, newdata=newX, type = "link", se.fit = TRUE)

newy

# Note new y is NOT a DATAFRAME

# Put x and y values together in new dataframe (pd - 'plot data')

pd <- data.frame(newX, newy) # this brings in all the values generated for x and y

# Look at the data

head(pd) 
tail(pd)

# Now we need to back transform the fitted values onto response scale 
# using exp()

pd <-mutate(pd, Fish_Abundance_2=exp(fit))
head (pd)

pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))
head (pd)


# Plot the graph

ggplot(alldata, aes(x = Coral_cover, y = Fish_Abundance_2)) +  
  # add raw data
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
   # add line of predicted values on response scale from type = 'response'
  # in predict.glm (stored in pd data frame I made)
  
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Proportion live coral")

# always add stat = identity to force it to do exactly what we want


##################################
##################################

# TEST 2 model - poisson
#######################################################################
# fish abundance ~ rugosity?

hist(alldata$Fish_Abundance_2)

# Count data, bounded 0 - infinity: Poission model

model_Fish_Abundance_2 <- glm(Fish_Abundance_2 ~ Rugosity, family = "poisson", data = alldata)

anova(model_Fish_Abundance_2, test = "Chisq")
summary(model_Fish_Abundance_2)
with(summary(model_Fish_Abundance_2), 1-deviance/null.deviance)

# Rugosity does explain fish abundance p=<0.001 (but only 3% explained)

# Plotting the graph with Confidence intervals 

newX <- expand.grid(Rugosity = seq(from = 0.01, to = 0.99, length = 30))

newX # look at it

newy <- predict(model_Fish_Abundance_2, newdata=newX, type = "link", se.fit = TRUE)

newy

# Note new y is NOT a DATAFRAME

# Put x and y values together in new dataframe (pd - 'plot data')

pd <- data.frame(newX, newy) # this brings in all the values generated for x and y

# Look at the data

head(pd) 
tail(pd)

# Now we need to back transform the fitted values onto response scale 
# using exp()

pd <-mutate(pd, Fish_Abundance_2=exp(fit))
head (pd)

pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))
head (pd)


# Plot the graph

ggplot(alldata, aes(x = Rugosity, y = Fish_Abundance_2)) +  
  # add raw data
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  # add line of predicted values on response scale from type = 'response'
  # in predict.glm (stored in pd data frame I made)
  
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Rugosity")

# always add stat = identity to force it to do exactly what we want



##################################
##################################

# TEST 3 model - poisson
#######################################################################
# fish abundance ~ vector dispersion?

hist(alldata$Fish_Abundance_2)

# Count data, bounded 0 - infinity: Poission model

model_Fish_Abundance_2 <- glm(Fish_Abundance_2 ~ Vector.Dispersion, family = "poisson", data = alldata)

anova(model_Fish_Abundance_2, test = "Chisq")
summary(model_Fish_Abundance_2)
with(summary(model_Fish_Abundance_2), 1-deviance/null.deviance)

# Vector dispersion is significant p<0.001 but only explains 0.003 of variation in fish abundance

# Plotting the graph with Confidence intervals 

newX <- expand.grid(Vector.Dispersion = seq(from = 0.01, to = 0.99, length = 30))

newX # look at it

newy <- predict(model_Fish_Abundance_2, newdata=newX, type = "link", se.fit = TRUE)

newy

# Note new y is NOT a DATAFRAME

# Put x and y values together in new dataframe (pd - 'plot data')

pd <- data.frame(newX, newy) # this brings in all the values generated for x and y

# Look at the data

head(pd) 
tail(pd)

# Now we need to back transform the fitted values onto response scale 
# using exp()

pd <-mutate(pd, Fish_Abundance_2=exp(fit))
head (pd)

pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))
head (pd)


# Plot the graph

ggplot(alldata, aes(x = Vector.Dispersion, y = Fish_Abundance_2)) +  
  # add raw data
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  # add line of predicted values on response scale from type = 'response'
  # in predict.glm (stored in pd data frame I made)
  
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Rugosity")

# always add stat = identity to force it to do exactly what we want


# TEST 4 model 
#######################################################################
# coral cover ~ rugosity?

hist(alldata$Coral_cover)

# Proportional data requires binomial model

model_coral <- glm (Coral_cover ~ Rugosity,family = "binomial", data = alldata)

par(mfrow = c(2,2))
plot(model_coral)
par(mfrow = c(1,1))

# Null Model

Null_coral <- glm(Coral_cover ~ 1, family = "binomial", data = alldata)

anova(Null_coral, model_coral, test = "LR")
summary(model_coral)
with(summary(model_coral), 1-deviance/null.deviance)

# No significant difference between Null model and 
# full model with all Acoustic Indices

# TEST 5 model 
#######################################################################
# coral cover ~ vector dispersion?

hist(alldata$Coral_cover)

# binomial model for % data

model_coral <- glm (Coral_cover ~ Vector.Dispersion, family = "binomial", data = alldata)

par(mfrow = c(2,2))
plot(model_coral)
par(mfrow = c(1,1))

# Null Model

Null_coral <- glm(Coral_cover ~ 1, family = "binomial", data = alldata)

anova(Null_coral, model_coral, test = "LR")
summary(model_coral)
with(summary(model_coral), 1-deviance/null.deviance)

# No significant difference between Null model and Vector dispersion p=0.221

########################################################################
########################################################################
# TEST 6 model 
#######################################################################
# Rugosity ~ acoustic indices?

hist(alldata$Rugosity)

# Rugosity - gaussian

# model_rugosity <- glm (Rugosity ~ HIGH_RMS + LOW_RMS + +RMS_Ratio + LOW_ROUGHNESS + HIGH_ROUGHNESS + 
                        # AC_FISH + AC_INVERTS + AD_FISH + AD_INVERTS + AEI_FISH + AEI_INVERTS + 
                        # BI_FISH + BI_INVERTS,family = "gaussian", data = alldata)

 model_rugosity <- glm (Rugosity ~ RMS_Ratio + ROUGHNESS_Ratio +
                          AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                          AEI_Ratio + BI_Ratio,family = "gaussian", data = alldata)

par(mfrow = c(2,2))
plot(model_rugosity)
par(mfrow = c(1,1))

# Null Model

Null_rugosity <- glm(Rugosity ~ 1, family = "gaussian", data = alldata)

anova(Null_rugosity, model_rugosity, test = "LRT")
summary(model_rugosity)
with(summary(model_rugosity), 1-deviance/null.deviance)

# No significant difference between Null model and 
# full model with all Acoustic Indices p=0.61

########################################################################
########################################################################

########################################################################
########################################################################
# TEST 7 model 
#######################################################################
# Vector dispersion ~ acoustic indices?

hist(alldata$Vector.Dispersion)

# Gaussian

model_vd <- glm (Vector.Dispersion ~ RMS_Ratio + ROUGHNESS_Ratio +
                   AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                   AEI_Ratio + BI_Ratio,family = "gaussian", data = alldata)

par(mfrow = c(2,2))
plot(model_vd)
par(mfrow = c(1,1))

# Null Model

Null_vd <- glm(Vector.Dispersion ~ 1, family = "gaussian", data = alldata)

anova(Null_vd, model_vd, test = "LRT")
summary(model_vd)
with(summary(model_vd), 1-deviance/null.deviance)

# No significant difference between Null model and 
# full model with all Acoustic Indices p=0.89

########################################################################
########################################################################
# TEST 8 model 
#######################################################################
# coral cover ~ acoustic indices?

hist(alldata$Coral_cover)

# Proportional data requires binomial model

model_coral <- glm (Coral_cover ~ RMS_Ratio + ROUGHNESS_Ratio +
                      AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                      AEI_Ratio + BI_Ratio,family = "binomial", data = alldata)

par(mfrow = c(2,2))
plot(model_coral)
par(mfrow = c(1,1))

# Null Model

Null_coral <- glm(Coral_cover ~ 1, family = "binomial", data = alldata)

anova(Null_coral, model_coral, test = "LR")
summary(model_coral)
with(summary(model_coral), 1-deviance/null.deviance)

# No significant difference between Null model and 
# full model with all Acoustic Indices


########################################################################
# MULTIPLE ACOUSTIC INDICATORS AS PREDICTORS OF FISH CATEGORIES        #
########################################################################

# TEST MODEL 9 poission

## Fish Abundance_2 ~ Acoustics?

hist(alldata$Fish_Abundance_2)

# Count data requires poission model

model_fish <- glm (Fish_Abundance_2 ~ HIGH_RMS + LOW_RMS + RMS_Ratio + LOW_ROUGHNESS + HIGH_ROUGHNESS +
                           ROUGHNESS_Ratio + AC_FISH + AC_INVERTS + AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                           AEI_Ratio + BI_FISH + BI_INVERTS + BI_Ratio, family = "poisson", data = alldata)

par(mfrow = c(2,2))
plot(model_fish)
par(mfrow = c(1,1))

anova(model_fish)
summary(model_fish)
with(summary(model_fish), 1-deviance/null.deviance)

# Null Model

Null_fish <- glm(Fish_Abundance_2 ~ 1, family = "poisson", data = alldata)
summary (Null_fish)
with(summary(Null_fish), 1-deviance/null.deviance)

anova(Null_fish, model_fish, test = "Chisq")

# There is a significant difference between the null model and 
# the full model - acoustic indices provide excellent explainatory 
# power (89%) of the variance in fish abundance.


# default backwards step

backwards <- step(model_fish)
anova(model_fish, backwards, test = "Chisq")
formula(backwards)

# backwards 

summary(backwards)

with(summary(backwards), 1-deviance/null.deviance)

backwards$deviance; model_fish$deviance

# forwards step

forwards = step(Null_fish, scope=list(lower=formula(Null_fish),upper=formula(model_fish)), direction="forward")
formula(forwards)
anova(model_fish, forwards, test = "Chisq")
summary(forwards)

# both ways step

bothways = step(Null_fish, list(lower=formula(Null_fish),upper=formula(model_fish)),
                direction="both",trace=0)

formula(bothways)
anova(model_fish, bothways, test = "Chisq")
summary(bothways)

formula (model_fish)
formula (backwards)
formula (forwards)
formula (bothways)

# Compare deviance of the models
anova(model_fish, backwards,forwards, bothways, test = "Chisq")
AIC(model_fish, backwards,forwards, bothways)
model_fish$deviance; backwards$deviance; forwards$deviance; bothways$deviance

# Best model (forwards) has 16 predictors Fish_Abundance_2 ~ AEI_INVERTS + ROUGHNESS_Ratio + LOW_RMS + 
# HIGH_ROUGHNESS + LOW_ROUGHNESS + AEI_FISH + AC_INVERTS + 
# AC_Ratio + AD_Ratio + BI_Ratio + AD_INVERTS + AD_FISH + AEI_Ratio + 
# RMS_Ratio + BI_FISH + AC_FISH

with(summary(forwards), 1-deviance/null.deviance)

# also explains 89.4%

formula (forwards)
summary(forwards)


## Diversity ~ Acoustics?

hist(alldata$Diversity)

# Count data requires poission model

model_fish <- glm (Diversity ~ HIGH_RMS + LOW_RMS + RMS_Ratio + LOW_ROUGHNESS + HIGH_ROUGHNESS +
                     ROUGHNESS_Ratio + AC_FISH + AC_INVERTS + AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                     AEI_Ratio + BI_FISH + BI_INVERTS + BI_Ratio, family = "poisson", data = alldata)

par(mfrow = c(2,2))
plot(model_fish)
par(mfrow = c(1,1))

anova(model_fish)
summary(model_fish)
with(summary(model_fish), 1-deviance/null.deviance)

# Null Model

Null_fish <- glm(Diversity ~ 1, family = "poisson", data = alldata)
summary (Null_fish)
with(summary(Null_fish), 1-deviance/null.deviance)

anova(Null_fish, model_fish, test = "Chisq")

# There is a significant difference between the null model and 
# the full model - acoustic indices provide excellent explainatory 
# power (89%) of the variance in fish abundance.


# default backwards step

backwards <- step(model_fish)
anova(model_fish, backwards, test = "Chisq")
formula(backwards)

# backwards 

summary(backwards)

with(summary(backwards), 1-deviance/null.deviance)

backwards$deviance; model_fish$deviance

# forwards step

forwards = step(Null_fish, scope=list(lower=formula(Null_fish),upper=formula(model_fish)), direction="forward")
formula(forwards)
anova(model_fish, forwards, test = "Chisq")
summary(forwards)

# both ways step

bothways = step(Null_fish, list(lower=formula(Null_fish),upper=formula(model_fish)),
                direction="both",trace=0)

formula(bothways)
anova(model_fish, bothways, test = "Chisq")
summary(bothways)

formula (model_fish)
formula (backwards)
formula (forwards)
formula (bothways)

# Compare deviance of the models
anova(model_fish, backwards,forwards, bothways, test = "Chisq")
AIC(model_fish, backwards,forwards, bothways)
model_fish$deviance; backwards$deviance; forwards$deviance; bothways$deviance

# Best model identified by both forwards and backwards  with lowest ACI is 
# Diversity ~ AEI_INVERTS + ROUGHNESS_Ratio + AEI_Ratio + BI_FISH
anova(Null_fish, forwards, test = "Chisq")
with(summary(forwards), 1-deviance/null.deviance)

# also explains 38.57%

formula (forwards)
summary(forwards)

## Fish_great_30 ~ Acoustics?

hist(alldata$Fish_great_30)

# Count data requires poission model

model_fish <- glm (Fish_great_30 ~ HIGH_RMS + LOW_RMS + RMS_Ratio + LOW_ROUGHNESS + HIGH_ROUGHNESS +
                     ROUGHNESS_Ratio + AC_FISH + AC_INVERTS + AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                     AEI_Ratio + BI_FISH + BI_INVERTS + BI_Ratio, family = "poisson", data = alldata)

par(mfrow = c(2,2))
plot(model_fish)
par(mfrow = c(1,1))

anova(model_fish)
summary(model_fish)
with(summary(model_fish), 1-deviance/null.deviance)

# Check for over/underdispersion in the model
# https://fukamilab.github.io/BIO202/04-C-zero-data.html
E2<-resid(model_fish, type = "pearson")
N <- nrow(alldata)
p <- length(coef(model_fish))
sum(E2^2)/(N - p)

# Null Model

Null_fish <- glm(Fish_great_30 ~ 1, family = "poisson", data = alldata)
summary (Null_fish)
with(summary(Null_fish), 1-deviance/null.deviance)

anova(Null_fish, model_fish, test = "Chisq")

# There is a significant difference between the null model and 
# the full model - acoustic indices provide excellent explainatory 
# power (87.5%) of the variance in fish abundance.


# default backwards step

backwards <- step(model_fish)
anova(model_fish, backwards, test = "Chisq")
formula(backwards)

# backwards 

summary(backwards)

with(summary(backwards), 1-deviance/null.deviance)

backwards$deviance; model_fish$deviance

# forwards step

forwards = step(Null_fish, scope=list(lower=formula(Null_fish),upper=formula(model_fish)), direction="forward")
formula(forwards)
anova(model_fish, forwards, test = "Chisq")
summary(forwards)

# both ways step

bothways = step(Null_fish, list(lower=formula(Null_fish),upper=formula(model_fish)),
                direction="both",trace=0)

formula(bothways)
anova(model_fish, bothways, test = "Chisq")
summary(bothways)

formula (model_fish)
formula (backwards)
formula (forwards)
formula (bothways)

# Compare deviance of the models
anova(model_fish, backwards,forwards, bothways, test = "Chisq")
AIC(model_fish, backwards,forwards, bothways)
model_fish$deviance; backwards$deviance; forwards$deviance; bothways$deviance

# Best model identified by backwards  with lowest ACI is 
# Fish_great_30 ~ HIGH_RMS + LOW_RMS + RMS_Ratio + LOW_ROUGHNESS + 
# ROUGHNESS_Ratio + AC_FISH + AC_INVERTS + AD_FISH + AD_INVERTS + 
# AD_Ratio + AEI_INVERTS + BI_FISH + BI_INVERTS + BI_Ratio
anova(Null_fish, backwards, test = "Chisq")
with(summary(backwards), 1-deviance/null.deviance)

# explains 87.3%

formula (forwards)
summary(forwards)

## Fishing_pressure ~ Acoustics?

hist(alldata$Fishing_pressure)

# Count data requires poission model

model_fish <- glm (Fishing_pressure ~ HIGH_RMS + LOW_RMS + RMS_Ratio + LOW_ROUGHNESS + HIGH_ROUGHNESS +
                     ROUGHNESS_Ratio + AC_FISH + AC_INVERTS + AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                    AEI_Ratio + BI_FISH + BI_INVERTS + BI_Ratio, family = "poisson", data = alldata)

par(mfrow = c(2,2))
plot(model_fish)
par(mfrow = c(1,1))

anova(model_fish)
summary(model_fish)
with(summary(model_fish), 1-deviance/null.deviance)

# Null Model

Null_fish <- glm(Fishing_pressure ~ 1, family = "poisson", data = alldata)
summary (Null_fish)
with(summary(Null_fish), 1-deviance/null.deviance)

anova(Null_fish, model_fish, test = "Chisq")

# There is a significant difference between the null model and 
# the full model


# default backwards step

backwards <- step(model_fish)
anova(model_fish, backwards, test = "Chisq")
formula(backwards)

# backwards 

summary(backwards)

with(summary(backwards), 1-deviance/null.deviance)

backwards$deviance; model_fish$deviance

# forwards step

forwards = step(Null_fish, scope=list(lower=formula(Null_fish),upper=formula(model_fish)), direction="forward")
formula(forwards)
anova(model_fish, forwards, test = "Chisq")
summary(forwards)

# both ways step

bothways = step(Null_fish, list(lower=formula(Null_fish),upper=formula(model_fish)),
                direction="both",trace=0)

formula(bothways)
anova(model_fish, bothways, test = "Chisq")
summary(bothways)

formula (model_fish)
formula (backwards)
formula (forwards)
formula (bothways)

# Compare deviance of the models
anova(model_fish, backwards,forwards, bothways, test = "Chisq")
AIC(model_fish, backwards,forwards, bothways)
model_fish$deviance; backwards$deviance; forwards$deviance; bothways$deviance

# Best model identified by backwards  with lowest ACI 
anova(Null_fish, forwards, test = "Chisq")
with(summary(forwards), 1-deviance/null.deviance)

# explains 34.5%

formula (forwards)
summary(forwards)

## Coral_health ~ Acoustics?

hist(alldata$Coral_health)

# Count data requires poission model

model_fish <- glm (Coral_health ~ HIGH_RMS + LOW_RMS + RMS_Ratio + LOW_ROUGHNESS + HIGH_ROUGHNESS +
                     ROUGHNESS_Ratio + AC_FISH + AC_INVERTS + AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                     AEI_Ratio + BI_FISH + BI_INVERTS + BI_Ratio, family = "poisson", data = alldata)

par(mfrow = c(2,2))
plot(model_fish)
par(mfrow = c(1,1))

anova(model_fish)
summary(model_fish)
with(summary(model_fish), 1-deviance/null.deviance)

# Null Model

Null_fish <- glm(Coral_health ~ 1, family = "poisson", data = alldata)
summary (Null_fish)
with(summary(Null_fish), 1-deviance/null.deviance)

anova(Null_fish, model_fish, test = "Chisq")

# There is a significant difference between the null model and 
# the full model


# default backwards step

backwards <- step(model_fish)
anova(model_fish, backwards, test = "Chisq")
formula(backwards)

# backwards 

summary(backwards)

with(summary(backwards), 1-deviance/null.deviance)

backwards$deviance; model_fish$deviance

# forwards step

forwards = step(Null_fish, scope=list(lower=formula(Null_fish),upper=formula(model_fish)), direction="forward")
formula(forwards)
anova(model_fish, forwards, test = "Chisq")
summary(forwards)

# both ways step

bothways = step(Null_fish, list(lower=formula(Null_fish),upper=formula(model_fish)),
                direction="both",trace=0)

formula(bothways)
anova(model_fish, bothways, test = "Chisq")
summary(bothways)

formula (model_fish)
formula (backwards)
formula (forwards)
formula (bothways)

# Compare deviance of the models
anova(model_fish, backwards,forwards, bothways, test = "Chisq")
AIC(model_fish, backwards,forwards, bothways)
model_fish$deviance; backwards$deviance; forwards$deviance; bothways$deviance

# Best model identified by backwards  with lowest ACI 
anova(Null_fish, forwards, test = "Chisq")
with(summary(forwards), 1-deviance/null.deviance)

# explains 27.3%

formula (backwards)
summary(backwards)

## Algal_control ~ Acoustics?

hist(alldata$Algal_control)

# Count data requires poission model

model_fish <- glm (Algal_control ~ HIGH_RMS + LOW_RMS + RMS_Ratio + LOW_ROUGHNESS + HIGH_ROUGHNESS +
                     ROUGHNESS_Ratio + AC_FISH + AC_INVERTS + AC_Ratio + AD_FISH + AD_INVERTS + AD_Ratio + AEI_FISH + AEI_INVERTS + 
                     AEI_Ratio + BI_FISH + BI_INVERTS + BI_Ratio, family = "poisson", data = alldata)

par(mfrow = c(2,2))
plot(model_fish)
par(mfrow = c(1,1))

anova(model_fish)
summary(model_fish)
with(summary(model_fish), 1-deviance/null.deviance)

# Null Model

Null_fish <- glm(Algal_control ~ 1, family = "poisson", data = alldata)
summary (Null_fish)
with(summary(Null_fish), 1-deviance/null.deviance)

anova(Null_fish, model_fish, test = "Chisq")

# There is a significant difference between the null model and 
# the full model


# default backwards step

backwards <- step(model_fish)
anova(model_fish, backwards, test = "Chisq")
formula(backwards)

# backwards 

summary(backwards)

with(summary(backwards), 1-deviance/null.deviance)

backwards$deviance; model_fish$deviance

# forwards step

forwards = step(Null_fish, scope=list(lower=formula(Null_fish),upper=formula(model_fish)), direction="forward")
formula(forwards)
anova(model_fish, forwards, test = "Chisq")
summary(forwards)

# both ways step

bothways = step(Null_fish, list(lower=formula(Null_fish),upper=formula(model_fish)),
                direction="both",trace=0)

formula(bothways)
anova(model_fish, bothways, test = "Chisq")
summary(bothways)

formula (model_fish)
formula (backwards)
formula (forwards)
formula (bothways)

# Compare deviance of the models
anova(model_fish, backwards,forwards, bothways, test = "Chisq")
AIC(model_fish, backwards,forwards, bothways)
model_fish$deviance; backwards$deviance; forwards$deviance; bothways$deviance

# Best model identified by backwards  with lowest ACI 
anova(Null_fish, backwards, test = "Chisq")
with(summary(backwards), 1-deviance/null.deviance)

# explains 81.3%

formula (backwards)
summary(backwards)

########################################
###       Individual predictors      ###
########################################

# Coral Cover ~ AI predictor

model_coral_HIGH_RMS <- glm (Coral_cover ~ HIGH_RMS,family = "binomial", data = alldata)
model_coral_LOW_RMS <- glm (Coral_cover ~ LOW_RMS,family = "binomial", data = alldata)
model_coral_RMS_Ratio <- glm (Coral_cover ~ RMS_Ratio,family = "binomial", data = alldata)

model_coral_LOW_ROUGHNESS <- glm (Coral_cover ~ LOW_ROUGHNESS,family = "binomial", data = alldata)
model_coral_HIGH_ROUGHNESS <- glm (Coral_cover ~ HIGH_ROUGHNESS,family = "binomial", data = alldata)
model_coral_ROUGHNESS_Ratio <- glm (Coral_cover ~ ROUGHNESS_Ratio,family = "binomial", data = alldata)

model_coral_AC_FISH <- glm (Coral_cover ~ AC_FISH,family = "binomial", data = alldata)
model_coral_AC_INVERTS <- glm (Coral_cover ~ AC_INVERTS,family = "binomial", data = alldata)
model_coral_AC_Ratio <- glm (Coral_cover ~ AC_Ratio,family = "binomial", data = alldata)

model_coral_AD_FISH <- glm (Coral_cover ~ AD_FISH,family = "binomial", data = alldata)
model_coral_AD_INVERTS <- glm (Coral_cover ~ AD_INVERTS,family = "binomial", data = alldata)
model_coral_AD_Ratio <- glm (Coral_cover ~ AD_Ratio,family = "binomial", data = alldata)

model_coral_AEI_FISH <- glm (Coral_cover ~ AEI_FISH,family = "binomial", data = alldata)
model_coral_AEI_INVERTS <- glm (Coral_cover ~ AEI_INVERTS,family = "binomial", data = alldata)
model_coral_AEI_Ratio <- glm (Coral_cover ~ AEI_Ratio,family = "binomial", data = alldata)

model_coral_BI_FISH <- glm (Coral_cover ~ BI_FISH,family = "binomial", data = alldata)
model_coral_BI_INVERTS <- glm (Coral_cover ~ BI_INVERTS,family = "binomial", data = alldata)
model_coral_BI_Ratio <- glm (Coral_cover ~ BI_Ratio,family = "binomial", data = alldata)

# Null model

Null_coral <- glm(Coral_cover ~ 1,family = "binomial", data = alldata)

#Set up models as list

models<-list(model_coral_HIGH_RMS, model_coral_LOW_RMS, model_coral_RMS_Ratio, model_coral_LOW_ROUGHNESS, 
             model_coral_HIGH_ROUGHNESS, model_coral_ROUGHNESS_Ratio, model_coral_AC_FISH, model_coral_AC_INVERTS, 
             model_coral_AC_Ratio, model_coral_AD_FISH, model_coral_AD_INVERTS, model_coral_AD_Ratio, model_coral_AEI_FISH, 
             model_coral_AEI_INVERTS, model_coral_AEI_Ratio, model_coral_BI_FISH, model_coral_BI_INVERTS,
             model_coral_BI_Ratio)


# Generating the dataframe for model analysis

n <-length(models) # number of models

# generation of dataframe for GLM model info

df <- data.frame(matrix(nrow=n, ncol=6))

# name columns of dataframe
colnames(df) <- c("Model", "Intercept", "Predictor", "slope p", "Anova null", "Deviance explained" )


# begin loop to fill dataframe with GLM outputs

for(i in 1:n) {
  df[i,1] <- i # model number
  df[i,2] <- models[[i]]$coefficients[[1]]# intercept estimate
  df[i,3] <- models[[i]]$coefficients[[2]]# predictor estimate
  modelsummary<-summary(models[[i]]) #
  df[i,4] <- modelsummary$coefficients[8]#p value of slope
  #anova against null model 
  nulltest<-anova(Null_coral, models[[i]], test = "Chisq")
  df[i,5] <- nulltest$Pr[[2]] # probability model is sig different from null model 
  df[i,6] <- with(summary(models[[i]]), 1-deviance/null.deviance) # deviance explained ratio
}

df
write_xlsx(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/coral.xlsx")


# Fish Abundance ~ AI predictor

model_fish_HIGH_RMS <- glm (Fish_Abundance_2 ~ HIGH_RMS,family = "poisson", data = alldata)
model_fish_LOW_RMS <- glm (Fish_Abundance_2 ~ LOW_RMS,family = "poisson", data = alldata)
model_fish_RMS_Ratio <- glm (Fish_Abundance_2 ~ RMS_Ratio,family = "poisson", data = alldata)

model_fish_LOW_ROUGHNESS <- glm (Fish_Abundance_2 ~ LOW_ROUGHNESS,family = "poisson", data = alldata)
model_fish_HIGH_ROUGHNESS <- glm (Fish_Abundance_2 ~ HIGH_ROUGHNESS,family = "poisson", data = alldata)
model_fish_ROUGHNESS_Ratio <- glm (Fish_Abundance_2 ~ ROUGHNESS_Ratio,family = "poisson", data = alldata)

model_fish_AC_FISH <- glm (Fish_Abundance_2 ~ AC_FISH,family = "poisson", data = alldata)
model_fish_AC_INVERTS <- glm (Fish_Abundance_2 ~ AC_INVERTS,family = "poisson", data = alldata)
model_fish_AC_Ratio <- glm (Fish_Abundance_2 ~ AC_Ratio,family = "poisson", data = alldata)

model_fish_AD_FISH <- glm (Fish_Abundance_2 ~ AD_FISH,family = "poisson", data = alldata)
model_fish_AD_INVERTS <- glm (Fish_Abundance_2 ~ AD_INVERTS,family = "poisson", data = alldata)
model_fish_AD_Ratio <- glm (Fish_Abundance_2 ~ AD_Ratio,family = "poisson", data = alldata)

model_fish_AEI_FISH <- glm (Fish_Abundance_2 ~ AEI_FISH,family = "poisson", data = alldata)
model_fish_AEI_INVERTS <- glm (Fish_Abundance_2 ~ AEI_INVERTS,family = "poisson", data = alldata)
model_fish_AEI_Ratio <- glm (Fish_Abundance_2 ~ AEI_Ratio,family = "poisson", data = alldata)

model_fish_BI_FISH <- glm (Fish_Abundance_2 ~ BI_FISH,family = "poisson", data = alldata)
model_fish_BI_INVERTS <- glm (Fish_Abundance_2 ~ BI_INVERTS,family = "poisson", data = alldata)
model_fish_BI_Ratio <- glm (Fish_Abundance_2 ~ BI_Ratio,family = "poisson", data = alldata)

# Null model

Null_fish <- glm(Fish_Abundance_2 ~ 1,family = "poisson", data = alldata)

#Set up models as list

models<-list(model_fish_HIGH_RMS, model_fish_LOW_RMS, model_fish_RMS_Ratio, model_fish_LOW_ROUGHNESS, 
             model_fish_HIGH_ROUGHNESS, model_fish_ROUGHNESS_Ratio, model_fish_AC_FISH, model_fish_AC_INVERTS, 
             model_fish_AC_Ratio, model_fish_AD_FISH, model_fish_AD_INVERTS, model_fish_AD_Ratio, model_fish_AEI_FISH, 
             model_fish_AEI_INVERTS, model_fish_AEI_Ratio, model_fish_BI_FISH, model_fish_BI_INVERTS,
             model_fish_BI_Ratio)

# Generating the dataframe for model analysis

n <-length(models) # number of models

# generation of dataframe for GLM model info

df <- data.frame(matrix(nrow=n, ncol=6))

# name columns of dataframe
colnames(df) <- c("Model", "Intercept", "Predictor", "slope p", "Anova null", "Deviance explained" )


# begin loop to fill dataframe with GLM outputs

for(i in 1:n) {
  df[i,1] <- i # model number
  df[i,2] <- models[[i]]$coefficients[[1]]# intercept estimate
  df[i,3] <- models[[i]]$coefficients[[2]]# predictor estimate
  modelsummary<-summary(models[[i]]) #
  df[i,4] <- modelsummary$coefficients[8]#p value of slope
    #anova against null model 
  nulltest<-anova(Null_fish, models[[i]], test = "Chisq")
  df[i,5] <- nulltest$Pr[[2]] # probability model is sig different from null model 
  df[i,6] <- with(summary(models[[i]]), 1-deviance/null.deviance) # deviance explained ratio
}

df
write_xlsx(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/abundance.xlsx")




#########################################################################################
# Plots for GLM models for individual Acoustic Indices providing best explainatory powers
#########################################################################################

# TOP MODELS ARE AEI INVERTS/AC FISH/RMS RATIO

# AEI INVERTS

newX <- expand.grid(AEI_INVERTS = seq(from = 0.19, to = 0.8, length = 27))
newy <- predict(model_fish_AEI_INVERTS, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Fish_Abundance_2=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AEI_INVERTS, y = Fish_Abundance_2)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Acoustic Evenness Index (1200Hz - 11050Hz)")

# AC_FISH

newX <- expand.grid(AC_FISH = seq(from = 95, to = 180, length = 34))
newy <- predict(model_fish_AC_FISH, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Fish_Abundance_2=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AC_FISH, y = Fish_Abundance_2)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Acoustic Complexity Index (100-1200Hz)")


# RMS Ratio

newX <- expand.grid(RMS_Ratio = seq(from = 4, to = 35, length = 34))
newy <- predict(model_fish_RMS_Ratio, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Fish_Abundance_2=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = RMS_Ratio, y = Fish_Abundance_2)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Root Mean Square ratio")



######################################################################
#####       Fish>30 ~ AI                                       #######
######################################################################

model_fish_HIGH_RMS <- glm (Fish_great_30 ~ HIGH_RMS,family = "poisson", data = alldata)
model_fish_LOW_RMS <- glm (Fish_great_30 ~ LOW_RMS,family = "poisson", data = alldata)
model_fish_RMS_Ratio <- glm (Fish_great_30 ~ RMS_Ratio,family = "poisson", data = alldata)

model_fish_LOW_ROUGHNESS <- glm (Fish_great_30 ~ LOW_ROUGHNESS,family = "poisson", data = alldata)
model_fish_HIGH_ROUGHNESS <- glm (Fish_great_30 ~ HIGH_ROUGHNESS,family = "poisson", data = alldata)
model_fish_ROUGHNESS_Ratio <- glm (Fish_great_30 ~ ROUGHNESS_Ratio,family = "poisson", data = alldata)

model_fish_AC_FISH <- glm (Fish_great_30 ~ AC_FISH,family = "poisson", data = alldata)
model_fish_AC_INVERTS <- glm (Fish_great_30 ~ AC_INVERTS,family = "poisson", data = alldata)
model_fish_AC_Ratio <- glm (Fish_great_30 ~ AC_Ratio,family = "poisson", data = alldata)

model_fish_AD_FISH <- glm (Fish_great_30 ~ AD_FISH,family = "poisson", data = alldata)
model_fish_AD_INVERTS <- glm (Fish_great_30 ~ AD_INVERTS,family = "poisson", data = alldata)
model_fish_AD_Ratio <- glm (Fish_great_30 ~ AD_Ratio,family = "poisson", data = alldata)

model_fish_AEI_FISH <- glm (Fish_great_30 ~ AEI_FISH,family = "poisson", data = alldata)
model_fish_AEI_INVERTS <- glm (Fish_great_30 ~ AEI_INVERTS,family = "poisson", data = alldata)
model_fish_AEI_Ratio <- glm (Fish_great_30 ~ AEI_Ratio,family = "poisson", data = alldata)

model_fish_BI_FISH <- glm (Fish_great_30 ~ BI_FISH,family = "poisson", data = alldata)
model_fish_BI_INVERTS <- glm (Fish_great_30 ~ BI_INVERTS,family = "poisson", data = alldata)
model_fish_BI_Ratio <- glm (Fish_great_30 ~ BI_Ratio,family = "poisson", data = alldata)

# Null Model

Null_fish <- glm(Fish_great_30 ~ 1, family = "poisson", data = alldata)

#Set up models as list

models<-list(model_fish_HIGH_RMS, model_fish_LOW_RMS, model_fish_RMS_Ratio, model_fish_LOW_ROUGHNESS, 
             model_fish_HIGH_ROUGHNESS, model_fish_ROUGHNESS_Ratio, model_fish_AC_FISH, model_fish_AC_INVERTS, 
             model_fish_AC_Ratio, model_fish_AD_FISH, model_fish_AD_INVERTS, model_fish_AD_Ratio, model_fish_AEI_FISH, 
             model_fish_AEI_INVERTS, model_fish_AEI_Ratio, model_fish_BI_FISH, model_fish_BI_INVERTS,
             model_fish_BI_Ratio)

# Generating the dataframe for model analysis

n <-length(models) # number of models

# generation of dataframe for GLM model info

df <- data.frame(matrix(nrow=n, ncol=6))

# name columns of dataframe
colnames(df) <- c("Model", "Intercept", "Predictor", "slope p", "Anova null", "Deviance explained" )


# begin loop to fill dataframe with GLM outputs

for(i in 1:n) {
  df[i,1] <- i # model number
  df[i,2] <- models[[i]]$coefficients[[1]]# intercept estimate
  df[i,3] <- models[[i]]$coefficients[[2]]# predictor estimate
  modelsummary<-summary(models[[i]]) #
  df[i,4] <- modelsummary$coefficients[8]#p value of slope
  #anova against null model 
  nulltest<-anova(Null_fish, models[[i]], test = "Chisq")
  df[i,5] <- nulltest$Pr[[2]] # probability model is sig different from null model 
  df[i,6] <- with(summary(models[[i]]), 1-deviance/null.deviance) # deviance explained ratio
}

df
write_xlsx(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/Fish_great_30cm.xlsx")




#########################################################################################
# Plots for GLM models for individual Acoustic Indices providing best explainatory powers
#########################################################################################
################################ FISH>30

# RMS_LOW

newX <- expand.grid(LOW_RMS = seq(from = 4000, to = 20000, length = 27))
newy <- predict(model_fish_LOW_RMS, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Fish_great_30=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = LOW_RMS, y = Fish_great_30)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Abundance Fish over 30cm length ", x = "RMS (100-1200Hz)")



######################################################################
#####       FISH SPECIES DIVERSITY ~ AI                        #######
######################################################################

model_fish_HIGH_RMS <- glm (Diversity ~ HIGH_RMS,family = "poisson", data = alldata)
model_fish_LOW_RMS <- glm (Diversity ~ LOW_RMS,family = "poisson", data = alldata)
model_fish_RMS_Ratio <- glm (Diversity ~ RMS_Ratio,family = "poisson", data = alldata)

model_fish_LOW_ROUGHNESS <- glm (Diversity ~ LOW_ROUGHNESS,family = "poisson", data = alldata)
model_fish_HIGH_ROUGHNESS <- glm (Diversity ~ HIGH_ROUGHNESS,family = "poisson", data = alldata)
model_fish_ROUGHNESS_Ratio <- glm (Diversity ~ ROUGHNESS_Ratio,family = "poisson", data = alldata)

model_fish_AC_FISH <- glm (Diversity ~ AC_FISH,family = "poisson", data = alldata)
model_fish_AC_INVERTS <- glm (Diversity ~ AC_INVERTS,family = "poisson", data = alldata)
model_fish_AC_Ratio <- glm (Diversity ~ AC_Ratio,family = "poisson", data = alldata)

model_fish_AD_FISH <- glm (Diversity ~ AD_FISH,family = "poisson", data = alldata)
model_fish_AD_INVERTS <- glm (Diversity ~ AD_INVERTS,family = "poisson", data = alldata)
model_fish_AD_Ratio <- glm (Diversity ~ AD_Ratio,family = "poisson", data = alldata)

model_fish_AEI_FISH <- glm (Diversity ~ AEI_FISH,family = "poisson", data = alldata)
model_fish_AEI_INVERTS <- glm (Diversity ~ AEI_INVERTS,family = "poisson", data = alldata)
model_fish_AEI_Ratio <- glm (Diversity ~ AEI_Ratio,family = "poisson", data = alldata)

model_fish_BI_FISH <- glm (Diversity ~ BI_FISH,family = "poisson", data = alldata)
model_fish_BI_INVERTS <- glm (Diversity ~ BI_INVERTS,family = "poisson", data = alldata)
model_fish_BI_Ratio <- glm (Diversity ~ BI_Ratio,family = "poisson", data = alldata)

# Null Model

Null_fish <- glm(Diversity ~ 1, family = "poisson", data = alldata)

#Set up models as list

models<-list(model_fish_HIGH_RMS, model_fish_LOW_RMS, model_fish_RMS_Ratio, model_fish_LOW_ROUGHNESS, 
             model_fish_HIGH_ROUGHNESS, model_fish_ROUGHNESS_Ratio, model_fish_AC_FISH, model_fish_AC_INVERTS, 
             model_fish_AC_Ratio, model_fish_AD_FISH, model_fish_AD_INVERTS, model_fish_AD_Ratio, model_fish_AEI_FISH, 
             model_fish_AEI_INVERTS, model_fish_AEI_Ratio, model_fish_BI_FISH, model_fish_BI_INVERTS,
             model_fish_BI_Ratio)

# Generating the dataframe for model analysis

n <-length(models) # number of models

# generation of dataframe for GLM model info

df <- data.frame(matrix(nrow=n, ncol=6))

# name columns of dataframe
colnames(df) <- c("Model", "Intercept", "Predictor", "slope p", "Anova null", "Deviance explained" )


# begin loop to fill dataframe with GLM outputs

for(i in 1:n) {
  df[i,1] <- i # model number
  df[i,2] <- models[[i]]$coefficients[[1]]# intercept estimate
  df[i,3] <- models[[i]]$coefficients[[2]]# predictor estimate
  modelsummary<-summary(models[[i]]) #
  df[i,4] <- modelsummary$coefficients[8]#p value of slope
  #anova against null model 
  nulltest<-anova(Null_fish, models[[i]], test = "Chisq")
  df[i,5] <- nulltest$Pr[[2]] # probability model is sig different from null model 
  df[i,6] <- with(summary(models[[i]]), 1-deviance/null.deviance) # deviance explained ratio
}

df
write_xlsx(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/Diversity.xlsx")


##########################################################################
################ CORAL HEALTH (Coralivores)~ AI  #########################
##########################################################################

model_fish_HIGH_RMS <- glm (Coral_health ~ HIGH_RMS,family = "poisson", data = alldata)
model_fish_LOW_RMS <- glm (Coral_health ~ LOW_RMS,family = "poisson", data = alldata)
model_fish_RMS_Ratio <- glm (Coral_health ~ RMS_Ratio,family = "poisson", data = alldata)

model_fish_LOW_ROUGHNESS <- glm (Coral_health ~ LOW_ROUGHNESS,family = "poisson", data = alldata)
model_fish_HIGH_ROUGHNESS <- glm (Coral_health ~ HIGH_ROUGHNESS,family = "poisson", data = alldata)
model_fish_ROUGHNESS_Ratio <- glm (Coral_health ~ ROUGHNESS_Ratio,family = "poisson", data = alldata)

model_fish_AC_FISH <- glm (Coral_health ~ AC_FISH,family = "poisson", data = alldata)
model_fish_AC_INVERTS <- glm (Coral_health ~ AC_INVERTS,family = "poisson", data = alldata)
model_fish_AC_Ratio <- glm (Coral_health ~ AC_Ratio,family = "poisson", data = alldata)

model_fish_AD_FISH <- glm (Coral_health ~ AD_FISH,family = "poisson", data = alldata)
model_fish_AD_INVERTS <- glm (Coral_health ~ AD_INVERTS,family = "poisson", data = alldata)
model_fish_AD_Ratio <- glm (Coral_health ~ AD_Ratio,family = "poisson", data = alldata)

model_fish_AEI_FISH <- glm (Coral_health ~ AEI_FISH,family = "poisson", data = alldata)
model_fish_AEI_INVERTS <- glm (Coral_health ~ AEI_INVERTS,family = "poisson", data = alldata)
model_fish_AEI_Ratio <- glm (Coral_health ~ AEI_Ratio,family = "poisson", data = alldata)

model_fish_BI_FISH <- glm (Coral_health ~ BI_FISH,family = "poisson", data = alldata)
model_fish_BI_INVERTS <- glm (Coral_health ~ BI_INVERTS,family = "poisson", data = alldata)
model_fish_BI_Ratio <- glm (Coral_health ~ BI_Ratio,family = "poisson", data = alldata)

# Null Model

Null_fish <- glm(Coral_health ~ 1, family = "poisson", data = alldata)

#Set up models as list

models<-list(model_fish_HIGH_RMS, model_fish_LOW_RMS, model_fish_RMS_Ratio, model_fish_LOW_ROUGHNESS, 
             model_fish_HIGH_ROUGHNESS, model_fish_ROUGHNESS_Ratio, model_fish_AC_FISH, model_fish_AC_INVERTS, 
             model_fish_AC_Ratio, model_fish_AD_FISH, model_fish_AD_INVERTS, model_fish_AD_Ratio, model_fish_AEI_FISH, 
             model_fish_AEI_INVERTS, model_fish_AEI_Ratio, model_fish_BI_FISH, model_fish_BI_INVERTS,
             model_fish_BI_Ratio)

# Generating the dataframe for model analysis

n <-length(models) # number of models

# generation of dataframe for GLM model info

df <- data.frame(matrix(nrow=n, ncol=6))

# name columns of dataframe
colnames(df) <- c("Model", "Intercept", "Predictor", "slope p", "Anova null", "Deviance explained" )


# begin loop to fill dataframe with GLM outputs

for(i in 1:n) {
  df[i,1] <- i # model number
  df[i,2] <- models[[i]]$coefficients[[1]]# intercept estimate
  df[i,3] <- models[[i]]$coefficients[[2]]# predictor estimate
  modelsummary<-summary(models[[i]]) #
  df[i,4] <- modelsummary$coefficients[8]#p value of slope
  #anova against null model 
  nulltest<-anova(Null_fish, models[[i]], test = "Chisq")
  df[i,5] <- nulltest$Pr[[2]] # probability model is sig different from null model 
  df[i,6] <- with(summary(models[[i]]), 1-deviance/null.deviance) # deviance explained ratio
}

df
write_xlsx(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/Corallivores.xlsx")

#########################################################################################
# Plots for GLM models for individual Acoustic Indices providing best explainatory powers
#########################################################################################
################################ Coral health indicator species

# NOTE: ALL <15% explanatory but showing the only 3 significant graphs


# AE_Ratio

newX <- expand.grid(AEI_Ratio = seq(from = 0.01, to = 1, length = 34))
newy <- predict(model_fish_AEI_Ratio, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Coral_health=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AEI_Ratio, y = Coral_health)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Abundance of Coral Health indicator species", x = "Acoustic Evenness Ratio")

# AEI 0 - 1200 Hz

newX <- expand.grid(AEI_FISH = seq(from = 0.03, to = 0.26, length = 34))
newy <- predict(model_fish_AEI_FISH, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Coral_health=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AEI_FISH, y = Coral_health)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Abundance of Coral Health indicator species", x = "Acoustic Evenness Index (0-1200Hz)")

##########################################################################
################ FISHING PRESSURE (Fish Indicator families ~ AI  #########
##########################################################################

model_fish_HIGH_RMS <- glm (Fishing_pressure ~ HIGH_RMS,family = "poisson", data = alldata)
model_fish_LOW_RMS <- glm (Fishing_pressure ~ LOW_RMS,family = "poisson", data = alldata)
model_fish_RMS_Ratio <- glm (Fishing_pressure ~ RMS_Ratio,family = "poisson", data = alldata)

model_fish_LOW_ROUGHNESS <- glm (Fishing_pressure ~ LOW_ROUGHNESS,family = "poisson", data = alldata)
model_fish_HIGH_ROUGHNESS <- glm (Fishing_pressure ~ HIGH_ROUGHNESS,family = "poisson", data = alldata)
model_fish_ROUGHNESS_Ratio <- glm (Fishing_pressure ~ ROUGHNESS_Ratio,family = "poisson", data = alldata)

model_fish_AC_FISH <- glm (Fishing_pressure ~ AC_FISH,family = "poisson", data = alldata)
model_fish_AC_INVERTS <- glm (Fishing_pressure ~ AC_INVERTS,family = "poisson", data = alldata)
model_fish_AC_Ratio <- glm (Fishing_pressure ~ AC_Ratio,family = "poisson", data = alldata)

model_fish_AD_FISH <- glm (Fishing_pressure ~ AD_FISH,family = "poisson", data = alldata)
model_fish_AD_INVERTS <- glm (Fishing_pressure ~ AD_INVERTS,family = "poisson", data = alldata)
model_fish_AD_Ratio <- glm (Fishing_pressure ~ AD_Ratio,family = "poisson", data = alldata)

model_fish_AEI_FISH <- glm (Fishing_pressure ~ AEI_FISH,family = "poisson", data = alldata)
model_fish_AEI_INVERTS <- glm (Fishing_pressure ~ AEI_INVERTS,family = "poisson", data = alldata)
model_fish_AEI_Ratio <- glm (Fishing_pressure ~ AEI_Ratio,family = "poisson", data = alldata)

model_fish_BI_FISH <- glm (Fishing_pressure ~ BI_FISH,family = "poisson", data = alldata)
model_fish_BI_INVERTS <- glm (Fishing_pressure ~ BI_INVERTS,family = "poisson", data = alldata)
model_fish_BI_Ratio <- glm (Fishing_pressure ~ BI_Ratio,family = "poisson", data = alldata)



# Null Model

Null_fish <- glm(Fishing_pressure ~ 1, family = "poisson", data = alldata)

#Set up models as list

models<-list(model_fish_HIGH_RMS, model_fish_LOW_RMS, model_fish_RMS_Ratio, model_fish_LOW_ROUGHNESS, 
             model_fish_HIGH_ROUGHNESS, model_fish_ROUGHNESS_Ratio, model_fish_AC_FISH, model_fish_AC_INVERTS, 
             model_fish_AC_Ratio, model_fish_AD_FISH, model_fish_AD_INVERTS, model_fish_AD_Ratio, model_fish_AEI_FISH, 
             model_fish_AEI_INVERTS, model_fish_AEI_Ratio, model_fish_BI_FISH, model_fish_BI_INVERTS,
             model_fish_BI_Ratio)

# Generating the dataframe for model analysis

n <-length(models) # number of models

# generation of dataframe for GLM model info

df <- data.frame(matrix(nrow=n, ncol=6))

# name columns of dataframe
colnames(df) <- c("Model", "Intercept", "Predictor", "slope p", "Anova null", "Deviance explained" )


# begin loop to fill dataframe with GLM outputs

for(i in 1:n) {
  df[i,1] <- i # model number
  df[i,2] <- models[[i]]$coefficients[[1]]# intercept estimate
  df[i,3] <- models[[i]]$coefficients[[2]]# predictor estimate
  modelsummary<-summary(models[[i]]) #
  df[i,4] <- modelsummary$coefficients[8]#p value of slope
  #anova against null model 
  nulltest<-anova(Null_fish, models[[i]], test = "Chisq")
  df[i,5] <- nulltest$Pr[[2]] # probability model is sig different from null model 
  df[i,6] <- with(summary(models[[i]]), 1-deviance/null.deviance) # deviance explained ratio
}

df
write_xlsx(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/Fishing_pressure.xlsx")

#########################################################################################
# Plots for GLM models for individual Acoustic Indices providing best explainatory powers
#########################################################################################
################################ Fishing Pressure indicator species

# LOW_RMS

newX <- expand.grid(LOW_RMS = seq(from = 4000, to = 20000, length = 34))
newy <- predict(model_fish_LOW_RMS, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Fishing_pressure=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = LOW_RMS, y = Fishing_pressure)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance (Fishing Pressure)", x = "RMS (0 - 1200Hz)")



##########################################################################
################ ALGAL CONTROL (Fish Indicator families ~ AI)    #########
##########################################################################

model_fish_HIGH_RMS <- glm (Algal_control ~ HIGH_RMS,family = "poisson", data = alldata)
model_fish_LOW_RMS <- glm (Algal_control ~ LOW_RMS,family = "poisson", data = alldata)
model_fish_RMS_Ratio <- glm (Algal_control ~ RMS_Ratio,family = "poisson", data = alldata)

model_fish_LOW_ROUGHNESS <- glm (Algal_control ~ LOW_ROUGHNESS,family = "poisson", data = alldata)
model_fish_HIGH_ROUGHNESS <- glm (Algal_control ~ HIGH_ROUGHNESS,family = "poisson", data = alldata)
model_fish_ROUGHNESS_Ratio <- glm (Algal_control ~ ROUGHNESS_Ratio,family = "poisson", data = alldata)

model_fish_AC_FISH <- glm (Algal_control ~ AC_FISH,family = "poisson", data = alldata)
model_fish_AC_INVERTS <- glm (Algal_control ~ AC_INVERTS,family = "poisson", data = alldata)
model_fish_AC_Ratio <- glm (Algal_control ~ AC_Ratio,family = "poisson", data = alldata)

model_fish_AD_FISH <- glm (Algal_control ~ AD_FISH,family = "poisson", data = alldata)
model_fish_AD_INVERTS <- glm (Algal_control ~ AD_INVERTS,family = "poisson", data = alldata)
model_fish_AD_Ratio <- glm (Algal_control ~ AD_Ratio,family = "poisson", data = alldata)

model_fish_AEI_FISH <- glm (Algal_control ~ AEI_FISH,family = "poisson", data = alldata)
model_fish_AEI_INVERTS <- glm (Algal_control ~ AEI_INVERTS,family = "poisson", data = alldata)
model_fish_AEI_Ratio <- glm (Algal_control ~ AEI_Ratio,family = "poisson", data = alldata)

model_fish_BI_FISH <- glm (Algal_control ~ BI_FISH,family = "poisson", data = alldata)
model_fish_BI_INVERTS <- glm (Algal_control ~ BI_INVERTS,family = "poisson", data = alldata)
model_fish_BI_Ratio <- glm (Algal_control ~ BI_Ratio,family = "poisson", data = alldata)

# Null Model

Null_fish <- glm(Algal_control ~ 1, family = "poisson", data = alldata)

#Set up models as list

models<-list(model_fish_HIGH_RMS, model_fish_LOW_RMS, model_fish_RMS_Ratio, model_fish_LOW_ROUGHNESS, 
             model_fish_HIGH_ROUGHNESS, model_fish_ROUGHNESS_Ratio, model_fish_AC_FISH, model_fish_AC_INVERTS, 
             model_fish_AC_Ratio, model_fish_AD_FISH, model_fish_AD_INVERTS, model_fish_AD_Ratio, model_fish_AEI_FISH, 
             model_fish_AEI_INVERTS, model_fish_AEI_Ratio, model_fish_BI_FISH, model_fish_BI_INVERTS,
             model_fish_BI_Ratio)

# Generating the dataframe for model analysis

n <-length(models) # number of models

# generation of dataframe for GLM model info

df <- data.frame(matrix(nrow=n, ncol=6))

# name columns of dataframe
colnames(df) <- c("Model", "Intercept", "Predictor", "slope p", "Anova null", "Deviance explained" )


# begin loop to fill dataframe with GLM outputs

for(i in 1:n) {
  df[i,1] <- i # model number
  df[i,2] <- models[[i]]$coefficients[[1]]# intercept estimate
  df[i,3] <- models[[i]]$coefficients[[2]]# predictor estimate
  modelsummary<-summary(models[[i]]) #
  df[i,4] <- modelsummary$coefficients[8]#p value of slope
  #anova against null model 
  nulltest<-anova(Null_fish, models[[i]], test = "Chisq")
  df[i,5] <- nulltest$Pr[[2]] # probability model is sig different from null model 
  df[i,6] <- with(summary(models[[i]]), 1-deviance/null.deviance) # deviance explained ratio
}

df
write_xlsx(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/Algal_control.xlsx")

#########################################################################################
# Plots for GLM models for individual Acoustic Indices providing best explainatory powers
#########################################################################################
################################ Algal Control indicator species

# AE_Ratio

newX <- expand.grid(AEI_Ratio = seq(from = 0.01, to = 1, length = 34))
newy <- predict(model_fish_AEI_Ratio, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Algal_control=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AEI_Ratio, y = Algal_control)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Acoustic Evenness Ratio")

# ACI 1200 - 11050 Hz

newX <- expand.grid(AC_INVERTS = seq(from = 1000, to = 1450, length = 34))
newy <- predict(model_fish_AC_INVERTS, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Algal_control=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AC_INVERTS, y = Algal_control)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Acoustic Complexity Index 1200 - 11050 Hz")

# AEI 0 - 1200 Hz

newX <- expand.grid(AEI_FISH = seq(from = 0.03, to = 0.26, length = 34))
newy <- predict(model_fish_AEI_FISH, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Algal_control=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AEI_FISH, y = Algal_control)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance (Fishing Pressure)", x = "Roughness (1200-11050Hz)")

# ADI 0 - 1200 Hz

newX <- expand.grid(AD_FISH = seq(from = 2.3, to = 2.5, length = 34))
newy <- predict(model_fish_AD_FISH, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Algal_control=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AD_FISH, y = Algal_control)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance (Fishing Pressure)", x = "Roughness (1200-11050Hz)")

# ACI 0 - 1200

newX <- expand.grid(AC_FISH = seq(from = 95, to = 170, length = 34))
newy <- predict(model_fish_AC_FISH, newdata = newX, type = "link", se.fit = TRUE)
pd <- data.frame(newX, newy) # this brings in all the values generated for x and y
pd <-mutate(pd, Algal_control=exp(fit))
pd <- mutate(pd, ucl=exp(fit + 1.96*se.fit))
pd <- mutate(pd, lcl=exp(fit - 1.96*se.fit))

# Plot the graph
ggplot(alldata, aes(x = AC_FISH, y = Algal_control)) +  
  geom_point(colour = 'cornflowerblue', size = 1, alpha = 0.5) +
  geom_smooth(aes(ymin=lcl, ymax=ucl), data = pd, stat='identity') +
  theme_bw(base_size = 15) +
  labs(y= "Fish Abundance", x = "Acoustic Complexity Index 100 - 1200 Hz")

