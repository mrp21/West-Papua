###########################################################################
# R code to extract acoustic idices from wav files stored in a directory and
# generate a csv file with file name and all acoustic idices from SEEWAVE and Soundecology

#Install SEEWAVE and supporting packages

#install.packages(c("fftw","tuneR","rgl","rpanel"), repos="http://cran.at.r-project.org/")
#install.packages("seewave", repos="http://cran.at.r-project.org/")

library(seewave)
library(fftw)
library(tuneR)
library(rpanel)

# Step 1: Clear R's Brain (rm = remove, ls = list all objects)

rm(list=ls())

# set working directory 
setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS")

############################# IMPORTANT USER SELECTION########################
# LOCATE THE DIRECTORY WITH THE AUDIO FILES FOR ANALYSIS

Audio_files_dir <- "F:/INDONESIA_2019/AUDIO/"

files <- list.files(path=Audio_files_dir, pattern="*.wav", full.names=TRUE, recursive=FALSE)

############################# IMPORTANT USER SELECTION########################
# Set the time slice for analysis in minutes

timestartmin <- 4 # start of audio clip in minutes
timestopmin <- 7 # end of audio clip in minutes

##############################################################################


timestart <- timestartmin*60 # start of audio clip in seconds
timestop <- timestopmin*60# end of audio clip in seconds

# Set the low/high frequency band

lowbandlow = 100
lowbandhigh = 1200
highbandlow = 1200
highbandhigh = 11050

# Generating the dataframe for model analysis

n <-length(files) # number of replicate files

df <- data.frame(matrix(nrow=n, ncol=19))

# name columns of dataframe

colnames(df) <- c("FILE", "LOW_RMS","HIGH_RMS","RMS_Ratio","LOW_ROUGHNESS",
                  "HIGH_ROUGHNESS","ROUGHNESS_Ratio","AC_FISH", "AC_INVERTS", 
                  "AC_Ratio","AD_FISH", "AD_INVERTS", "AD_Ratio", "AEI_FISH", 
                  "AEI_INVERTS", "AEI_Ratio", "BI_FISH", "BI_INVERTS","BI_Ratio")
colnames(df[[1]])

colnames[[1]]
# begin loop to fill dataframe with SEEWAVE outputs: RMS and Roughness



for(i in 1:n) {
  df[i, 1] <- files[[i]] # model name/default to model number if gets complex!
  
  wav <- readWave(files[[i]])
  
  wavsection <- extractWave(wav, from = timestart, to = timestop, xunit = "time")
  
  # low frequency band-pass 
  
  lowfreq<-fir(wavsection,from=lowbandlow,to=lowbandhigh)
  #spectro(lowfreq, f=48000)
  
  # high frequency band pass
  
  highfreq<-fir(wavsection,from=highbandlow,to=highbandhigh)
  #spectro(highfreq, f=48000)
  
  df[[i,2]] <- rms(lowfreq)
  df[[i,3]] <- rms(highfreq)
  df[[i,4]] <- df[[i,2]]/df[[i,3]]
  df[[i,5]] <- roughness(lowfreq)
  df[[i,6]] <- roughness(highfreq)
  df[[i,7]] <- df[[i,5]]/df[[i,6]]
  }


###############################################################################
# Soundecology package for other Indices

#install soundecology

#install.packages("soundecology")

library(soundecology)

#  Acoustic Complexity Index (ACI): from Pieretti, et al. 2011. The ACI is based on the "observation
#  that many biotic sounds, such as bird songs, are characterized by an intrinsic variability of intensities, 
#  while some types of human generated noise (such as car passing or airplane transit) present
#  very constant intensity values" (Pieretti, et al. 2011)

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/AC_LOW.csv",
                soundindex = "acoustic_complexity", min_freq = 100, max_freq = 1200, j = 5, from = timestart, to = timestop, units="seconds")

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/AC_HIGH.csv",
                soundindex = "acoustic_complexity", min_freq = 1200, max_freq = 11050, j = 5, from = timestart, to = timestop, units="seconds")

# Acoustic Diversity Index from Villanueva-Rivera et al. 2011. The ADI is calculated by dividing
# the spectrogram into bins (default 10) and taking the proportion of the signals in each bin above a
# threshold (default -50 dBFS). The ADI is the result of the Shannon index applied to these bins.

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/AD_LOW.csv",
                soundindex = "acoustic_diversity", max_freq = 1200, db_threshold = -50, 
                freq_step = 100, from = timestart, to = timestop, units="seconds")

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/AD_HIGH.csv",
                soundindex = "acoustic_diversity", max_freq = 11000, db_threshold = -50, 
                freq_step = 1000, from = timestart, to = timestop, units="seconds")

# Acoustic Evenness Index from Villanueva-Rivera et al. 2011 (band evenness using the Gini index).
# The AEI is calculated by dividing the spectrogram into bins (default 10) and taking the proportion
# of the signals in each bin above a threshold (default -50 dBFS). The AEI is the result of the Gini
# index applied to these bins.

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/AEI_LOW.csv",
                soundindex = "acoustic_evenness", max_freq = 1200, db_threshold = -50, 
                freq_step = 100, from = timestart, to = timestop, units="seconds")

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/AEI_HIGH.csv",
                soundindex = "acoustic_evenness", max_freq = 11000, db_threshold = -50, 
                freq_step = 1000, from = timestart, to = timestop, units="seconds")

# Bioacoustic Index is calculated as the "area under each curve included all frequency bands
# associated with the dB value that was greater than the minimum dB value for each curve. The area
# values are thus a function of both the sound level and the number of frequency bands used by the
# avifauna" (Boelman, et al. 2007)

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/BI_LOW.csv",
                soundindex = "bioacoustic_index", min_freq = 100, max_freq = 1200, fft_w = 512, from = timestart, to = timestop, units="seconds")

multiple_sounds(directory = Audio_files_dir,
                resultfile = "C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS/BI_HIGH.csv",
                soundindex = "bioacoustic_index", min_freq = 1200, max_freq = 11050, fft_w = 512, 
                from = timestart, to = timestop, units="seconds")

#############################################
#Import the csv files for Acoustic indices and extract values to df
setwd("C:/Users/mrp21/Dropbox/R/IDCF_Analysis/R CODE/DATASETS")
ACI_LOW <- read.csv('AC_LOW.csv')
ACI_HIGH <- read.csv('AC_HIGH.csv')
ADI_LOW <- read.csv('AD_LOW.csv')
ADI_HIGH <- read.csv('AD_HIGH.csv')
AEI_LOW <- read.csv('AEI_LOW.csv')
AEI_HIGH <- read.csv('AEI_HIGH.csv')
BI_LOW <- read.csv('BI_LOW.csv')
BI_HIGH <- read.csv('BI_HIGH.csv')

for(i in 1:n) {  

df[[i,8]] <- ACI_LOW[[i, 11]] 
df[[i,9]] <- ACI_HIGH[[i, 11]] 
df[[i,10]] <- ACI_LOW[[i, 11]]/ACI_HIGH[[i, 11]] # ACI ratio
df[[i,11]] <- ADI_LOW[[i, 10]] 
df[[i,12]] <- ADI_HIGH[[i, 10]] 
df[[i,13]] <- ADI_LOW[[i, 10]]/ADI_HIGH[[i, 10]] # ADI ratio 
df[[i,14]] <- AEI_LOW[[i, 10]] 
df[[i,15]] <- AEI_HIGH[[i, 10]] 
df[[i,16]] <- AEI_LOW[[i, 10]]/AEI_HIGH[[i, 10]] #AEI ratio
df[[i,17]] <- BI_LOW[[i, 10]]
df[[i,18]] <- BI_HIGH[[i, 10]]
df[[i,19]] <- BI_LOW[[i, 10]]/BI_HIGH[[i, 10]] # BI ratio


}

df
#write.csv(df, path = "ACOUSTIC_INDICES.csv")
write.csv(df,"C:/Users/mrp21/Dropbox/R/IDCF_Analysis/RESULTS/ACOUSTIC_INDICES_4_7.csv", row.names = FALSE)
