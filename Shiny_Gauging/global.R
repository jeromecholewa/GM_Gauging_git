library("dplyr")
library("readxl")
library(plotly)
#library(data.table)
library(zoo)
library(xlsx)
library(pracma) #for findpeaks

# Some global variables
chinaHeader <- c("Height1(mm)", "Height2(mm)", "Resistance1(\xa6\xb8)",
                 "Resistance2(\xa6\xb8)", "Current Fuel Capacity(L)",
                 "Flow Rate(L/H)", "Deformation(mm)", "Output current(A)",
                 "Output voltage(V)", "Test Time(min)")
chinaHeaderXL <- c("Height1(mm)", "Height2(mm)", "Resistance1(¦¸)",
                   "Resistance2(¦¸)", "Current Fuel Capacity(L)",
                   "Flow Rate(L/H)", "Deformation(mm)", "Output current(A)",
                   "Output voltage(V)", "Test Time(min)")



############################
# The function below creates a dataframe  for each segment
#   average value, min, max, std dev, length (number of data points)
padTable <- function(pad_listt) {
    length_list <- length(pad_listt)
    pad_nb <- 1:length_list
    pad_averages <- 1:length_list
    pad_stdev <- 1:length_list
    pad_length <- 1:length_list
    pad_min <- 1:length_list
    pad_max <- 1:length_list
    pad_range <- 1:length_list
    pad_ohm_gap2next <- 1:length_list
    pad_list_tablee <- as.data.frame(cbind (pad_nb, pad_averages,
                                            pad_min, pad_max, pad_range,
                                            pad_length, pad_stdev,
                                            pad_ohm_gap2next))
    # calculate ohm average, std dev and length within each pad
    for (i in 1:length_list) {
        pad_list_tablee$pad_averages[i] <-mean(pad_listt[[i]])
        pad_list_tablee$pad_stdev[i] <- sd(pad_listt[[i]])
        pad_list_tablee$pad_length[i] <- length(pad_listt[[i]])
        pad_list_tablee$pad_min[i] <- min(pad_listt[[i]])
        pad_list_tablee$pad_max[i] <- max(pad_listt[[i]])
        pad_list_tablee$pad_range[i] <- pad_list_tablee$pad_max[i] - pad_list_tablee$pad_min[i]

    }

    #### DETERMINE THE gap  (in OHMS) between 2 segments
    for (i in 1:(length_list-1)) {
        pad_list_tablee$pad_ohm_gap2next[i] <- abs(pad_list_tablee$pad_averages[i+1] -
                                                       pad_list_tablee$pad_averages[i])
    }
    pad_list_tablee$pad_ohm_gap2next[length_list] <- pad_list_tablee$pad_ohm_gap2next[length_list-1]

    return(pad_list_tablee)
}

##############
##### This function detects the peaks in the vector of rolling std deviation
##### it creates a small dataframe of value of peaks, peak indices
##### and gaps between peaks
peakTableFunc <- function(peakByRegion, vectRollStd, LLength) {
  # peakByRegion <- rev(as.numeric( unlist(strsplit(gsub(" ", "",
  #                                                      peakByRegionInput,
  #                                                      fixed = TRUE),
  #                                                 ",", fixed = TRUE))))
  #
  # peakByRegion[peakByRegion == 0 | is.na(peakByRegion)] <- 0.15

  #length_g <- length(gauging$Ohms)
  peaksIndex <- data.frame(V1 = 0, V2 = 0)
  lengPbR <- length(peakByRegion)
  for (i in 0:(lengPbR-1)) {
    # peaksTable <- findpeaks(gauging$rollMeanOfStdev[(floor((length_g %/% lengPbR) * i)+1):min((floor((length_g %/% lengPbR) * (i+1)))+20, length_g)],
    #                         nups = 5, zero = "0", peakpat = NULL,
    #                         minpeakheight = peakByRegion[i+1],
    #                         minpeakdistance = 1,
    #                         threshold = 0, npeaks = 0, sortstr = FALSE)
    peaksTable <- findpeaks(vectRollStd[(floor((LLength %/% lengPbR) * i)+1):min((floor((LLength %/% lengPbR) * (i+1)))+20, LLength)],
                            nups = 5, zero = "0", peakpat = NULL,
                            minpeakheight = peakByRegion[i+1],
                            minpeakdistance = 1,
                            threshold = 0, npeaks = 0, sortstr = FALSE)
    if (!is.null(peaksTable)) {
      peaksTable <- as.data.frame(peaksTable)[,1:2]
      # peaksTable[,2] <- peaksTable[,2] + floor((length_g %/% lengPbR) * i)
      peaksTable[,2] <- peaksTable[,2] + floor((LLength %/% lengPbR) * i)
      peaksIndex <- rbind(peaksIndex, peaksTable)
    }
  }
  # i <- 1
  # str(peaksTable)
  # str(as.data.frame(peaksTable))
  # is.null(peaksTable)

  peaksIndex <- peaksIndex[-1,]
  rownames(peaksIndex) <- NULL   # resets the row names from 1 without "holes"

  peaksIndex$gap2next <- 0
  for (i in 1:(length(peaksIndex$V1)-1)) {
    peaksIndex$gap2next[i] <- peaksIndex[i+1,"V2"] - peaksIndex[i,"V2"]
  }
  peaksIndex$gap2next[length(peaksIndex$V1)] <- mean(peaksIndex$gap2next[1:length(peaksIndex$V1)-1])
  peaksIndex$keep <- TRUE

  #### remove redundant peaks
  peaksIndex <-  peaksIndex[ peaksIndex$gap2next != 0 ,]

  #### correcly position the peak (which seems to be biased towards the higher "Liters" value
  peaksIndex$V2 <- peaksIndex$V2 +2

  # peaksIndexNew <- peaksIndex
  # head(peaksIndex,15)
  # head(peaksIndexNew,15)

  #meanPeakGapMin <- mean(peaksIndex$gap2next)/4

  # peaksIndex[peaksIndex$gap2next <meanPeakGapMin,]

  ##################  eliminate peaks too close to another bigger peak.
  #################    NOT FINISHED - NOT READY FOR RELEASE
  # for (i in 1:(length(peaksIndex$V1)-1)) {
  #     if (peaksIndex$gap2next[i] < meanPeakGapMin  ) {
  #
  #     }
  # }
  ################
  ################
  return(peaksIndex)

}

