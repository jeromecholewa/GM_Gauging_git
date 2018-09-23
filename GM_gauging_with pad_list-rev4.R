# This script will actually identify each pad and be able to assign each
# observation to a particular pad (= segment).
# The principle is:
#   1. to smoothened the values by calculating a rolling mean. This will flatten out
#                local outliers
#   2. Calculate a rolling standard dev of that rolling mean. This will show peaks
#                 at each segment change
#   3. Locate these local peaks (by taking a rolling max of this rolling stdev) by the
#                 time of the observation
#   4. Assign a "Change flag to 1" for each of these peak
#   5. Create a pad list thanks to this change flag
#   6. Process the GM table thanks to this pad list.

library("dplyr")
library("readxl")
library(xlsx) # for write.xlsx
library(zoo)  # for rollmean (but we can just use zoo::rollmean)
library(plotly)
library(pracma)
#library(data.table)
#library("beepr")

# The initial file .csv must be "clean" which means:
#   - NO HEADERS
#   - the data starts directly at the line 1, in the cell(B1), with no titles - volume in liters
#   - the data has 9 columns: counting column without title(A), Liters(B), H1 (C), H2 (D),
#                             Ohms1 (E),  Ohms2 (F), and (G), (H) (I)
#                             ##########

#setwd("~/GM Korea/E2XX Korea/PVP/R-transformation/9BUX Raw data of AWD_FWD_Drain_Filling/9BUX Raw data of AWD Main Drain/")  # enter correct folder for original file   ON PC

setwd("~/Documents/Education/Coursera/Datascience with R/GM_gauging/")  # enter correct folder for original file   ON MAC

rm(list = ls())

###########
# characteristics of the tank and project
dyn_unuse <- 0  # acc to GMW 14474, must be given by GM
useable_vol <- 50
##########

filename <- "China/S_002 AWD filling" #  REMOVE the '.xlsx' or '.csv" from original file name  S_002 FWD draining for xlsx;  S_002 AWD filling for csv
#filename <- "E2SC_DSL_56L_6_Fill_Curve" #  REMOVE the '.xlsx' or '.csv" from original file name  E2SC_DSL_56L_6_Fill_Curve

### FIRST WE NEED TO CHECK THE FILE
# so we download only a few lines ith header
#
# gauging <- data.frame(read_excel(paste(filename, ".xlsx", sep = ""),
#                                  sheet = 1, n_max = 10,
#                                  #skip = 1,
#                                  na = "", col_names = FALSE))

gauging <- read.csv(paste(filename, ".csv", sep = ""), nrows = 10,
                    header = FALSE,
                    #encoding="chinese", # useless
                    stringsAsFactors=FALSE)

str(gauging)
chinaHeader <- c("Height1(mm)", "Height2(mm)", "Resistance1(\xa6\xb8)",
                 "Resistance2(\xa6\xb8)", "Current Fuel Capacity(L)",
                 "Flow Rate(L/H)", "Deformation(mm)", "Output current(A)",
                 "Output voltage(V)", "Test Time(min)")
chinaHeaderXL <- c("Height1(mm)", "Height2(mm)", "Resistance1(¦¸)",
                   "Resistance2(¦¸)", "Current Fuel Capacity(L)",
                   "Flow Rate(L/H)", "Deformation(mm)", "Output current(A)",
                   "Output voltage(V)", "Test Time(min)")


if (identical(chinaHeader, as.character(gauging[1,]))) {
  print("china")
  gauging <- read.csv(paste(filename, ".csv", sep = ""),
                      header = FALSE, skip = 1) # need to skip
                              # 1st row because of encoding problem

  # gauging <- data.frame(read_excel(paste(filename, ".xlsx", sep = ""),
  #                                  sheet = 1,
  #                                  na = "", col_names = TRUE))
  # head(gauging)
  # str(gauging)

  names(gauging) <- chinaHeader

  gauging <- gauging[, c("Test Time(min)", "Current Fuel Capacity(L)",
                         "Height1(mm)",
                         "Resistance1(\xa6\xb8)",
                         "Height2(mm)",
                         "Resistance2(\xa6\xb8)")]
  # str(gauging)
  colnames(gauging) <- c( "Time_s", "Liters", "mm", "Ohms", "H2",  "Ohms2" )
} else if (dim(gauging)[2] == 9 && gauging[1,7] == 0 && gauging[1,8] == 0) {
  print("not China")
  gauging <- read.csv(paste(filename, ".csv", sep = ""),
                      header = FALSE)
  gauging <-  as.data.frame(sapply(gauging[,1:9], as.numeric))
  colnames(gauging) <- c( "Time_s", "Liters", "mm", "H2", "Ohms",
                          "Ohms2" , "G", "H", "I" )

  # Remove the G H I columns (c(7,8,9)) by keeping the rest
  gauging <- gauging[,c("Time_s", "Liters", "mm", "Ohms", "H2", "Ohms2")]

} else   print("Wrong")

##################

# gauging <- read.csv(paste(filename, ".csv", sep = ""),
#                     header = FALSE)
# gauging <-  as.data.frame(sapply(gauging[,1:9], as.numeric))
# gauging_x <- data.frame(read_excel(paste(filename, ".xlsx", sep = ""),
#                                    sheet = 1,
#                                    #skip = 1,
#                                    na = "", col_names = FALSE))
# str(gauging_x)
# gauging_x <- as.data.frame(sapply(gauging_x[,1:9], as.numeric))
# head(gauging,25)
# dim(gauging)


# str(gauging)
# str(gauging_x)
# head(gauging, 5)
# head(gauging_x, 5)

# if (dim(gauging)[2] != 9 || dim(gauging)[1] <1000 ) {
#     print("Wrong")
#     }
# colnames(gauging) <- c( "Time_s", "Liters", "mm", "H2", "Ohms",
#                         "Ohms2" , "G", "H", "I" )

# Remove the G H I columns (c(7,8,9)) by keeping the rest
#gauging <- gauging[,c("Time_s", "Liters", "mm", "Ohms", "H2", "Ohms2")]


#sum(is.na(gauging$H2))
gauging$H2[is.na(gauging$H2)] <- 0

# correct "programmer" way
secondary <- mean(gauging$H2) > 3


#head(gauging,10)

#remove the 2 columns of the filling direction, if we want only the DRAINING direction
#gauging <- gauging[,-(2:3)]
#head(gauging,10)
# tail(gauging)
#str(gauging)

# remove all the lines at the end/beginngin of the dataframe where Liters is NA (useless lines).
gauging <- gauging[!(is.na(gauging$Liters)),]
#tail(gauging,10)

# replace the NA of the "mm" and "H2" column by 0
gauging[is.na(gauging$mm),"mm"] <-  0
gauging[is.na(gauging$H2), "H2"] <-  0

#head(gauging,6)
#head(gauging[,"mm"],6)
# replace the first NA of the "Ohms" and "Ohms2" column with the first actual measured value
gauging[is.na(gauging$Ohms),"Ohms"] <- gauging[!is.na(gauging$Ohms),"Ohms"][1]
gauging[is.na(gauging$Ohms2),"Ohms2"] <- gauging[!is.na(gauging$Ohms2),"Ohms2"][1]

# gauging_origin <- gauging
#head(gauging,10)
# reverses the order of the dataframe to have descending liters
# no matter whether the test was draining or filling
gauging <- gauging[with(gauging, order(-Liters)), ]
rownames(gauging) <- NULL   # resets the row names from 1 without "holes"

#gauging <- arrange(gauging, -row_number()) # not accurate since we do not control
#  by which column we sort

gauging$Liters <- gauging$Liters - dyn_unuse

length_g <- length(gauging$Ohms)
#gauging$row <- 1:length_g  ## is it useful later??

#################
################# START the processing for PRIMARY SENDER

rolling_k <- 40  # sample size to calculate rolling mean
gauging$rollMean <- rollmean(gauging$Ohms, rolling_k, fill = 0)
# gauging$xx <- "x"   # will be used to match each data point with 1 single pad

gauging$rollMean[1:(round(rolling_k/2)+2)] <- gauging$rollMean[round(rolling_k/2)+3]
gauging$rollMean[(length_g-(round(rolling_k/2)+2)):length_g] <- gauging$rollMean[length_g-(round(rolling_k/2)+3)]

# add the difference between Raw ohm and rolling mean
#gauging$diff <- gauging$rollMean - gauging$Ohms

# add Ohms / 35  (for display during exploration)
# gauging$ohm_div <-  gauging$Ohms / 35

# add rollmean/ 35  (for display during exploration)
# gauging$rollMean_div <-  gauging$rollMean / 35

#head(gauging,25)
# tail(gauging, 25)
# adding rolling standard deviation
gauging$rollStd <- sqrt((rolling_k/(rolling_k-1)) *
                            (abs(rollmean((gauging$rollMean)^2,
                                           rolling_k, fill = 0) -
                                 rollmean(gauging$rollMean,
                                               rolling_k, fill = 0)^2)))

# gauging$rollStd
# sum(is.nan(gauging$rollStd))

# gauging$rollStd[1:(round(rolling_k/2)+2)] <- gauging$rollStd[round(rolling_k/2)+3]
# gauging$rollStd[(length_g-(round(rolling_k/2)+2)):length_g] <- gauging$rollStd[length_g-(round(rolling_k/2)+3)]



####### GOOD
# gauging$rollStd <- sqrt((rolling_k/(rolling_k-1)) *
#                             (zoo::rollmean((gauging$rollMean)^2, rolling_k, fill = 0) -
#                                  zoo::rollmean(gauging$rollMean, rolling_k, fill = 0)^2))
# gauging$rollStd[1:(round(rolling_k/2)+2)] <- gauging$rollStd[round(rolling_k/2)+3]
# gauging$rollStd[(length_g-(round(rolling_k/2)+2)):length_g] <- gauging$rollStd[length_g-(round(rolling_k/2)+3)]
#

# str(gauging$rollStd)
# mean(gauging$rollStd)
# max(gauging$rollStd)
# min(gauging$rollStd)
# sum(is.nan(gauging$rollStd))

# RROO <- zoo::rollmean(gauging$Ohms^2, rolling_k, fill = 0)
# str(RROO)
# mean(RROO)
# max(RROO)
# min(RROO)
# RO2 <- zoo::rollmean(gauging$Ohms, rolling_k, fill = 0)^2
# str(RO2)
# mean(RO2)
# max(RO2)
# min(RO2)


# mean(gauging$Ohms)
# sum(is.nan(gauging$Ohms))
# sum(is.na(gauging$Ohms))


# adding rolling mean of that roll Stdev (deviation)!!! In order to smoothen the peaks
rolling_k2 <- 30 # to smoothen the rolling std Dev
gauging$rollMeanOfStdev <- rollmean(gauging$rollStd, rolling_k2, fill = 0)
# gauging$xx <- "x"   # will be used to match each data point with 1 single pad

# gauging$rollMeanOfStdev[1:(round(rolling_k2/2)+2)] <- gauging$rollMeanOfStdev[round(rolling_k2/2)+3]
# gauging$rollMeanOfStdev[(length_g-(round(rolling_k2/2)+2)):length_g] <- gauging$rollMeanOfStdev[length_g-(round(rolling_k2/2)+3)]

#sum(is.nan(gauging$rollMeanOfStdev))

# head(gauging,23)

##############################################
# We will now assign each observation (from rolling means) to a pad or delete it (considered an outlier)
#pad_list <- list(0,0,0) # initialize the list of pad to 3 vectors containing
                        # each the value 0 only
######## initialization
# pad_number <- 1
# counter_in_same_pad <- 1
# pad_list[[1]][1] <- gauging$Ohms[1]
#############
# for (i in 1:(length(gauging$Ohms)-1)) {
#   if (abs(gauging$Ohms[i+1] - gauging$Ohms[i]) < pad_gap) {
#     counter_in_same_pad <- counter_in_same_pad+1
#     pad_list[[pad_number]][counter_in_same_pad] <- gauging$Ohms[i+1]
#   }
#   else {
#     pad_number <- pad_number+1
#     counter_in_same_pad <- 1
#     pad_list[[pad_number]] <- 0
#     pad_list[[pad_number]][counter_in_same_pad] <- gauging$Ohms[i+1]
#
#   }
# }

######### finding local peaks 0.239
#peakByRegionInput<- "0.35, 0.30,0.25, 0.20,  0.22"  #
peakByRegionInputPrim <- "0.35, 0.30,0.25, 0.20,  0.22"  # app default primary: 0.35, 0.30,0.25, 0.20,  0.22
# str(peakByRegionInput)
# length(peakByRegionInput)

peakByRegionPrim <- rev(as.numeric( unlist(strsplit(gsub(" ", "",
                                                     peakByRegionInputPrim,
                                                     fixed = TRUE),
                                                ",", fixed = TRUE))))

peakByRegionPrim[peakByRegionPrim == 0 | is.na(peakByRegionPrim)] <- 0.15

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
  #pad_ok_min <- 1:length_list
  #pad_ok_max <- 1:length_list
  pad_range <- 1:length_list
  pad_ohm_gap2next <- 1:length_list
  #ok_gap2next <- 1:length_list
  #xx <- 1:length_list
  pad_list_tablee <- as.data.frame(cbind (pad_nb, pad_averages,
                                          #xx,
                                          pad_min, pad_max, pad_range,
                                          #pad_ok_min, pad_ok_max, ok_gap2next
                                          pad_length, pad_stdev, pad_ohm_gap2next
                                          ))
  # calculate ohm average, std dev and length within each pad
  for (i in 1:length_list) {
    #pad_list_tablee$xx[i] <- "x"
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

peaksIndexPrim <- peakTableFunc(peakByRegionPrim,
                                gauging$rollMeanOfStdev,
                                length_g)
# i <- 1
# min_peak <- 0.35
# peaksTableF <- findpeaks(gauging$rollStd, nups = 5, zero = "0", peakpat = NULL, minpeakheight = min_peak, minpeakdistance = 1, threshold = 0, npeaks = 0, sortstr = FALSE)
# peaksTableDF <- as.data.frame(peaksTableF[,1:2])
#
# identical(peaksIndex, peaksTableDF)   # TRUE


# create "pad_change" column in the "gauging" table
gauging$padChange <- 0
gauging[peaksIndexPrim[,2], "padChange"] <- 1
# head(gauging, 50)
# tail(gauging, 50)

# create "pad_nb" column by cumulative calculation of pad_change
gauging$pad_nb <- cumsum(gauging$padChange) + 1


######## AT THIS POINT we will associate each volume point to a pad number
## So we will build the pad list, simply based on the pad number!
## but now we are recording the raw Ohm values, not the rolling mean

lengg <- max(gauging$pad_nb)
pad_list_final <- vector("list", length = lengg) # initialize the list of segments
########
for (i in 1:lengg) {
    pad_list_final[[i]] <- gauging[gauging$pad_nb == i, "Ohms"]
}

# pad_list_final
# length(pad_list_final)

# get all points from the pad_list
# pad_vector <- unlist(pad_list_final, recursive = TRUE)
# length(pad_vector)
# length(gauging$Ohms)
#head(pad_vector, 50)
#head(gauging$Ohms, 50)

pad_list_table_final <- padTable(pad_list_final)
# head(pad_list_table_final)
# pad_list_table_final$pad_length
# max(pad_list_table_final$pad_stdev)

#gauging[gauging$pad_nb == 3, "Ohms"]
# pad_list_final[[3]]

# head(pad_list_table_roll)
#############
######  The next for loop will get min Liter and max Liter for each pad.
# pad_list_table_roll$Liter_start <- pad_list_table_roll$pad_nb
# pad_list_table_roll$Liter_end <- pad_list_table_roll$pad_nb
# pad_list_table_roll$liter_gap <- pad_list_table_roll$pad_nb
# for (i in pad_list_table_roll$pad_nb) {
#   pad_list_table_roll$Liter_start[i] <- min(final_DF[final_DF$pad_nb == i , ]$Liters)
#   pad_list_table_roll$Liter_end[i] <- max(final_DF[final_DF$pad_nb == i , ]$Liters)
# }
# head(pad_list_table_roll,5)
# tail(pad_list_table_roll,6)

#head(pad_list_table_final)

pad_list_table_final$Liter_Start <- pad_list_table_final$pad_nb
pad_list_table_final$Liter_Stop <- pad_list_table_final$pad_nb
pad_list_table_final$Height_Start <- pad_list_table_final$pad_nb
pad_list_table_final$Height_Stop <- pad_list_table_final$pad_nb

for (i in pad_list_table_final$pad_nb) {
    pad_list_table_final$Liter_Start[i] <- min(gauging[gauging$pad_nb == i , ]$Liters)
    pad_list_table_final$Liter_Stop[i] <- max(gauging[gauging$pad_nb == i , ]$Liters)
    pad_list_table_final$Height_Start[i] <- tail(gauging[gauging$pad_nb == i,"mm"],1)
    pad_list_table_final$Height_Stop[i] <- head(gauging[gauging$pad_nb == i,"mm"],1)
}

###### correcting overlaps of heights
for (i in 1:(lengg-1)) {
  if (pad_list_table_final$Height_Start[i] < pad_list_table_final$Height_Stop[i+1]) {
    pad_list_table_final$Height_Start[i] <- (pad_list_table_final$Height_Start[i] + pad_list_table_final$Height_Stop[i+1])/2 + 0.01
    pad_list_table_final$Height_Stop[i+1] <- pad_list_table_final$Height_Start[i] -0.02

  }
}

# head(gauging[gauging$pad_nb == 12,],10)
# tail(gauging[gauging$pad_nb == 12,],10)
# gauging[gauging$pad_nb == 12,][1,"mm"]
# gauging[gauging$pad_nb == 12,][length(gauging[gauging$pad_nb == 12,]$mm),
#                                "mm"]
# tail(gauging[gauging$pad_nb == 12,"mm"],1)
# head(gauging[gauging$pad_nb == 12,"mm"],1)

#### Just for info, we are displaying the gap in liters separating 2 segments
# this is just to check that there are no big gaps between segments
pad_list_table_final$liter_gap <- pad_list_table_final$pad_nb
for (i in 1:length(pad_list_table_final$pad_nb)-1) {
    pad_list_table_final$liter_gap[i] <- abs(pad_list_table_final$Liter_Stop[i+1] - pad_list_table_final$Liter_Start[i])
}

############### Insert a pad mid point marker (padChange = 0.5 to position plot text correctly)
pad_list_table_final$padMidPos <- round(pad_list_table_final$pad_length[1] / 2)
for ( i in 2:lengg) {
  pad_list_table_final$padMidPos[i] <- pad_list_table_final$padMidPos[i-1] +
    round(pad_list_table_final$pad_length[i-1] / 2)+
    round(pad_list_table_final$pad_length[i] / 2)
}

gauging[pad_list_table_final$padMidPos, ]$padChange <- 0.5



############################
###### BUILDING THE GM TABLE (for Primary level sensor)
GM_table <- pad_list_table_final[,c("pad_nb","pad_averages", "Height_Start",
                                    "Height_Stop", "Liter_Start",
                                    "Liter_Stop", "liter_gap", "pad_stdev",
                                    "pad_ohm_gap2next")]
GM_table$Liter_Start[GM_table$Liter_Start <0] <-  0
GM_table$Liter_Stop[GM_table$Liter_Stop <0] <-  0

#head(GM_table)

names(GM_table) <- c("Resistance Change", "Primary Level Sensor Ohms",
                     "Height Start", "Height Stop",
                     "Start Liters UsableFuel InTank",
                     "End Liters UsableFuel InTank", "Liter gap",
                     "Segment stddev",
                     "Segment ohm gap to next")

GM_table$`Secondary Level Sensor Ohms` <- 0
GM_table$`Start Percent UsableFuel InTank` <- 100 * GM_table$`Start Liters UsableFuel InTank` / useable_vol
GM_table$`End Percent UsableFuel InTank` <- 100 * GM_table$`End Liters UsableFuel InTank` / useable_vol
GM_table$`Primary Percent 5V Ref` <- 100 * GM_table$`Primary Level Sensor Ohms` / (243+GM_table$`Primary Level Sensor Ohms`)
GM_table$`Secondary Percent 5V Ref` <- 100 * GM_table$`Secondary Level Sensor Ohms` / (243+GM_table$`Secondary Level Sensor Ohms`)

##### We put all columns in correct order
GM_table <-GM_table[,c("Resistance Change", "Primary Level Sensor Ohms",
                       "Secondary Level Sensor Ohms",
                       "Height Start", "Height Stop",
                       "Start Percent UsableFuel InTank", "End Percent UsableFuel InTank",
                       "Start Liters UsableFuel InTank", "End Liters UsableFuel InTank",
                       "Primary Percent 5V Ref", "Secondary Percent 5V Ref", "Liter gap",
                       "Segment stddev",
                       "Segment ohm gap to next")]

#head(GM_table, 5)
#head(pad_list_table_final)
#tail(GM_table, 10)
#tail(pad_list_table_final, 10)

#GM_table[8:15, ]
#pad_list_table_final[8:15, ]

###############
############### NOW CALCULATION OF SEGMENTS FOR SECONDARY SENDER
if (secondary) {

  rolling_kSec <- 36  # sample size to calculate rolling mean
  gauging$rollMeanSec <- rollmean(gauging$Ohms2, rolling_kSec, fill = 0)
  # gauging$xx <- "x"   # will be used to match each data point with 1 single pad

  gauging$rollMeanSec[1:(round(rolling_kSec/2)+2)] <- gauging$rollMeanSec[round(rolling_kSec/2)+3]
  gauging$rollMeanSec[(length_g-(round(rolling_kSec/2)+2)):length_g] <- gauging$rollMeanSec[length_g-(round(rolling_kSec/2)+3)]

  # adding rolling standard deviation
  gauging$rollStdSec <- sqrt((rolling_kSec/(rolling_kSec-1)) *
                               (abs(rollmean((gauging$rollMeanSec)^2,
                                             rolling_kSec, fill = 0) -
                                      rollmean(gauging$rollMeanSec,
                                               rolling_kSec, fill = 0)^2)))

  # adding rolling mean of that roll Stdev (deviation)!!! In order to smoothen the peaks
  rolling_k2Sec <- 20 # to smoothen the rolling std Dev
  gauging$rollMeanOfStdevSec <- rollmean(gauging$rollStdSec,
                                         rolling_k2Sec, fill = 0)

  ######### finding local peaks
  peakByRegionInputSec <- "0.40, 0.25,0.20, 0.30, 0.22, 0.25"  # app default Sec : 0.40, 0.25,0.20, 0.30, 0.22, 0.25  # app default primary: 0.35, 0.30,0.25, 0.20,  0.22
  peakByRegionSec <- rev(as.numeric( unlist(strsplit(gsub(" ", "",
                                                           peakByRegionInputSec,
                                                           fixed = TRUE),
                                                      ",", fixed = TRUE))))

  peakByRegionSec[peakByRegionSec == 0 | is.na(peakByRegionSec)] <- 0.15

  peaksIndexSec <- peakTableFunc(peakByRegionSec,
                                  gauging$rollMeanOfStdevSec,
                                  length_g)


  # create "padChangeSec" column for Secondary sender
  gauging$padChangeSec <- 0
  gauging[peaksIndexSec[,2], "padChangeSec"] <- 1
  # head(gauging[peaksIndexSec, ])
  # tail(gauging[peaksIndexSec, ])

  # create "pad_nbSec" column by cumulative calculation of padChangeSec
  gauging$pad_nbSec <- cumsum(gauging$padChangeSec) + 1

  ######## AT THIS POINT we will associate each volume point to a pad number
  ## So we will build the pad list, simply based on the pad number!
  ## but now we are recording the raw Ohm values, not the rolling mean

  lenggSec <- max(gauging$pad_nbSec)
  pad_list_finalSec <- vector("list", length = lenggSec) # initialize the list of segments
  ########
  for (i in 1:lenggSec) {
    pad_list_finalSec[[i]] <- gauging[gauging$pad_nbSec == i, "Ohms2"]
  }

  pad_list_table_finalSec <- padTable(pad_list_finalSec)
  #############
  ######  The next for loop will get min Liter and max Liter for each pad.
  pad_list_table_finalSec$Liter_start <- pad_list_table_finalSec$pad_nb
  pad_list_table_finalSec$Liter_end <- pad_list_table_finalSec$pad_nb
  for (i in pad_list_table_finalSec$pad_nb) {
    pad_list_table_finalSec$Liter_start[i] <- min(gauging[gauging$pad_nbSec == i , ]$Liters)
    pad_list_table_finalSec$Liter_end[i] <- max(gauging[gauging$pad_nbSec == i , ]$Liters)
  }

  #### Just for info, we are displaying the gap in liters separating 2 segments
  # this is just to check that there are no big gaps between segments
  pad_list_table_finalSec$liter_gap <- pad_list_table_finalSec$pad_nb
  for (i in 1:length(pad_list_table_finalSec$pad_nb)-1) {
    pad_list_table_finalSec$liter_gap[i] <- abs(pad_list_table_finalSec$Liter_end[i+1] - pad_list_table_finalSec$Liter_start[i])
  }

  ######### Insert a pad mid point marker (padChange = 0.5 to position plot text correctly)
  pad_list_table_finalSec$padMidPos <- round(pad_list_table_finalSec$pad_length[1] / 2)
  for ( i in 2:lenggSec) {
    pad_list_table_finalSec$padMidPos[i] <- pad_list_table_finalSec$padMidPos[i-1] +
      round(pad_list_table_finalSec$pad_length[i-1] / 2)+
      round(pad_list_table_finalSec$pad_length[i] / 2)
  }

  gauging[pad_list_table_finalSec$padMidPos, ]$padChangeSec <- 0.5



  ############################
  ###### BUILDING THE GM TABLE (for SECONDARY level sensor)
  GM_tableSec <- pad_list_table_finalSec[,c("pad_nb","pad_averages",
                                            "Liter_start",
                                            "Liter_end", "liter_gap",
                                            "pad_stdev", "pad_ohm_gap2next")]
  GM_tableSec$Liter_start[GM_tableSec$Liter_start <0] <-  0
  GM_tableSec$Liter_end[GM_tableSec$Liter_end <0] <-  0

  names(GM_tableSec) <- c("Resistance Change", "Secondary Level Sensor Ohms",
                          "Start Liters UsableFuel InTank",
                          "End Liters UsableFuel InTank", "Liter gap",
                          "Segment stddev",
                          "Segment ohm gap to next")

  GM_tableSec$`Primary Level Sensor Ohms` <- 0
  GM_tableSec$`Start Percent UsableFuel InTank` <- 100 * GM_tableSec$`Start Liters UsableFuel InTank` / useable_vol
  GM_tableSec$`End Percent UsableFuel InTank` <- 100 * GM_tableSec$`End Liters UsableFuel InTank` / useable_vol
  GM_tableSec$`Primary Percent 5V Ref` <- 100 * GM_tableSec$`Primary Level Sensor Ohms` / (243+GM_tableSec$`Primary Level Sensor Ohms`)
  GM_tableSec$`Secondary Percent 5V Ref` <- 100 * GM_tableSec$`Secondary Level Sensor Ohms` / (243+GM_tableSec$`Secondary Level Sensor Ohms`)

  ##### We put all columns in correct order
  GM_tableSec <-GM_tableSec[,c("Resistance Change", "Primary Level Sensor Ohms", "Secondary Level Sensor Ohms",
                               "Start Percent UsableFuel InTank", "End Percent UsableFuel InTank",
                               "Start Liters UsableFuel InTank", "End Liters UsableFuel InTank",
                               "Primary Percent 5V Ref", "Secondary Percent 5V Ref", "Liter gap",
                               "Segment stddev",
                               "Segment ohm gap to next")]

} else
{
  GM_tableSec <- data.frame(first = 0,
                             Ohms = "NO SECONDARY SENDER")
}

########## END OF SECONDARY



# print the matrix as XL file
write.xlsx( x = GM_table,
            file = paste( filename, "_GM.xlsx", sep = ""),
            sheetName = "GM_GaugingSheet_Primary",
            row.names = FALSE)

if (secondary) {
  write.xlsx(x = GM_tableSec,
           file = paste( filename, "_GM.xlsx", sep = ""),
           sheetName="GM_GaugingSheet_Secondary",
           append=TRUE, row.names=FALSE)
}


# PLOTTING Ohms and rolling means + rolling stdev of rolling mean (to see the peaks)

gauging$couleur <- gauging$pad_nb %% 4
gauging$couleur[gauging$couleur == 0] <- 4
# unique(gauging$couleur)
# gauging$couleur[900:1800]
###### rollStDev for display only (x20  for visibility on the graph)
displayFactor <- 40
gauging$rollStdDisplay <- displayFactor * gauging$rollStd
gauging$rollMeanOfStdevDisplay <- displayFactor * gauging$rollMeanOfStdev
peakByRegionDisplay <- peakByRegionPrim * displayFactor ##############

thresholdByRegion <- data.frame(threshold = peakByRegionPrim, index_x = 1,
                                index_xend = length_g, volx = 0,
                                volxend= max(gauging$Liters))

#head(gauging)
# (floor((length_g %/% lengPbR) * i)+1):(floor((length_g %/% lengPbR) * (i+1)))

lengPbR <- length(peakByRegionPrim)
for ( i in 1:(lengPbR-1) ) {
    thresholdByRegion$index_x[i+1] <- floor((length_g %/% lengPbR) * i)
    thresholdByRegion$index_xend[i] <-  thresholdByRegion$index_x[i+1]
    thresholdByRegion$volx[i] <- gauging$Liters[thresholdByRegion$index_xend[i]]
    thresholdByRegion$volxend[i+1] <- thresholdByRegion$volx[i]
}


showrollStdDev <- TRUE
showOhms <-  TRUE
showRollMean <- TRUE

xMin <- -1
xMax <- 55
xMinSec <- -1
xMaxSec <- 55
yMin <- -5
yMax <- 270
yMinSec <- -5
yMaxSec <- 270

## define colors for threshold and for raw data + StDev
couleursThreshold <- c('red', 'green', 'blue', 'orange')
couleursOhms <- c('orange', 'blue', 'green', 'black')
valuesColors_threshold <- vector(mode = "character",
                                 length = length(thresholdByRegion$threshold))
valuesColors_threshold[] <- couleursThreshold
names(valuesColors_threshold) <- paste0(as.character(thresholdByRegion$index_x),
                                        "prim")

#####################################    G G P L O T   Primary
##### for plotly GRAPH SEE DOWN BELOW
ggplotGauging <- function() {

  g <- ggplot(gauging, aes(Liters)) +
    theme(plot.title = element_text(hjust = 0.8,
                                    margin = margin(t = 30, b = -50))) +
    labs(x="Volume (liters)", y="Resistance (Ohms)",
         title=paste0("\U25CF Raw data (multicolor)                                    \n\U2500 smooth line (rolling mean) (blue continuous)\n\U25CF Rolling StdDev (x",
                      displayFactor,
                      " for visibility) (multicolor)"))  #+

  g <-  g + coord_cartesian(ylim = c(yMin, yMax),
                            xlim = c(xMin, xMax))   + #
    # coord_cartesian(ylim = c(-2, 100),
    #                 xlim = c(14, 21 ))
    scale_x_continuous(expand=c(0,0), breaks = round(seq(xMin, xMax, length.out = 25), 1)) +
    scale_y_continuous(expand=c(0,0), breaks = round(seq(yMin, yMax, length.out = 30), 1))

  g <- g + theme(panel.background = element_rect(fill = 'white'),
                 panel.grid.major = element_line(colour = "lightgrey",
                                                 size = 0.2),
                 axis.line = element_line(colour = "black"))

  g <-  g + geom_segment(data = thresholdByRegion, show.legend=F,
                         aes(x = volx,  xend = volxend,
                             y = threshold * displayFactor,
                             yend = threshold * displayFactor,
                             colour = paste0(as.character(thresholdByRegion$index_x),
                                             "prim"))) +
    scale_color_manual(values=valuesColors_threshold)  # We need a named vector with at least
     # the same length as the data   #  it seems valuesColors_All is not even necessary?


  if (showOhms) {
    g <-  g + geom_point( aes(y = Ohms, fill = as.character(couleur)),
                          color = "transparent", show.legend=F,
                          pch = 21, size = 1.5)
    g <-  g + scale_fill_manual(values=couleursOhms)  # couleursOhms works without being a named vector
    g <-  g + geom_text( aes(y = Ohms, label=ifelse(padChange ==0.5 & pad_nb %% 2 == 0 ,as.character(pad_nb),'')),
                         # hjust=1.4,
                         vjust= 1.7, size = 3)
  }

  if (showRollMean) {
    g <- g + geom_line(aes(Liters, rollMean), colour = 'blue')
  }

  if (showrollStdDev) {
    g <-  g + geom_point(aes(y = rollMeanOfStdevDisplay, fill = as.character(couleur)),
                         color = "transparent",
                         show.legend=F,
                         pch = 21, size = 0.7)
  }
g

}

ggplotGauging()

############ plot 2nd graph (SECONDARY)

gauging$couleurSec <- gauging$pad_nbSec %% 4
gauging$couleurSec[gauging$couleurSec == 0] <- 4
displayFactorSec <- 45
gauging$rollStdDisplaySec <- displayFactorSec * gauging$rollStdSec
gauging$rollMeanOfStdevDisplaySec <- displayFactorSec * gauging$rollMeanOfStdevSec
#peakByRegionDisplaySec <- peakByRegionSec * displayFactorSec ##############

thresholdByRegionSec <- data.frame(threshold = peakByRegionSec, index_x = 1,
                                   index_xend = length_g, volx = 0,
                                   volxend= max(gauging$Liters))

#head(gauging)

lengPbRSec <- length(peakByRegionSec)
for ( i in 1:(lengPbRSec-1) ) {
  thresholdByRegionSec$index_x[i+1] <- floor((length_g %/% lengPbRSec) * i)
  thresholdByRegionSec$index_xend[i] <-  thresholdByRegionSec$index_x[i+1]
  thresholdByRegionSec$volx[i] <- gauging$Liters[thresholdByRegionSec$index_xend[i]]
  thresholdByRegionSec$volxend[i+1] <- thresholdByRegionSec$volx[i]
}

showrollStdDevSec <- TRUE
showOhmsSec <-  TRUE
showRollMeanSec <- TRUE


ggplotGaugingSec <- function () {


  #SHOULD  BE IMPLEMENTED
  valuesColors_thresholdSec <- vector(mode = "character",
                                   length = length(thresholdByRegionSec$threshold))
  valuesColors_thresholdSec[] <- couleursThreshold
  names(valuesColors_thresholdSec) <-paste0(as.character(thresholdByRegionSec$index_x),
                                         "Sec")


  gsec <- ggplot(gauging, aes(Liters)) +
    theme(plot.title = element_text(hjust = 0.8,
                                    margin = margin(t = 30, b = -50))) +
    labs(x="Volume (liters)", y="Resistance (Ohms)",
         title=paste0("SECONDARY\n\U25CF Raw data (multicolor)                                    \n\U2500 smooth line (rolling mean) (blue continuous)\n\U25CF Rolling StdDev (x",
                      displayFactorSec,
                      " for visibility) (multicolor)"))  #+

  gsec <-  gsec + coord_cartesian(ylim = c(yMinSec, yMaxSec),
                                  xlim = c(xMinSec, xMaxSec))    + #
    # coord_cartesian(ylim = c(-2, 100),
    #                 xlim = c(14, 21 ))
    scale_x_continuous(expand=c(0,0), breaks = round(seq(xMinSec,
                                                         xMaxSec, length.out = 25), 1)) +
    scale_y_continuous(expand=c(0,0), breaks = round(seq(yMinSec,
                                                         yMaxSec, length.out = 30), 1))

  gsec <- gsec + theme(panel.background = element_rect(fill = 'white'),
                 panel.grid.major = element_line(colour = "lightgrey",
                                                 size = 0.2),
                 axis.line = element_line(colour = "black"))

  gsec <-  gsec + geom_segment(data = thresholdByRegionSec, show.legend=F,
                         aes(x = volx,  xend = volxend,
                             y = threshold * displayFactorSec,
                             yend = threshold * displayFactorSec,
                             colour = paste0(as.character(thresholdByRegionSec$index_x),
                                             "Sec"))) +
    scale_color_manual(values=valuesColors_thresholdSec)  # We need a named vector with at least
  # the same length as the data   #  it seems valuesColors_All is not even necessary?


  if (showOhmsSec) {
    gsec <-  gsec + geom_point( aes(y = Ohms2, fill = as.character(couleurSec)),
                          color = "transparent", show.legend=F,
                          pch = 21, size = 1.5)
    gsec <-  gsec + scale_fill_manual(values=couleursOhms)  # couleursOhms works without being a named vector
    gsec <-  gsec + geom_text( aes(y = Ohms2,
                                   label=ifelse(padChangeSec ==0.5 & pad_nbSec %% 2 == 0 ,
                                                as.character(pad_nbSec),'')),
                         # hjust=1.4,
                         vjust= 1.7, size = 3)
  }

  if (showRollMeanSec) {
    gsec <- gsec + geom_line(aes(Liters, rollMeanSec), colour = 'blue')
  }

  if (showrollStdDevSec) {
    gsec <-  gsec + geom_point(aes(y = rollMeanOfStdevDisplaySec, fill = as.character(couleurSec)),
                         color = "transparent",
                         show.legend=F,
                         pch = 21, size = 0.7)
  }
  gsec


}

if (secondary) {
  ggplotGaugingSec()

} else {
  plot(x = 5, y = 3, main="THERE IS NO SECONDARY SENDER",
       xlab="Liters",
       ylab="Secondary Resistance (Ohms)", las=1)
}










#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################














plotGauging <- function() {
  p <- plot_ly(gauging,x = ~Liters)

  if (showOhms) {
    p <- p %>%
      add_trace( y = ~Ohms,type = "scatter", mode ="markers",
                 sizes = .1, color = ~couleur,
                 name = 'Raw Resistances', hoverinfo = 'text',
                 text = ~paste('Pad_Nb: ', pad_nb,
                               '</br></br> Volume: ', Liters,
                               '</br> Ohms: ', Ohms))
  }

  if (showRollMean) {
    p <- p %>%
      add_trace( y = ~rollMean,type = "scatter", mode ="lines+markers",
                 marker = list(size = 2, opacity = 0.3, color = "blue"),
                 line = list(opacity = 0.3, dash = 'dot', color = "blue"),
                 #sizes = .1,
                 name = 'Rolling Mean Resistances\n   = smoothened',
                 hoverinfo = 'text',
                 text = ~paste('Pad_Nb: ', pad_nb,
                               '</br></br> Volume: ', Liters,
                               '</br> RollMean: ', rollMean))
  }

  if (showrollStdDev) {
    # p <- p %>%
    #   add_trace( y = ~rollStdDisplay, type = "scatter", mode ="markers",
    #             sizes = .1, color = ~couleur, name = 'Rolling StDev (x26)',
    #             hoverinfo = 'text',
    #             text = ~paste('Pad_Nb: ', pad_nb,
    #                           '</br> </br> Volume: ', Liters,
    #                           '</br> rollStdDisplay: ',
    #                           rollStd))
    p <- p %>%
      add_trace( y = ~rollMeanOfStdevDisplay, type = "scatter",
                 mode ="markers",
                 #line = list(color = ''black),
                 #marker = list(color = 'red'),
                 sizes = 0.5, color = ~couleur,
                 name = paste0('Rolling Mean of rolling StDev (x',
                               displayFactor, ')'),
                 hoverinfo = 'text',
                 text = ~paste('Pad_Nb: ', pad_nb,
                               '</br> </br> Volume: ', Liters,
                               '</br> rollMeanOfStdevDisplay: ',
                               rollMeanOfStdev))
  }


  #                  %>%
  # add_trace(x = ~Liters, y = ~Ohms,type = "scatter", mode ="markers",
  #           sizes = .1, color = ~couleur,
  #           name = 'Raw Resistances', hoverinfo = 'text',
  #           text = ~paste('Pad_Nb: ', pad_nb,
  #                         '</br></br> Volume: ', Liters,
  #                         '</br> Ohms: ', Ohms)) %>%
  # add_trace(x = ~Liters, y = ~rollStdDisplay, type = "scatter", mode ="markers",
  #           sizes = .1, color = ~couleur, name = 'Rolling StDev (x20)',
  #           hoverinfo = 'text',
  #           text = ~paste('Pad_Nb: ', pad_nb,
  #                         '</br> </br> Volume: ', Liters,
  #                         '</br> rollStdDisplay: ',
  #                         rollStdDisplay)) %>%
  # add_trace(x = ~Liters, y = ~rollMeanOfStdevDisplay, type = "scatter", mode ="markers",
  #           sizes = .1, color = ~couleur, name = 'Rolling Mean of rolling StDev (x20)',
  #           hoverinfo = 'text',
  #           text = ~paste('Pad_Nb: ', pad_nb,
  #                         '</br> </br> Volume: ', Liters,
  #                         '</br> rollMeanOfStdevDisplay: ',
  #                         rollMeanOfStdevDisplay)) %>%
  # # add_trace(x = ~Liters, y = ~pad_nb_Display,type = "scatter", mode ="markers",
  #           sizes = .1, color = I("yellow"), name = 'Pad_number') %>%
  # add_trace(x = ~Liters, y = ~pad_nb_up_Display,type = "scatter", mode ="markers",
  #           sizes = .1, color = I("pink"), name = 'Pad_number_up') %>%
  p <- p %>%
    layout(title = paste0("Raw line and smooth line (rolling average) &\nrolling StDev (x",
                          displayFactor, " for visibility)"),
           # shapes=list(type='line', x0= thresholdByRegion$volx[3],
           #             x1= thresholdByRegion$volxend[3],
           #             y0=thresholdByRegion$threshold[3] * displayFactor,
           #             y1=thresholdByRegion$threshold[3] * displayFactor,
           #             line=list(dash='dot', width=1)),
           xaxis = list(title = "Volume (liters)"),
           yaxis = list(title = "Resistance (Ohms)"),
           legend = list(x = 0.7, y = 0.9)

    ) %>%
    hide_colorbar()
  #p

  # p2 <- p
  # p <- p2

  # for (i in 1:lengPbR) {     # works but very slow and adds in the legend!!
  #     p <- p %>% add_segments( x = thresholdByRegion$volx[i] ,
  #                              y = thresholdByRegion$threshold[i]* displayFactor,
  #                              xend = thresholdByRegion$volxend[i],
  #                              yend = thresholdByRegion$threshold[i]* displayFactor,
  #                              showlegend = FALSE)
  # }
  # p

  for (i in 1:lengPbR) {   # WORKS
    p <- p %>% add_lines(x = c(thresholdByRegion$volx[i],
                               thresholdByRegion$volxend[i]),
                         y = c(thresholdByRegion$threshold[i]* displayFactor,
                               thresholdByRegion$threshold[i]* displayFactor),
                         inherit = FALSE, showlegend = FALSE,
                         hoverinfo = 'text',
                         text = paste('Threshold: ',
                                      thresholdByRegion$threshold[i]))
  }
  p
}

plotGauging()


############ plot 2nd graph (SECONDARY)

gauging$couleurSec <- gauging$pad_nbSec %% 4
displayFactorSec <- 45
gauging$rollStdDisplaySec <- displayFactorSec * gauging$rollStdSec
gauging$rollMeanOfStdevDisplaySec <- displayFactorSec * gauging$rollMeanOfStdevSec
#peakByRegionDisplaySec <- peakByRegionSec * displayFactorSec ##############

thresholdByRegionSec <- data.frame(threshold = peakByRegionSec, index_x = 1,
                                index_xend = length_g, volx = 0,
                                volxend= max(gauging$Liters))

#head(gauging)

lengPbRSec <- length(peakByRegionSec)
for ( i in 1:(lengPbRSec-1) ) {
  thresholdByRegionSec$index_x[i+1] <- floor((length_g %/% lengPbRSec) * i)
  thresholdByRegionSec$index_xend[i] <-  thresholdByRegionSec$index_x[i+1]
  thresholdByRegionSec$volx[i] <- gauging$Liters[thresholdByRegionSec$index_xend[i]]
  thresholdByRegionSec$volxend[i+1] <- thresholdByRegionSec$volx[i]
}

showrollStdDevSec <- TRUE
showOhmsSec <-  TRUE
showRollMeanSec <- TRUE

plotGaugingSec <- function () {
  pSec <- plot_ly(gauging,x = ~Liters)

  if (showOhmsSec) {
    pSec <- pSec %>%
      add_trace( y = ~Ohms2,type = "scatter", mode ="markers",
                 sizes = .1, color = ~couleurSec,
                 name = 'Raw Resistances Secondary', hoverinfo = 'text',
                 text = ~paste('Pad_Nb: ', pad_nbSec,
                               '</br></br> Volume: ', Liters,
                               '</br> Ohms Secondary: ', Ohms2))
  }

  if (showRollMeanSec) {
    pSec <- pSec %>%
      add_trace( y = ~rollMeanSec,type = "scatter", mode ="lines+markers",
                 marker = list(size = 2, opacity = 0.3, color = "blue"),
                 line = list(opacity = 0.3, dash = 'dot', color = "blue"),
                 #sizes = .1,
                 name = 'Rolling Mean Resistances\n   = smoothened',
                 hoverinfo = 'text',
                 text = ~paste('Pad_Nb: ', pad_nbSec,
                               '</br></br> Volume: ', Liters,
                               '</br> RollMean: ', rollMeanSec))
  }

  if (showrollStdDevSec) {
    pSec <- pSec %>%
      add_trace( y = ~rollMeanOfStdevDisplaySec, type = "scatter",
                 mode ="markers",
                 sizes = .1, color = ~couleurSec,
                 name = paste0('Rolling Mean of rolling\nStDev Secondary(x',
                               displayFactorSec, ')'),
                 hoverinfo = 'text',
                 text = ~paste('Pad_Nb: ', pad_nbSec,
                               '</br> </br> Volume: ', Liters,
                               '</br> rollMeanOfStdevDisplaySecond.: ',
                               rollMeanOfStdevSec))
  }


  pSec <- pSec %>%
    layout(title = paste0("SECONDARY\nRaw line and smooth line (rolling average) &\nrolling StDev (x",
                          displayFactorSec, " for visibility)"),
           xaxis = list(title = "Volume (liters)"),
           yaxis = list(title = "Resistance (Ohms)"),
           legend = list(x = 0.7, y = 0.9)

    ) %>%
    hide_colorbar()

  for (i in 1:lengPbRSec) {   # WORKS
    pSec <- pSec %>% add_lines(x = c(thresholdByRegionSec$volx[i],
                               thresholdByRegionSec$volxend[i]),
                         y = c(thresholdByRegionSec$threshold[i]* displayFactorSec,
                               thresholdByRegionSec$threshold[i]* displayFactorSec),
                         inherit = FALSE, showlegend = FALSE,
                         hoverinfo = 'text',
                         text = paste('Threshold: ',
                                      thresholdByRegionSec$threshold[i]))
  }
  pSec
}
if (secondary) {
  plotGaugingSec()

} else {
  pSec <- plot_ly(x = 5, y = 3, type = "scatter", mode ="markers") %>%
    layout(title = "There is NO SECONDARY Sender",
           xaxis = list(title = "Volume (liters)"),
           yaxis = list(title = "Resistance (Ohms)"),
           legend = list(x = 0.7, y = 0.9))
  pSec
}

# p <- p %>% add_trace(x = c(thresholdByRegion$volx[i],   #WORKS
#                            thresholdByRegion$volxend[i]),
#                      y= c(thresholdByRegion$threshold[i]* displayFactor,
#                           thresholdByRegion$threshold[i]* displayFactor),
#                      mode = "lines", inherit = FALSE)
#p

p_index <- plot_ly(gauging) %>%
    add_trace( y = ~rollMean,type = "scatter", mode ="lines",
               sizes = .1, color = I("green"), name = 'Rolling Mean Resistances\n   = smoothened') %>%
    # add_trace( y = ~Ohms,type = "scatter", mode ="lines",
    #            sizes = .1, color = I("red"), name = 'Raw Resistances') %>%
    add_trace( y = ~rollStdDisplay, type = "scatter", mode ="lines",
               sizes = .1, color = I("blue"), name = 'Rolling StDev') %>%
    layout(title = "Raw line and smooth line (rolling averages) &\nrolling StDev",
           xaxis = list(title = "Volume (liters)"),
           yaxis = list(title = "Index"),
           legend = list(x = 0.5, y = 1)
    ) %>%
    hide_colorbar()
p_index



# PLOTTING rolling stdev and rollingMean of StdDev
p_std <- plot_ly(gauging) %>%
    add_trace(x = ~Liters, y = ~rollStd,type = "scatter", mode ="markers",
              sizes = .1, color = I("blue"), name = 'Rolling StDev') %>%
    add_trace(x = ~Liters, y = ~rollMeanOfStdev,type = "scatter", mode ="lines",
              sizes = .1, color = I("orange"), name = 'RollMean of RollStdDev') %>%
    # add_trace(x = ~Liters, y = ~ohm_div,type = "scatter", mode ="markers",
    #           sizes = .1, color = I("green"), name = 'Ohm divided by 50') %>%
    layout(title = "Standard Dev and Diff",
           xaxis = list(title = "Volume (liters)"),
           yaxis = list(title = "Resistance (Ohms)"),
           legend = list(x = 0.55, y = 1)
    )
p_std


#####  plot resistance change per pad
plot(x = GM_table$`Resistance Change`, y = GM_table$`Segment ohm gap to next`,
     main="PRIMARY SENDER\nResistance gap between 2 consecutive segments",
     sub="(allows anomaly detection)", xlab="Segment number",
     ylab="Primary Resistance gap (Ohms)", las=1)

####### plot resistance change for SECONDARY sender
if (secondary) {
  plot(x = GM_tableSec$`Resistance Change`,
     y = GM_tableSec$`Segment ohm gap to next`,
     main="SECONDARY\nResistance gap between 2 consecutive segments",
     sub="(allows anomaly detection)", xlab="Segment number",
     ylab="Secondary Resistance gap (Ohms)", las=1)
} else {
  plot(x = 5, y = 3, main="THERE IS NO SECONDARY SENDER",
       xlab="Liters",
       ylab="Resistance (Ohms)", las=1)
}



# plot gaugin$Liters vs. mm
p_Liter_height <- plot_ly(gauging) %>%
  add_trace(x = ~Liters, y = ~mm,type = "scatter", mode ="markers",
            sizes = .1, color = I("blue"), name = 'Height vs. volume')
p_Liter_height

####### PLOT original data
p_origin  <- plot_ly(gauging_origin) %>%
  add_trace( x = ~mm, y = ~Ohms,type = "scatter", mode ="markers",
             sizes = .1, color = I("orange"),
             name = 'Raw Resistances by height')
p_origin

gauging_originOrder <- gauging_origin[with(gauging_origin, order(-mm)), ]
rownames(gauging) <- NULL


############   PLAYGROUND
############
###### NAMING THE SAVE FILE WITH KOREAN DATE, not server DATE
paste0(sub(x = "9BUX_AWD_TY_4_SUB_Drain.xlsx",
           pattern = "\\.csv|\\.xls.", ""),
       format(as.POSIXlt(Sys.time(), tz="Asia/Seoul"), "%Y-%m-%d_%H%M%S"), "_GM_Table", ".xlsx")

# 9BUX_AWD_TY_4_SUB_Drain.csv

# sub(x = "9BUX_AWD_TY_4_SUB_Drain.xlsx",
#     pattern = "\\.csv|\\.xls|\\.xlsx", "")
sub(x = "9BUX_AWD_TY_4_SUB_Drain.xlsx",
    pattern = "\\.csv|\\.xls.", "")

is.numeric(as.numeric("!")) &&     as.numeric("!") <5  # the 1st is TRUE!!
as.numeric("!") <5   # NA (num)
str(as.numeric("e6"))  # num NA
!is.na(as.numeric("!"))

str(is.numeric("3"))
is.numeric(NA)

!is.na(as.numeric("!")) && as.numeric("!") <5
vvaal <- "3.1425"
if (!is.na(as.integer(vvaal)) &&
    as.integer(vvaal) <5 ) vvaal <- as.integer(vvaal) else vvaal <- 29


