#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library("dplyr")
# library("readxl")
# library(plotly)


# The initial file .csv must be "clean" which means:
#   - NO HEADERS
#   - the data starts directly at the line 1, in the cell(B1), with no titles - volume in liters
#   - the data has 9 columns: counting column without title(A), Liters(B), H1 (C), H2 (D),
#                             Ohms1 (E),  Ohms2 (F), and (G), (H) (I)
#                             ##########

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  observe({

    ###########
    # characteristics of the tank and project
    # dyn_unuse <- reactive({if (is.numeric(as.numeric(input$dynamic_unuse)) &&
    #                            as.numeric(input$dynamic_unuse) <5)
    #     as.numeric(input$dynamic_unuse)
    #     })
    if (!is.na(as.numeric(input$dynamic_unuse)) &&
        as.numeric(input$dynamic_unuse) <5) {
      dyn_unuse <- as.numeric(input$dynamic_unuse)
    } else dyn_unuse <- 0

    if (!is.na(as.numeric(input$useVol)) &&
        as.numeric(input$useVol) > 5 &&
        as.numeric(input$useVol) <150) {
      useable_vol <- as.numeric(input$useVol)
    } else useable_vol <- 50

    if (!is.na(as.integer(input$rollingk)) &&
        as.integer(input$rollingk) >= 10 &&
        as.integer(input$rollingk) <= 150) {
      rolling_k <-as.integer(input$rollingk)
    } else rolling_k <- 50

    if (!is.na(as.integer(input$rollingk2)) &&
        as.integer(input$rollingk2) >= 2 &&
        as.integer(input$rollingk2) <= 50) {
      rolling_k2 <-as.integer(input$rollingk2)
    } else rolling_k2 <- 10

    if (!is.na(as.integer(input$rollingkSec)) &&
        as.integer(input$rollingkSec) >= 10 &&
        as.integer(input$rollingkSec) <= 150) {
      rolling_kSec <-as.integer(input$rollingkSec)
    } else rolling_kSec <- 50

    if (!is.na(as.integer(input$rollingk2Sec)) &&
        as.integer(input$rollingk2Sec) >= 2 &&
        as.integer(input$rollingk2Sec) <= 50) {
      rolling_k2Sec <-as.integer(input$rollingk2Sec)
    } else rolling_k2Sec <- 10

    ##### checking range input for each graph
    if (!is.na(as.numeric(input$x_min)) && !is.na(as.numeric(input$x_max)) &&
        as.numeric(input$x_min) < as.numeric(input$x_max)) {
      xMin <- as.numeric(input$x_min)
      xMax <- as.numeric(input$x_max)
    } else {
      xMin <- -1
      xMax <- 55
    }

    if (!is.na(as.numeric(input$x_minSec)) && !is.na(as.numeric(input$x_maxSec)) &&
        as.numeric(input$x_minSec) < as.numeric(input$x_maxSec)) {
      xMinSec <- as.numeric(input$x_minSec)
      xMaxSec <- as.numeric(input$x_maxSec)
    } else {
      xMinSec <- -1
      xMaxSec <- 55
    }

    if (!is.na(as.numeric(input$y_min)) && !is.na(as.numeric(input$y_max)) &&
        as.numeric(input$y_min) < as.numeric(input$y_max)) {
      yMin <- as.numeric(input$y_min)
      yMax <- as.numeric(input$y_max)
    } else {
      yMin <- -5
      yMax <- 270
    }

    if (!is.na(as.numeric(input$y_minSec)) && !is.na(as.numeric(input$y_maxSec)) &&
        as.numeric(input$y_minSec) < as.numeric(input$y_maxSec)) {
      yMinSec <- as.numeric(input$y_minSec)
      yMaxSec <- as.numeric(input$y_maxSec)
    } else {
      yMinSec <- -5
      yMaxSec <- 270
    }



    #filename <- "KOR+Attendance+Report_20170626_144515.xlsx"
    filename2 <- input$filenameInit
    ext <- tools::file_ext(filename2$name)
    ifelse (is.null(filename2) || (ext != "xls" && ext != "xlsx" && ext != "csv"),
            { textFILE <- "The file should be .xls or .xlsx or .csv"
            #output$FILE <- renderText({textFILE})
            output$errorMessage <- renderText({" "})
            return(NULL)},
            textFILE <- input$filenameInit$name
    )
    output$FILE <- renderText({textFILE})

    # file.rename(filename2$datapath,
    #             paste(filename2$datapath, ext, sep="."))

    if (ext == "xls" || ext == "xlsx") {
      gauging <- data.frame(read_excel(filename2$datapath,
                                       sheet = 1, n_max = 10,
                                       na = "", col_names = FALSE))

      if (identical(chinaHeaderXL, as.character(gauging[1,]))) {
        gauging <- data.frame(read_excel(filename2$datapath,
                                         sheet = 1,
                                         na = "", col_names = TRUE))
        names(gauging) <- chinaHeader

        gauging <- gauging[, c("Test Time(min)", "Current Fuel Capacity(L)",
                               "Height1(mm)",
                               "Resistance1(\xa6\xb8)",
                               "Height2(mm)",
                               "Resistance2(\xa6\xb8)")]
        # str(gauging)
        colnames(gauging) <- c( "Time_s", "Liters", "mm", "Ohms", "H2",  "Ohms2" )
        output$errorMessage <- renderText({"File from CHINA"})

      } else if (dim(gauging)[2] == 9 && gauging[1,7] == 0 &&
                 gauging[1,8] == 0) {

        gauging <- data.frame(read_excel(filename2$datapath,
                                         sheet = 1,
                                         #skip = 1,
                                         na = "", col_names = FALSE))

        gauging <- as.data.frame(sapply(gauging[,1:9], as.numeric))
        colnames(gauging) <- c( "Time_s", "Liters", "mm", "H2", "Ohms",
                                "Ohms2" , "G", "H", "I" )
        # Remove the  G H I columns (c(7,8,9)) by keeping the rest
        gauging <- gauging[,c("Time_s", "Liters", "mm", "Ohms",
                              "H2", "Ohms2")]
        output$errorMessage <- renderText({"File from KOR"})
        }
      } else {

        gauging <- read.csv(filename2$datapath, nrows = 10, header = FALSE,
                            stringsAsFactors=FALSE)

        if (identical(chinaHeader, as.character(gauging[1,]))) {
          #print("china")
          gauging <- read.csv(filename2$datapath, header = FALSE,
                              skip = 1) # need to skip
                        # 1st row because of encoding problem
          names(gauging) <- chinaHeader

          gauging <- gauging[, c("Test Time(min)", "Current Fuel Capacity(L)",
                                 "Height1(mm)",
                                 "Resistance1(\xa6\xb8)",
                                 "Height2(mm)",
                                 "Resistance2(\xa6\xb8)")]

          colnames(gauging) <- c( "Time_s", "Liters", "mm", "Ohms",
                                  "H2", "Ohms2" )
          output$errorMessage <- renderText({"File from CHINA"})
        } else if (dim(gauging)[2] == 9 && gauging[1,7] == 0 &&
                   gauging[1,8] == 0) {
          gauging <- read.csv(filename2$datapath, header = FALSE)
          gauging <-  as.data.frame(sapply(gauging[,1:9], as.numeric))
          colnames(gauging) <- c( "Time_s", "Liters", "mm", "H2", "Ohms",
                                  "Ohms2" , "G", "H", "I" )

          # Remove the G H I columns (c(7,8,9)) by keeping the rest
          gauging <- gauging[,c("Time_s", "Liters", "mm", "Ohms",
                                "H2", "Ohms2")]
          output$errorMessage <- renderText({"File from KOR"})

        } else {
          output$errorMessage <- renderText({"Not a correct extract from KOR or CHINA gauging machine"})
          return(NULL)
        }


      # gauging <- read.csv(filename2$datapath,
      #                     header = FALSE)
      # gauging <- as.data.frame(sapply(gauging[,1:9], as.numeric))
    }

    # ddim1 <- paste0("Dim1 = ", dim(gauging)[1])
    # ddim2 <- paste0("Dim2 = ", dim(gauging)[2])
    # cc11 <- paste0("First cell = ", gauging[1,1])
    # output$dim1 <- renderText({ddim1})
    # output$dim2 <- renderText({ddim2})
    # output$c11 <- renderText({cc11})

    # if (dim(gauging)[2] != 9 || dim(gauging)[1] <1000 ) {
    #   output$errorMessage <- renderText({"This isn't a correct extract from the gauging machine"})
    #   return(NULL)
    #   }

    # output$errorMessage <- renderText({" "})


    ########### WORK ON THE DATA

    gauging$H2[is.na(gauging$H2)] <- 0
    secondary <- mean(gauging$H2) > 3

    # remove all the lines at the end of the dataframe where Liters is NA (useless lines).
    gauging <- gauging[!(is.na(gauging$Liters)),]

    # replace the NA of the "mm" and "H2" column by 0
    gauging[is.na(gauging$mm),"mm"] <-  0
    gauging[is.na(gauging$H2), "H2"] <-  0

    # replace the first NA of the "Ohms" and "Ohms2" column with the first actual measured value
    gauging[is.na(gauging$Ohms),"Ohms"] <-  gauging[!is.na(gauging$Ohms),"Ohms"][1]
    gauging[is.na(gauging$Ohms2),"Ohm2"] <-  gauging[!is.na(gauging$Ohms2),"Ohms2"][1]

    # reverses the order of the dataframe to have descending liters
    # no matter whether the test was draining or filling
    gauging <- gauging[with(gauging, order(-Liters)), ]
    rownames(gauging) <- NULL   # resets the row names from 1 without "holes"

    gauging$Liters <- gauging$Liters - dyn_unuse

    length_g <- length(gauging$Ohms)
    gauging$row <- 1:length(gauging$Ohms) # is it useful??


    #################
    ################# START the processing for PRIMARY SENDER
    gauging$rollMean <- rollmean(gauging$Ohms, rolling_k, fill = 0)
    #        gauging$xx <- "x"   # will be used to match each data point with 1 single pad

    gauging$rollMean[1:(round(rolling_k/2)+2)] <- gauging$rollMean[round(rolling_k/2)+3]
    gauging$rollMean[(length_g-(round(rolling_k/2)+2)):length_g] <- gauging$rollMean[length_g-(round(rolling_k/2)+3)]

    # adding rolling standard deviation
    gauging$rollStd <- sqrt((rolling_k/(rolling_k-1)) *
                              (abs(rollmean((gauging$rollMean)^2,
                                            rolling_k, fill = 0) -
                                     rollmean(gauging$rollMean,
                                              rolling_k, fill = 0)^2)))

    # gauging$rollStd[1:(round(rolling_k/2)+2)] <- gauging$rollStd[round(rolling_k/2)+3]
    # gauging$rollStd[(length_g-(round(rolling_k/2)+2)):length_g] <- gauging$rollStd[length_g-(round(rolling_k/2)+3)]


    # adding rolling mean of that roll Stdev (deviation)!!! In order to smoothen the peaks
    #rolling_k2 <- 10 # to smoothen the rolling std Dev
    gauging$rollMeanOfStdev <- rollmean(gauging$rollStd, rolling_k2, fill = 0)
    # gauging$xx <- "x"   # will be used to match each data point with 1 single pad

    # gauging$rollMeanOfStdev[1:(round(rolling_k2/2)+2)] <- gauging$rollMeanOfStdev[round(rolling_k2/2)+3]
    # gauging$rollMeanOfStdev[(length_g-(round(rolling_k2/2)+2)):length_g] <- gauging$rollMeanOfStdev[length_g-(round(rolling_k2/2)+3)]

    ######### finding local peaks
    peakByRegionInputPrim <- input$minpeak
    peakByRegionPrim <- rev(as.numeric( unlist(strsplit(gsub(" ", "",
                                                             peakByRegionInputPrim,
                                                             fixed = TRUE),
                                                        ",", fixed = TRUE))))
    peakByRegionPrim[peakByRegionPrim == 0 | is.na(peakByRegionPrim)] <- 0.15

    #
    # peaksIndex <- data.frame(V1 = 0, V2 = 0)
    # lengPbR <- length(peakByRegion)
    # for (i in 0:(lengPbR-1)) {
    #   peaksTable <- findpeaks(gauging$rollMeanOfStdev[(floor((length_g %/% lengPbR) * i)+1):min((floor((length_g %/% lengPbR) * (i+1)))+20, length_g)], nups = 5, zero = "0", peakpat = NULL,
    #                           minpeakheight = peakByRegion[i+1], minpeakdistance = 1,
    #                           threshold = 0, npeaks = 0, sortstr = FALSE)
    #   if (!is.null(peaksTable)) {
    #     peaksTable <- as.data.frame(peaksTable)[,1:2]
    #     peaksTable[,2] <- peaksTable[,2] + floor((length_g %/% lengPbR) * i)
    #     peaksIndex <- rbind(peaksIndex, peaksTable)
    #   }
    # }
    #
    # peaksIndex <- peaksIndex[-1,]
    # rownames(peaksIndex) <- NULL   # resets the row names from 1 without "holes"
    #
    # peaksIndex$gap2next <- 0
    # for (i in 1:(length(peaksIndex$V1)-1)) {
    #   peaksIndex$gap2next[i] <- peaksIndex[i+1,"V2"] - peaksIndex[i,"V2"]
    # }
    # peaksIndex$gap2next[length(peaksIndex$V1)] <- mean(peaksIndex$gap2next[1:length(peaksIndex$V1)-1])
    # peaksIndex$keep <- TRUE
    #
    # ## remove redundant peaks
    # peaksIndex <-  peaksIndex[ peaksIndex$gap2next != 0 ,]
    #
    # ## correcly position peaks, which seem biased towards higher "Liters" values
    # peaksIndex$V2 <- peaksIndex$V2 +2

    peaksIndexPrim <- peakTableFunc(peakByRegionPrim,
                                    gauging$rollMeanOfStdev,
                                    length_g)

    gauging$padChange <- 0
    gauging[peaksIndexPrim[,2], "padChange"] <- 1

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

    ########

    pad_list_table_final <- padTable(pad_list_final)

  #####  The next for loop will get and min Liter and max Liter for each pad.
    pad_list_table_final$Liter_Start <- pad_list_table_final$pad_nb
    pad_list_table_final$Liter_Stop <- pad_list_table_final$pad_nb
    pad_list_table_final$Height_Start <- pad_list_table_final$pad_nb
    pad_list_table_final$Height_Stop <- pad_list_table_final$pad_nb

    for (i in pad_list_table_final$pad_nb) {
      pad_list_table_final$Liter_Start[i] <- min(gauging[gauging$pad_nb == i , ]$Liters)
      pad_list_table_final$Liter_Stop[i] <- max(gauging[gauging$pad_nb == i , ]$Liters)
      pad_list_table_final$Height_Start[i] <- tail(gauging[gauging$pad_nb == i,"mm"],1)
      pad_list_table_final$Height_Stop[i] <- head(gauging[gauging$pad_nb == i,"mm"],1)    }

    ###### correcting overlaps of heights
    for (i in 1:(lengg-1)) {
      if (pad_list_table_final$Height_Start[i] < pad_list_table_final$Height_Stop[i+1]) {
        pad_list_table_final$Height_Start[i] <- (pad_list_table_final$Height_Start[i] + pad_list_table_final$Height_Stop[i+1])/2 + 0.01
        pad_list_table_final$Height_Stop[i+1] <- pad_list_table_final$Height_Start[i] -0.02

      }
    }

    #### Just for info, we are displaying the gap in liters separating 2 segments
    # this is just to check that there are no big gaps between segments
    pad_list_table_final$liter_gap <- pad_list_table_final$pad_nb
    for (i in 1:length(pad_list_table_final$pad_nb)-1) {
      pad_list_table_final$liter_gap[i] <- abs(pad_list_table_final$Liter_Stop[i+1] - pad_list_table_final$Liter_Start[i])
    }

    ######### Insert a pad mid point marker (padChange = 0.5 to position plot text correctly)
    pad_list_table_final$padMidPos <- round(pad_list_table_final$pad_length[1] / 2)
    for ( i in 2:lengg) {
      pad_list_table_final$padMidPos[i] <- pad_list_table_final$padMidPos[i-1] +
        round(pad_list_table_final$pad_length[i-1] / 2)+
        round(pad_list_table_final$pad_length[i] / 2)
    }

    gauging[pad_list_table_final$padMidPos, ]$padChange <- 0.5



    ###### BUILDING THE GM TABLE
    GM_table <- pad_list_table_final[,c("pad_nb","pad_averages",
                                        "Height_Start",
                                        "Height_Stop", "Liter_Start",
                                        "Liter_Stop", "liter_gap",
                                        "pad_stdev","pad_ohm_gap2next")]
    GM_table$Liter_Start[GM_table$Liter_Start <0] <-  0
    GM_table$Liter_Stop[GM_table$Liter_Stop <0] <-  0


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

    ##### We put all columsn in correct order
    GM_table <-GM_table[,c("Resistance Change", "Primary Level Sensor Ohms",
                           "Secondary Level Sensor Ohms",
                           "Height Start", "Height Stop",
                           "Start Percent UsableFuel InTank", "End Percent UsableFuel InTank",
                           "Start Liters UsableFuel InTank", "End Liters UsableFuel InTank",
                           "Primary Percent 5V Ref", "Secondary Percent 5V Ref", "Liter gap",
                           "Segment stddev",
                           "Segment ohm gap to next")]

    output$GM_table <- renderTable({
      GM_table
    })

###############
############### NOW CALCULATION OF SEGMENTS FOR SECONDARY SENDER
    if (secondary) {

      gauging$rollMeanSec <- rollmean(gauging$Ohms2, rolling_kSec, fill = 0)
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
      peakByRegionInputSec <- input$minpeakSec
      peakByRegionSec <- rev(as.numeric(unlist(strsplit(gsub(" ", "",
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

      # create "pad_nbSec" column by cumulative calculation of padChangeSec
      gauging$pad_nbSec <- cumsum(gauging$padChangeSec) + 1

      ######## AT THIS POINT we will associate each volume point to a pad
      ## number so we will build the pad list, simply based on the pad
      ## number! but now we are recording raw Ohm values, not rolling mean

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


        # GM_tableSec <-  data.frame(first = 0,
        #                             Ohms = "THERE IS a SECONDARY SENDER")

          } else
    {
      GM_tableSec <- data.frame(first = 0,
                                 Ohms = "NO SECONDARY SENDER")
    }

    output$GM_table_Sec <- renderTable({
      GM_tableSec
    })

#####################


    output$downloadData <- downloadHandler(
      filename = function(file) {
        paste0(sub(x = textFILE,
                   pattern = "\\.csv|\\.xls.", ""),
               format(as.POSIXlt(Sys.time(), tz="Asia/Seoul"), "%Y-%m-%d_%H%M%S"), "_GM_Table", ".xlsx")
      },
      content = function(con) {
        write.xlsx( x = GM_table,
                    con,
                    sheetName = "GM_GaugingSheet_Primary",
                    row.names = FALSE)
        write.xlsx(x = GM_tableSec, con,
                   sheetName="GM_GaugingSheet_Secondary",
                   append=TRUE, row.names=FALSE)
      }
    )

    #############  Plots of raw data points and smoothened data points
    gauging$couleur <- gauging$pad_nb %% 4
    gauging$couleur[gauging$couleur == 0] <- 4

    ###### rollStDev for display only (x26  for visibility on the graph)

    ### DEFINE DISPLAY FACTOR (for std dev)
    if (!is.na(as.integer(input$display_fact))) {
      displayFactor <- as.integer(input$display_fact)
    } else displayFactor <- 40


    gauging$rollStdDisplay <- displayFactor * gauging$rollStd
    gauging$rollMeanOfStdevDisplay <- displayFactor * gauging$rollMeanOfStdev

    thresholdByRegion <- data.frame(threshold = peakByRegionPrim,
                                    index_x = 1,
                                    index_xend = length_g, volx = 0,
                                    volxend= max(gauging$Liters))

    lengPbR <- length(peakByRegionPrim)
    for ( i in 1:(lengPbR-1) ) {
      thresholdByRegion$index_x[i+1] <- floor((length_g %/% lengPbR) * i)
      thresholdByRegion$index_xend[i] <-  thresholdByRegion$index_x[i+1]
      thresholdByRegion$volx[i] <- gauging$Liters[thresholdByRegion$index_xend[i]]
      thresholdByRegion$volxend[i+1] <- thresholdByRegion$volx[i]
    }

    ## define colors for threshold and for raw data + StDev
    couleursThreshold <- c('red', 'green', 'blue', 'orange')
    couleursOhms <- c('orange', 'blue', 'green', 'black')
    valuesColors_threshold <- vector(mode = "character",
                                     length = length(thresholdByRegion$threshold))
    valuesColors_threshold[] <- couleursThreshold
    names(valuesColors_threshold) <- paste0(as.character(thresholdByRegion$index_x),
                                            "prim")

    #thresholdByRegion$valuesColors_threshold <- valuesColors_threshold

      output$GaugingPlot1 <- renderPlot({

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


        if (input$showOhms) {
          g <-  g + geom_point( aes(y = Ohms, fill = as.character(couleur)),
                                color = "transparent", show.legend=F,
                                pch = 21, size = 1.5)
          g <-  g + scale_fill_manual(values=couleursOhms)  # couleursOhms works without being a named vector
          g <-  g + geom_text( aes(y = Ohms, label=ifelse(padChange ==0.5 & pad_nb %% 2 == 0 ,as.character(pad_nb),'')),
                               # hjust=1.4,
                               vjust= 1.7, size = 3)
        }

        if (input$showRollMean) {
          g <- g + geom_line(aes(Liters, rollMean), colour = 'blue')
        }

        if (input$showrollStdDev) {
          g <-  g + geom_point(aes(y = rollMeanOfStdevDisplay, fill = as.character(couleur)),
                               color = "transparent",
                               show.legend=F,
                               pch = 21, size = 0.7)
        }
        g
      })


#########  PLOT the 2nd graph (SECONDARY)
      if (secondary) {

        gauging$couleurSec <- gauging$pad_nbSec %% 4
        gauging$couleurSec[gauging$couleurSec == 0] <- 4
        ### DEFINE DISPLAY FACTOR SECONDARY(for std dev)
        if (!is.na(as.integer(input$display_factSec))) {
          displayFactorSec <- as.integer(input$display_factSec)
        } else displayFactorSec <- 45

        gauging$rollStdDisplaySec <- displayFactorSec * gauging$rollStdSec
        gauging$rollMeanOfStdevDisplaySec <- displayFactorSec * gauging$rollMeanOfStdevSec
#        peakByRegionDisplaySec <- peakByRegionSec * displayFactorSec

        thresholdByRegionSec <- data.frame(threshold = peakByRegionSec, index_x = 1,
                                           index_xend = length_g, volx = 0,
                                           volxend= max(gauging$Liters))

        lengPbRSec <- length(peakByRegionSec)
        for ( i in 1:(lengPbRSec-1) ) {
          thresholdByRegionSec$index_x[i+1] <- floor((length_g %/% lengPbRSec) * i)
          thresholdByRegionSec$index_xend[i] <-  thresholdByRegionSec$index_x[i+1]
          thresholdByRegionSec$volx[i] <- gauging$Liters[thresholdByRegionSec$index_xend[i]]
          thresholdByRegionSec$volxend[i+1] <- thresholdByRegionSec$volx[i]
        }

        #SHOULD  BE IMPLEMENTED
        valuesColors_thresholdSec <- vector(mode = "character",
                                         length = length(thresholdByRegionSec$threshold))
        valuesColors_thresholdSec[] <- couleursThreshold
        names(valuesColors_thresholdSec) <-paste0(as.character(thresholdByRegionSec$index_x),
                                                  "Sec")

        #### Actual plotting SECONDARY
        output$GaugingPlotSec <- renderPlot({

          gsec <- ggplot(gauging, aes(Liters)) +
            theme(plot.title = element_text(hjust = 0.8,
                                            margin = margin(t = 30, b = -50))) +
            labs(x="Volume (liters)", y="Resistance (Ohms)",
                 title=paste0("SECONDARY\n\U25CFRaw data (multicolor)                                    \n\U2500 smooth line (rolling mean) (blue continuous)\n\U25CF Rolling StdDev (x",
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


          if (input$showOhmsSec) {
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

          if (input$showRollMeanSec) {
            gsec <- gsec + geom_line(aes(Liters, rollMeanSec), colour = 'blue')
          }

          if (input$showrollStdDevSec) {
            gsec <-  gsec + geom_point(aes(y = rollMeanOfStdevDisplaySec, fill = as.character(couleurSec)),
                                       color = "transparent",
                                       show.legend=F,
                                       pch = 21, size = 0.7)
          }
          gsec
        })
      } else {
        output$GaugingPlotSec <- renderPlot({
          plot(x = 5, y = 3, main="THERE IS NO SECONDARY SENDER",
               xlab="Liters",
               ylab="Secondary Resistance (Ohms)", las=1)
        })
      }

      output$resistances <- renderPlot({
        plot(x = GM_table$`Resistance Change`,
             y = GM_table$`Segment ohm gap to next`,
             main="Resistance gap between 2 consecutive segments",
             sub="(allows anomaly detection)", xlab="Segment number",
             ylab="Resistance gap (Ohms)", las=1)
      })

      if (secondary) {
        output$resistancesSec <- renderPlot({
          plot(x = GM_tableSec$`Resistance Change`,
               y = GM_tableSec$`Segment ohm gap to next`,
               main="SECONDARY SENDER\nResistance gap between 2 consecutive segments",
               sub="(allows anomaly detection)", xlab="Segment number",
               ylab="Secondary Resistance gap (Ohms)", las=1)
        })
      } else {
        output$resistancesSec <- renderPlot({
          plot(x = 5, y = 3, main="THERE IS NO SECONDARY SENDER",
               xlab="Segment #",
               ylab="Secondary Resistance gap (Ohms)", las=1)
        })
      }


  })

})
