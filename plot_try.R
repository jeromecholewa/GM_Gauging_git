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

  p <- p %>%
    layout(title = paste0("Raw line and smooth line (rolling average) &\nrolling StDev (x",
                          displayFactor, " for visibility)"),
           xaxis = list(title = "Volume (liters)"),
           yaxis = list(title = "Resistance (Ohms)"),
           legend = list(x = 0.7, y = 0.9)

    ) %>%
    hide_colorbar()
  #p


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

#####################
######################   G G P L O T
#####################

# showRollMean <- TRUE
# showrollStdDev <- TRUE
# showOhms <- TRUE
# head(gauging)

# unique(gauging$couleur)
# xMin <- 10  # -1
# xMax <- 18  # 55
xMin <- -1
xMax <-  55
xMinSec <- -1
xMaxSec <- 55
# yMin <- 80  # -5
# yMax <- 150 # 270
yMin <- -5
yMax <-   270
yMinSec <- -5
yMaxSec <- 270
couleursThreshold <- c('red', 'green', 'blue', 'orange')
couleursOhms <- c('orange', 'blue', 'green', 'black')
# names(couleursOhms) <- unique(as.character(gauging$couleur))
# gauging$couleurs <- 'a'
# gauging$couleurs <- couleurs[gauging$couleur+1]

valuesColors_threshold <- vector(mode = "character", length = length(thresholdByRegion$threshold))
valuesColors_threshold[] <- couleursThreshold
names(valuesColors_threshold) <- as.character(thresholdByRegion$threshold)

# head(pad_list_table_final)
pad_list_table_final$padMidPos <- round(pad_list_table_final$pad_length[1] / 2)
for ( i in 2:lengg) {
  pad_list_table_final$padMidPos[i] <- pad_list_table_final$padMidPos[i-1] +
    round(pad_list_table_final$pad_length[i-1] / 2)+
    round(pad_list_table_final$pad_length[i] / 2)
}

gauging[pad_list_table_final$padMidPos, ]$padChange <- 0.5

gauging[(gauging$padChange ==1 | gauging$padChange ==0.5) & gauging$pad_nb %% 2 == 0,]

ggplotGauging <- function() {
  #g <- ggplot(gauging)
#  summary(g)

  g <- ggplot(gauging, aes(Liters)) +
    theme(plot.title = element_text(hjust = 0.8,
                                    margin = margin(t = 30, b = -50))) +
    labs(x="Volume (liters)", y="Resistance (Ohms)",
         title=paste0("\U25CF Raw data (multicolor)                                    \n\U2500 smooth line (rolling mean) (blue continuous)\n\U25CF Rolling StdDev (x", 40," for visibility) (multicolor)"))  #+

  g <-  g + coord_cartesian(ylim = c(yMin, yMax),
                            xlim = c(xMin, xMax))   + #
    # coord_cartesian(ylim = c(-2, 100),
    #                 xlim = c(14, 21 ))
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))

  g <- g + theme(panel.background = element_rect(fill = 'white'),
                 panel.grid.major = element_line(colour = "lightgrey",
                                                 size = 0.2),
                 axis.line = element_line(colour = "black"))

  g <-  g + geom_segment(data = thresholdByRegion, show.legend=F,
                         aes(x = volx,  xend = volxend,
                             y = threshold * displayFactor,
                             yend = threshold * displayFactor,
                             colour = as.character(threshold))) +
    scale_color_manual(values=valuesColors_threshold)  # We need a named vector with at least the same length as the data   #  it seems valuesColors_All is not even necessary?


  if (showOhms) {
    #### define colors for Ohms and StDev
    # valuesColors_Ohms <- gauging$couleur
    # valuesColors_Ohms <- couleursOhms[gauging$couleur]
    # names(valuesColors_Ohms) <- as.character(gauging$couleur)
    # # head(valuesColors_Ohms,980)
    # # str(valuesColors_Ohms)
    # gauging$couleurs <- valuesColors_Ohms
    #
    # valuesColors_All <- c(valuesColors_Ohms, valuesColors_threshold)
    # str(valuesColors_Ohms)

    g <-  g + geom_point( aes(y = Ohms, fill = as.character(couleur)),
                          color = "transparent", show.legend=F,
                          pch = 21, size = 1.5)
    g <-  g + scale_fill_manual(values=couleursOhms)  # couleursOhms works without being a named vector
    g <-  g + geom_text( aes(y = Ohms, label=ifelse(padChange ==0.5 & pad_nb %% 2 == 0 ,as.character(pad_nb),'')),
                         # hjust=1.4,
                         vjust= 1.7, size = 3)

    # g <-  g + scale_fill_gradient2(midpoint= sort(unique(gauging$couleur))[3] ,low="yellow", mid = 'blue', high="black")
    # g <- g + aes( Ohms )
    # g <-  g + geom_point(aes(y = Ohms, fill = as.factor(gauging$couleur)) , # no control over the colors
    #                      pch = 20, size = 1, show.legend=F, colour = NA)
    #
    # # scale_color_gradientn(midpoint= sort(unique(gauging$couleur))[3] ,low="yellow", mid = 'blue', high="black") +   # creates a problem of "Error: Discrete value supplied to continuous scale"
    # g <-  g + scale_fill_gradientn( colors= c("yellow", 'blue', 'orange', "black"))   #

  }

  # length(row.names(gauging))

  if (showRollMean) {
    #if (FALSE) {
    g <- g + geom_line(aes(Liters, rollMean), colour = 'blue')
    # g <-  g + scale_colour_manual(name='',
    #                               values=c('Rolling Mean'='blue'))
  }

  if (showrollStdDev) {
    #if (FALSE) {
    # g <-  g + geom_point(aes(Liters, rollMeanOfStdevDisplay),
    #                      color  = gauging$couleur+2, # no control over the colors
    #                      size = 0.5, show.legend=F)
    g <-  g + geom_point(aes(y = rollMeanOfStdevDisplay, fill = as.character(couleur)),
                         color = "transparent",
                         pch = 21, size = 0.7)

  }

  g
}

ggplotGauging()

# labs(x="Date", y=expression(paste("Temperature ( ", degree ~ F, " )")), title="Temperature")


gauging$couleur



############################################################
############################################################
############################################################




  # p <- plot_ly(gauging,x = ~Liters)
  #
  # if (showOhms) {
  #   p <- p %>%
  #     add_trace( y = ~Ohms,type = "scatter", mode ="markers",
  #                sizes = .1, color = ~couleur,
  #                name = 'Raw Resistances', hoverinfo = 'text',
  #                text = ~paste('Pad_Nb: ', pad_nb,
  #                              '</br></br> Volume: ', Liters,
  #                              '</br> Ohms: ', Ohms))
  # }
  #
  # if (showRollMean) {
  #   p <- p %>%
  #     add_trace( y = ~rollMean,type = "scatter", mode ="lines+markers",
  #                marker = list(size = 2, opacity = 0.3, color = "blue"),
  #                line = list(opacity = 0.3, dash = 'dot', color = "blue"),
  #                #sizes = .1,
  #                name = 'Rolling Mean Resistances\n   = smoothened',
  #                hoverinfo = 'text',
  #                text = ~paste('Pad_Nb: ', pad_nb,
  #                              '</br></br> Volume: ', Liters,
  #                              '</br> RollMean: ', rollMean))
  # }
  #
  # if (showrollStdDev) {
  #   p <- p %>%
  #     add_trace( y = ~rollMeanOfStdevDisplay, type = "scatter",
  #                mode ="markers",
  #                #line = list(color = ''black),
  #                #marker = list(color = 'red'),
  #                sizes = 0.5, color = ~couleur,
  #                name = paste0('Rolling Mean of rolling StDev (x',
  #                              displayFactor, ')'),
  #                hoverinfo = 'text',
  #                text = ~paste('Pad_Nb: ', pad_nb,
  #                              '</br> </br> Volume: ', Liters,
  #                              '</br> rollMeanOfStdevDisplay: ',
  #                              rollMeanOfStdev))
  # }
  #
  # p <- p %>%
  #   layout(title = paste0("Raw line and smooth line (rolling average) &\nrolling StDev (x",
  #                         displayFactor, " for visibility)"),
  #          xaxis = list(title = "Volume (liters)"),
  #          yaxis = list(title = "Resistance (Ohms)"),
  #          legend = list(x = 0.7, y = 0.9)
  #
  #   ) %>%
  #   hide_colorbar()
  # #p
  #
  #
  # for (i in 1:lengPbR) {   # WORKS
  #   p <- p %>% add_lines(x = c(thresholdByRegion$volx[i],
  #                              thresholdByRegion$volxend[i]),
  #                        y = c(thresholdByRegion$threshold[i]* displayFactor,
  #                              thresholdByRegion$threshold[i]* displayFactor),
  #                        inherit = FALSE, showlegend = FALSE,
  #                        hoverinfo = 'text',
  #                        text = paste('Threshold: ',
  #                                     thresholdByRegion$threshold[i]))
  # }
  # p
#}

