#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Extracting the GM table from the gauging raw data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        h5("Questions? Wishes? Bug report? Contact jeromecholewa@gmail.com"),
        fileInput("filenameInit", "Pick your xlsx, xls or csv extract\n from the gauging machine",
                  accept = c(".xls", ".xlsx", ".csv")),
        textOutput("FILE"),
        textOutput("errorMessage"),
        # textOutput("dim1"),
        # textOutput("dim2"),
        # textOutput("c11"),
        br(),
        textInput("dynamic_unuse", "Enter the dynamic unuseable volume", value = "0" ),
        textInput("useVol", "Enter the full useable volume", value = "50" ),
        h3("PARAMETERS for extraction"),
        textInput("rollingk", "Enter the range between 10 & 150, to calculate the rolling mean", value = "50" ),
        textInput("rollingk2", "Enter the range between 2 & 50, to calculate the rolling mean of Std Dev", value = "10" ),
        textInput("minpeak", "Enter list of detection peaks with commas, e.g. 0.35, 0.29,0.14, 0.14,  0.14 to detect a peak in Std Dev", value = "0.35, 0.30,0.25, 0.20,  0.14"),
        h6("For example, if you enter 3 values, the range of fuel volume will be divided into 3 regions of equal length in which each threshold will be used to detect peaks"),
        br(), br(), br(),br(), br(),
        submitButton("Start Calculation"),
        downloadButton("downloadData", "Save GM table"),
        br(),br(), br(), br(),
        textInput("rollingkSec", "Enter the range between 10 & 150, to calculate the rolling mean for SECONDARY sender", value = "40" ),
        textInput("rollingk2Sec", "Enter the range between 2 & 50, to calculate the rolling mean of Std Dev for SECONDARY sender", value = "10" ),
        textInput("minpeakSec", "SECONDARY: Enter list of detection peaks with commas, e.g. 0.35, 0.29,0.14, 0.14,  0.14 to detect a peak in Std Dev", value = "0.40, 0.25,0.20, 0.30, 0.22, 0.14")
        ),

    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(3, checkboxInput("showOhms", "Resistances", TRUE)),
               column(3, checkboxInput("showrollStdDev",
                                       "Standd Dev", TRUE)),
               column(5, checkboxInput("showRollMean",
                                       "Resistances Rolling Average",
                                       TRUE)),
               column(5,
                      textInput("display_fact",
                                "Enter a display factor for the Std Dev",
                                value = "50" ))),
      plotlyOutput("GaugingPlot1"),
      plotOutput("resistances"),
      fluidRow(column(3, checkboxInput("showOhmsSec", "Sub Resistances", TRUE)),
               column(3, checkboxInput("showrollStdDevSec",
                                       "Sub Standd Dev", TRUE)),
               column(5, checkboxInput("showRollMeanSec",
                                       "Sub Resistances Rolling Average",
                                       TRUE)),
               column(5,
                      textInput("display_factSec",
                                "Enter a display factor for the Sub Std Dev",
                                value = "45" ))),
      plotlyOutput("GaugingPlotSec"),
      plotOutput("resistancesSec"),
      h4("GM TABLE"),
      tableOutput("GM_table"),
      h4("GM TABLE FOR SECONDARY SENDER"),
      tableOutput("GM_table_Sec")

    )
  )
))
