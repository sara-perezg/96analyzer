#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjqui)
library(shinythemes)
library(shinyalert)
library(tidyverse)
library(shinyFiles)

generate96plate = function(rows, cols){
    # rows = c("A","B","C","D","F","G","H","I","J")
    # cols = seq(1,12)
    my96plate = data.frame()
    for (i in 1:length(rows)){
        for (j in 1:length(cols)){
            my96plate[i,j]=paste(rows[i], cols[j], sep = "")
        }
    }
    
    row.names(my96plate) = rows
    colnames(my96plate) = cols
    
    return(my96plate)
    
}

theme = "sandstone"
palette1 = "Pastel1"
palette2 = "Pastel2"
palette3 = "Accent"
userS_default = "strain1"
unselected = "unknown"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # tags$head(
    #   tags$style(HTML('
    #     #selBut{
    #       background-color:green;
    #       padding:20px
    #     }
    #     #clearBut{
    #       padding:4px; 
    #       font-size:80% 
    #     }
    #     '))
    # ),
    # theme = shinythemes::shinytheme(theme),
    tabsetPanel(
        tabPanel("Options",icon = icon("cogs"), 
                 sidebarLayout( 
                     sidebarPanel(width = 2,
                                  h3(strong("Create your plate layout")),
                                  radioButtons("rb", "Selection",
                                               c("Strain" = "strain",
                                                 "Condition" = "condition",
                                                 "Replicates" = "rep")),
                                  textInput("userS", "Your labels", value = userS_default),
                                  verbatimTextOutput("value"),
                                  actionButton("selBut", "Add", icon = icon("plus")),
                                  actionButton("clearBut", "ClearAll", icon = icon("broom")),
                                  tags$hr(),
                                  shinyDirButton("directory", "Folder select", "Please select a folder"),
                                  # fileInput("file", "Upload your data"),
                                  # textInput("saveF", "Save you plate layout", value = "layout.csv"),
                                  # actionButton("saveBut","Save", icon = icon("save")),
                                  tags$hr(),
                                  shinySaveButton("save", "Save file", "Save file as...", filetype = list(text = "txt", picture = c("jpeg", "jpg")), viewtype = "icon")
                                  # fileInput("fileUpload", "Upload your plate layout")
                                  
                     ),
                     mainPanel(
                         fluidRow(
                             column(width = 7,
                                    h2("Select the corresponding wells"),
                                    selectableTableOutput("tbl", selection_mode = "cell")
                             ),
                             column(width = 3, 
                                    h2("print1"),
                                    verbatimTextOutput("print1")),
                             column(width = 2, 
                                    h2("print2"),
                                    verbatimTextOutput("print2"))
                         ),
                         fluidRow(
                             column(width = 4,
                                    h2("Strains"),
                                    plotOutput("plotS")),
                             column(width = 4,
                                    h2("Conditions"),
                                    plotOutput("plotC")),
                             column(width = 4,
                                    h2("Replicates"),
                                    #        h3(textOutput("selOut", container = span)),
                                    plotOutput("plotR")
                             )
                             
                         )
                         #  fluidRow(
                         #    column(width = 4,
                         #      h3(textOutput("selOut", container = span)),
                         #      plotOutput("plotS"),
                         #      tableOutput("tableP")
                         #    ),
                         #    column(width = 4,
                         #           h3(textOutput("selOut", container = span)),
                         #           plotOutput("plotC"),
                         #           tableOutput("tableP")
                         #    ),
                         #    column(width = 4,
                         #           h3(textOutput("selOut", container = span)),
                         #           plotOutput("plotR"),
                         #           tableOutput("tableP")
                         #    )
                         #  )
                     )
                 )
        ),
        tabPanel("Plots",icon = icon("chart-bar"),"contents")
    )
    
))
