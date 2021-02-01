#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
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

    #--------- global stuff? -----------------
    userS = reactive({input$userS})
    volumes = c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyDirChoose(input, "directory", roots = volumes, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
    shinyFileSave(input, "save", roots = volumes, restrictions = system.file(package = "base"))
    # here im generating the 96 well plate
    rows = c("A","B","C","D","F","G","H","I","J")
    rowsN = seq(9,1, by = -1) #this is because the shinyjqui funcion returns rows with numbers
    cols = seq(1,12)
    
    # wide format table
    table96 = generate96plate(rows, cols) # we generate the 96 plate in which the user selects the rows and cols
    table96L = table96 # I create a copy of it, so that I can modify and compare with the selection
    # typeof(table96L)
    table96L$rows = rowsN # I add the rows in numbers
    # we change the plat to long format
    table96L = table96L %>% gather(key = cols, value = wells, -rows)
    table96L$wells = paste(table96L$rows, table96L$cols, sep = ".")
    table96L$rows = factor(table96L$rows, levels = rowsN)
    table96L$cols = factor(table96L$cols, levels = cols)
    # table96L$selection = "unknown"
    table96L$strain = unselected
    table96L$condition = unselected
    table96L$replicate = unselected
    # View(table96L)
    # iniciatication of the parameters
    # ------- counter to check first click of user ---------
    first_aplication = reactiveValues(value = TRUE)
    # my_selection = c(1.1,1.2,1.3)
    user= reactiveValues(selection = c(1.1,1.2,1.3))
    table96L = reactiveValues(data = table96L)
    colSelected = reactiveValues(value = 5) #default colum to strains
    # -------- plot options ----------
    border_color = "white"
    width = 350 #width of the plot
    heigth = 300 #height of the plot
    legend.position = "bottom"
    
    
    # --------------- output things -------------
    # sidepanel
    cap = eventReactive(input$selBut, {input$userS}) # this I dont understand tbh
    observeEvent(input$selBut, {first_aplication$value = FALSE})
    output$selOut = renderText({cap()})
    output$tbl = renderTable(table96) # we show the table
    
    # ----------- adding parameters to the plate layout -----------------
    
    
    
    add_selected = eventReactive(input$selBut, {
        
        user$selection = paste(input$tbl_selected$rows, input$tbl_selected$columns, sep = ".")
        #
        # if (input$rb == "strain"){
        #   colSelected$value = 5
        # }else{
        #   if (input$rb == "condition"){
        #     colSelected$value = 6
        #   }else{
        #     if(input$rb == "rep"){
        #       colSelected$value = 7
        #     }
        #   }
        # }
        # View(table96L)
        # table[,1] = rows, 2 cols, 3 wells, 4 selection, 5 strain, 6 condition, 7 replicates
        
        for(i in 1:length(table96L$data[,3])){ #for i in each well
            if(table96L$data[i,3] %in% user$selection){ #is this well is selected by user
                table96L$data[i,colSelected$value] = cap() #change the selection with the user label and in strain, condition or replicates
            }
        }
        ggplot(table96L$data, aes(cols,rows, fill = table96L$data[,colSelected$value])) + geom_tile(color = border_color)+
            theme(panel.grid = element_blank(),
                  panel.background = element_blank(),
                  legend.position = legend.position)+labs(fill = "Selection")
    })
    
    observeEvent(input$selBut, {
        showNotification(as.character(first_aplication$value))
        
    })
    
    observeEvent(input$clearBut, {
        showNotification(as.character(first_aplication$value))
        
    })
    
    # ------------- strains ------------------
    addSelection = eventReactive(input$selBut,{
        user$selection = paste(input$tbl_selected$rows, input$tbl_selected$columns, sep = ".")
        # table[,1] = rows, 2 cols, 3 wells, 4 selection, 5 strain, 6 condition, 7 replicates
        for(i in 1:length(table96L$data[,3])){ #for i in each well
            if(table96L$data[i,3] %in% user$selection){ #is this well is selected by user
                table96L$data[i,colSelected$value] = cap() #change the selection with the user label and in strain, condition or replicates
            }
        }
    })
    
    output$plotS = renderPlot({
        
        if (first_aplication$value){
            table96L$data$strain = unselected
            ggplot(table96L$data, aes(cols,rows, fill = strain)) + geom_tile(color = border_color)+
                theme(panel.grid = element_blank(),
                      panel.background = element_blank(),
                      legend.position = legend.position)+scale_fill_brewer(palette = palette1)
        } else{
            addSelection()
            ggplot(table96L$data, aes(cols,rows, fill = strain)) + geom_tile(color = border_color)+
                theme(panel.grid = element_blank(),
                      panel.background = element_blank(),
                      legend.position = legend.position)+labs(fill = "Selection")+scale_fill_brewer(palette = palette1)
        }
        # ggplot(table96L, aes(cols,rows, fill = selection)) + geom_tile(color = border_color)+
        #   theme(panel.grid = element_blank(),
        #         panel.background = element_blank())
    }, width = width, height = heigth)
    
    
    # ------------- conditions -------------------
    addCondition = eventReactive(input$selBut,{
        user$selection = paste(input$tbl_selected$rows, input$tbl_selected$columns, sep = ".")
        # table[,1] = rows, 2 cols, 3 wells, 4 selection, 5 strain, 6 condition, 7 replicates
        for(i in 1:length(table96L$data[,3])){ #for i in each well
            if(table96L$data[i,3] %in% user$selection){ #is this well is selected by user
                table96L$data[i,6] = cap() #change the selection with the user label and in strain, condition or replicates
            }
        }
    })
    output$plotC = renderPlot({
        
        if (first_aplication$value){
            table96L$data$condition = unselected
            ggplot(table96L$data, aes(cols,rows, fill = condition)) + geom_tile(color = border_color)+
                theme(panel.grid = element_blank(), panel.background = element_blank(),
                      legend.position = legend.position)+scale_fill_brewer(palette = palette2)
        } else{
            addSelection()
            ggplot(table96L$data, aes(cols,rows, fill = condition)) + geom_tile(color = border_color)+
                theme(panel.grid = element_blank(), panel.background = element_blank(),
                      legend.position = legend.position)+scale_fill_brewer(palette = palette2)
        }
        # ggplot(table96L, aes(cols,rows, fill = selection)) + geom_tile(color = border_color)+
        #   theme(panel.grid = element_blank(),
        #         panel.background = element_blank())
    }, width = width, height = heigth)
    
    # -------------- replicates ------------------
    
    output$plotR = renderPlot({
        
        if (first_aplication$value){
            table96L$data$replicate = unselected
            ggplot(table96L$data, aes(cols,rows, fill = replicate)) + geom_tile(color = border_color)+
                theme(panel.grid = element_blank(),
                      panel.background = element_blank(),
                      legend.position = legend.position)+scale_fill_brewer(palette = palette3)
        } else{
            addSelection()
            ggplot(table96L$data, aes(cols,rows, fill = replicate)) + geom_tile(color = border_color)+
                theme(panel.grid = element_blank(),
                      panel.background = element_blank(),
                      legend.position = legend.position)+scale_fill_brewer(palette = palette3)
        }
        # ggplot(table96L, aes(cols,rows, fill = selection)) + geom_tile(color = border_color)+
        #   theme(panel.grid = element_blank(),
        #         panel.background = element_blank())
    }, width = width, height = heigth)
    
    
    output$print1 = renderPrint({
        
        if (input$rb == "strain"){
            colSelected$value = 4
        }else{
            if (input$rb == "condition"){
                colSelected$value = 5
            }else{
                if(input$rb == "rep"){
                    colSelected$value = 6
                }
            }
        }
        head(input$rb, n = 10)
    })
    output$print2 = renderPrint({
        head(table96L$data, n = 10)
    })
    observeEvent(input$clearBut,{
        first_aplication$value = TRUE #everthing is reseted
    })
    # output$tableP = renderTable({
    #   data = input$tbl_selected
    #   data$selection = paste(data$rows, data$columns, sep = ".")
    #   head(data, n = 5)
    # })
    observe({
        path <-""
        shinyFileSave(input, "save", roots=volumes)
        fileinfo <- parseSavePath(volumes, input$save)
        data <- data.frame(a=c(1,2))
        if (nrow(fileinfo) > 0) {
            write.xlsx(data, as.character(fileinfo$datapath))
        }
    })

})
