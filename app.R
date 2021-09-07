#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Adapted from https://github.com/kyamagu/bbox-annotator/
# Edited original JS to add color_list as an option
# ...should be the same length as labels
# ...and controls the color of the rectangle
# ...will probably be broken for input_method = "fixed" or "text"
# Also added color as a value in each rectangle entry

js <- '
    $(document).ready(function() {
       // define options to pass to bounding box constructor
        var options = {
          url: "https://www.dropbox.com/s/s8vh1wdpok15lz5/worm.PNG?raw=1",
          input_method: "select", 
          labels: [""],
          color_list:  [""], 
          onchange: function(entries) {
                Shiny.onInputChange("rectCoord", JSON.stringify(entries, null, "  "));
          }
        };

        // Initialize the bounding-box annotator.
        var annotator = new BBoxAnnotator(options);

        // Initialize the reset button.
        $("#reset_button").click(function(e) {
            annotator.clear_all();
        })

        // define function to reset the bbox
        // ...upon choosing new label category or new url
        function reset_bbox(options) {
          document.getElementById("bbox_annotator").setAttribute("style", "display:inline-block");
          $(".image_frame").remove();
          annotator = new BBoxAnnotator(options);
        }

        // update image url from shiny
        Shiny.addCustomMessageHandler("change-img-url", function(url) {
          options.url = url;
          options.width = null;
          options.height = null;
          reset_bbox(options);
        });

        // update colors and categories from shiny
        Shiny.addCustomMessageHandler("update-category-list", function(vals) {
          options.labels = Object.values(vals);
          options.color_list = Object.keys(vals);
          reset_bbox(options);
        });
        
        //update list from CSV
        Shiny.addCustomMessageHandler("rectCoord", function(value) {
          Shiny.setInputValue("rectCoord", value);
            });
        
        
        // redraw rectangles based on list of entries
        Shiny.addCustomMessageHandler("redraw-rects", function(vals) {
          var arr = JSON.parse(vals)
          arr.forEach(function(rect){
             annotator.add_entry(rect);
          });
          if (annotator.onchange) {
             annotator.onchange(annotator.entries);
          }
        }); 
    });
'





library(shiny)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(survminer)
library(survival)
library(openxlsx)
library(rdrop2)
library(jsonlite)
library(stringr)
library(shinyalert)

token <- readRDS("/Users/shreyaramakrishnan/Documents/kaeberlein lab/wormbot_app/droptoken.rds")
drop_auth(rdstoken = "/Users/shreyaramakrishnan/Documents/kaeberlein lab/wormbot_app/droptoken.rds")

drop_imgdir <- "/wormbot_data/image_series1/" 
drop_csvdirdone <- "/wormbot_data/finished_csv_files/" 

round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}


#define the UI 
ui <- fluidPage(
    tags$head(tags$script(HTML(js)),
              tags$head(
                  tags$script(src = "bbox_annotation.js")
              )),
    titlePanel("Neural Network Training"),
    useShinyalert(), 
    sidebarLayout(
        sidebarPanel(
            actionButton("loadButton", "Load Images"),
            actionButton("nextButton", "Next"),
            actionButton("backButton", "Back"),
            actionButton("doneButton", "Finish Session"),
            
            selectInput("category_type", "Label Category", c("worms", "geography")),
            div(HTML(
                '<input id="reset_button" type="reset" />'
            )),
            HTML(
                '<input id="annotation_data" name="annotation_data" type="hidden" />'
            ),
            hr(),
            h4("Entries"),
            verbatimTextOutput("rectCoordOutput")
            
        ),
        mainPanel(div(id = "bbox_annotator", style = "display:inline-block"))
    )
)


# pick a random number in range 1:#of packets 
# find # of packets using the drop_dir function on the packets folder 
# take that number and set it to the packet # 
# set links file to the links file within the folder full path + "packet_" + packetNum 
# set drop_csvdir to the csv folder within the folder full path + "packet_" + packetNum 
#initialize links file as an empty variable, then set it within the load button 

server <- function(input, output, session) {
    imgNum <- reactiveVal()
    packet_num <- reactiveVal()
    path_name <- reactiveVal()
    drop_csvdir <- reactiveVal()
    links_file <- reactiveVal()
    allFiles <- reactiveVal()
    palette <- c("red","red4", "orangered", "darkorange3", 
                 "orange", "coral", "goldenrod", "gold", 
                 "yellow", "yellowgreen", "olivedrab1", "limegreen", 
                 "springgreen", "seagreen1", "palegreen", "aquamarine1", 
                 "cyan", "turquoise", "blue", "dodgerblue1", "blueviolet", 
                 "darkorchid", "darkslateblue", "purple", "mediumorchid", 
                 "violet", "lightpink2", "hotpink", "maroon1", "magenta")
    # user choices
    output$rectCoordOutput <- renderPrint({
        if(!is.null(input$rectCoord)) {
            as.data.frame(jsonlite::fromJSON(input$rectCoord))
        }
    })

observeEvent(input$loadButton, {
  # while loop checks if this packet has already been annotated 
  flag <- FALSE
  packet_num(6)
  while (flag == FALSE){ 
    packet_num(sample(1:nrow(drop_dir("/wormbot_data/packets/")), 1))
    path_check <- paste("/wormbot_data/packets/packet_", packet_num(), "/globalAnnotationData.csv", sep = "")
    if (drop_exists(path = path_check, dtoken = token)) {
      packet_num(sample(1:nrow(drop_dir("/wormbot_data/packets/")), 1))
    } else {
      flag <- TRUE 
    }
  }
  path_name(paste("/wormbot_data/packets/packet_", packet_num(), "/image_annotation_links.csv", sep = "")) 
  temp_links_file <- drop_read_csv(path_name(), header = FALSE, stringsAsFactors = FALSE, dest = tempdir(), dtoken = token)
  links_file(temp_links_file)
  csv_paths <- drop_dir(paste("/wormbot_data/packets/packet_", packet_num(), "/csv_files_series1", sep = ""))
  print(csv_paths)
  for (k in 1:dim(csv_paths)[1]){
      new_data <- drop_read_csv(csv_paths$path_lower[k],dtoken = token)
      new_data$index <- k
      if (k == 1){
          totalFiles <- new_data
      }else {
          totalFiles <- rbind(totalFiles,new_data)
      }
      allFiles(totalFiles)
  }
    
    #loads up first image and sends it to server 
    imgNum(1)
    
    imageonlyworms <- allFiles() %>% filter(index == imgNum()) %>% select(-index)
    fileURL <- temp_links_file[imgNum(),2]
    realULRcorr <- gsub("dl=0","raw=1", fileURL)
    session$sendCustomMessage("change-img-url", realULRcorr)
    
    
    #convert image only worms to javascript format 
    #send boxes using bbox annotator 
    new_data <- imageonlyworms
    new_data1<-subset(new_data,select=-c(dataCells1,dataCells2))
    new_data1 <- round_df(new_data1,0)
    new_data2 <- new_data1
    
    data_as_char <- 1:dim(new_data2)[1]
    as.character(data_as_char)
    new_data2$F <- data_as_char
    new_data2$G <- palette[1:dim(new_data2)[1]]

    colLabels <- c("left","top","width","height","label","color")
    colnames(new_data2) <- colLabels
    jsonfile <- toJSON(new_data2,pretty=TRUE)
    jsonfile2 <- str_replace_all(jsonfile, "[\r\n]" , "\n")
    
    #send message to load pre-annotted bounding boxes into the annotater program
    session$sendCustomMessage("redraw-rects", jsonfile2)
    
    
})

#NOTE: we need to probably sort the global csv at the end to be ordered using k value 
#not sure how exactly we are keeping track of the k value right now though 
observeEvent(input$nextButton, {
  
  if (is.null(links_file()) || is.null(imgNum())) { 
    shinyalert("ERROR", "Please click the Load Images button before proceeding", type = "error")
  }
  print("check")
  #update reactive val 
  imgNum(imgNum()+1)
  if (imgNum() > dim(links_file()[1])[1]){ 
    imgNum(dim(links_file()[1])[1])
    shinyalert("Packet complete", "Please click the finish session button", type="success")
  }
  print(imgNum())
  
  #send new boxes as a csv 
  if(!is.null(input$rectCoord)) {
    csvfinished <- as.data.frame(jsonlite::fromJSON(input$rectCoord))
  }
  csvupdate <- subset(csvfinished,select=-c(color))
  csvupdate$index <- imgNum()-1
  
  #delete from global csv 
  imgNameLabel <- allFiles() %>% filter(index == (imgNum()-1) ) %>% select(dataCells1)
  imgNameLabel <- imgNameLabel[[1,1]]
  print(imgNameLabel)
  allFiles(allFiles() %>% filter(index != (imgNum() - 1)))
  #print(allFiles())
  
  csvupdate$dataCells1 <- imgNameLabel
  csvupdate <- csvupdate %>% relocate(dataCells1)
  
  relocate(csvupdate, label, .after = dataCells1)
  
  csvupdate <- rename(csvupdate, dataCells1 = dataCells1, dataCells2 = label, dataCells3 = left, dataCells4 = top, dataCells5 = width, dataCells6 = height)
  #print(csvupdate)
  #merge to global csv 
  allFiles(rbind(allFiles(),csvupdate))
  
  
  # pull that row from the image link csv file
  # load the image
  fileURL <- links_file()[imgNum(),2]
  realULRcorr <- gsub("dl=0","raw=1", fileURL)
  session$sendCustomMessage("change-img-url", realULRcorr)
  
  imageonlyworms <- allFiles() %>% filter(index == imgNum()) %>% select(-index)
  
  #convert image only worms to javascript format 
  #send boxes using bbox annotator 
  new_data <- imageonlyworms
  #if statement checking if dataCells2 is set to worms or if it's a series of numbers 
  #check if it is "worms"
  #if it is, leave current code 
  
  new_data1<-subset(new_data,select=-c(dataCells1,dataCells2))
  new_data1 <- round_df(new_data1,0)
  new_data2 <- new_data1
  
  #if it is not 
  
  data_as_char <- 1:dim(new_data2)[1]
  as.character(data_as_char)
  #print(new_data[1,2])
  if (new_data[1,2] == "worm") { 
    print(data_as_char)
    new_data2$F <- data_as_char
  } else { 
    new_data2$F <- new_data$dataCells2
  }
  new_data2$G <- palette[1:dim(new_data2)[1]]
  colLabels <- c("left","top","width","height","label","color")
  colnames(new_data2) <- colLabels
  jsonfile <- toJSON(new_data2,pretty=TRUE)
  jsonfile2 <- str_replace_all(jsonfile, "[\r\n]" , "\n")
  #print(new_data2)
  #print(csvupdate$dataCells1)
  #send message to load pre-annotted bounding boxes into the annotater program
  session$sendCustomMessage("redraw-rects", jsonfile2)
  
})     



observeEvent(input$backButton, {
  imgNum(imgNum()-1)
  if (imgNum() < 1){ 
    imgNum(1)
    }
  
  #send new boxes as a csv 
  if(!is.null(input$rectCoord)) {
    csvfinished <- as.data.frame(jsonlite::fromJSON(input$rectCoord))
  }
  csvupdate <- subset(csvfinished,select=-c(color))
  csvupdate$index <- imgNum()+1
  
  #delete from global csv 
  imgNameLabel <- allFiles() %>% filter(index == (imgNum()+1) ) %>% select(dataCells1)
  imgNameLabel <- imgNameLabel[[1,1]]
  
  #imgNameLabel <- imgNameLabel[[1]]
  #print(imgNameLabel)
  allFiles(allFiles() %>% filter(index != (imgNum() + 1)))
  
  csvupdate$dataCells1 <- imgNameLabel[[1]]
  csvupdate <- csvupdate %>% relocate(dataCells1)
 
  relocate(csvupdate, label, .after = dataCells1)
  
  csvupdate <- rename(csvupdate, dataCells1 = dataCells1, dataCells2 = label, dataCells3 = left, dataCells4 = top, dataCells5 = width, dataCells6 = height)
  
  #merge to global csv 
  allFiles(rbind(allFiles(),csvupdate))
  
    # pull that row from the image link csv file
    # load the image
    fileURL <- links_file()[imgNum(),2]
    
    realULRcorr <- gsub("dl=0","raw=1", fileURL)
    session$sendCustomMessage("change-img-url", realULRcorr)
    
    imageonlyworms <- allFiles() %>% filter(index == imgNum()) %>% select(-index)
    
    
    #convert image only worms to javascript format 
    #send boxes using bbox annotator 
    new_data <- imageonlyworms
    new_data1<-subset(new_data,select=-c(dataCells1,dataCells2))
    new_data1 <- round_df(new_data1,0)
    new_data2 <- new_data1
    
    
    data_as_char <- 1:dim(new_data2)[1]
    as.character(data_as_char)
    #new_data2$F <- data_as_char
    if (new_data[1,2] == "worm") { 
      new_data2$F <- data_as_char
    } else { 
      new_data2$F <- new_data$dataCells2
    }
    new_data2$G <- palette[1:dim(new_data2)[1]]
    colLabels <- c("left","top","width","height","label","color")
    colnames(new_data2) <- colLabels
    jsonfile <- toJSON(new_data2,pretty=TRUE)
    jsonfile2 <- str_replace_all(jsonfile, "[\r\n]" , "\n")
    
    
    #send message to load pre-annotted bounding boxes into the annotater program
    session$sendCustomMessage("redraw-rects", jsonfile2)
    
    
}) 


observeEvent(input$doneButton, {
    tempcsv <- allFiles()
    print(tempcsv)
    #print(tempcsv$dataCells1)
    #print(unlist(tempcsv$dataCells1))
    tempcsv$dataCells1 <- unlist(tempcsv$dataCells1)
    print(tempcsv)
    column <- tempcsv$dataCells2
    
    if ("worm" %in% column) { 
      write.csv(tempcsv, "incompleteGlobalAnnotationData.csv")
      finished_path = paste("/wormbot_data/packets/packet_", packet_num(), "/", sep = "")
      drop_upload("incompleteGlobalAnnotationData.csv", path = finished_path, dtoken = token)
    } else { 
      write.csv(tempcsv, "globalAnnotationData.csv")
      finished_path = paste("/wormbot_data/packets/packet_", packet_num(), "/", sep = "")
      drop_upload("globalAnnotationData.csv", path = finished_path, dtoken = token)
    }
    
}) 

observeEvent(input$category_type, {
    vals <- switch(input$category_type,
                   worms = list("red" = "1","red4" = "2", "orangered" = "3", "darkorange3" = "4", 
                                "orange" = "5", "coral" = "6", "goldenrod" = "7", "gold" = "8", 
                                "yellow" = "9", "yellowgreen" = "10", "olivedrab1" = "11", "limegreen" = "12", 
                                "springgreen" = "13", "seagreen1" = "14", "palegreen" = "15", "aquamarine1" = "16", 
                                "cyan" = "17", "turquoise" = "18", "blue" = "19", "dodgerblue1" = "20", "blueviolet" = "21", 
                                "darkorchid" = "22", "darkslateblue" = "23", "purple" = "24", "mediumorchid" = "25", 
                                "violet" = "26", "lightpink2" = "27", "hotpink" = "28", "maroon1" = "29", 
                                "magenta" = "30"),
                   geography = list("grey" = "well",
                                    "brown" = "bacteria",
                                    "tan" = "something else")
    )
    #update category list
    session$sendCustomMessage("update-category-list", vals)
    #redraw rectangles
    session$sendCustomMessage("redraw-rects", input$rectCoord)
})

}

# Run the application 
shinyApp(ui = ui, server = server)


    
    