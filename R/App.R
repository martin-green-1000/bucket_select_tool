



library(shiny)
library(sortable)
library(readr)
library(readxl)
library(tidyverse)
library(bslib)
library(shinydashboard)
library(arsenal)
library(shinyjs)
library(tableHTML)
library(cli)


ui <- dashboardPage(
  
  dashboardHeader(titleWidth=920, title = span(" ", span("Selection Tool for EKE ", 
                                                         style = "color: WHITE; font-size: 28px")),
                  tags$li(class = "dropdown", tags$a(HTML(paste("Click horizontal lines to see explanations ", textOutput("See explanations "))),style = "color: silver; font-size: 14px;font-family: Arial;"))
  ),
  
  dashboardSidebar(collapsed = TRUE, width = 500,
                   h4("Explanations of Selectable Items",style="color:white"),
                   id=h4(tableOutput("table"),style =  " line-height: 3px;font-size:11px;")
  ),
  
  
  dashboardBody(
    
    fluidRow(
      column(
        width = 10, offset=1,
        height = 2,
        
        h6(textInput("filename", em("Input your name here"), value = ("")),style="background-color:#ff8282;color:white; border-color:#ff8282;padding:2px; font-size:120%; font-family: Arial"),
        
        tabsetPanel(
          
          type = "tabs",
          
          tabPanel(
            
            h4("Items to Select", style="background-color:white;color:blue;font-weight: bold;  font-size:100%; font-family: Arial"),
            uiOutput("sortable", style =  " line-height: 3px;font-weight: bold;font-size:14px;font-family: Arial")
          ),
          
          tabPanel(
            
            actionButton("saveBtn", "Submit", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
          ),
          
          
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          tabPanel(""),
          
          tabPanel(
            h6("Moderator", style="background-color:white;font-size:75%; font-family: Arial"),
            
            tabsetPanel(
              
              tabPanel(
                
                h6("Collect Data"),
                fluidRow(column(3,
                                numericInput("password", "Password:", "enter", width = "100px"),
                                column(4,  
                                       tags$style("#name_openend {font-size:20px;
               color:red;
               display:block;
               font-style: italic;
              width:400px}"),
                                       uiOutput("name_openend"))
                                
                )),
                br(),
                actionButton("saveBtn2", "PRESS HERE TO COLLECT DATA", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                br(),
                br(),
                tags$head(
                  tags$style(HTML(".myclass pre {
        color: blue;
        font-size:150%
        background-color: pink;
        font-weight: bolder;
        font-size: 12px;
        font-family: Arial;
        width: 180px;
      }"))),
                div(class = "myclass",verbatimTextOutput("done", placeholder = FALSE)),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("Loading...",id="loadmessage"))
                
              ),
              
              
              tabPanel(
                h6("Participants Submitted"),
                
                fluidRow(column(4, 
                                br(),
                                tableOutput('table3'))),
              ),    
              
              
              
              tabPanel(
                h6("Results"),
                fluidRow(column(9, offset=1, 
                                br(),
                                br(),
                                h5(verbatimTextOutput("info", placeholder = FALSE)) ,
                                tags$head(tags$style("#info{border-color:#6BADCE;;background-color:#6BADCE;color: white; font-size: 12px;font-weight: bold;font-family: Arial;}"
                                )),
                                plotOutput("plot1", width = "100%"),
                                br(),
                                br(),
                                tableOutput('table1'),
                                br(),
                                tableOutput('table4'),
                                br(),
                                tags$head(
                                  tags$style( HTML(" #table5{
                                  text-align:right; } "
                                  ))),
                                tableOutput('table5'),
                                br(),
                                br()
                )
                )),
              
              tabPanel(
                h6("Download Results Files"),
                br(),
                br(),
                downloadButton("saveBtn_raw", "PRESS HERE TO SAVE RAW DATA", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                br(),  
                br(), 
                h4("Raw Data Table"),
                br(),
                tableOutput('raw_data')
                
              ),
              
              
              tabPanel(""),
              tabPanel(""),
              tabPanel(""),
              tabPanel(""),
              tabPanel(""),
              tabPanel(""),
              
              tabPanel(
                h6("Set up Input Files"),
                
                tabsetPanel(
                  
                  tabPanel(
                    h6("Upload file"),
                    br(),
                    br(),
                    h4("The data input (CSV) file requires two columns WITHOUT HEADINGS:"),
                    h4("Column 1 should contain the choice of items to select and column 2 their explanations"),
                    br(),
                    fileInput("file1", "Choose CSV File"),
                    
                    br(),
                    br(),
                    tableOutput('contents'),
                    br(),
                    br(),
                    actionButton("saveBtn_input", "PRESS HERE SEND INPUT FORM - REQUIRES PASSWORD UNDER COLLECTION TAB", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                    br(),
                    br(),
                    useShinyjs(),
                    actionButton("refresh", "Then refresh here", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                    br(),
                    ),
                  
                  tabPanel(
                    h6("Server file viewer"),
                    br(),
                    br(),
                    h4("Table of all App files on the server"),
                    br(),
                    br(),
                    tableOutput('files_all'),
                    br(),
                    numericInput("nmb_del","Enter row number of file to delete, then press delete button below.... App will refresh in a few seconds", "" ),
                    br(),
                    actionButton("del_one", "Delete row of table", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                    br(),
                    br()
                    
                  )
                ))
              
            ))
          
          
        ))))) 


server <- function(input, output, session) {
  
  mydata <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, header=FALSE)#, sep=input$sep,  dec = input$dec)
    
    return(tbl)
  })
  
  input_wc_terms <- as.data.frame(read_csv("input_wc_terms.csv"))
  
  observeEvent(input$saveBtn_input, {
    
    
    
    if(input$password==1234){
      
      write_csv(mydata(), paste0("input_wc_terms_2", ".csv"))
      
      input_new <- as.data.frame(read_csv(paste0("input_wc_terms_2", ".csv")))
      
      write_csv(input_new, "input_wc_terms.csv")
      
    }
  }) 
  
  observeEvent(input$refresh, {
    refresh()
  })
  
  
  ## LOAD NAME INPUT FILE INTO RANKING TOOL
  
  input_data_names <- as.data.frame(read_csv("input_wc_terms.csv"))
  input_data_names_2 <- sample(as.vector(noquote((input_data_names$V1))))
  labels <- unique(input_data_names_2)
  names_explained <- input_data_names
  colnames(names_explained) <- c("Item", "Explanation")
  
  
  output$sortable <- renderUI({
    
    bucket_list(
      header = "",
      add_rank_list(
        text = h5("Move to right hand box to select (press 'Submit' when finalised)", style="color:blue; font-size:100%; font-family: Arial; text-align: center"),
        labels = labels,
        input_id = "rank_list_basic"
      ),
      add_rank_list(
        text = h5("SELECTED", style="color:blue; font-size:100%; font-family: Arial; text-align: center"),
        labels = NULL,
        input_id = "rank_list_2"
      ))
    
  })
  
  output$results_basic <- renderDataTable({
    (input$rank_list_2 )# This matches the input_id of the rank list
  })
  
  
  
  output$table <- renderTable({names_explained 
  })
  
  # Downloadable csv of selected dataset ----
  
  my_string<-sort(c(as.vector(substr(labels, start = 1, stop = 1)), as.vector(substr(labels, start = min(nchar(labels)) , stop = min(nchar(labels)) ))))
  updated_string <- paste(my_string,collapse='')
  updated_string2 <-gsub(" ", "", updated_string)
  
  
  observeEvent(input$saveBtn, {
    
    validate(
      need(length(input$rank_list_2 )>0, "Please select at least one item")
    )
    
    write_csv(as.data.frame(input$rank_list_2 ), paste0(input$filename, "_WC747_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv"))
    
  })
  
  ############ FOR MODERATOR TABS ###
  
  p_w <- reactive(input$password) 
  
  output$name_openend <- renderUI({
    req(input$password)
    if(input$password==1234){
      condition <- tags$div(
        tags$i(class = "fa fa-check")) #"Correct"
    } else{
      condition <- "PASSWORD INCORRECT"
    }
  })
  
  ##
  
  observeEvent(input$saveBtn2, {
    
    list_in <- as.data.frame(list.files(pattern="*.csv"))
    colnames(list_in)[1] <- "nme"
    files <- dplyr::filter(list_in, grepl(updated_string2,nme))
    files$name_included <- substr(files$nme,1,nchar(files$nme)-(20+ nchar(updated_string2)))
    
    files_all <- as.data.frame(files) ## FOR THE LATER SERVER VIEWING OF FILES
    colnames(files_all) <- c("FILE CODE_DATE", "Participant")
    
    db_all <- data.frame()
    db_selected <- data.frame()
    for( i in 1:nrow(files)){
      test <- read_csv(files$nme[i])
      test$name <- files$name[i]
      test$selected <- "Y"
      colnames(test)[1]<- "V1"
      db_all <- rbind(db_all, test)
      
      db_all_selected_names <- as.data.frame(input_data_names)
      names_not_selected <- as.data.frame(setdiff(db_all_selected_names$V1, test$V1))
      names_not_selected$name <- files$name[i]
      colnames(names_not_selected)[1]<- "V1"
      names_not_selected$selected <- "N"
      colnames(db_all)[1] <- "V1"
      
      selected_y_n <- rbind(db_all, names_not_selected)
      db_selected <- rbind(db_selected, selected_y_n)
      db_selected$name <- gsub(paste0("_WC747_",updated_string2),'_',db_selected$name)
    }
    
    
    
    colnames(db_all)[1] <- "item"
    colnames(db_selected)[1] <- "item"
    db_all_out <<- db_selected
    
    
    
    
    if(p_w() == 1234){
      
      
      output$done <- renderText({
        paste0(nrow(files)," participants data received" )
      })
    }
    
    
    #nrow(db_all)
    colnames(db_all)[1] <- c("item")
    #view(db_all)
    
    group_wc <- db_all %>%
      group_by(item) %>%
      summarise(mean_wc_score = n()/nrow(files))%>%
      arrange(-mean_wc_score)
    
    colnames(group_wc)<- c("Selected_items", "Proportion_participants_selected")
    
    output$plot1 <- renderPlot({
      ggplot(group_wc, aes(x=reorder(Selected_items, -Proportion_participants_selected, FUN=median), y=Proportion_participants_selected))+
        geom_point( size = 3) +
        ylab("Proportion selecting item")+
        xlab("")+
        ylim(0,1)+
        ggtitle("Rating of Items")+
        theme(axis.text.x=element_text(angle = 60, hjust = 1), axis.text=element_text(size=12),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size=14),  plot.title = element_text( size=18))
      
      
    })
    
    
    
    
    WC_RESULT <- 
      
      df_wc<- reactive({data.frame(group_wc)
        wc_tab <- group_wc
        colnames(wc_tab)<- c("Selected Items", "Proportion of Participants Selecting")
        wc_tab
        
      })
    
   
    all_wc_in <- as.data.frame(input_data_names)
    all_wc <- as.data.frame(unique(all_wc_in$V1))
    
    selected_wc <- count(db_all,item)
    colnames(selected_wc)[1]<- "wc"
    colnames(all_wc)[1]<- "wc"
    not_included_calc <- as.data.frame(c(selected_wc$wc, all_wc$wc ) )
    colnames(not_included_calc)[1]<- "wc"
    not_included_calc2 <- count(not_included_calc, wc)
    not_included_calc2$n <- as.numeric(not_included_calc2$n)
    colnames(not_included_calc2)[2] <- "numb"
    Not_Included <- not_included_calc2 %>%  filter(numb<2)
    
    
    
    WC_not_in <- 
      df_not_3 <- reactive({data.frame(Not_Included)
        df_not_4 <- as.data.frame(Not_Included[,1])
        colnames(df_not_4) <- "Items not selected"
        df_not_4
        
      })
    
    
    output$table1 <- renderTable({
      WC_RESULT()
    })
    output$table4 <- renderTable({
      WC_not_in()
    })
    output$info <- renderText({
      paste0(nrow(files)," input files included in graph" )
    })
    
    names_done <- reactive({
      df_name <- as.data.frame(t(files$name_included))
      names(df_name) <- NULL
      df_name
    })
    
    output$table3 <- renderTable({
      names_done()
    })
    
    
    group_exp <- db_all %>%
      group_by(name) %>%
      summarise(count_exp_sel = n())
    
    #group_exp$name <- gsub('.{16}$', '', group_exp$name)
    group_exp$name <- gsub(paste0("_WC747_",updated_string2),'_',group_exp$name)
    group_exp$prop <- group_exp$count_exp_sel / nrow(all_wc)
    group_exp$overall_mean <- round(mean(group_exp$prop),2)
    group_exp[2:nrow(group_exp),4] <- NA
    group_exp$overall_mean <- as.character(group_exp$overall_mean)
    group_exp <- group_exp %>% 
      mutate_if(is.character, ~replace_na(.,""))
    
    
    exp_selected <- reactive({
      
      df_exp <- as.data.frame(group_exp)
      colnames(df_exp)<- c("Participant", "Number items selected", "Proportion items selected", "Mean proportion selected by all participants")
      df_exp
    })
    
    output$table5 <- renderTable({
      exp_selected()
    })
    
    ## FILE VIEWER FOR MODERATOR
    
    files_all$Row <- seq(1,nrow(files_all),1)
    files_all <- files_all[,c(3,2,1)]
    observeEvent(input$del_one, {
      
      file.remove(paste0(files_all$`FILE CODE_DATE`[input$nmb_del]))
      
      session$reload()
      return()
    })
    
    
    output$files_all<- renderTable({
      
      files_all
          }, digits = 0)
    
    
  })
  
  output$contents <- renderTable({
    
    
    mydata()
    
  })
  
  output$raw_data<- renderTable({
    
    db_all_out
    
  })
  
  output$saveBtn_raw <- downloadHandler(
    filename = function() {
      paste("Raw_welfare_conseq_data", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(db_all_out, file)
    }
  )
  
  
  
}


shinyApp(ui, server)
