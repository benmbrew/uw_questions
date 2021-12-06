library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(DT)

source('global.R')
header <- dashboardHeader(title="Life insurance applications")
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            text="Search PDFs",
            tabName="search",
            icon=icon("eye")),
        menuItem(
            text = 'Download PDFs',
            tabName = 'download',
            icon = icon("cog", lib = "glyphicon"))
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        tabItem(
            tabName="search",
            fluidPage(
                fluidRow(
                    column(4,
                           div(textInput(inputId = 'keywords', 
                                     label = 'Search key words', 
                                     value = NULL), style = 'font-size:120%')
                           ),
                    column(4, 
                           uiOutput('company_name_ui')
                           )),
                fluidRow(
                    uiOutput('extend_text_ui')
                )
                    
                    
                ,
                fluidRow(
                    div(DT::dataTableOutput('keyword_table'), style = 'font-size:120%')
                )
            )
        ),
        tabItem(
            tabName = 'download',
            fluidPage(
                
            )
        )
    )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
    
    # create input for company name based on the keywords found
    output$company_name_ui <- renderUI({
        kw <- input$keywords
        if(is.null(kw) | kw==''){
            NULL
        } else {
            tmp <- pdfs[grepl(kw, pdfs$app_text, ignore.case = T),]
            cn <- sort(unique(tmp$company))
            cn <- Hmisc::capitalize(cn)
            div(selectInput('company_name',
                        label = 'Choose company',
                        choices = cn,
                        selected = cn[1]),style = 'font-size:120%')
        }
    })
    
    # create input to allow user to extend text based on the company chosen
    output$extend_text_ui <- renderUI({
        cn <- input$company_name
        if(is.null(cn)){
            NULL
        } else {
            fluidPage(
                column(4,
                       div(sliderInput(inputId = 'more_words',
                                   label = "Show additional rows",
                                   min = 0,
                                   max = 10,
                                   value = 0)),style = 'font-size:120%'),
                column(4,
                       div(sliderInput(inputId = 'more_words_back',
                                   label = "Show previous rows",
                                   min = 0,
                                   max = 10,
                                   value = 0)), style = 'font-size:120%') 
            )
        }
    })

   
   get_text <- reactive({
       # kw <- 'felony'
       # cn <- 'aaa life insurance company'
       # mw <- 0
       # mwb <- 4
       kw <- input$keywords
       cn <- input$company_name
       mw <- input$more_words
       mwb <- input$more_words_back
       if(is.null(mw)){
           NULL
       } else {
           if(cn==''){
               out <- data_frame(' '= 'No questions meet the search criteria')
           } else {
               cn <- tolower(cn)
               sub_pdfs <- pdfs[pdfs$company == cn,]
               kw_index <- which(grepl(kw,sub_pdfs$app_text, ignore.case = T))
               index_list <- list()
               for(i in 1:length(kw_index)){
                   this_index <- kw_index[i]
                   tmp <- sub_pdfs[this_index,]
                   tmp$app_text <- trimws(str_replace_all(tmp$app_text, "[[:punct:]]", " "), which = 'both')
                   
                   # if starts with lower case, add the last line to the index
                   if(Hmisc::capitalize(tmp$app_text) != tmp$app_text){
                       this_index <- c(this_index, this_index-1)
                   } 
                   
                   if(mw>0){
                       end <- max(this_index)+mw
                       this_index <- c(this_index, (max(this_index)+1):end) 
                   }
                   if(mwb>0){
                       end <- min(this_index)-mwb
                       this_index <- c(this_index, (min(this_index)-1):end)  
                   }
                   index_list[[i]] <- this_index
               }
               kw_index <- unlist(index_list)
               out <- sub_pdfs[sort(unique(kw_index)),]
           }
           return(out)
       }

   })

   output$keyword_table <- renderDataTable({
       out <- get_text()
       if(is.null(out)){
          NULL
       } else {
           if(ncol(out)==1){
               datatable(out, options = list(dom = 't',lengthChange = FALSE),
                         rownames= FALSE) 
           } else {
               names(out) <- c('Question', 'Company Name')
               datatable(out, options = list(dom = 't',lengthChange = FALSE),
                         rownames= FALSE) 
           }
          
       }
       
       
   })
}

shinyApp(ui, server)