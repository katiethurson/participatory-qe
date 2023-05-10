library(shiny)
library(shinydashboard)
library(visNetwork)
library(tidyverse)
library(DT)
library(shiny.i18n)


# Define UI
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "CONNECT"),
  dashboardSidebar(
    fileInput("networkFile", "Upload Network Data (CSV format):"),
    actionButton("loadNetwork", "Load Network Data"),
    br(),
    helpText("Create nodes and labels for the network."),
    textInput("nodeId", "Node ID:"),
    textInput("nodeLabel", "Node Label:"),
    selectInput("nodeColor", "Node Color:", c("red", "blue", "green")),
    actionButton("addNode", "Add Node"),
    br(),
    downloadButton("downloadNetwork", "Download Network Data")
  ),
  dashboardBody(box(title = "Node Table", status = "primary", width = 6, collapsible = TRUE,
    dataTableOutput("nodeTable"),
    visNetworkOutput("network")),
    box(
      title = "Upload Data",
      status = "primary", 
      width = 6,
      fileInput("file", "Upload Data (.csv, .xls, or .txt)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xls",
                           ".xlsx",
                           ".txt")),
      collapsible = TRUE
    ),
    box(
      title = "Transcript Data",
      status = "primary", 
      width = 6,
      collapsible = TRUE,
      DT::dataTableOutput("table")
    ),
    )
  )


# Define server
server <- function(input, output, session) {
  
  # Initialize nodes and edges as reactive values
  nodes <- reactiveValues(data = data.frame(id = character(0), label = character(0), color = character(0), stringsAsFactors = FALSE))
  edges <- reactiveValues(data = data.frame(from = character(0), to = character(0), id = character(0), stringsAsFactors = FALSE))
  
  # Load network data from uploaded file
  observeEvent(input$loadNetwork, {
    if (is.null(input$networkFile)) {
      return(NULL)
    }
    network_data <- read.csv(input$networkFile$datapath)
    if ("id" %in% colnames(network_data) && "label" %in% colnames(network_data)) {
      nodes$data <- network_data[, c("id", "label", "color")]
      edges$data <- network_data[, c("from", "to", "id")]
    } else if ("from" %in% colnames(network_data) && "to" %in% colnames(network_data) && "id" %in% colnames(network_data)) {
      edges$data <- network_data
    } else {
      return(NULL)
    }
  })
  
  data_input <- reactive({
    req(input$file)
    
    tryCatch(
      {
        
        if (tolower(tools::file_ext(input$file$datapath)) == "xlsx") { 
          clean_data <- read_excel(input$file$datapath)
        }
        
        else {
          clean_data <- read.csv(input$file$datapath)
        }
        
        names(clean_data) <- make.names(names(clean_data),unique = TRUE)
        
        clean_data
        
      },
      
      error = function(e) {
        stop(safeError(e))
      }
    )
    
  })
  
  output$table <- DT::renderDataTable({
    
    data_input() %>%
      datatable(
        #filter = "bottom",
        #server = TRUE, 
        rownames = FALSE,
        class = "cell-border stripe",
        options = list(
          paging = TRUE,
          #searching = FALSE,
          autoWidth = TRUE)
      )
  })
  
  # Render node table
  output$nodeTable <- DT::renderDataTable({
    nodes$data
  }, editable = TRUE, rownames = FALSE)
  
  # Render the network diagram
  output$network <- renderVisNetwork({
    visNetwork(nodes$data, edges$data) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical"), manipulation = TRUE)
  })
  
  # Add new node on button click
  observeEvent(input$addNode, {
    new_node <- data.frame(id = input$nodeId, label = input$nodeLabel, color = input$nodeColor, stringsAsFactors = FALSE)
    nodes$data <- rbind(nodes$data, new_node)
  })
  
  # Handle node table edit event
  observeEvent(input$nodeTable_cell_edit, {
    info <- input$nodeTable_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value
    nodes$data[row, col] <- value
  })
  
  # Handle node delete event
  observeEvent(input$network_delete, {
    nodes$data <- nodes$data[-input$network_delete$id, ]
    edges$data <- edges$data[!(edges$data$from %in% input$network_delete$id | edges$data$to %in% input$network_delete$id), ]
  })
  
  # Add new edge on button click
  observeEvent(input$addEdge, {
    new_edge <- data.frame(from = input$edgeFrom, to = input$edgeTo, id = paste0(input$edgeFrom, "-", input$edgeTo), stringsAsFactors = FALSE)
    edges$data <- rbind(edges$data, new_edge)
  })
  
  # Download network data as CSV file, it looks greyed out due to shiny specs not bc it doesn't work
  output$downloadNetwork <- downloadHandler(
    filename = "network_data.csv",
    content = function(file) {
      write.csv(rbind(nodes$data, edges$data[, c("from", "to", "id")]), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}
# Run the app
shinyApp(ui = ui, server = server)
