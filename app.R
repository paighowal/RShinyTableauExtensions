# filetype: shinyApp

library(shiny)
library(shinytableau)
library(promises)
library(shinyvalidate)
library(ggplot2)
library(igraph)
library(networkD3)

manifest <- tableau_manifest_from_yaml()

ui <- function(req) {
  fillPage(theme = shinytableau_theme(),
           simpleNetworkOutput("plot", height = "100%"
           )
  )
}

server <- function(input, output, session) {
  df <- reactive_tableau_data("data_spec",
                              options = list(includeAllColumns = TRUE)
  )
  
  observeEvent(input$plot_brush, {
    worksheet <- req(tableau_setting("data_spec")$worksheet)
    tableau_select_marks_by_brush_async(worksheet, input$plot_brush)
  })
  
  output$plot  <- renderSimpleNetwork({
    plot_title <- tableau_setting("plot_title")
    xvar <- tableau_setting("xvar")
    yvar <- tableau_setting("yvar")
    df() %...>% {
    # Plot
   simpleNetwork(., height="100px", width="100px",        
                       Source = 1,                 # column number of source
                       Target = 2,                 # column number of target
                       linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                       charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                       fontSize = 14,               # size of the node names
                       fontFamily = "serif",       # font og node names
                       linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                       nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                       opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                       zoom = T                    # Can you zoom on the figure?
    )
      }
  })
}

config_ui <- function(req) {
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title"),
      choose_data_ui("data", "Choose data"),
      uiOutput("var_selection_ui")
    ),
    mainPanel(
      tableOutput("preview")
    )
  )
}

config_server <- function(input, output, session, iv) {
  iv$add_rule("xvar", sv_required())
  iv$add_rule("yvar", sv_required())
  
  data_spec <- choose_data("data", iv = iv)
  
  
  data <- reactive_tableau_data(data_spec,
                                options = list(includeAllColumns = TRUE, maxRows = 5)
  )
  
  output$preview <- renderTable({
    data()
  })
  
  
  schema <- reactive_tableau_schema(data_spec)
  
  output$var_selection_ui <- renderUI({
    vars <- schema()$columns$fieldName
    tagList(
      selectInput("xvar", "Dimension", vars),
      selectInput("yvar", "Measure", vars)
    )
  })
  
  
  save_settings <- function() {
    update_tableau_settings_async(
      plot_title = input$title,
      data_spec = data_spec(),
      xvar = input$xvar,
      yvar = input$yvar
    )
  }
  return(save_settings)
}

tableau_extension(
  manifest, ui, server, config_ui, config_server,
  options = ext_options(config_width = 900, config_height = 600, port = 2468)
)
