#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinycssloaders)
library(reactable)
library(shinyBS)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinytheme("simplex"), 
    
    # Open popover
    tags$head(HTML("
                   <script type=text/javascript>
                        $(function() { 
                            setTimeout(function() {
                                $('#submitBtn').popover('show');
                                console.log('Event fired');
                            }, 1000)
                         }); 
                   </script>
                   ")),

    # Application title
    titlePanel("ESG Simulator"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            
            # Demand 
            fileInput("demFile", "Choose demand file", 
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")), 
            
            tags$a(href='data/demand_02.csv',
                   target='blank', 
                   'Use this template for the demand file',
                   download = "demand_template.csv"),
            
            hr(),
            
            # Supply 
            fileInput("supFile", "Choose supply file", 
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            tags$a(href='data/portfolios_01.csv',
                   target='blank', 
                   'Use this template for the supply file',
                   download = "supply_template.csv"),
            
            hr(),
            
            helpText("Download and modify the data in the templates. 
                     Then re-upload and click on the Update button to 
                     run the simulation. Files must be in .csv and must 
                     have the same columns from the templates. "),
            
            # Submit
            actionButton("submitBtn", "Update", icon("refresh")),
            
            bsPopover("submitBtn",
                      "Getting started",
                      "Click on the Update button to get started.", 
                      placement = "right", trigger = "focus")

        ),

        # Main content
        mainPanel(
            
            tabsetPanel(
                tabPanel("Market results",
                         
                         tags$br(),
                         
                         fluidRow(
                             column(6, withSpinner(plotlyOutput("plot_prices"))),
                             column(6, withSpinner(plotlyOutput("plot_demand")))
                         ), 
                         
                         tags$br(),
                         
                         fluidRow(
                             column(6, withSpinner(plotlyOutput("plot_flow"))),
                             column(6, withSpinner(plotlyOutput("plot_tax")))
                         ),
                         
                         tags$br(),
                         
                         dataTableOutput("res_market")
                ), 
                
                tabPanel("P&L",
                         reactableOutput("res_yr"),
                         downloadButton("downloadFullData", "Download full data")),
                
                # @TODO: modify this to generate days automatically
                tabPanel("Day 1",
                         withSpinner(plotlyOutput("day1_plot", height = "700px")), 
                         dataTableOutput("day1_data")), 
                
                tabPanel("Day 2",
                         withSpinner(plotlyOutput("day2_plot", height = "700px")), 
                         dataTableOutput("day2_data")), 
                
                tabPanel("Day 3",
                         withSpinner(plotlyOutput("day3_plot", height = "700px")), 
                         dataTableOutput("day3_data")), 
                
                tabPanel("Day 4",
                         withSpinner(plotlyOutput("day4_plot", height = "700px")), 
                         dataTableOutput("day4_data"))
            )

        )
    )
))
