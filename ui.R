library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Load custom CSS
  includeCSS("bayes.css"),
  
  hr(),
  
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      width = 4,  
      
      fluidPage(
        
        fluidRow(strong("Contingency Table")),
        fluidRow(helpText("Fill in the cells of the table with the results the trial under evaluation.")),
        
        fluidRow(column(width = 3),
                 column(h5("Event Occurred"), width = 3),
                 column(h5("No Event Occurred"), width = 3),
                 column(h5("Total"), width = 3)),
        
        fluidRow(column(h5("Intervention"), width = 3),
                 column(numericInput(inputId = "int_pos",
                                     label = NULL, 
                                     value = 134,
                                     min = 0),
                        width = 3),
                 column(numericInput(inputId = "int_neg",
                                     label = NULL, 
                                     value = 232,
                                     min = 0),
                        width = 3),
                 column(verbatimTextOutput("int_tot"),
                        width = 3)),
        
        fluidRow(column(h5("Control"), width = 3),
                 column(numericInput(inputId = "con_pos",
                                     label = NULL, 
                                     value = 158,
                                     min = 0),
                        width = 3),
                 column(numericInput(inputId = "con_neg",
                                     label = NULL, 
                                     value = 206,
                                     min = 0),
                        width = 3),
                 column(verbatimTextOutput("con_tot"),
                        width = 3)),
        
        fluidRow(column(h5("Total"), width = 3),
                 column(verbatimTextOutput("tot_pos"), width = 3),
                 column(verbatimTextOutput("tot_neg"), width = 3),
                 column(verbatimTextOutput("tot_tot"), width = 3))
      ),
      
      hr(),
      
      fluidPage(
        fluidRow(strong("Prior")),
        fluidRow(helpText("Use the slider to select the 95% centiles for the event rate for both the control and intervention groups")),
        fluidRow(column(h5("Control Group"),
                        width = 3),
                 column(sliderInput(inputId = "priorCon",
                                    label = "",
                                    min = 1,
                                    max = 99,
                                    value = c(40, 60),
                                    dragRange = TRUE),
                        width = 8),
        ),
        fluidRow(column(h5("Intervention Group"),
                        width = 3),
                 column(sliderInput(inputId = "priorInt",
                                    label = "",
                                    min = 1,
                                    max = 99,
                                    value = c(28, 48),
                                    dragRange = TRUE),
                        width = 8),
        ),
      ),
      
      hr(),
      
      fluidPage(
        fluidRow(strong("Plot Settings")),
        fluidRow(column(h5("Highest Density Interval (%)"),
                        helpText("Select the width for the HDI."),
                        width = 6),
                 column(numericInput(inputId = "hdi",
                                     label = NULL, 
                                     value = 95,
                                     min = 1,
                                     max = 99),
                        width = 3),
        ),
        fluidRow(column(h5("Region of Practical Equivalence (%)"),
                        helpText("Select the range above and below 0 which indicates the ROPE."),
                        width = 6),
                 column(numericInput(inputId = "rope",
                                     label = NULL, 
                                     value = 5,
                                     min = 1,
                                     max = 99),
                        width = 3),
        )
      ),
      
      hr(),
      
      fluidPage(
        fluidRow(actionButton(inputId = "plotButton",
                              label = "Plot!"))
      )
    ),
    
    mainPanel(
      width = 8,
      fluidPage(
        fluidRow(
          column(h5("Posterior Distribution"),
                 width = 12,
                 height = 12,
                 plotOutput("plotPost")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(h5("Prior Distribution"),
                 width = 6,
                 height = 6,
                 plotOutput("plotPrior")
          ),
          column(h5("Description"),
                 width = 6,
                 height = 6,
                 style = "border-left: 1px solid`",
                 htmlOutput("plotDesc")
          )
        ),
      )
    ),
  ),
  
  hr(),
  
  helpText(HTML("Example data from: Pearse RM, Harrison DA, MacDonald N, Gillies MA, Blunt M, Ackland G, et al. <em>Effect of a Perioperative, Cardiac Output–Guided Hemodynamic Therapy Algorithm on Outcomes Following Major Gastrointestinal Surgery: A Randomized Clinical Trial and Systematic Review</em>. JAMA. 2014 Jun 4;311(21):2181. 
<a href=\"https://jamanetwork.com/journals/jama/fullarticle/1873985\">https://doi.org/10.1001/jama.2014.5305</a>.
")),
)