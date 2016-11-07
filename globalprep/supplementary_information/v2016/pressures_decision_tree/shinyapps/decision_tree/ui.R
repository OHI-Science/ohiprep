library(shiny)
library(shinyapps)
library(raster)
library(rgdal)
library(rasterVis)

shinyUI(fluidPage(
  
  titlePanel("Decision Tree for Pressures"),
  
  sidebarLayout(
    sidebarPanel(   
      selectInput('layer',"Pressures layer",
                  list('Organic Carbon' = 'carbon',
                       'Ultraviolet_all' = 'uv',
                       'Ultraviolet_positive' = 'uv_null',
                       'Sea Level Rise'='slr',
                       'Shipping' = 'ship',
                       'Fishing' = 'fish',
                       'Pesticide' = 'pest',
                       'Sea Surface Temperature'='sst', #big file
                       'Ocean Acidification' = 'acid',
                       'Inorganic Nitrogen' = 'in_nitro',
                       'Organic Nitrogen' = 'on_nitro',
                       'Marine Plastics (count size 1)' = 'plastic_count1',
                       'Marine Plastics (weight size 1)' = 'plastic_weight1',
                       'Marine Plastics (count size 4)' = 'plastic_count4',
                       'Marine Plastics (weight size 4)' = 'plastic_weight4')),
      
      selectInput('trans',"Transformation:",
                  list('None'='none',
                    'log' = 'log',
                    'log10' = 'log10')),
      
      numericInput('quant','Quantile:',
                   0),
      
      selectInput('ref',"Reference Point:",
                list('None' = 'none',
                    'Max value' = 'max',
                    'Max value + 110%' = 'max1.1',
                    'Quantile' = 'quant')),
      
      h4("Range"),
      verbatimTextOutput("range"),
      
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Spatial Resolution"),
      verbatimTextOutput("resolution"),
      
      h4("Number of Cells"),
      verbatimTextOutput("ncells")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map",plotOutput("mapPlot")),
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Boxplot",plotOutput("boxPlot")),
        tabPanel("Quantiles",tableOutput("table"))
      )
    
  )
)
)
)



