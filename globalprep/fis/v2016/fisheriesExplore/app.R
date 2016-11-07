#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(reshape2)
library(DT)
library(rhandsontable)
library(ggplot2)

data <- read.csv('data.csv')

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Exploring Fisheries Goal Options"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # radioButtons("weight",
         #             label = h3("Weighting options:"),
         #             choices = c('Geometric Mean' = 'geom',
         #                         'Catch weighted' = 'weight'),
         #             selected = 'Geometric Mean'),
         # 
         # br(),
         # 
         # checkboxGroupInput("penalty", label = h3("Penalty"),
         #                    choices = c("Taxonomic Only","Unreported Catch","Both"),
         #                    selected = 'Taxonomic Only'),
         # 
         # br(),
        
        h4("Scenario 1:"),
        h5("High amount of catch from underfished stocks (Bbmsy > 1.05)"),
        h4("Scenario 2:"),
        h5("High amount of catch from overfished stocks (Bbmsy<0.95)"),
        h4("Scenario 3:"),
        h5("Majority of catch at species level"),
        h4("Scenario 4:"),
        h5("Majority of catch at higher taxon levels"),
         
         h3("Taxonomic Penalties"),
         
         rHandsontableOutput('table')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        plotOutput("Plot"),
        
        tableOutput('result')

      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  #taxonomic penalty table
  
  values <- reactiveValues(df=data.frame('Level'  = c("Species","Genus","Family","Order","Class","Other"),
                                           'Number' =  c(1:6),
                                           'Penalty' =  c(1, 0.9, 0.8, 0.5, 0.25, 0.01)))
  
  observe({
    if(!is.null(input$table))
      values$df <- hot_to_r(input$table)
    
  })
  
  
  output$table <- renderRHandsontable({
    rhandsontable(values$df)
  })
  
  
  tTable <- reactive({ 
    
    t <- data%>%rowwise()%>%
      mutate(StockScore     = ifelse(bbmsy<0.95,bbmsy,ifelse(bbmsy >1.05,max(1-0.5*(bbmsy-1.05),0.25),1)))%>%
      data.frame()%>%
      mutate(score_tpenalty = StockScore * values$df$Penalty[match(.$taxon_level,values$df$Number)],
             score_cpenalty = StockScore * (1-un_prop),
             score_bpenalty = StockScore * ((1-un_prop)*(values$df$Penalty[match(.$taxon_level,values$df$Number)])))
  })
  
  output$result <- renderTable({
    
   tTable()
  })
  
  output$Plot <- renderPlot({
  
    score_table<-tTable()%>%
      mutate(score_tpen_geom = score_tpenalty^wprop,
             score_tpen_weight = score_tpenalty*wprop,
             score_cpen_geom = score_cpenalty^wprop,
             score_cpen_weight = score_cpenalty*wprop,
             score_bpen_geom = score_bpenalty^wprop,
             score_bpen_weight = score_bpenalty*wprop)%>%
      group_by(scenario)%>%
      summarise(geom_c = prod(score_cpen_geom)*100,
                geom_t = prod(score_tpen_geom)*100,
                geom_b = prod(score_bpen_geom)*100,
                weight_c = sum(score_cpen_weight)*100,
                weight_t = sum(score_tpen_weight)*100,
                weight_b = sum(score_bpen_weight)*100)%>%
      melt("scenario")%>%
      separate(variable, c('weight','pen'), sep = '_')%>%
      mutate(value = ifelse(value > 100, 100, value))
    
    
    
    # plot_table <- score_table%>%
    #                 filter(weight == input$weight)
    
    
    ggplot(score_table,aes(x = scenario, y = value)) +
      geom_point(aes(colour = weight, shape = pen),size = 4)+scale_shape_discrete(name = "Penalty",
                                                                                 breaks=c("b", "c","t"),
                                                                                 labels=c("Both", "Unreported catch", "Taxonomic"))+
      xlab("Scenario")+ylab("Score")+theme_grey(base_size = 18) 
    
  })
    
})

# Run the application 
shinyApp(ui = ui, server = server)

