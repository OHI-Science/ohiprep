#
# This is a Shiny web application to explore OHI fisheries data



source('~/github/ohiprep/src/R/common.R')
source('plots.R')
library(readr)
library(shiny)
library(ggplot2)

# This file takes a long time to read in so i'm just going to read in the catch_pre_bbmsy instead
## catch         <- read_csv(file.path(dir_M,'git-annex/globalprep/fis/v2016/int/catch_saup.csv'))

catch           <- read.csv('../int/catch_pre_bbmsy.csv')

#need to aggregate the ram data to FAO region before adding to this
#ram_ts         <- read_csv('../ram/ram_timeseries.csv')

cmsy_bbmsy      <- read.csv('../int/cmsy_bbmsy.csv')%>%
                     separate(stock_id,into=c("species","fao_rgn"),sep="-",remove=F)%>%
                      select(-species)
  
cmsy_bbmsy_uni  <- read.csv('../int/cmsy_bbmsy_uni_prior.csv')%>%
                    separate(stock_id,into=c("species","fao_rgn"),sep="-",remove=F)%>%
                     select(-species)
  
#comsir_bbmsy   <- 

datasets <- c("cmsy_bbmsy","cmsy_bbmsy_uni")
  

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Exploring Fisheries Data for OHI 2016"),

   sidebarLayout(
     
     sidebarPanel(
       selectInput('dataset', 'Choose Dataset', choices = c("cmsy_bbmsy","cmsy_bbmsy_uni")),
       br(),
       
        uiOutput("choose_species"),
        br(),
        
        uiOutput("choose_fao"),
        br()

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("bbmsyPlot"),
        plotOutput("catchPlot")
      )
   )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  
species_opts <- reactive({
    
    dat <- get(input$dataset)%>%
              filter(!is.na(bbmsy_mean))
    sp <- unique(as.character(dat$common))%>%
            sort
    return(sp)
  })

  
  output$choose_species <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    # Create the checkboxes and select them all by default
    selectInput("species", "Choose species", 
                choices  = species_opts())
  })
  
fao_opts <- reactive({
  
    dat <- get(input$dataset)%>%
            filter(common==as.character(input$species))
    
    fao_rgns<-unique(dat$fao_rgn)
    
  })
  
  output$choose_fao <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$species))
      return()
    
    # Create the checkboxes and select them all by default
    radioButtons("fao", "Choose FAO area", 
                 choices  = fao_opts())

    
  })
   
   output$bbmsyPlot <- renderPlot({

      x <- get(input$dataset)%>%
          filter(common == input$species,
               fao_rgn == input$fao)

      ggplot(x,aes(x = year, y = bbmsy_mean))+
               geom_line()+
      geom_hline(yintercept=1, linetype="dashed",
                                     color = "red")

   })

   output$catchPlot <- renderPlot({

     x<- catch%>%
       filter(common == input$species,
              fao_rgn == input$fao)

     ggplot(x,aes(x = year, y = tons))+
       geom_line()

   })
})



   
   
   
# Run the application 
shinyApp(ui = ui, server = server)

