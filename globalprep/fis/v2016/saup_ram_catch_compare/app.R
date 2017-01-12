#

df <- read.csv('~/github/ohiprep/globalprep/fis/v2016/saup_ram_catch_compare/saup_ram_catch_ts.csv', stringsAsFactors = F)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Compare SAUP and RAM catch data for species at FAO level"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("species", "Species Code:", 
                  choices=as.character(unique(df$species_code)))
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("catchPlot")  
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$catchPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- df%>%
              filter(species_code == input$species)
      
      # plot
           p <- ggplot() + 
                geom_line(data = x, aes(x = year, y = ram_catch, color = "RAM")) +
                geom_line(data = x, aes(x = year, y = saup_catch, color = "SAUP"))  +
                xlab('Years') +
                ylab('Catch(tons)')+
                labs(color="Database", title = x$common)
           library(scales)
           p+scale_y_continuous(labels = comma)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

