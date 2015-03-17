library(shiny)
library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output) {
  
  dataInput <- reactive({
    if (input$layer == "carbon")
      data <- raster("data/OrganicCarbon.tif")
    else if (input$layer == "uv")
      data <- raster("data/UV_change.tif")
    else if (input$layer == 'uv_null')
      data<-raster('data/uv_change_null.tif')
    else if(input$layer=='slr')
      data<-raster("data/SLR_null.tif")
    else if(input$layer=='acid')
      data<-raster('data/Acid_change.tif')
    else if(input$layer=='plastic_count1')
      data<-raster('data/count_density_size1.tif')
    else if (input$layer=='plastic_weight1')
      data<-raster('data/weight_density_size1.tif')
    else if(input$layer=='plastic_count4')
      data<-raster('data/count_density_size4.tif')
    else if (input$layer=='plastic_weight4')
      data<-raster('data/weight_density_size4.tif')
    else if (input$layer=='in_nitro'){
      d<- raster('data/InorganicNitrogen.tif')
      data <- flip(d,'y')}
    else if (input$layer=='on_nitro')
      data<-raster('data/OrganicNitrogen.tif')
    else if (input$layer=='sst')
      data<-raster('data/sst.tif')
  })
  
  trans <- reactive({
    
    if(input$trans == 'none')
      t = dataInput()
    else if(input$trans == 'log')
      t = log(dataInput()+1)
    else if(input$trans=='log10')
      t = log10(dataInput()+1)
    
  })
  
  quant <- reactive({
   if(input$quant>0){
      n = quantile(dataInput(),input$quant)
      q = calc(trans(),fun=function(x){x/n})
    }
  else if(input$quant==0){
    q = trans()
  }
  })
  
  ref <- reactive({
    
    if(input$ref=='none')
      r = trans()
    else if(input$ref=='max'){
      max = cellStats(trans(),stat='max')
      r = calc(trans(),fun=function(x){x/max})
    }
     else if(input$ref=='max1.1'){
      max = (cellStats(trans(),stat='max'))
      r = calc(trans(),fun=function(x){x/(max*1.1)})
    }
    else if(input$ref=='quant'){
      r = quant()
    }
    
  })
  
  output$histPlot <- renderPlot({
    
    histogram(ref())
  })
  
  
  output$boxPlot <- renderPlot({
    
    boxplot(ref())
  })
  
  output$mapPlot <- renderPlot({
    
    plot(ref(),col=cols)
    
  })
  
  output$table<-renderTable({
    
    data.frame(quantile(dataInput(),probs=c(0.001,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.999,0.9999)))
    
  })
  
  output$summary<-renderPrint({
    summary(dataInput())
  })  

})

