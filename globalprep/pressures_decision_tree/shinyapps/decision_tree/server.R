library(shiny)
library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
options(shiny.maxRequestSize=100*1024^2)

carbon = raster("data/OrganicCarbon.tif")
uv = raster("data/UV_change.tif")
uv_null = raster('data/uv_change_null.tif')
slr = raster("data/SLR_null.tif")
ship = raster('data/shipping.tif')
acid = raster('data/Acid_change.tif')
plastic_count1 = raster('data/count_density_size1.tif')
plastic_weight1 = raster('data/weight_density_size1.tif')
plastic_count4 = raster('data/count_density_size4.tif')
plastic_weight4 = raster('data/weight_density_size4.tif')
in_nitro = flip(raster('data/InorganicNitrogen.tif'),'y')
on_nitro = raster('data/OrganicNitrogen.tif')
sst = raster('data/sst.tif')
pest = raster('data/plumes_pest.tif')
fish = raster('data/demersal_destructive_fishing.tif')



shinyServer(function(input, output) {
  
  dataInput <- reactive({
    data <- switch(input$layer,
                   'acid'=acid,
                   'carbon'=carbon,
                   'uv' = uv,
                   'uv_null'=uv_null,
                   'slr' = slr,
                   'plastic_count1'=plastic_count1,
                   'plastic_weight1'=plastic_weight1,
                   'plastic_count4'=plastic_count4,
                   'plastic_weight4'=plastic_weight4,
                   'in_nitro' = in_nitro,
                   'on_nitro' = on_nitro,
                   'sst'=sst)
    
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
      n = quantile(trans(),input$quant)
      q = calc(trans(),fun=function(x){ifelse(x>n,n/n,x/n)})
      
    }
  else{
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
    
    data.frame(quantile(trans(),probs=c(0.001,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,0.999,0.9999)))
    
  })
  
  output$summary<-renderPrint({
    summary(trans())
  })
  
  output$resolution <-renderPrint({
    res(dataInput())
  })
  
  output$ncells <- renderPrint({
    ncell(dataInput())
  })
  
  output$range <- renderPrint({
    cellStats(dataInput(),stat='range')
  })

})

