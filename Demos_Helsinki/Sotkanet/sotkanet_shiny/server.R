# Shiny script

# This script is part of the Louhos-project (http://louhos.github.com/)

# Copyright (C) 2012-2013 Juuso Parkkinen and Leo Lahti.
# Contact: <http://louhos.github.com/contact>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

library(shiny)


shinyServer(function (input, output) {

  data.mc <- reactive({

    # Use given region cateogry
    sotkanet.df <- sotkanet.list[[input$region.category]]
    # Take subset based on chosen indicator categories (only when input given)
    if (!is.null(input$indicator.category)) {
      indicator.categories <- c("YLEISET", input$indicator.category)
      message(indicator.categories)
      inds.totake <- unlist(lapply(indicator.categories, function(x) grep(x, sotkanet.df$Muuttuja)))
      sotkanet.df <- droplevels(sotkanet.df[inds.totake,])
    }
    return(sotkanet.df)
  })
  
  data.map.gvis <- reactive({
    
    # Use data for given indicator and year
    map.df <- droplevels(subset(sotkanet.list$maakunta, 
                                Muuttuja == input$map.indicator & Vuosi == input$map.year))
    map.df <- merge(map.df, unique(kunnat[c("maakunta.title.fi", "Code")]), 
                    by.x="Maakunta", by.y="maakunta.title.fi")#, all.y=TRUE)
    
    return(map.df)
  })
  
  data.map.ggplot <- reactive({
    
    # Use data for given indicator (all available years)
    map.df <- droplevels(subset(sotkanet.list$maakunta, Muuttuja==input$map.indicator))
    map.df <- merge(map.df, unique(kunnat[c("maakunta.title.fi", "Code")]), 
                    by.x="Maakunta", by.y="maakunta.title.fi", all.y=TRUE)
    map.df <- merge(mk.df, map.df, all.x=TRUE)

    return(map.df)
  })
  
  output$motionchart <- renderGvis({
        
    sotkanet.wide <- reshape2::dcast(data.mc(), Alue + Vuosi + Maakunta ~ Muuttuja, value.var="Arvo")
    
    mchart <- googleVis::gvisMotionChart(sotkanet.wide, idvar="Alue", timevar="Vuosi", 
                                         colorvar = "Maakunta", 
                                         sizevar = "[YLEISET] Väestö 31.12.", 
                                         options=list(height=500, width=600))
    return(mchart)
  })
  
  output$map.status <- renderText({
    status <- paste("Muuttuja:", input$map.indicator)
    if (nrow(data.map.gvis())==0)
      status <- paste(status, "VIRHE: ANNETULLE VUODELLE EI LÖYDY DATAA. VAIHDA VUOSI!")

    return(status)
  })
  
  output$map.gvis <- renderGvis({
    map.df <- data.map.gvis()
    if (nrow(map.df)==0)
      return(NULL)
    map <- googleVis::gvisGeoMap(map.df, locationvar="Code", numvar="Arvo", hovervar="Maakunta", options=list(region="FI"))
    return(map)
  })
  
  output$map.ggplot <- renderPlot({
    map.df <- data.map.ggplot()
    p.map <- ggplot2::ggplot(map.df, aes(x=long, y=lat, fill=Arvo)) + 
      geom_polygon(aes(group=Maakunta), colour="white", size=0.1) + 
      facet_wrap(~ Vuosi, ncol=11) + 
      theme(legend.position="top", axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
            legend.key.width = unit(2, "cm"), legend.direction="horizontal") + 
      labs(x=NULL, y=NULL, fill=NULL) + 
      coord_equal() + 
      ggtitle(levels(map.df$Muuttuja)[1])
    print(p.map)
  })
    
  outputOptions(output, "motionchart", suspendWhenHidden = FALSE)
})