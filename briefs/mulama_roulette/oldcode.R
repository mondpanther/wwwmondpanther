


renderPrint({ 
  # Collect the parameters
  keys   <- c("firms", 
              "periods", 
              "alphL", 
              "alphM", 
              "gam", 
              "signua", 
              "signulam",
              "signumu",
              "sigalphmu",
              "signuk",
              "signul",
              "philam",
              "phia")
  values=c()
  
  for(kk in keys){
    expr=paste0("values=c(values,",kk,")")
    eval(parse(text=expr))
    #print(expr)
  }
  
  params <- setNames(as.list(values), keys)
  
  
  monte(params)
})







renderPrint({ 
  # Collect the parameters
  keys   <- c("firms", 
              "periods", 
              "alphL", 
              "alphM", 
              "gam", 
              "signua", 
              "signulam",
              "signumu",
              "sigalphmu",
              "signuk",
              "signul",
              "philam",
              "phia")
  values=c()
  
  for(kk in keys){
    expr=paste0("values=c(values,",kk,")")
    eval(parse(text=expr))
    #print(expr)
  }
  
  params <- setNames(as.list(values), keys)
  
  
  monte(params)
})













######





server <- function(input, output) {
  # Collect the parameters
  
  firms=100
  periods=10
  
  alphL=0.3
  alphM=0.6
  gam=1.2
  
  
  signua=1/4
  signulam=1/4
  signumu=1/4
  
  sigalphmu=1
  signuk=1
  signul=1
  
  philam=.5
  phia=.4
  
  
  
  #firms=input$firms
  #periods=input$periods
  
  
  keys   <- c("firms", 
              "periods", 
              "alphL", 
              "alphM", 
              "gam", 
              "signua", 
              "signulam",
              "signumu",
              "sigalphmu",
              "signuk",
              "signul",
              "philam",
              "phia")
  values=c()
  
  for(kk in keys){
    expr=paste0("values=c(values,",kk,")")
    eval(parse(text=expr))
    #print(expr)
  }
  
  params <- setNames(as.list(values), keys)
  
  res=monte(params)
  output$summary=renderText(res)
  #output$distPlot <- renderPlot({
  #  hist(rnorm(input$obs))
}

shinyApp(ui, server)


