## this is for illustration purposes with randomly chosen defaults

library(shiny)

ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(fluidRow(column(3,actionButton("play","Play")))
    ),
    mainPanel(
      tabsetPanel(tabPanel("Distribution",plotOutput('mygraph')))
    )
  )
)

server<-function(input,output){
  
  waits<-list(data=c(),count=0,nsims=100,running=FALSE)

  forward<-function(n=1){
    if (waits$count<waits$nsims) {
      n<-max(round(waits$count/5),2)
      waits$data <<-c(waits$data,rnorm(n)) 
      waits$count<<-waits$count+n
    } else {
      waits$running<<-FALSE
    }
  }

  observeEvent(input$play,{
    waits$running<<-TRUE
  })

  ## main plot output
  output$mygraph<-renderPlot({
    doIt<-input$play
    if (waits$running) {
      forward()
    } 
    if (waits$count>0) {
      if (waits$count<waits$nsims) {
        invalidateLater(1)
      } 
      nsim<-waits$count
      pts<-data.frame(x=1:nsim,y=waits$data)
      bp<-ggplot(data=pts,aes(x=x,y=y))+geom_line(aes(x=x,y=y))
      bp
    } 
  })

}

runApp(shinyApp(ui,server),launch.browser = TRUE)
