headerText<-function(txt){
  
  column(width=12,offset=0,style="padding-top:0px;margin-top: -30px;margin-bottom:2px;",
         helpText(a(txt))
  )
}

helpHeaderText<-function(txt){
  
  column(width=12,offset=0,style="padding-top:0px;margin-top: -30px;margin-bottom:0px;",
         helpText(a(txt))
  )
}