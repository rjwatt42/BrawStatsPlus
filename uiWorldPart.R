# world tab
worldPanel<-function(prefix="world",asTable=FALSE) {
 if (asTable) {
   wP
   } else {
  wellPanel(
    style = paste("background: ",subpanelcolours$hypothesisC,";"),
    wP,
    width="100%"
  )
 }
}