# Funktioner til brug i shinyApp

# Laver dataframe med punkter til trekanten
dfForSingleShotTri <-  function(shot) {
  g1=c(120, 36)
  g2=c(120,44)
  x=c(shot[1],g1[1],g2[1],shot[1])
  y=c(shot[2],g1[2],g2[2],shot[2])
  resshot=data.frame(x,y)
  colnames(resshot)=c("sx","sy")
  return(resshot)
}