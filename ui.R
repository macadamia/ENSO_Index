
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("ENSO Index"),


  helpText('Potgieter, A. B., G. L. Hammer, H. Meinke, R. C. Stone, and L. Goddard. 2005. Three Putative Types of El Niño Revealed by Spatial Variability in Impact on Australian Wheat Yield. Journal of Climate 18:1566–1574.'),
  downloadButton('downloadPDF','Download PDF'),
  downloadButton('downloadENSO', 'Download ENSO classification'),
  plotOutput("soiPlot"),

  plotOutput("sstPlot")


))
