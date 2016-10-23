library(shiny)
library(shinyFiles)

shinyUI(pageWithSidebar(
  headerPanel(
    'ExtractoR - ScrappeR of webpages'
  ),
  sidebarPanel(
    shinyDirButton('directory', 'Folder select', 'Please select a folder')
    ),
  mainPanel(
    verbatimTextOutput('directorypath')
          )
    ))