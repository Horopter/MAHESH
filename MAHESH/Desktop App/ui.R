library(shiny)
library(shinythemes) 
shinyUI(
  navbarPage("MAHESH",theme = shinytheme("cerulean"),
             tabPanel("Training",
                       headerPanel(h1("Data training module")),
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(
                             'data1',
                             'Movie and trailer dates',
                             accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                           ),
                           fileInput(
                             'data2',
                             'Public opinion statistics',
                             accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                           ),
                           fileInput(
                             'data3',
                             'Economic statistics',
                             accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                           ),
                           checkboxInput('header', 'Header', TRUE),
                           radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),',')
                         ),
                         mainPanel(
                           tabsetPanel(type="tab",
                                       tabPanel("Dates",tableOutput('Dates')),
                                       tabPanel("Opinion",tableOutput('Opinion')),
                                       tabPanel("Economics",tableOutput('Economics')),
                                       tabPanel("dat1",tableOutput('dat1')),
                                       tabPanel("dat2",tableOutput('dat2')),
                                       tabPanel("dat3",tableOutput('dat3')),
                                       tabPanel("Result",tableOutput('result')),
                                       tabPanel("Generated Coefficients",tableOutput('coeff')),
                                       tabPanel("Verdict",htmlOutput('verdict_train'))
                                       
                           )
                         )
                       )
             ),
             tabPanel("Testing",
                       headerPanel(h1("Data testing module")),
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(
                             'data4',
                             'Movie and trailer dates',
                             accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                           ),
                           fileInput(
                             'data5',
                             'Public opinion statistics',
                             accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                           ),
                           fileInput(
                             'data6',
                             'Economic statistics',
                             accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                           ),
                           checkboxInput('header', 'Header', TRUE),
                           radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),',')
                         ),
                         mainPanel(
                           tabsetPanel(type="tab",
                                       tabPanel("Dates",tableOutput('Dates2')),
                                       tabPanel("Opinion",tableOutput('Opinion2')),
                                       tabPanel("Economics",tableOutput('Economics2')),
                                       tabPanel("dat4",tableOutput('dat4')),
                                       tabPanel("dat5",tableOutput('dat5')),
                                       tabPanel("dat6",tableOutput('dat6')),
                                       tabPanel("Result",tableOutput('result2')),
                                       tabPanel("Verdict",htmlOutput('verdict_test'))
                           )
                         )
                       )
             ),
             tabPanel("Training-graphs",
                      headerPanel(h1("Graphs based on training data")),
                      mainPanel(width=12,
                      tabsetPanel(type="tab",
                                  tabPanel("FW/Gross Relationship",plotOutput('first_rel_train')),
                                  tabPanel("Estimate 1",plotOutput('est_rel_train_0')),
                                  tabPanel("Estimate 2",plotOutput('est_rel_train_1')),
                                  tabPanel("Estimate 3",plotOutput('est_rel_train_2')),
                                  tabPanel("Estimate 4",plotOutput('est_rel_train_3')),
                                  tabPanel("Estimate 5",plotOutput('est_rel_train_4')),
                                  tabPanel("Predict Max",plotOutput('predict_max_train')),
                                  tabPanel("Predict Mean",plotOutput('predict_mean_train')),
                                  tabPanel("Predict Min",plotOutput('predict_min_train'))
                                  
                          )
                        )
             ),
			 tabPanel("Testing-graphs",
                      headerPanel(h1("Graphs based on testing data")),
                      mainPanel(width=12,
                      tabsetPanel(type="tab",
                                  tabPanel("FW/Gross Relationship",plotOutput('first_rel_test')),
                                  tabPanel("Estimate 1",plotOutput('est_rel_test_0')),
                                  tabPanel("Estimate 2",plotOutput('est_rel_test_1')),
                                  tabPanel("Estimate 3",plotOutput('est_rel_test_2')),
                                  tabPanel("Estimate 4",plotOutput('est_rel_test_3')),
                                  tabPanel("Estimate 5",plotOutput('est_rel_test_4')),
                                  tabPanel("Predict Max",plotOutput('predict_max_test')),
                                  tabPanel("Predict Mean",plotOutput('predict_mean_test')),
                                  tabPanel("Predict Min",plotOutput('predict_min_test'))
                                  
                          )
                        )
             )
      )
)
