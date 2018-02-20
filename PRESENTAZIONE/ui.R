
ui <- fluidPage(
  titlePanel("Un pacchetto R: betareg"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("inputDataset", "Selezionare il dataset desiderato",
                  c("GasolineYield" = "GasolineYield",
                    "StressAnxiety"="StressAnxiety")),

      
      conditionalPanel(
        condition = "input.inputDataset == 'GasolineYield'",
        h3(textOutput("InfoDatasetGas"))),
      
      conditionalPanel(
        condition = "input.inputDataset == 'FoodExpenditure'",
        h3(textOutput("InfoDatasetFood"))),
      
      conditionalPanel(
        condition = "input.inputDataset == 'StressAnxiety'",
        h3(textOutput("InfoDatasetAnxiety"))),
      
      
      conditionalPanel(
        condition = "input.TabSet == 'Confronti' && input.inputDataset == 'GasolineYield'",
        checkboxInput("loglog", "m1.loglog", TRUE),
        checkboxInput("logit", "m1", FALSE),
        checkboxInput("probit", "m1.probit", FALSE),
        checkboxInput("noOutliers","m2", FALSE),
        checkboxInput("eteroske", "m1.eteroske", FALSE)
        ),
  
      
      
      conditionalPanel(
        condition = "input.inputDataset == 'GasolineYield'",
        selectInput("gasolineModel", "Seleziona il modello di beta regressione:",
                    c("m1<-betareg(yield~batch+temp, data=GasolineYield)" = "m1",
                      "m2 <- betareg(yield~batch+temp, data=GasolineYield,subset=-4)" = "m2",
                      "m1.eteroske <- betareg(yield~batch+temp | temp, data=GasolineYield)" = "m1.eteroske",
                      "m1.probit<-betareg(yield~batch+temp, link='probit', data=GasolineYield)"="m1.probit",
                      "m1.loglog<-betareg(yield~batch+temp, link='loglog', data=GasolineYield)"="m1.loglog"
                    ))),
      
      
      conditionalPanel(
        condition = "input.inputDataset == 'StressAnxiety'",
        selectInput("anxietyModel", "Seleziona il modello di beta regressione:",
                    c("regressione_lineare<-lm(anxiety~stress,data=StressAnxiety)" = "regressione_lineare",
                      "regressione_beta<-betareg(anxiety~stress,data=StressAnxiety)" = "regressione_beta",
                      "reg_beta_disp<-betareg(anxiety~stress|stress,data=StressAnxiety)" = "reg_beta_disp")
                    ))

    ),
    
    
    
    

    mainPanel(

      tabsetPanel(
        tabPanel("Riepilogo Dataset",verbatimTextOutput("str"), verbatimTextOutput("summary"), h4(htmlOutput("List")),      conditionalPanel(
                                                                                                  condition = "input.inputDataset == 'StressAnxiety'",
                                                                                                  plotOutput("AnxietyPlot"))), 
        tabPanel("Riepilogo Modello", verbatimTextOutput("modelSummary"),h2(htmlOutput("HTMLDiagnostic")),fluidRow(column(6, align="right",plotOutput("diagnostic", width="500px",height="500px")),
                                                                                  column(6,plotOutput("diagnostic2", width="500px",height="500px")),
                                                                                  column(6, align="right",plotOutput("diagnostic3", width="500px",height="500px")),
                                                                                  column(6,plotOutput("diagnostic4", width="500px",height="500px"))
                                                                                  
                                                                                  
                                                                                  ),plotOutput("cookPlot")), 
        tabPanel("Fitted curves",h2(htmlOutput("HTMLFitted")) ,plotOutput("modelPlot")), 
        tabPanel("Confronti",conditionalPanel(
                                                            condition = "input.inputDataset == 'StressAnxiety'",h3(htmlOutput("bptestHtml")),verbatimTextOutput("bptest3"),
                                                            h3(htmlOutput("modelloBeta3Html")),
                                                            plotOutput("twoPlotAnxiety"),
                                                            h3(htmlOutput('lrtest3title')),
                                                            verbatimTextOutput("lrtest3"),
                                                            h3(htmlOutput('aic3title')),
                                                            verbatimTextOutput("aic3"),
                                                            h3(htmlOutput("sully3title")),
                                                            verbatimTextOutput("supply3")),
                            conditionalPanel(
                                                              condition = "input.inputDataset == 'GasolineYield'",
                                                              plotOutput("plotGasolineF"),
                                                              h3(htmlOutput("lrtest1html")),
                                                              verbatimTextOutput("lrtest1"),
                                                              h3(htmlOutput("wald1html")),
                                                              verbatimTextOutput("wald1"),
                                                              h3(htmlOutput('rquadro1html')),
                                                              verbatimTextOutput("rquadro1"),
                                                              h3(htmlOutput('bic1html')),
                                                              verbatimTextOutput("bic1"),
                                                              h3(htmlOutput("aic1html")),
                                                              verbatimTextOutput("aic1"))
                ),id = "TabSet"
      )
    )
  )
)


