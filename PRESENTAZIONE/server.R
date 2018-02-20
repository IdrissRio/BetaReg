

  # Define server logic to plot various variables against mpg ----
  server <- function(input, output) {
    
    informazioniGas <- reactive({ 
      "Dataset relativo alla quota di petrolio greggio trasformato in benzina dopo la distillazione."
    })
    informazioniFood <- reactive({ 
      "Dataset relativo alla quota di petrolio greggio trasformato in benzina dopo la distillazione."
    })
    informazioniAnxiety<-reactive({
      "Dataset relativo al livello di ansia causato dallo stress."
    })
    
    output$InfoDatasetFood<-renderText({
      informazioniFood()
    })
    output$InfoDatasetFoode<-renderText({
      informazioniFood()
    })
    output$InfoDatasetGas<-renderText({
      informazioniGas()
    })
    output$InfoDatasetAnxiety<-renderText({
      informazioniAnxiety()
    })
    
    
    
    
    datasetInput <- reactive({
      switch(input$inputDataset,
             "GasolineYield" = GasolineYield,
             "FoodExpenditure" = FoodExpenditure,
             "StressAnxiety"=StressAnxiety)
    })
    
    
    
    
    modelSelection <- reactive({
      if(input$inputDataset=="GasolineYield"){
      switch(input$gasolineModel,
             "m1" = betareg(yield~batch+temp, data=GasolineYield),
             "m2" = betareg(yield~batch+temp, data=GasolineYield,subset=-4),
             "m1.eteroske" = betareg(yield~batch+temp | temp, data=GasolineYield),
             "m1.probit" = betareg(yield~batch+temp, link="probit", data=GasolineYield),
             "m1.loglog" = betareg(yield~batch+temp, link="loglog", data=GasolineYield)
      )}else if(input$inputDataset=="StressAnxiety"){
               switch(input$anxietyModel,
                      "regressione_lineare"=lm(anxiety~stress,data=StressAnxiety),
                      "regressione_beta"=betareg(anxiety~stress,data=StressAnxiety),
                      "reg_beta_disp"=betareg(anxiety~stress|stress,data=StressAnxiety)
                      )
             }
    })
    
    
    
    modelSelectionForPlot <- reactive({
      if(input$inputDataset=="GasolineYield"){
        switch(input$gasolineModel,
               "m1" = betareg(yield~temp, data=GasolineYield),
               "m2" = betareg(yield~temp, data=GasolineYield,subset=-4),
               "m1.eteroske" = betareg(yield~temp | temp, data=GasolineYield),
               "m1.probit" = betareg(yield~temp, link="probit", data=GasolineYield),
               "m1.loglog" = betareg(yield~temp, link="loglog", data=GasolineYield)
        )}else{
          
        }
    })
    
    
    
    output$bptestHtml<-renderUI(HTML('<center><p style="color:#AF0000">Test sul modello di regressione lineare: <code><a href="https://www.rdocumentation.org/packages/lmtest/versions/0.9-35/topics/bptest" target="_blank">lmtest::bptest</code> </a> '))
    output$modelloBeta3Html<-renderUI(HTML('<center><p style="color:#AF0000">Modello di regressione beta: con e senza dispersione'))
    output$lrtest3title<-renderUI(HTML('<center><p style="color:#AF0000">Test sui modelli di regressione beta: <code><a href="https://www.rdocumentation.org/packages/lmtest/versions/0.9-35/topics/lrtest" target="_blank">lmtest::lrtest</a></code>'))
    output$aic3title<-renderUI(HTML('<center><p style="color:#AF0000">Akaike information criterion: AIC'))
    output$sully3title<-renderUI(HTML('<center><p style="color:#AF0000">Test performance funzione di collegamento: <code> <a href="https://www.rdocumentation.org/packages/memisc/versions/0.99.14.9/topics/Sapply" target="_blank">sapply</a></code>'))
    output$HTMLDiagnostic<- renderUI(HTML('<center><p style="color:#AF0000">Plot Diagnostici</p></center>'))
    
    
    output$lrtest1html<-renderUI(HTML('<center><p style="color:#AF0000">Confronto tra il modello <code>m1.eteroske</code> e <code>m1</code> utilizzando il test di ratio-verosimiglianza: <code><a href="https://www.rdocumentation.org/packages/lmtest/versions/0.9-35/topics/lrtest" target="_blank">lmtest::lrtest</a></code>.'))
    output$wald1html<-renderUI(HTML('<center><p style="color:#AF0000">Test di Wald tra il modello <code>m1.eteroske</code> e <code>m1</code>: <code><a href="https://www.rdocumentation.org/packages/lmtest/versions/0.9-35/topics/waldtest" target="_blank">lmtest::waldtest</a></code>. Un basso p-valore indica che il parametro φ non è costante.' ))
    output$rquadro1html<-renderUI(HTML('<center><p style="color:#AF0000"> Differenza tra l\' indice R<sup>2</sup> del modello <code>m1.probit</code> e il modello <code>m1</code>'))
    output$bic1html<-renderUI(HTML('<center><p style="color:#AF0000">Indice BIC effettutato sui modelli  <code>m1.probit</code> e il modello <code>m1</code>'))
    output$aic1html<-renderUI(HTML('<center><p style="color:#AF0000"> Indice AIC su tutti i modelli: <code>m1.probit, m1.eteroske, m1.logit, m1.loglog</code>'))
    
    output$lrtest1<-renderPrint({
      data("GasolineYield")
      library("lmtest")
      m1 <- betareg(yield~batch+temp, data=GasolineYield)
      m2 <- betareg(yield~batch+temp, data=GasolineYield,subset=-4)
      m1.eteroske <- betareg(yield~batch+temp | temp, data=GasolineYield)
      m1.probit<-betareg(yield~batch+temp, link="probit", data=GasolineYield)
      m1.loglog<-betareg(yield~batch+temp, link="loglog", data=GasolineYield)
      lmtest::lrtest(m1,m1.eteroske)
    })
    
    output$wald1<-renderPrint({
      data("GasolineYield")
      library("lmtest")
      m1 <- betareg(yield~batch+temp, data=GasolineYield)
      m2 <- betareg(yield~batch+temp, data=GasolineYield,subset=-4)
      m1.eteroske <- betareg(yield~batch+temp | temp, data=GasolineYield)
      m1.probit<-betareg(yield~batch+temp, link="probit", data=GasolineYield)
      m1.loglog<-betareg(yield~batch+temp, link="loglog", data=GasolineYield)
      lmtest::waldtest(m1,m1.eteroske)
    })
    
    output$rquadro1<-renderPrint({
      data("GasolineYield")
      library("lmtest")
      m1 <- betareg(yield~batch+temp, data=GasolineYield)
      m2 <- betareg(yield~batch+temp, data=GasolineYield,subset=-4)
      m1.eteroske <- betareg(yield~batch+temp | temp, data=GasolineYield)
      m1.probit<-betareg(yield~batch+temp, link="probit", data=GasolineYield)
      m1.loglog<-betareg(yield~batch+temp, link="loglog", data=GasolineYield)
      summary(m1.probit)$pseudo.r.squared-summary(m1)$pseudo.r.squared
    })
    
    
    output$bic1<-renderPrint({
      data("GasolineYield")
      library("lmtest")
      m1 <- betareg(yield~batch+temp, data=GasolineYield)
      m2 <- betareg(yield~batch+temp, data=GasolineYield,subset=-4)
      m1.eteroske <- betareg(yield~batch+temp | temp, data=GasolineYield)
      m1.probit<-betareg(yield~batch+temp, link="probit", data=GasolineYield)
      m1.loglog<-betareg(yield~batch+temp, link="loglog", data=GasolineYield)
      BIC(m1,m1.probit)
    })
    
    output$aic1<-renderPrint({
      data("GasolineYield")
      library("lmtest")
      m1 <- betareg(yield~batch+temp, data=GasolineYield)
      m2 <- betareg(yield~batch+temp, data=GasolineYield,subset=-4)
      m1.eteroske <- betareg(yield~batch+temp | temp, data=GasolineYield)
      m1.probit<-betareg(yield~batch+temp, link="probit", data=GasolineYield)
      m1.loglog<-betareg(yield~batch+temp, link="loglog", data=GasolineYield)
      AIC(m1,m1.eteroske,m1.probit,m1.loglog)
    })
    

    
    output$HTMLFitted<- renderUI(HTML('<center><p style="color:#AF0000">Fitted curves</p></center>'))
    
    output$List <- renderUI(
                        if(input$inputDataset=="GasolineYield"){
                          HTML("<ul>
                                <li>
                                  <b>yield</b>: tasso di petrolio greggio trasformato in benzina dopo la distillazione.
                                </li>
<br>
                                <li>
                                  <b>gravity</b>: gravità del petrolio greggio.
                                 </li>
<br>
                                <li>
                                 <b> pressure</b>: pressione della materiale grezzo (libre/pollice <sup>2</sup> ).
                                 </li>
<br>
                                <li>
                                  <b>temp10</b>: temperatura per la quale il 10% di petrolio greggio vaporizza.
                                 </li>
<br>
                                <li>
                                  <b>temp</b>: temperatura per la quale il 100% di petrolio greggio vaporizza.
                                 </li>
<br>
                                <li>
                                  <b>batch</b>: variabile fattore di 10 livelli. Ognuno di essi ha una propria gravità, pressione e temperatura d’evaporazione al 10%.
                                 </li>
                                 
                                 </ul>")}
      else if (input$inputDataset=="StressAnxiety"){
        HTML("<ul>
                                <li>
             <b>Anxiety</b>: variabile numerica con valori (0,1) che indica il livello di ansia delle persone. Variabile risposta.
             </li>
             <br>
             <li>
             <b>Stress</b>: variabile numerica con valori (0,1) che indica il livello di stress delle persone. Variabile regressore.
             </li>
             <br>
             </ul>
             "
    
    )  })
    
    output$lrtest3<-renderPrint({
      regressione_beta<-betareg(anxiety~stress,data=StressAnxiety)
      reg_beta_disp<-betareg(anxiety~stress|stress,data=StressAnxiety)
      lrtest(regressione_beta,reg_beta_disp)
    })
    
    output$aic3<-renderPrint({
      data("StressAnxiety")
      regressione_lineare<-lm(anxiety~stress,data=StressAnxiety)
      regressione_beta<-betareg(anxiety~stress,data=StressAnxiety)
      reg_beta_disp<-betareg(anxiety~stress|stress,data=StressAnxiety)
      AIC(regressione_beta,reg_beta_disp)
    })
    
    output$supply3<-renderPrint({
      data("StressAnxiety")
      regressione_lineare<-lm(anxiety~stress,data=StressAnxiety)
      regressione_beta<-betareg(anxiety~stress,data=StressAnxiety)
      reg_beta_disp<-betareg(anxiety~stress|stress,data=StressAnxiety)
      sapply(c("logit","probit","cloglog","cauchit","loglog"), function(x)logLik(update(reg_beta_disp,link=x)))
    })
    
    output$bptest3<-renderPrint({
      data("StressAnxiety")
      library("lmtest")
      regressione_lineare<-lm(anxiety~stress,data=StressAnxiety)
      bptest(regressione_lineare)
    })
    
    
    
    output$summary <- renderPrint({
      library("betareg")
      data("GasolineYield")
      data("FoodExpenditure")
      data("StressAnxiety")
      ODS <- datasetInput()
      summary(ODS)
    })
    
    output$str<-renderPrint({
      library("betareg")
      data("GasolineYield")
      data("FoodExpenditure")
      data("StressAnxiety")
      ODS <- datasetInput()
      str(ODS)
    })
    
    
    output$modelSummary <- renderPrint({
      library("betareg")
      data("GasolineYield")
      data("FoodExpenditure")
      data("AnxietyStress")
      m <- modelSelection()
      summary(m)
    })
    
    output$diagnostic<- renderPlot({
      if(input$anxietyModel=="regressione_lineare"){
        plot(modelSelection(),which=1,sub.caption="")
      }else{
      plot(modelSelection(), which = 1, type = "deviance", sub.caption = "")
      }
    })
    
    
    output$diagnostic2<- renderPlot({
      if(input$anxietyModel=="regressione_lineare"){
        plot(modelSelection(),which=2,sub.caption="")
      }else{
      plot(modelSelection(), which = 1, type = "pearson", sub.caption = "")
      }
    })
    
    output$diagnostic3<- renderPlot({
      if(input$anxietyModel=="regressione_lineare"){
        plot(modelSelection(),which=3,sub.caption="")
      }else{
      plot(modelSelection(), which = 3)
      }
    })
    
    output$diagnostic4<- renderPlot({
      if(input$anxietyModel=="regressione_lineare"){
        plot(modelSelection(),which=4,sub.caption="")
      }else{
      plot(modelSelection(), which = 5, type = "deviance", sub.caption = "")
      }
    })
    
    output$diagnostic5<- renderPlot({
      if(input$anxietyModel=="regressione_lineare"){
        plot(modelSelection(),which=5,sub.caption="")
      }else{
      plot(modelSelection(), which = 4, type = "pearson", sub.caption = "") 
      }
    })
    
    output$diagnostic6<- renderPlot({
      if(input$anxietyModel=="regressione_lineare"){
        plot(modelSelection(),which=6,sub.caption="")
      }else{
      plot(modelSelection(), which = 6, type = "pearson", sub.caption = "")
      }
    })
    
    output$cookPlot<-renderPlot({
      if(input$anxietyModel=="regressione_lineare"){
        
      }else{
      plot(modelSelection(), which = 2, sub.caption="")
      }
    })
    
    
    
    output$plotGasolineF<-renderPlot({
      GModel=modelSelectionForPlot()
      GasolineYield2<-subset(GasolineYield, temp!= 407)
      m1<-betareg(yield~temp, data=GasolineYield)
      m2<- betareg(yield~temp, data=GasolineYield,subset=-4)
      m1.eteroske<- betareg(yield~temp | temp, data=GasolineYield)
      m1.probit<- betareg(yield~temp, link="probit", data=GasolineYield)
      m1.loglog<- betareg(yield~temp, link="loglog", data=GasolineYield)
      GModel=modelSelectionForPlot()
      ggplot(GasolineYield, aes(x = GasolineYield$temp, y = GasolineYield$yield)) +
        geom_point(size = 4, aes(fill = GasolineYield$batch), shape = 21) +{
          if(input$probit){
            geom_line(aes(y = predict(m1.probit, GasolineYield),
                          colour = "probit", linetype = "probit"),lwd=1)}}+ 
                          {
           if(input$logit){
            geom_line(aes(y = predict(m1, GasolineYield),
                colour = "logit", linetype = "logit"),lwd=1)}}+
                {
                  if(input$loglog){
                    geom_line(aes(y = predict(m1.loglog, GasolineYield),
                                  colour = "loglog", linetype = "loglog"),lwd=1)}}+
                                  {
                                    if(input$eteroske){
                                      geom_line(aes(y = predict(m1.eteroske, GasolineYield),
                                                    colour = "eteroske", linetype = "eteroske"),lwd=1)}}+
                                                    {
                                                      if(input$noOutliers){
                                                        geom_line(aes(y = predict(m2, GasolineYield),
                                                                      colour = "m2.noOutliers", linetype = "m2.noOutliers"),lwd=1)}}+
        
        scale_colour_manual("", values = c("black","red","blue","yellow", "brown")) +
        scale_fill_grey() +
        theme_bw()
    })
    
    output$twoPlotAnxiety<-renderPlot({
      library("betareg")
      data("StressAnxiety")
      
      StressAnxiety <- StressAnxiety[order(StressAnxiety$stress),]
      regressione_beta<-betareg(anxiety~stress,data=StressAnxiety)
      reg_beta_disp<-betareg(anxiety~stress|stress,data=StressAnxiety)
      plot(jitter(anxiety)~jitter(stress),data=StressAnxiety,xlim = c(0, 1), ylim = c(0, 1))
      lines(fitted(regressione_beta) ~ StressAnxiety$stress, col='red',lwd=2,lty = 2)
      lines(fitted(reg_beta_disp) ~ StressAnxiety$stress,col='blue', lwd=3, lty = 2)
      legend("topleft", c("reg_beta", "reg_beta_disp"), lty = 2, lwd = c(2,3),col = c('red','blue'),bty = "n")
    })
    
    output$AnxietyPlot<-renderPlot({
      par(mfrow=c(1,2),pty="s")
      hist(StressAnxiety$anxiety,xlab="anxiety" ,main = "",probability = TRUE)
      dens<-density(StressAnxiety$anxiety)
      lines(dens, col="red",lwd=3)
      hist(StressAnxiety$stress,xlab="stress",main = "",probability = TRUE)
      dens<-density(StressAnxiety$stress)
      lines(dens,col="red",lwd=3)
      
    })
    
    output$modelPlot <- renderPlot({
      library(ggplot2)
     
      if(input$inputDataset=="GasolineYield" & input$gasolineModel=="m2"){
        GModel=modelSelectionForPlot()
        GasolineYield2<-subset(GasolineYield, temp!= 407)
          ggplot(GasolineYield, aes(x = GasolineYield$temp, y =GasolineYield$yield)) +
          geom_point(size = 4, aes(fill = GasolineYield$batch), shape = 21) +
          scale_fill_grey() +
          geom_line(aes(y = predict(GModel, GasolineYield),
                        colour = "loglog", linetype = "loglog"),lwd=1) +
          scale_colour_manual("", values = c("black","black")) +
          scale_linetype_manual("", values = c("solid","dot")) +
          theme_bw()+
         geom_point(data=GasolineYield2,aes(x=GasolineYield$temp[-4], y=fitted.values(GModel)),size=2,col="red")+
         geom_segment(data=GasolineYield2,aes(x=GasolineYield$temp[-4], y=GasolineYield$yield[-4], xend=GasolineYield$temp[-4],yend=fitted.values(GModel)), col="blue")
      }else if(input$inputDataset=="GasolineYield"){
        GModel=modelSelectionForPlot()
        ggplot(GasolineYield, aes(x = GasolineYield$temp, y = GasolineYield$yield)) +
          geom_point(size = 4, aes(fill = GasolineYield$batch), shape = 21) +
          scale_fill_grey() +
          geom_line(aes(y = predict(GModel, GasolineYield),
                        colour = "loglog", linetype = "loglog"),lwd=1) + 
          scale_colour_manual("", values = c("black","black")) +
          scale_linetype_manual("", values = c("solid","dot")) +
          theme_bw()+
          geom_point(aes(x=GasolineYield$temp, y=fitted.values(GModel)),size=2,col="red")+
          geom_segment(aes(x=GasolineYield$temp, y=GasolineYield$yield, xend=GasolineYield$temp,yend=fitted.values(GModel)), col="blue")
      }else if(input$inputDataset=="StressAnxiety"){
        GModel=modelSelection()
        if(input$anxietyModel=="regressione_lineare"){
          StressAnxiety <- StressAnxiety[order(StressAnxiety$stress),]
        plot(jitter(anxiety)~jitter(stress),data=StressAnxiety,pch=1,xlab='stress',ylab = 'anxiety')
        abline(modelSelection(),col='red',lwd=2)}
        else{
          StressAnxiety <- StressAnxiety[order(StressAnxiety$stress),]
          regressione_beta<-betareg(anxiety~stress,data=StressAnxiety)
          reg_beta_disp<-betareg(anxiety~stress|stress,data=StressAnxiety)
          plot(jitter(anxiety)~jitter(stress),data=StressAnxiety,xlim = c(0, 1), ylim = c(0, 1))
          if(input$anxietyModel=="reg_beta_disp")
          lines(fitted(reg_beta_disp) ~ StressAnxiety$stress,col='blue', lwd=3, lty = 2)
          else
          lines(fitted(regressione_beta) ~ StressAnxiety$stress, col='red',lwd=2,lty = 2)
        }
        
      }
      
      
    })
  }