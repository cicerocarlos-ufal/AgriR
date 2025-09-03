library(shiny)
#-----------
  ui = fluidPage(
    
    navbarPage("App Statistic",
               

#------------------------INICIO SIMULATION---------------------           
                     
        navbarMenu("Simulation",
                   #---------------------BEM VINDOS------------------
                   tabPanel("About",
                            sidebarLayout(
                               sidebarPanel(
                                  h4("Univerisdade Federal de Alagoas"),
                                  h4("Campus Arapiraca"),
                                  h4("Mestrado Agricultura e Ambiente")
                               ),
                               mainPanel(
                               h2("Welcome"),plotOutput("map5")))),
                   #---------------------BEM VINDOS------------------
                   tabPanel("Histogram", 
                            sidebarLayout(
                              sidebarPanel(
                                 sliderInput("mean3", label = h6("Mean"), 50,150,1,value=100),
                                 sliderInput("sd3", label = h6("Standard Deviation"), 8,20,1,value=10),
                                 sliderInput("n3", label = h6("Sample"), 20,200,1,value=100),
                                 submitButton("Update View")
                                 ),
                              mainPanel(
                                h2("Histogram"),plotOutput("map4")))),
                   
                   tabPanel("Confidance Interval", 
                                   sidebarLayout(
                                     sidebarPanel(
                                       p("Provide the mean, Standard Deviation, Samples, Distribution type "),
                                       fluidRow(
                                         column(6,
                                                sliderInput("mean1", label = h6("Mean A"), 50,150,1,value=100),
                                                sliderInput("sd1", label = h6("Standard Deviation A"), 8,20,1, value=14),
                                                sliderInput("n1", label = h6("Sample A"), 3,100,1)),
                                                                                            
                                         column(6,
                                              sliderInput("mean2", label = h6("Mean B"), 50,150,1,value=100),
                                              sliderInput("sd2", label = h6("Standard Deviation B"), 8,20,1, value=14),
                                              sliderInput("n2", label = h6("Sample B"), 3,100,1))),
                                              
                                       radioButtons('dist', 'Distribution',c('Normal'="normal",'t-test'="t"), 'normal'),      
                                       sliderInput('alpha', 'Alpha',0.01,0.05,0.05),
                                       submitButton("Update View")  
                                     ),
                                     
                                     mainPanel(
                                       h3("Confidence interval for Mean A"),plotOutput("map"),
                                       h3("Confidence interval for Mean B"),plotOutput("map1"))
                                   ),
                          ),
                   #---------------------------INICIO TESTE MEDIAS----------------------------
                   tabPanel("Mean test",
                            sidebarLayout(
                               sidebarPanel(
                                  p("Simulation mean test"),
                                  fluidRow(
                                     column(6,
                                            sliderInput("mean4", label = h6("Mean A"), 50,150,1,value=100),
                                            sliderInput("sd4", label = h6("Standard Deviation A"), 8,20,1, value=14),
                                            sliderInput("n4", label = h6("Sample A"), 3,100,1)),
                                     
                                     column(6,
                                            sliderInput("mean5", label = h6("Mean B"), 50,150,1,value=100),
                                            sliderInput("sd5", label = h6("Standard Deviation B"), 8,20,1, value=14),
                                            sliderInput("n5", label = h6("Sample B"), 3,100,1))),
                                  
                                  sliderInput('alpha1', 'Alpha',0.01,0.05,0.05),
                                  submitButton("Update View")
                                  
                               ),
                               
                               mainPanel(
                                  h4("Mean Test"),plotOutput("map3"))
                            ),
                            
                   )
                   
                   #---------------------------FIM TESTE MEDIAS----------------------------
                   
                   

                   
                   
        ),
                   
#------------------------FIM SIMULATION-------------------------       

#-----------------------INICIO EXPERIMENTAL DESING-------------        
        tabPanel("Experimental Desing",
                 
                 sidebarLayout(
                   sidebarPanel( 
                     sliderInput(inputId = "trt1",
                                 label = "Número de Níveis (Fator 1):",
                                 min = 2,
                                 max = 30,
                                 value = 2),
                     sliderInput(inputId = "trt2",
                                 label = "Número de Níveis (Fator 2):",
                                 min = 2,
                                 max = 30,
                                 value = 2),
                     sliderInput(inputId = "rep",
                                 label = "Número de Repetições:",
                                 min = 3,
                                 max = 10,
                                 value = 3),
                     #Parte2
                     
                     radioButtons("des_type", "Desing type",c("crd", "rbd")),
                     radioButtons("factor", "Factor",c("one", "two")),
                     radioButtons("splitplot", "SplitPlot",c("no", "yes")),
                     submitButton("Update View")
                   ),
                
                   
                   mainPanel(
                     h1("Experimental desing"),
                     plotOutput(outputId = "desing"))
                 )       
                                   
                   ),
#----------------------------FIM EXPERIMENTAL DESING------------                  


#----------------------------INCIO DESCRITIVA------------------

navbarMenu("Basic Estatistics",
           
           tabPanel("Descriptive", 
                    
                    sidebarPanel(
                      #dados        
                      fileInput('file1', 'Choose CSV File',
                                accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                      checkboxInput('header1', 'Header', TRUE),
                      fluidRow( column(6,
                                       radioButtons('sep1', 'Separator',
                                                    c(Comma=',',
                                                      Semicolon=';',
                                                      Tab='\t'),
                                                    'Comma')),
                                column(6,
                                       radioButtons('dec1', 'Desimal seperator',  c('Comma'=",",
                                                                                   'Dot'="."), 'Comma'))
                      
                                
                                
                                ),
                      fluidRow(
                        column(6,
                              selectInput("var.desc", "Variable", choices=NULL)),
                        column(6, 
                               radioButtons("bygroups", "By Groups",c("no", "yes"))
                               ),
                        column(6,
                              selectInput("var.group", "Groups", choices=NULL))
                               ),
                      submitButton("Update View")
                      
                      ),
                      
                  
                    #Resultados    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Show Data",         verbatimTextOutput("view1")),
                        tabPanel("Statistics", verbatimTextOutput("descritive"), plotOutput("desc.plot"))        
                            
                                
                      )
                    )
           ),
           #-----------------------------INICIO DO HISTOGRAM--------------         
           
           tabPanel("Histogram", 
                    
                    sidebarPanel(
                      #dados        
                      fileInput('file2', 'Choose CSV File',
                                accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                      checkboxInput('header2', 'Header', TRUE),
                      fluidRow( column(6,
                                       radioButtons('sep2', 'Separator',
                                                    c(Comma=',',
                                                      Semicolon=';',
                                                      Tab='\t'),
                                                    'Comma')),
                                column(6,
                                       radioButtons('dec2', 'Desimal seperator',  c('Comma'=",",
                                                                                   'Dot'="."), 'Comma'))
                                
                                
                                
                      ),
                      fluidRow(
                        column(6,
                               selectInput("var.hist", "Variable", choices=NULL)),
                        column(6,
                               radioButtons("radio", "breaks type:",
                                             choices=list("Empiric" = "empiric", "Sturges" = "sturges", "Rice Rule" = "ricerule","scott" = "scott", "Freedman-Diaconis"="fd"),
                                             selected="empiric"))
                      ),
                      submitButton("Update View")
                      
                    ),
                    
                    
                    #Resultados    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Show Data",         verbatimTextOutput("view2")),
                        tabPanel("Frequence Distribution", verbatimTextOutput("freq.dist")),
                        tabPanel("Histogram", plotOutput("histogram"))        
                    
                      )
                    )
           ),
         #----------------------------FIM DO HISTOGRAM
           
         
         #--------------------------INICIO MEDIAS--------------
         tabPanel("Means",
                  
                  sidebarPanel(
                    #dados        
                    fileInput('file3', 'Choose CSV File',
                              accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                    checkboxInput('header3', 'Header', TRUE),
                    radioButtons('sep3', 'Separator', c(Comma=',', Semicolon=';', Tab='\t'),'Comma'),
                    radioButtons('dec3', 'Desimal seperator',  c('Comma'=",",'Dot'="."), 'Comma'),
                    selectInput("var.means", "Variable", choices=NULL),
                    checkboxInput('weighted', 'Weighted', FALSE),
                    selectInput("weig.means", "Weighted", choices=NULL),
                    submitButton("Update View")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Show Data",         verbatimTextOutput("view3")),
                      tabPanel("Means", verbatimTextOutput("means"))))),
         
         #-------------------------FIM MEDIAS-----------------
         

         #-------------------------INCIO DO TESTE DE MEDIAS----------------------
            tabPanel("Parwise Test Means",
               
               sidebarPanel(
                  fileInput('file.m1', 'Choose CSV File1',
                            accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                  checkboxInput('header.m', 'Header', TRUE),
                  radioButtons('sep.m', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
                  radioButtons('dec.m', 'Desimal seperator',  c('Comma'=",",'Dot'="."), 'Comma'),
                  selectInput("var.m1", "Variable A", choices=NULL),
                  selectInput("var.m2", "Variable B", choices=NULL),
                  sliderInput('alpha.m1', 'Alpha',0.01,0.05,0.05),
                  submitButton("Update View")
               ),
               
               mainPanel(
                  tabsetPanel(
                     tabPanel("Show Data",    verbatimTextOutput("view.m1")),
                     tabPanel(h4("Mean Test"),plotOutput("map.m1"), verbatimTextOutput("info.m1"))
                  ))
               
            )


         #--------------------------FIM TESTE DE MEDIAS

),

         
#-----------------------------FIM DESCRITIVA-------------------


#---------------------------INICIO ANOVA-------------------------------       
    
       
        tabPanel("Anova",
                 sidebarPanel( 
                    
                    #----------------------------------
                    fileInput('file.a', 'Choose CSV File',
                              accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
                    ,
                    tags$hr(),
                    checkboxInput('header.a', 'Header', TRUE),
                    #-----------------------dados-------------------------------   
                    fluidRow( column(6,
                                     radioButtons('sep.a', 'Separator',
                                                  c(Comma=',',
                                                    Semicolon=';',
                                                    Tab='\t'),
                                                  'Comma')),
                              column(6,
                                     radioButtons('dec.a', 'Desimal seperator',
                                                  c('Comma'=",",
                                                    'Dot'="."), 'Comma'))
                    ),
                    #-----------------------delineamentos---------------------------        
                    fluidRow( column(4,
                                     radioButtons("des_type.a", "Desing type",c("crd", "rbd"))),
                              column(4,
                                     radioButtons("fator", "Factor",c("one", "two"))),
                              column(4,
                                     radioButtons("splitplot.a", "SplitPlot",c("no", "yes")))
                    ),
                    
                    #-------------------area de teste
                    
                    
                    #---------------------fim da area
                    #-------------------Anova Data one factor-------------------
                    #textInput("vectorname", "Enter Data frame: ", "My_anova"),
                    fluidRow(column(6,
                                    selectInput("treat1", "Factor 1", choices=NULL),
                                    checkboxInput('quali_fac1', 'Qualitative Factor', TRUE)),
                             column(6,
                                    selectInput("treat2", "Factor2", choices=NULL),
                                    checkboxInput('quali_fac2', 'Qualitative Factor', TRUE))
                    ),
                    
                    fluidRow(column(6,
                                    selectInput("resp", "Variable", choices=NULL)),
                             column(6,       
                                    selectInput("block", "Block", choices=NULL))
                    ),
                    #------------------Mcomp 
                    fluidRow(column(6,
                                    selectInput('mcomp', 'Mcomp', c('tukey','lsd','duncan','snk','scheffe','skottknott'))),
                             column(6,
                                    selectInput("graph_type", "Graph type",c("barplotb", "barplotg", "pointplotg","boxplotb")))
                    ),
                    submitButton("Update View")        
                 ),
                 
                 
                 mainPanel(
                    #---------------------------------------- 
                    tableOutput(outputId = 'table.output'),
                    #-----------------------------------------
                    h2(textOutput("caption")),
                    
                    tabsetPanel(
                       tabPanel("Show Data",         verbatimTextOutput("view")),
                       tabPanel("Structure",         verbatimTextOutput("structure")),
                       tabPanel("Normality",         verbatimTextOutput("normality")),
                       tabPanel("Box Plot",          plotOutput("gsPlot")),
                       tabPanel("ANOVA Table",       verbatimTextOutput("anovatable")),
                       tabPanel("Graph Output", plotOutput("tukeyplot", width = "1000px", height = "800px"), verbatimTextOutput("tukey"))
                    )
                    
                 )
        )        

#------------------------FIM ANOVA---------------------------

       )
  )
    
  server = function(input, output,session) {
     #---------------------BEM VINDOS------------------
     output$map5 = renderPlot({
        means.test (120,60,20,20,10,10, alpha=0.05)
    
     })

#--------------------INICIO SIMULACAO-------------
output$map = renderPlot({
      
      aproximate(input$mean1,input$sd1,input$n1,input$dist, input$alpha)
})
output$map1 <- renderPlot({
      aproximate(input$mean2,input$sd2,input$n2,input$dist, input$alpha)	

})

output$map4 <- renderPlot({
  sam=rnorm(input$n3,input$mean3,input$sd3)
  
  histg(sam)

}) 

#-----------------INICIO MEANS---------------
output$map3 = renderPlot({
   
   means.test(input$mean4,input$mean5,input$sd4, input$sd5, input$n4, input$n5, input$alpha1)
   
})

#----------------FIM MEANS

#--------------------FIM SIMULACAO-------------

#-------------------INICIO EXPERIMENTAL DESING---------------------
    output$desing <- renderPlot({
      if (input$des_type == "crd" && input$factor=="one"){des.crd(input$trt1,input$rep)}
      if (input$des_type == "crd" && input$factor=="two"){des.fat2.crd(input$trt1,input$trt2,input$rep)}
      if (input$des_type == "rbd" && input$factor=="one"){des.rbd(input$trt1,input$rep)}
      if (input$des_type == "rbd" && input$factor=="two"){des.fat2.rbd(input$trt1,input$trt2,input$rep)}
      if (input$des_type == "crd" && input$factor=="two" && input$splitplot=="yes"){des.sp.crd(input$trt1,input$trt2,input$rep)}
      if (input$des_type == "rbd" && input$factor=="two" && input$splitplot=="yes"){des.sp.rbd(input$trt1,input$trt2,input$rep)}
    })
#------------------FIM EXPERIMENTAL DESING------------------------

#------------------INICIO DESCRITIVA----------------------------    
    desc.data <- reactive({
      
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      
      dados1 <- read.csv(inFile1$datapath, header=input$header1, sep=input$sep1,  dec = input$dec1)
      
      return(dados1)
    })
    
    output$view1 <- renderPrint({
  
      desc.data()	
      })
   
     
  
    observe({
      data1 <- input$file1
      if (is.null(data1))return(NULL)
      updateSelectInput(session,"var.desc", label="Select Variable", choices=colnames(desc.data()),selected="")
      updateSelectInput(session,"var.group", label="Select Groups", choices=colnames(desc.data()),selected="")
      })
    
    get_desc_var <- reactive({return(as.character(input$var.desc))})
    get_groups <-  reactive({return(as.character(input$var.group))})
    
   output$descritive <- renderPrint({
     datasheet.desc <- desc.data()
      resp_desc <- get_desc_var()
      variable=with(datasheet.desc,(get(resp_desc)))
     
     
     if (input$bygroups == "no"){desc(variable)}
     if (input$bygroups == "yes"){
       groups <- get_groups()
       groups1=with(datasheet.desc,(get(groups)))
       desc(variable, groups1)}

   })
   
   output$desc.plot <- renderPlot({
     
     datasheet.desc <- desc.data()
          resp_desc <- get_desc_var()
     variable=with(datasheet.desc,(get(resp_desc)))
     
     
     if (input$bygroups == "no"){boxplot(variable)}
     if (input$bygroups == "yes"){
       groups <- get_groups()
       groups1=with(datasheet.desc,(get(groups)))
       boxplot(variable~groups1)}
     
   })
    
#-----------------FIM DESCRITIVA--------------------------------
 
#----------------INICIO HISTOGRAMA----------------------------
   hist.data <- reactive({
     
     inFile2 <- input$file2
     if (is.null(inFile2))
       return(NULL)
     
     dados2 <- read.csv(inFile2$datapath, header=input$header2, sep=input$sep2,  dec = input$dec2)
     
     return(dados2)
   })
   
   output$view2 <- renderPrint({
     
     hist.data()	
   })
   
   
   
   observe({
     data2 <- input$file2
     if (is.null(data2))return(NULL)
     updateSelectInput(session,"var.hist", label="Select Variable", choices=colnames(hist.data()),selected="")
     
   })
   
   get_hist_var <- reactive({return(as.character(input$var.hist))})

   output$freq.dist <- renderPrint({
     datasheet.hist <- hist.data()
     resp_hist <- get_hist_var()
     
     variable=with(datasheet.hist,(get(resp_hist)))
     
     histg(variable,scores=(input$radio))
     
   })
   
   output$histogram <- renderPlot({
     datasheet.hist <- hist.data()
     resp_hist <- get_hist_var()
     
     variable=with(datasheet.hist,(get(resp_hist)))
     
     histg(variable,scores=(input$radio))
     
   })
   
#-----------------FIM HISTOGRAMA
   
   #---------------------INIO MEDIAS----------------------
   means.data <- reactive({
     
     inFile3 <- input$file3
     if (is.null(inFile3))
       return(NULL)
     
     dados3 <- read.csv(inFile3$datapath, header=input$header3, sep=input$sep3,  dec = input$dec3)
     
     return(dados3)
   })
   
   output$view3 <- renderPrint({
     
     means.data()	
   })
   
   observe({
     data3 <- input$file3
     if (is.null(data3))return(NULL)
     updateSelectInput(session,"var.means", label="Select Variable", choices=colnames(means.data()),selected="")
     updateSelectInput(session,"weig.means", label="Select Weighted", choices=colnames(means.data()),selected="")
   })
   
   get_means_var <- reactive({return(as.character(input$var.means))})
   get_means_wei <- reactive({return(as.character(input$weig.means))})
   
   output$means <- renderPrint({
     datasheet.means <- means.data()
     resp_means <- get_means_var()
     
     vari1=with(datasheet.means,(get(resp_means)))
     cat("--------------------------\n")
     cat("Aritimetric mean\n")
     print(ari_mean(vari1))
     cat("--------------------------\n")
     
     cat("--------------------------\n")
     cat("Geometric mean\n")
     print(geo_mean(vari1))
     cat("--------------------------\n")
     
     cat("--------------------------\n")
     cat("Harmonic mean\n")
     print(har_mean(vari1))
     cat("--------------------------\n")
     
     if (input$weighted==TRUE){
       weig_means <- get_means_wei()
       weigt = with(datasheet.means,(get(weig_means)))
       
       cat("--------------------------\n")
       cat("Weighted mean\n")
       print(pon_mean(vari1,weigt ))
       cat("--------------------------\n")}
     
   })  
 
#------------------FIM MEDIAS--------------------
   
   #---------------------INICIO TEST DE MEDIAS--------------------
   m1.data <- reactive({
      inFile.m1 <- input$file.m1
      if (is.null(inFile.m1))
         return(NULL)
      
      dados.m1 <- read.csv(inFile.m1$datapath, header=input$header.m, sep=input$sep.m,  dec = input$dec.m)
      
      return(dados.m1)
   })
   
   output$view.m1 <- renderPrint({
      
      m1.data()	
   })
   
   observe({
      m1.data <- input$file.m1
      if (is.null(m1.data))return(NULL)
      updateSelectInput(session,"var.m1", label="Select Variable A", choices=colnames(m1.data()),selected="")
      updateSelectInput(session,"var.m2", label="Select Variable B", choices=colnames(m1.data()),selected="")
   })
   get_var.m1 <- reactive({return(as.character(input$var.m1))})
   get_var.m2 <- reactive({return(as.character(input$var.m2))})
   
   output$map.m1 = renderPlot({
      datasheet.m1 <- m1.data()
      resp_var.m1 <- get_var.m1()
      resp_var.m2 <- get_var.m2()
      
      variable1=with(datasheet.m1,(get(resp_var.m1)))
      variable2=with(datasheet.m1,(get(resp_var.m2)))
      
      par.test(variable1,variable2,input$alpha.m1)
      
   })
   output$info.m1 <- renderPrint({
      datasheet.m1 <- m1.data()
      resp_var.m1 <- get_var.m1()
      resp_var.m2 <- get_var.m2()
      
      variable1=with(datasheet.m1,(get(resp_var.m1)))
      variable2=with(datasheet.m1,(get(resp_var.m2)))
      
      cat("--------------------------\n")
      cat("Aritimetric mean A\n")
      print(mean(variable1))
      cat("--------------------------\n")
      
      cat("--------------------------\n")
      cat("Aritimetric mean B\n")
      print(mean(variable2))
      cat("--------------------------\n")
   })
   #---------------------FIM TESTE DE MEDIAS-------------------- 
#------------------------INICIO ANOVA
   
   mydata.a <- reactive({
      
      inFile.a <- input$file.a
      if (is.null(inFile.a))
         return(NULL)
      
      dados.a <- read.csv(inFile.a$datapath, header=input$header.a, sep=input$sep.a,  dec = input$dec.a)
      
      return(dados.a)
   })
   #-------------------------------------------------------------------------------------
   
   #Update para seleção de fatores e variáveis
   
   observe({
      data.a <- input$file.a
      if (is.null(data.a))return(NULL)
      updateSelectInput(session,"treat1", label="Select Factor 1", choices=colnames(mydata.a()), selected="")
      updateSelectInput(session,"treat2", label="Select Factor 2", choices=colnames(mydata.a()),selected="")
      updateSelectInput(session,"resp", label="Select Variable", choices=colnames(mydata.a()),selected="")
      updateSelectInput(session,"block", label="Select Blocks", choices=colnames(mydata.a()),selected="")
   })
   
   
   #Obter variável
   get_resp_var <- reactive({return(as.character(input$resp))})
   
   #Obter bloco
   get_block <- reactive({return(as.character(input$block))})
   
   #One factor
   get_factors <- reactive({
      if (input$des_type.a == "crd" && input$fator=="one"){return(as.character(input$treat1))}
      if (input$des_type.a == "rbd" && input$fator=="one"){return(as.character(input$treat1))}
   })
   
   #Two factors
   get_factor1 <- reactive({
      if (input$des_type.a == "crd" && input$fator=="two"){return(as.character(input$treat1))}
      if (input$des_type.a == "rbd" && input$fator=="two"){return(as.character(input$treat1))}
   })
   get_factor2 <- reactive({
      if (input$des_type.a == "crd" && input$fator=="two"){return(as.character(input$treat2))}
      if (input$des_type.a == "rbd" && input$fator=="two"){return(as.character(input$treat2))}
   })
   
   
   #Visualizar dados
   
   output$view <- renderPrint({
      
      mydata.a()				
      
   })
   #######################################################
   # Display the structure of the selected data frame    #
   #######################################################
   output$structure <- renderPrint({
      
      # dataset=whichdataset()		
      dataset.a=mydata.a()			#Cícero
      
      str(dataset.a)
   }) 
   
   #######################################################
   # Display the normality of the selected data frame    #
   #######################################################
   output$normality <- renderPrint({
      dataset.a <- mydata.a()	
      #Para one factor
      resp_var <- get_resp_var()
      factors <- get_factors()
      factor1 <- get_factor1()
      factor2 <- get_factor2()
      if (input$des_type.a == "crd" && input$fator=="one"){print(with(dataset.a,tapply(get(resp_var), get(factors), shapiro.test)))}
      if (input$des_type.a == "rbd" && input$fator=="one"){print(with(dataset.a,tapply(get(resp_var), get(factors), shapiro.test)))}
      if (input$des_type.a == "crd" && input$fator=="two"){
         
         trt1=(with(dataset.a,tapply(get(resp_var), get(factor1), shapiro.test)))
         trt2=(with(dataset.a,tapply(get(resp_var), get(factor2), shapiro.test)))
         print(input$treat1)
         print(trt1)
         print(input$treat2)
         print(trt2)
      }
      if (input$des_type.a == "rbd" && input$fator=="two"){
         
         trt1=(with(dataset.a,tapply(get(resp_var), get(factor1), shapiro.test)))
         trt2=(with(dataset.a,tapply(get(resp_var), get(factor2), shapiro.test)))
         print(input$treat1)
         print(trt1)
         print(input$treat2)
         print(trt2)
      }
   })  
   
   
   
 #######################################################
# Display the ANOVA Table                             #
#######################################################

output$anovatable <- renderPrint({

  # Obter dados e variáveis do input
  dataset.a <- mydata.a()
  resp_var <- get_resp_var()
  factor1 <- get_factor1()
  factor2 <- get_factor2()
  block_var <- get_block()
  
  # Vetor da variável resposta
  vari <- dataset.a[[resp_var]]
  
  # Vetor de bloco, se houver
  repet <- if (!is.null(block_var)) dataset.a[[block_var]] else NULL
  
  # Vetor dos fatores
  if (input$fator == "one") {
    trt <- dataset.a[[get_factors()]]
  }
  
  if (input$fator == "two") {
    d.trt1 <- dataset.a[[factor1]]
    d.trt2 <- dataset.a[[factor2]]
  }
  
  # ANOVA para um fator
  if (input$fator == "one") {
    if (input$des_type.a == "crd") {
      aov.res <- anova.crd(trt, vari,
                            quali = input$quali_fac1,
                            graph.type = input$graph_type,
                            mcomp = input$mcomp,
                            graph = FALSE)
    } else if (input$des_type.a == "rbd") {
      aov.res <- anova.rbd(trt, vari, repet,
                            quali = input$quali_fac1,
                            graph.type = input$graph_type,
                            mcomp = input$mcomp,
                            graph = FALSE)
    }
  }
  
  # ANOVA para dois fatores
  if (input$fator == "two") {
    quali_factors <- c(input$quali_fac1, input$quali_fac2)
    
    if (input$splitplot.a == "no") {
      if (input$des_type.a == "crd") {
        aov.res <- anova.fat2.crd(d.trt1, d.trt2, vari,
                                   quali = quali_factors,
                                   graph = FALSE)
      } else if (input$des_type.a == "rbd") {
        aov.res <- anova.fat2.rbd(d.trt1, d.trt2, repet, vari,
                                   quali = quali_factors,
                                   graph = FALSE)
      }
    }
    
    if (input$splitplot.a == "yes") {
      if (input$des_type.a == "crd") {
        aov.res <- anova.sp2.crd(d.trt1, d.trt2, repet, vari,
                                  quali = quali_factors,
                                  graph = FALSE)
      } else if (input$des_type.a == "rbd") {
        aov.res <- anova.sp2.rbd(d.trt1, d.trt2, repet, vari,
                                  quali = quali_factors,
                                  graph = FALSE)
      }
    }
  }
  
  # Exibir resultado da ANOVA
  print(aov.res)
  
  br(); br()
})

   
   
   
   ################################################
   # Generate a boxplot of the requested variable #
   # include outliers                             #
   ################################################
   output$gsPlot <- renderPlot({
      
      #dataset=whichdataset()
      dataset.a=mydata.a()						#Cícero
      if (input$fator=="one"){
         resp_var <- get_resp_var()
         factors <- get_factors()
         #block_f <- get_block()
         
         #repet= with(dataset,(get(block_f)))
         trt=with(dataset.a,(get(factors)))
         vari=with(dataset.a,(get(resp_var)))
         # mt=paste(input$treat1, " : " ,input$resp)
         boxplot(vari~trt, col="skyblue", ylab=as.character(resp_var), xlab=as.character(factors))
      }
      if (input$fator=="two"){
         par(mfrow=c(1,2))
         factor1 <- get_factor1()
         factor2 <- get_factor2()
         d.trt1=with(dataset.a, get(factor1))
         d.trt2=with(dataset.a, get(factor2))
         resp_var <- get_resp_var()
         vari=with(dataset.a,(get(resp_var)))
         
         boxplot(vari~d.trt1, col="skyblue", ylab=as.character(resp_var), xlab=as.character(factor1))
         boxplot(vari~d.trt2, col="skyblue", ylab=as.character(resp_var), xlab=as.character(factor2))
      }
   }) 
   
   
   #######################################################
   # plot Tukey Groups                                   #
   #######################################################
   output$tukeyplot <- renderPlot({
      
      # dataset=whichdataset()	
      dataset.a=mydata.a()						#Cícero
      resp_var <- get_resp_var()
      factors <- get_factors()
      factor1 <- get_factor1()
      factor2 <- get_factor2()
      block_f = get_block()
      repet= with(dataset.a,(get(block_f)))
      
      if (input$fator=="one"){trt=with(dataset.a,(get(factors)))}
      
      
      #Criar vetore para os fatores 
      if (input$fator=="two"){
         d.trt1=with(dataset.a, get(factor1))
         
         d.trt2=with(dataset.a, get(factor2))
      }
      #Vetor variavel
      vari=with(dataset.a,(get(resp_var)))
      
      #Fazer anova para one fator
      
      if (input$des_type.a == "crd" && input$fator=="one"){aov.res = anova.crd(trt,vari,quali = input$quali_fac1, graph.type=input$graph_type, mcomp=input$mcomp)}
      if (input$des_type.a == "rbd" && input$fator=="one"){aov.res = anova.rbd(trt,vari,repet,quali = input$quali_fac1, graph.type=input$graph_type, mcomp=input$mcomp)}
      
      #Anova para fatorial
      fac.name=c(as.character(input$treat1),as.character(input$treat2))
      if (input$splitplot.a=="no" && input$des_type.a == "crd" && input$fator=="two"){aov.res = anova.fat2.crd(d.trt1,d.trt2,vari, quali = c(input$quali_fac1,input$quali_fac2), graph.type=input$graph_type, mcomp=input$mcomp, fac.names=fac.name)}
      if (input$splitplot.a=="no" && input$des_type.a == "rbd" && input$fator=="two"){aov.res = anova.fat2.rbd(d.trt1,d.trt2,repet,vari, quali = c(input$quali_fac1,input$quali_fac2), graph.type=input$graph_type, mcomp=input$mcomp,fac.names=fac.name)}
      
      if (input$splitplot.a=="yes" && input$des_type.a == "crd" && input$fator=="two"){aov.res = anova.sp2.crd(d.trt1,d.trt2,repet,vari, quali = c(input$quali_fac1,input$quali_fac2), graph.type=input$graph_type, mcomp=input$mcomp, fac.names=fac.name)}
      if (input$splitplot.a=="yes" && input$des_type.a == "rbd" && input$fator=="two"){aov.res = anova.sp2.rbd(d.trt1,d.trt2,repet,vari, quali = c(input$quali_fac1,input$quali_fac2), graph.type=input$graph_type, mcomp=input$mcomp,fac.names=fac.name)}
      
      
      #print(aov.res$Mcomp)
      
   })  
   
   
   
#---------------------FIM ANOVA-------------------
   
   
   
}


  shinyApp(ui = ui, server = server)
# 
