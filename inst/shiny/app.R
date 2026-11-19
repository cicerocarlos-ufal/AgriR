# Interactive application of the AgriR package.
# Start it with AgriR::run_app()

library(shiny)
library(AgriR)

csv_inputs <- function(id) {
  tagList(
    fileInput(paste0("file", id), "Choose CSV File",
              accept = c("text/csv", "text/comma-separated-values,text/plain",
                         ".csv")),
    checkboxInput(paste0("header", id), "Header", TRUE),
    fluidRow(
      column(6, radioButtons(paste0("sep", id), "Separator",
                             c(Comma = ",", Semicolon = ";", Tab = "\t"),
                             ";")),
      column(6, radioButtons(paste0("dec", id), "Decimal separator",
                             c(Comma = ",", Dot = "."), ","))
    )
  )
}

read_csv_input <- function(input, id) {
  f <- input[[paste0("file", id)]]
  if (is.null(f)) return(NULL)
  sep <- input[[paste0("sep", id)]]
  dec <- input[[paste0("dec", id)]]
  validate(need(sep != dec,
                "The column separator and the decimal separator must be different."))
  utils::read.csv(f$datapath, header = input[[paste0("header", id)]],
                  sep = sep, dec = dec)
}

column_of <- function(data, name) {
  req(data, name, nzchar(name), name %in% names(data))
  data[[name]]
}

ui <- fluidPage(
  navbarPage("AgriR",
    #------------------------------ SIMULATION ------------------------------
    navbarMenu("Simulation",
      tabPanel("About",
        sidebarLayout(
          sidebarPanel(
            h4("Universidade Federal de Alagoas"),
            h4("Campus Arapiraca"),
            h4("Mestrado Agricultura e Ambiente")
          ),
          mainPanel(h2("Welcome"), plotOutput("map5"))
        )
      ),
      tabPanel("Histogram",
        sidebarLayout(
          sidebarPanel(
            sliderInput("mean3", h6("Mean"), 50, 150, value = 100, step = 1),
            sliderInput("sd3", h6("Standard Deviation"), 8, 20, value = 10,
                        step = 1),
            sliderInput("n3", h6("Sample"), 20, 200, value = 100, step = 1),
            submitButton("Update View")
          ),
          mainPanel(h2("Histogram"), plotOutput("map4"))
        )
      ),
      tabPanel("Confidence Interval",
        sidebarLayout(
          sidebarPanel(
            p("Provide the mean, standard deviation, sample size and distribution"),
            fluidRow(
              column(6,
                sliderInput("mean1", h6("Mean A"), 50, 150, value = 100, step = 1),
                sliderInput("sd1", h6("Standard Deviation A"), 8, 20, value = 14,
                            step = 1),
                sliderInput("n1", h6("Sample A"), 3, 100, value = 10, step = 1)),
              column(6,
                sliderInput("mean2", h6("Mean B"), 50, 150, value = 100, step = 1),
                sliderInput("sd2", h6("Standard Deviation B"), 8, 20, value = 14,
                            step = 1),
                sliderInput("n2", h6("Sample B"), 3, 100, value = 30, step = 1))
            ),
            radioButtons("dist", "Distribution", c(Normal = "normal", t = "t"),
                         "normal"),
            sliderInput("alpha", "Alpha", 0.01, 0.10, 0.05),
            submitButton("Update View")
          ),
          mainPanel(
            h3("Confidence interval for Mean A"), plotOutput("map"),
            h3("Confidence interval for Mean B"), plotOutput("map1")
          )
        )
      ),
      tabPanel("Mean test",
        sidebarLayout(
          sidebarPanel(
            p("Simulation of the comparison of two means"),
            fluidRow(
              column(6,
                sliderInput("mean4", h6("Mean A"), 50, 150, value = 100, step = 1),
                sliderInput("sd4", h6("Standard Deviation A"), 8, 20, value = 14,
                            step = 1),
                sliderInput("n4", h6("Sample A"), 3, 100, value = 10, step = 1)),
              column(6,
                sliderInput("mean5", h6("Mean B"), 50, 150, value = 110, step = 1),
                sliderInput("sd5", h6("Standard Deviation B"), 8, 20, value = 14,
                            step = 1),
                sliderInput("n5", h6("Sample B"), 3, 100, value = 10, step = 1))
            ),
            sliderInput("alpha1", "Alpha", 0.01, 0.10, 0.05),
            submitButton("Update View")
          ),
          mainPanel(h4("Mean Test"), plotOutput("map3"))
        )
      )
    ),
    #------------------------- EXPERIMENTAL DESIGN --------------------------
    tabPanel("Experimental Design",
      sidebarLayout(
        sidebarPanel(
          sliderInput("trt1", "Number of levels (factor 1):", 2, 30, 4),
          sliderInput("trt2", "Number of levels (factor 2):", 2, 30, 2),
          sliderInput("trt3", "Number of levels (factor 3):", 2, 10, 2),
          sliderInput("rep", "Number of replications / blocks:", 2, 10, 3),
          radioButtons("des_type", "Design type",
                       c("Completely randomized (CRD)" = "crd",
                         "Randomized blocks (RBD)" = "rbd")),
          radioButtons("factor", "Factors",
                       c(One = "one", Two = "two", Three = "three")),
          radioButtons("splitplot", "Split-plot (two factors) / split-split-plot (three factors)",
                       c(No = "no", Yes = "yes")),
          submitButton("Update View")
        ),
        mainPanel(
          h1("Experimental design"),
          plotOutput("desing", height = "600px"),
          h3("Field book"),
          tableOutput("book")
        )
      )
    ),
    #------------------------- BASIC STATISTICS -----------------------------
    navbarMenu("Basic Statistics",
      tabPanel("Descriptive",
        sidebarPanel(
          csv_inputs("1"),
          fluidRow(
            column(6, selectInput("var.desc", "Variable", choices = NULL)),
            column(6, radioButtons("bygroups", "By Groups", c("no", "yes"))),
            column(6, selectInput("var.group", "Groups", choices = NULL))
          ),
          submitButton("Update View")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Show Data", verbatimTextOutput("view1")),
            tabPanel("Statistics", verbatimTextOutput("descritive"),
                     plotOutput("desc.plot"))
          )
        )
      ),
      tabPanel("Histogram",
        sidebarPanel(
          csv_inputs("2"),
          fluidRow(
            column(6, selectInput("var.hist", "Variable", choices = NULL)),
            column(6, radioButtons("radio", "Breaks type:",
                                   choices = list("Empiric" = "empiric",
                                                  "Sturges" = "sturges",
                                                  "Rice Rule" = "ricerule",
                                                  "Scott" = "scott",
                                                  "Freedman-Diaconis" = "fd"),
                                   selected = "empiric"))
          ),
          submitButton("Update View")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Show Data", verbatimTextOutput("view2")),
            tabPanel("Frequency Distribution", verbatimTextOutput("freq.dist")),
            tabPanel("Histogram", plotOutput("histogram"))
          )
        )
      ),
      tabPanel("Means",
        sidebarPanel(
          csv_inputs("3"),
          selectInput("var.means", "Variable", choices = NULL),
          checkboxInput("weighted", "Weighted", FALSE),
          selectInput("weig.means", "Weights", choices = NULL),
          submitButton("Update View")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Show Data", verbatimTextOutput("view3")),
            tabPanel("Means", verbatimTextOutput("means"))
          )
        )
      ),
      tabPanel("Pairwise Test Means",
        sidebarPanel(
          csv_inputs(".m"),
          selectInput("var.m1", "Variable A", choices = NULL),
          selectInput("var.m2", "Variable B", choices = NULL),
          sliderInput("alpha.m1", "Alpha", 0.01, 0.10, 0.05),
          submitButton("Update View")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Show Data", verbatimTextOutput("view.m1")),
            tabPanel("Mean Test", plotOutput("map.m1"),
                     verbatimTextOutput("info.m1"))
          )
        )
      )
    ),
    #------------------------------- ANOVA ----------------------------------
    tabPanel("Anova",
      sidebarPanel(
        csv_inputs(".a"),
        tags$hr(),
        fluidRow(
          column(4, radioButtons("des_type.a", "Design type",
                                 c(CRD = "crd", RBD = "rbd"))),
          column(4, radioButtons("fator", "Factors",
                                 c(One = "one", Two = "two", Three = "three"))),
          column(4, radioButtons("splitplot.a", "Split-plot",
                                 c(No = "no", Yes = "yes")))
        ),
        fluidRow(
          column(6, selectInput("treat1", "Factor 1", choices = NULL),
                 checkboxInput("quali_fac1", "Qualitative Factor", TRUE)),
          column(6, selectInput("treat2", "Factor 2", choices = NULL),
                 checkboxInput("quali_fac2", "Qualitative Factor", TRUE))
        ),
        fluidRow(
          column(6, selectInput("treat3", "Factor 3", choices = NULL),
                 checkboxInput("quali_fac3", "Qualitative Factor", TRUE))
        ),
        fluidRow(
          column(6, selectInput("resp", "Variable", choices = NULL)),
          column(6, selectInput("block", "Block / Repetition", choices = NULL))
        ),
        fluidRow(
          column(6, selectInput("mcomp", "Mcomp",
                                c("tukey", "lsd", "duncan", "snk", "scheffe",
                                  "scottknott"))),
          column(6, selectInput("graph_type", "Graph type",
                                c("barplotb", "barplotg", "pointplotg",
                                  "boxplotb")))
        ),
        radioButtons("fac.fix", "Unfolding of a significant interaction",
                     c("Factor 1 inside factor 2" = "fac.A",
                       "Factor 2 inside factor 1" = "fac.B")),
        submitButton("Update View")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Show Data", verbatimTextOutput("view")),
          tabPanel("Structure", verbatimTextOutput("structure")),
          tabPanel("Normality", verbatimTextOutput("normality")),
          tabPanel("Box Plot", plotOutput("gsPlot")),
          tabPanel("ANOVA Table", verbatimTextOutput("anovatable")),
          tabPanel("Graph Output",
                   plotOutput("tukeyplot", width = "1000px", height = "800px"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #------------------------------ SIMULATION ------------------------------
  output$map5 <- renderPlot({
    means.test(120, 60, 20, 20, 10, 10, alpha = 0.05)
  })
  output$map <- renderPlot({
    aproximate(input$mean1, input$sd1, input$n1, input$dist, input$alpha)
  })
  output$map1 <- renderPlot({
    aproximate(input$mean2, input$sd2, input$n2, input$dist, input$alpha)
  })
  output$map4 <- renderPlot({
    histg(rnorm(input$n3, input$mean3, input$sd3))
  })
  output$map3 <- renderPlot({
    means.test(input$mean4, input$mean5, input$sd4, input$sd5, input$n4,
               input$n5, input$alpha1)
  })

  #------------------------- EXPERIMENTAL DESIGN --------------------------
  design <- reactive({
    nf <- switch(input$factor, one = 1, two = 2, three = 3)
    split <- nf > 1 && input$splitplot == "yes"
    crd <- input$des_type == "crd"
    list(nf = nf, two = nf == 2, split = split, crd = crd)
  })
  output$desing <- renderPlot({
    d <- design()
    res <- if (d$nf == 3) {
      if (d$split) {
        if (d$crd) des.ssp.crd(input$trt1, input$trt2, input$trt3, input$rep) else
          des.ssp.rbd(input$trt1, input$trt2, input$trt3, input$rep)
      } else {
        if (d$crd) des.fat3.crd(input$trt1, input$trt2, input$trt3, input$rep) else
          des.fat3.rbd(input$trt1, input$trt2, input$trt3, input$rep)
      }
    } else if (!d$two) {
      if (d$crd) des.crd(input$trt1, input$rep) else des.rbd(input$trt1, input$rep)
    } else if (d$split) {
      if (d$crd) des.sp.crd(input$trt1, input$trt2, input$rep) else
        des.sp.rbd(input$trt1, input$trt2, input$rep)
    } else {
      if (d$crd) des.fat2.crd(input$trt1, input$trt2, input$rep) else
        des.fat2.rbd(input$trt1, input$trt2, input$rep)
    }
    book(res$Book)
  })
  book <- reactiveVal(NULL)
  output$book <- renderTable(book())

  #--------------------------- DESCRIPTIVE --------------------------------
  desc.data <- reactive(read_csv_input(input, "1"))
  output$view1 <- renderPrint(desc.data())
  observe({
    d <- desc.data()
    req(d)
    updateSelectInput(session, "var.desc", choices = colnames(d))
    updateSelectInput(session, "var.group", choices = colnames(d))
  })
  output$descritive <- renderPrint({
    d <- desc.data()
    variable <- column_of(d, input$var.desc)
    if (input$bygroups == "no") desc(variable) else
      desc(variable, column_of(d, input$var.group))
  })
  output$desc.plot <- renderPlot({
    d <- desc.data()
    variable <- column_of(d, input$var.desc)
    if (input$bygroups == "no") {
      boxplot(variable, col = "skyblue")
    } else {
      groups <- column_of(d, input$var.group)
      boxplot(variable ~ groups, col = "skyblue")
    }
  })

  #----------------------------- HISTOGRAM --------------------------------
  hist.data <- reactive(read_csv_input(input, "2"))
  output$view2 <- renderPrint(hist.data())
  observe({
    d <- hist.data()
    req(d)
    updateSelectInput(session, "var.hist", choices = colnames(d))
  })
  output$freq.dist <- renderPrint({
    variable <- column_of(hist.data(), input$var.hist)
    histg(variable, scores = input$radio)
  })
  output$histogram <- renderPlot({
    variable <- column_of(hist.data(), input$var.hist)
    histg(variable, scores = input$radio)
  })

  #------------------------------- MEANS ----------------------------------
  means.data <- reactive(read_csv_input(input, "3"))
  output$view3 <- renderPrint(means.data())
  observe({
    d <- means.data()
    req(d)
    updateSelectInput(session, "var.means", choices = colnames(d))
    updateSelectInput(session, "weig.means", choices = colnames(d))
  })
  output$means <- renderPrint({
    d <- means.data()
    x <- column_of(d, input$var.means)
    cat("Arithmetic mean:", ari_mean(x), "\n")
    cat("Geometric mean: ", tryCatch(geo_mean(x), error = conditionMessage), "\n")
    cat("Harmonic mean:  ", tryCatch(har_mean(x), error = conditionMessage), "\n")
    if (isTRUE(input$weighted)) {
      cat("Weighted mean:  ", pon_mean(x, column_of(d, input$weig.means)), "\n")
    }
  })

  #------------------------- PAIRWISE MEANS TEST --------------------------
  m1.data <- reactive(read_csv_input(input, ".m"))
  output$view.m1 <- renderPrint(m1.data())
  observe({
    d <- m1.data()
    req(d)
    updateSelectInput(session, "var.m1", choices = colnames(d))
    updateSelectInput(session, "var.m2", choices = colnames(d))
  })
  output$map.m1 <- renderPlot({
    d <- m1.data()
    par.test(column_of(d, input$var.m1), column_of(d, input$var.m2),
             input$alpha.m1)
  })
  output$info.m1 <- renderPrint({
    d <- m1.data()
    a <- column_of(d, input$var.m1)
    b <- column_of(d, input$var.m2)
    cat("Mean A:", mean(a, na.rm = TRUE), "\n")
    cat("Mean B:", mean(b, na.rm = TRUE), "\n\n")
    print(stats::t.test(a, b, var.equal = TRUE,
                        conf.level = 1 - input$alpha.m1))
  })

  #------------------------------- ANOVA ----------------------------------
  mydata.a <- reactive(read_csv_input(input, ".a"))
  observe({
    d <- mydata.a()
    req(d)
    for (id in c("treat1", "treat2", "treat3", "resp", "block")) {
      updateSelectInput(session, id, choices = colnames(d))
    }
  })
  output$view <- renderPrint(mydata.a())
  output$structure <- renderPrint(str(mydata.a()))

  # Runs the analysis selected in the interface.
  run_anova <- function(graph) {
    d <- mydata.a()
    req(d)
    vari <- column_of(d, input$resp)
    trt1 <- column_of(d, input$treat1)
    needs_block <- input$des_type.a == "rbd" ||
      (input$fator != "one" && input$splitplot.a == "yes")
    repet <- if (needs_block) column_of(d, input$block) else NULL
    common <- list(graph = graph, graph.type = input$graph_type,
                   mcomp = input$mcomp)
    if (input$fator == "one") {
      args <- c(list(trt1, vari), if (!is.null(repet)) list(repet),
                list(quali = input$quali_fac1), common)
      fun <- if (input$des_type.a == "crd") anova_crd else anova_rbd
      return(do.call(fun, args))
    }
    trt2 <- column_of(d, input$treat2)
    if (input$fator == "three") {
      trt3 <- column_of(d, input$treat3)
      extra3 <- c(list(quali = c(input$quali_fac1, input$quali_fac2,
                                 input$quali_fac3),
                       fac.names = c(input$treat1, input$treat2, input$treat3)),
                  common)
      fun <- if (input$splitplot.a == "yes") {
        if (input$des_type.a == "crd") anova_ssp_crd else anova_ssp_rbd
      } else {
        if (input$des_type.a == "crd") anova_fat3_crd else anova_fat3_rbd
      }
      args <- c(list(trt1, trt2, trt3),
                if (!is.null(repet)) list(repet), list(vari), extra3)
      return(do.call(fun, args))
    }
    quali <- c(input$quali_fac1, input$quali_fac2)
    fac.names <- c(input$treat1, input$treat2)
    extra <- c(list(quali = quali, fac.names = fac.names,
                    fac.fix = input$fac.fix), common)
    if (input$splitplot.a == "no") {
      if (input$des_type.a == "crd") {
        do.call(anova_fat2_crd, c(list(trt1, trt2, vari), extra))
      } else {
        do.call(anova_fat2_rbd, c(list(trt1, trt2, repet, vari), extra))
      }
    } else {
      fun <- if (input$des_type.a == "crd") anova_sp2_crd else anova_sp2_rbd
      do.call(fun, c(list(trt1, trt2, repet, vari), extra))
    }
  }

  output$normality <- renderPrint({
    d <- mydata.a()
    vari <- column_of(d, input$resp)
    f1 <- column_of(d, input$treat1)
    cat("Shapiro-Wilk test by level of", input$treat1, "\n")
    print(tapply(vari, f1, function(v) tryCatch(shapiro.test(v)$p.value,
                                                error = function(e) NA)))
    if (input$fator != "one") {
      f2 <- column_of(d, input$treat2)
      cat("\nShapiro-Wilk test by level of", input$treat2, "\n")
      print(tapply(vari, f2, function(v) tryCatch(shapiro.test(v)$p.value,
                                                  error = function(e) NA)))
    }
  })

  output$anovatable <- renderPrint({
    invisible(run_anova(graph = FALSE))
  })

  output$gsPlot <- renderPlot({
    d <- mydata.a()
    vari <- column_of(d, input$resp)
    trt1 <- column_of(d, input$treat1)
    if (input$fator == "one") {
      boxplot(vari ~ trt1, col = "skyblue", ylab = input$resp,
              xlab = input$treat1)
    } else {
      ids <- c(input$treat1, input$treat2,
               if (input$fator == "three") input$treat3)
      op <- par(mfrow = c(1, length(ids)))
      on.exit(par(op))
      for (id in ids) {
        boxplot(vari ~ column_of(d, id), col = "skyblue", ylab = input$resp,
                xlab = id)
      }
    }
  })

  output$tukeyplot <- renderPlot({
    capture.output(run_anova(graph = TRUE))
  })
}

shinyApp(ui = ui, server = server)
