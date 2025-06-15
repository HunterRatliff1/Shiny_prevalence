

navbarPage(
  "PPV & Bayes",
  # ----------------------- FIRST PANEL ----------------------------------------
  tabPanel("Graphical representation",
    sidebarLayout(      
      
    
    # Sidebar
      sidebarPanel(
        sliderInput("sensitivity",label="Sensitivity:", min = 0, max = 100, post  = " %", value = 80),
        sliderInput("specificity",label="Specificity:", min = 0, max = 100, post  = " %", value = 95),
        hr(),
        h4(strong("Set Prevalence")),
        textOutput("prevalence"),br(),
        sliderInput("prevNumerator",label="Numerator", min = 0, max = 100, value = 25, post=" affected"),
        selectInput("prevDenominator", label="Denominator", selected = c("Per 100 population"=100),
                    c("Per 100 population"=10^2, "Per 1,000"=10^3, "Per 10,000"=10^4,
                      "Per 100,000"=10^5, "Per million"=10^6,
                      "Per 100 million"=10^8))
      ),
      
      # Main panel
      mainPanel(
        fluidRow(
          column(6, 
            h4("2 x 2 table"),
            tableOutput("TwoByTwo")
          ),
          column(6, wellPanel(
            h4("Positive Predictive Value"),
            p("Among those who test positive, ", code(textOutput("ppv", inline=T)), " will actually have the disease")
          ))),
        tabsetPanel(
          type = "tabs",
          tabPanel("Plot", 
            plotOutput("conditionalPlot"),
            radioButtons("cases", "Show which cases?", inline=T,
                         choices = c("Both", "Test positive", "Test negative"),
                         selected="Both")
            # verbatimTextOutput("print")
          ),
          tabPanel(
            "Math", 
            p("The positive predictive value can be calculated via:"), 
            uiOutput("PPVmath"),
            helpText("For this example, we'll assume a population of 100 to make the math easy"),
            hr(),
            p("These numbers (from the 2x2 table) are calculated using a formula derived from using Bayes' theorem:"),
            withMathJax(
              "$$P(\\ {\\sf HasDz}\\ |\\ {\\sf Test+}\\ )\\ =\\ \\frac{P(\\ {\\sf Test+}\\ |\\ {\\sf HasDz}\\ )\\ \\ P(\\ {\\sf HasDz}\ )}{P(\\ {\\sf Test+}\\ )}$$"
            ),
            br(), br(),
            p('Further details on this calculation can be found on the "Derivation" tab at the top')
          )
        )
      )
    )
  ),
  
  # ----------------------- SECOND PANEL ----------------------------------------
  tabPanel("Likelihood ratios",
           sidebarLayout(
             
             # Sidebar
             sidebarPanel(
               sliderInput("sensitivity2",label="Sensitivity:", min = 0, max = 100, post  = " %", value = 80),
               sliderInput("specificity2",label="Specificity:", min = 0, max = 100, post  = " %", value = 95),
               verbatimTextOutput("results")
             ),

             # Main panel
             mainPanel(
               fluidRow(
                 column(12,
                        # plotOutput("probPlot"),
                        plotlyOutput("probPlotly"),
                        checkboxInput("log_x_scale", "Use log10 scale for X-axis", value = FALSE)
                        # verbatimTextOutput("print"),

                 ),
                 column(12, wellPanel(
                   h4("Post-test probability"),
                   tableOutput("postTestTbl")
                 ))),

             )
           )
  ),
  
  tabPanel(
    "Sens & Spec",
    
    fluidRow(
      column(6, wellPanel(
             h3("Controls for black line"),
             sliderInput("sensitivity1",label="Sensitivity (black):", min = 0, max = 100, post  = " %", value = 80),
             sliderInput("specificity1",label="Specificity (black):", min = 0, max = 100, post  = " %", value = 95)
      )),
      column(6, wellPanel(
        h3("Controls for red line"),
        sliderInput("sensitivity2",label="Sensitivity (red):", min = 0, max = 100, post  = " %", value = 95),
        sliderInput("specificity2",label="Specificity (red):", min = 0, max = 100, post  = " %", value = 99)
      ))
    ),
    plotOutput("tab2plot")
    
    
  ),
  tabPanel("Derivation", p(
      "Recall that the formula for", strong("positive predictive value"), "is true positives divided by all positives.",
      withMathJax("$$PPV = \\frac{TP}{TP + FP}$$"),
      "This is expressed visually below:", br(),br(),
      htmlOutput("table"),
      hr(),
      
      "We can also calculate the same value using Bayes' theorem, where the numerator is equal to true positives",
      withMathJax(
        "$$P(\\ {\\sf HasDz}\\ |\\ {\\sf Test+}\\ )\\ =\\ 
        \\frac{P(\\ {\\sf Test+}\\ |\\ {\\sf HasDz}\\ )\\ \\ P(\\ {\\sf HasDz}\ )}{P(\\ {\\sf Test+}\\ )}$$"
      ),
      # uiOutput("bayes"),
      "To break this down", code("P( HasDz | Test+)"), "is expressing the", 
      strong("PPV"), "as the PPV by definition is the proportion of those who have the disease, given the test is positive.",
      "Similarly, the definition of sensitivity is the proportion of those who test positive, given they have the disease (i.e.",
      code("P( Test+ | HasDz )"), ").", "If we multiply the sensitivity by", code("P(HasDz)"), "(prevalence), we get the percent",
      "of true positives.",
      br(),br(),
      "Our", code("P(Test+)"), "is really the same denominator as before (TP + FP). We've already seen how to use 
      prevalence and sensitivity to calculate TP, and the FP can be calculated using", code("P(Test- | noDz) P(noDz)"), 
      br(),br(),
      "This gives us:",
      withMathJax(
        "$$P(\\ {\\sf HasDz}\\ |\\ {\\sf Test+}\\ )\\ =\\ 
        \\frac{\\Big[P(\\ {\\sf Test+}\\ |\\ {\\sf HasDz}\\ )\\ \\ P(\\ {\\sf HasDz}\ )\\Big]}
        {
          \\Big[P(\\ {\\sf Test+}\\ |\\ {\\sf HasDz}\\ )\\ \\ P(\\ {\\sf HasDz}\ )\\Big]\\ +\\ 
          \\Big[P(\\ {\\sf Test-}\\ |\\ {\\sf noDz}\\ )\\ \\ P(\\ {\\sf noDz}\ )\\Big]
        }$$"
      ),
      "After you substitute in the variables, you get the Bayes' derived equation, using prevalence, sensitivity and specificity:",
      withMathJax(
        "$$P(\\ {\\sf HasDz}\\ |\\ {\\sf Test+}\\ )\\ =\\ 
        \\frac{\\Big[{\\sf Sens}\\ \\times\\ {\\sf Prev}\\Big]}
        {
          \\Big[{\\sf Sens}\\ \\times\\ {\\sf Prev}\\Big]\\ +\\ 
          \\Big[(1-{\\sf Spec})\\ \\times\\ (1-{\\sf Prev})\\Big]
        }$$"
      ),
      hr(),
      "To illustrate this, imagine a population of 25 people and a disease that has a 40% prevalence. 
      If we have a screening test that has a 80% sensitivity and a 67% specificity, we get the following values",
      br(),br(),
      tags$table(
        tags$tr(
          tags$td("TP: 8", style="padding: 5px;border: 1px solid black;color:white;background-color:#008C48"), 
          tags$td("FP: 5", style="padding: 5px;border: 1px solid black;color:white;background-color:#EE2E2F")
        ),
        tags$tr(
          tags$td("FN: 2", style="padding: 5px;border: 1px solid black;color:white;background-color:#F47D23"), 
          tags$td("TN: 10", style="padding: 5px;border: 1px solid black;color:white;background-color:#185AA9")
        )
      ), br(),
      "You can use these values to calculate the PPV using both methods:",
      withMathJax("$$PPV = \\frac{TP}{TP + FP} = \\frac{8}{8 + 5} = 61.5\\%$$"),
      withMathJax(
        "$$P(\\ {\\sf HasDz}\\ |\\ {\\sf Test+}\\ )\\ =\\ \\frac{0.8\\ \\times\\ 0.4}{(0.8\\times0.4)\\ +\\ \\big[ (1-0.67)\\times(1-0.4) \\big]} = 61.5\\%$$"
      )
  ))
)

