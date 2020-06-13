

# Define a server for the Shiny app
function(input, output) {
  
  # Calculate common values, return as list
  calcVals <- reactive({
    n <- 1
    prev <- as.numeric(input$prevNumerator) / as.numeric(input$prevDenominator)
    
    sens <- input$sensitivity/100
    spec <- input$specificity/100
    
    TP <- prev * sens * n
    FN <- prev * (1-sens) * n
    TN <- (1-prev) * spec * n
    FP <- (1-prev) * (1-spec) * n
    haveDz <- (TP + FN) 
    noDz   <- (FP + TN) 
    
    PPV <- (sens * prev) / ({sens * prev} + {(1-spec)*(1-prev)})
    
    list(prev=prev, sens=sens, spec=spec, 
         TP=TP, TN=TN, FP=FP, FN=FN,
         haveDz=haveDz, noDz=noDz, 
         PPV=PPV, n=n)
  })
  
  # Creates dataframe
  makeDf <- reactive({
    nums <- calcVals()
    
    tibble(
      boxes = c("FN", "TP", "TN", "FP"),
      dz   = c(T,T,F,F),
      test = c("(-)", "(+)", "(-)", "(+)"),
      vals = c(nums$FN, nums$TP, nums$TN, nums$FP),
      xmin = c(0, 0, nums$haveDz, nums$haveDz),
      xmax = c(nums$haveDz, nums$haveDz, nums$n, nums$n),
      ymin = c(0, nums$FN/nums$haveDz, 0, nums$TN/nums$noDz),
      ymax = c(nums$FN/nums$haveDz, nums$haveDz/nums$haveDz, nums$TN/nums$noDz, nums$noDz/nums$noDz)
    ) 
  })
  
  
  output$TwoByTwo <- renderTable(rownames = T, {
    nums <- calcVals()
    x <- c(
      paste0("TP: ",percent(nums$TP)),
      paste0("FN: ",percent(nums$FN)),
      paste0("FP: ",percent(nums$FP)),
      paste0("TN: ",percent(nums$TN))
    )
    
    x <- matrix(x, nrow = 2, ncol=2)
    colnames(x) <- c("Has Disease", "No Disease")
    rownames(x) <- c("Test (+)", "Test (-)")
    x
  })
  
  output$print <- renderPrint({
    calcVals()
  })
  
  output$prevalence <- renderText({
    prevNum <- as.numeric(input$prevNumerator)
    prevDen <- as.numeric(input$prevDenominator)
    paste0("Disease prevalence: ", prevNum," per ",scales::comma(prevDen),
           " (",scales::percent(prevNum/prevDen),")")
  })
  
  output$conditionalPlot <- renderPlot({
    nums <- calcVals() # make values to grab for annotations
    plotDf <- makeDf()
    if(input$cases == "Test positive") {plotDf <- filter(plotDf, test=="(+)")}
    if(input$cases == "Test negative") {plotDf <- filter(plotDf, test=="(-)")}
    
    
    plotDf %>%
      group_by(boxes) %>%
      mutate(vals = ifelse(max(vals)<1, scales::percent(vals), round(vals))) %>%
      ungroup() %>%
      mutate(boxes = factor(boxes, levels = c("TP", "TN", "FP", "FN"))) %>%
      ggplot(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, 
                 group=boxes, fill=boxes)) +
      
      # labels
      annotate("errorbarh", y=0, xmin=0, xmax=nums$prev, height = .05) +
      annotate("text", y=-0.025, x=nums$prev/2, label="Dz") +
      annotate("errorbarh", y=0, xmin=nums$prev, xmax=1, height = .05) +
      annotate("text", y=-0.025, x=nums$prev + ((1-nums$prev)/2), label="Healthy") +
      annotate("errorbar", x=0, ymin=1-nums$sens, ymax=1, width=0.025) +
      annotate("text", x=-0.025, y=1-nums$sens/2, label="Sensitivity", angle = 90) +
      annotate("errorbar", x=1, ymin=0, ymax=nums$spec, width=0.025) +
      annotate("text", x=1.025, y=nums$spec/2, label="Specificity", angle = 90) +
      
      # geoms
      geom_rect(alpha=0.9) +
      geom_text(aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=vals), 
                hjust="left") +
      
      scale_y_continuous(labels = scales::percent, limits = c(-0.05,1.05)) +
      scale_x_continuous(labels = scales::percent, limits = c(-0.05,1.05)) +
      scale_fill_manual(values = c("TP"="#008C48", "TN"="#185AA9", "FP"="#EE2E2F", "FN"="#F47D23")) +
      # scale_fill_few() +
      theme_bw() +
      labs(x="% of total population", y="% of condition")
  })
  
  output$PPVmath <- renderUI({
    x <- calcVals()
    withMathJax(paste0(
      "$$ PPV = \\frac{TP}{TP + FP} = \\frac{",
      round(x$TP*100,2), "}{", round(x$TP*100,2)," + ",round(x$FP*100,2),
      "} = ", round(x$PPV*100, 2), "\\%$$"
    ))
    
    
  })
  
  output$ppv <- renderText({
    x <- calcVals()
    
    percent(x$PPV)
    
  })
  
  output$tab2plot <- renderPlot({
    
    makePPV_black <- function(prev, sens=input$sensitivity1/100, spec=input$specificity1/100) {
      prev <- 1/prev
      (sens * prev) / ({sens * prev} + {(1-spec)*(1-prev)})
    }
    
    makePPV_red <- function(prev, sens=input$sensitivity2/100, spec=input$specificity2/100) {
      prev <- 1/prev
      (sens * prev) / ({sens * prev} + {(1-spec)*(1-prev)})
    }
    
    tibble(
      prev = c(seq(1, 9, by=2), seq(10, 490, by=10), seq(500, 2500, by=100))
    ) %>% 
      mutate(PPV = makePPV_black(prev)) %>%
      mutate(PPV2 = makePPV_red(prev)) %>%
      ggplot(aes(x=prev)) + 
      geom_line(aes(y=PPV), color="black") +
      geom_line(aes(y=PPV2), color="red") +
      xlim(c(NA,2500)) +
      scale_y_continuous(labels=percent, limits=c(0,1)) +
      labs(x="1/Prevalence")
    
  })
  
  
  
  
  output$table <- renderUI({
    HTML(ppvHTML)
  })
  
  output$bayes <- renderUI({
    withMathJax(paste0("$$P(\\ {\\sf HasDz}\\ |\\ {\\sf Test+}\\ )\\ =\\ \\frac{P(\\ {\\sf Test+}\\ |\\ {\\sf HasDz}\\ )\\ \\ P(\\ {\\sf HasDz}\ )}{P(\\ {\\sf Test+}\\ )}$$"))
  })
}

