library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

function(input, output){
  
  newgraph1 <- reactive({
    x = input$abc
    y = input$def
    
    table <- as.data.frame(seq(1:101))
    colnames(table) <- 'weight'
    table <- table %>% mutate(weight = (weight - 1)/100)
    corr <- -0.8
    
    standard <- function (w, corr, sd1, sd2){
      a = (w * sd1)^2
      b = ((1 - w)*sd2)^2
      c = 2 * w * (1 - w) * sd1*sd2*corr
      value = (a + b + c)^(1/2)
      return (value)
    }
    
    expected <- function (w, er1, er2){
      a = w * er1
      b = (1 - w) * er2
      return (a+b)
    }
    
    table <- table %>% mutate(stdev = standard(weight, corr, 
                                               newalloc$SD[x], newalloc$SD[y]), 
                              expreturn = expected(weight,
                                                   newalloc$ER[x], newalloc$ER[y]))
    
    table <- table %>% mutate(weight = weight*100, stdev = stdev*100,
                              expreturn = expreturn*100)
    
  })
  
  newgraph2 <- reactive({
    x = input$abc
    y = input$def
    allocation <- allocation %>% mutate(newstockval = newstockval * 100,
                                        newbondval = newbondval * 100,
                                        newEUROval = newEUROval * 100,
                                        newASIAval = newASIAval * 100,
                                        newIGval = newIGval * 100,
                                        newHYval = newHYval *100)
    colnames(allocation) <- c('year', 1:6)
    newbargraph <- allocation %>% select(x, y)
  })
  
  output$frontier <- renderPlot(
    newgraph1 %>%
      ggplot(aes(x = stdev, y = expreturn, colour = weight)) + 
      geom_point() +
      labs(title = 'Efficient Frontier', 
           x = 'Standard Deviation',
           y = 'Expected Return') +
      theme_bw()
  )
  
  output$bargraph <- renderPlot(
    newgraph2 %>%
      ggplot() +
      geom_bar(aes(x = x, y = y, fill = 'stocks'),
             stat = 'identity', width = 0.3, position = 'dodge') +
      geom_bar(aes(x = x, y = y, fill = 'bonds'),
            stat = 'identity', width = 0.3, position = position_nudge(x = 0.3)) +
      labs(title = 'Historical Performance',
           y = 'Returns') +
      theme_bw()
  )
  
  output$statistics <- renderTable(newalloc)
  output$allnumbers <- renderTable(table)
}