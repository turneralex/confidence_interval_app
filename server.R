library(shiny)
library(dplyr)
library(purrr)
library(boot)
library(fGarch)
library(magrittr)
library(ggplot2)

shinyServer(function(input, output) {
  
  hist_df <- eventReactive(input$sample, {
    if (input$dist_type == "Normal") {
      temp_1 <- data_frame(values = rnorm(input$sample_size, mean = 0, sd = input$sd))
    } else if (input$dist_type == "Right skewed") {
      temp_1 <- data_frame(values = rsnorm(input$sample_size, mean = 0, sd = input$sd, xi = 100))
    } else if (input$dist_type == "Left skewed") {
      temp_1 <- data_frame(values = rsnorm(input$sample_size, mean = 0, sd = input$sd, xi = -100))
    }
  })
  
  sample_stats <- eventReactive(input$sample, {
    if (input$conf_level == "0.99") {
      x <- mean(hist_df()$values)
      if (input$dist_type == "Normal") {
        y <- c(x - (qnorm(0.995) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.995) * input$sd / sqrt(input$sample_size)))
      } else if (input$sample_size > 30) {
        y <- c(x - (qnorm(0.995) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.995) * input$sd / sqrt(input$sample_size)))
      } else {
        y <- boot(hist_df()$values, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
          boot.ci(conf = 0.99, type = "bca") %>% 
          pluck("bca") %>% 
          .[4:5]
      }
      temp_2 <- c(x, y)
    } else if (input$conf_level == "0.95") {
      x <- mean(hist_df()$values)
      if (input$dist_type == "Normal") {
        y <- c(x - (qnorm(0.975) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.975) * input$sd / sqrt(input$sample_size)))
      } else if (input$sample_size > 30) {
        y <- c(x - (qnorm(0.975) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.975) * input$sd / sqrt(input$sample_size)))
      } else {
        y <- boot(hist_df()$values, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
          boot.ci(conf = 0.95, type = "bca") %>% 
          pluck("bca") %>% 
          .[4:5]
      }
      temp_2 <- c(x, y)
    } else if (input$conf_level == "0.90") {
      x <- mean(hist_df()$values)
      if (input$dist_type == "Normal") {
        y <- c(x - (qnorm(0.95) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.95) * input$sd / sqrt(input$sample_size)))
      } else if (input$sample_size > 30) {
        y <- c(x - (qnorm(0.95) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.95) * input$sd / sqrt(input$sample_size)))
      } else {
        y <- boot(hist_df()$values, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
          boot.ci(conf = 0.90, type = "bca") %>% 
          pluck("bca") %>% 
          .[4:5]
      }
      temp_2 <- c(x, y)
    } else if (input$conf_level == "0.50") {
      x <- mean(hist_df()$values)
      if (input$dist_type == "Normal") {
        y <- c(x - (qnorm(0.75) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.75) * input$sd / sqrt(input$sample_size)))
      } else if (input$sample_size > 30) {
        y <- c(x - (qnorm(0.75) * input$sd / sqrt(input$sample_size)), x + (qnorm(0.75) * input$sd / sqrt(input$sample_size)))
      } else {
        y <- boot(hist_df()$values, statistic = function(x, index) mean(x[index]), R = 1000) %>% 
          boot.ci(conf = 0.50, type = "bca") %>% 
          pluck("bca") %>% 
          .[4:5]
      }
      temp_2 <- c(x, y)
    }
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(hist_df(), aes(values)) +
      geom_histogram(fill = "springgreen3") 
    
    p1 <- p + geom_point(aes(sample_stats()[1], 0), size = 2) +
      geom_errorbarh(aes(xmin = sample_stats()[2], 
                         xmax = sample_stats()[3],
                         y = 0),
                     height = 0.2,
                     size = 1) +
      labs(title = "Histogram of sample",
           x = "Sample values",
           y = "Count") +
      theme(plot.title = element_text(size = 20),
            axis.title = element_text(size = 15))
    
    p2 <- p + geom_point(aes(sample_stats()[1], 0), size = 15) +
      geom_errorbarh(aes(xmin = sample_stats()[2], 
                         xmax = sample_stats()[3],
                         y = 0),
                     height = 0.2,
                     size = 2) +
      coord_cartesian(xlim = c(sample_stats()[2] - 0.2, sample_stats()[3] + 0.2),
                      ylim = c(-0.2, 0.2)) +
      labs(title = "Confidence interval zoom",
           x = "Sample values",
           y = "") +
      theme(plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    cowplot::plot_grid(p1, p2, ncol = 1)
    
  })
  
  output$mean <- renderText({
    paste("Sample mean =", round(sample_stats()[1], 2))
  })
  
  output$ci <- renderText({
    paste0(
      "Confidence interval = ",
      "[",
      round(sample_stats()[2], 2),
      ", ",
      round(sample_stats()[3], 2),
      "]"
    )
  })
  
  output$mean_captured <- renderText({
    if (0 > sample_stats()[2] & 0 < sample_stats()[3]) {
      print("Confidence interval captures true mean")
    } else {
      print("Confidence interval does not capture true mean")
    }
  })
  
})
