library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Explore the effects of distribution shape, sample size & variance on estimates"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Each distribution has a true mean of 0."),
      p("When the central limit theorem will hold (sample size > 30 or population distribution is normal),
        standard normal theory confidence intervals are used. In the case of the skewed distributions with small
        samples, BCa bootstrap intervals based on 1,000 resamples from the empirical distribution are used.
        This can result in asymmetric CIs for the latter."),
      selectInput("dist_type",
                  "Choose the distribution shape",
                  choices = c("Normal", "Right skewed", "Left skewed")),
      sliderInput("sample_size", 
                  "Choose a sample size:", 
                  min = 10,
                  max = 1000,
                  step = 10,
                  value = 100),
      sliderInput("sd", 
                  "Choose the standard deviation:", 
                  min = 1,
                  max = 5,
                  value = 1),
      selectInput("conf_level",
                  "Choose the confidence level",
                  choices = c("0.99", "0.95", "0.90", "0.50"),
                  selected = "0.95"),
      actionButton("sample", "Draw sample"),
      verbatimTextOutput("mean"),
      verbatimTextOutput("ci"),
      verbatimTextOutput("mean_captured")
    ),
    mainPanel(
      plotOutput("plot", height = 800)
    )
  )
)
)