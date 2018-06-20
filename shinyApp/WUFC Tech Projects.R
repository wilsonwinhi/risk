library(shiny)
library(ggplot2)
library(shinythemes)
library(scales)
library(plotly)

ui <- shiny::navbarPage(
  theme = shinytheme("flatly"),
  title = "WUFC Tech Projects",
  tabPanel("Interactive Finance Tutorial",
  h1("Risk in Finance", align = "center"),
  h3(em("Techniques and Applications"), align = "center"),
  hr(),
  h3("Imagine This"),
  p("Say you have 10 million dollar to invest, each time you invest 1 million to the market,
you make an investment once a year. Each time, choose either bank or higher risk investment 
to invest. See how much you will have ten years later and see what type of investor you are.
     "),
  sidebarLayout(
    sidebarPanel(
      sliderInput("money", "How much risk you want to take?", 0, 100, 50, step = 1),
      actionButton("invest", 'Invest!')
    ),
    mainPanel(
      h3("Money Trend"),
      plotOutput("plotxy")
    )
  ),
  p("We see from the scenario that not every investment has a guaranteed return. 
    That is, every investment involves certain level of risk. In investment, it is crucial to 
    understand that if you're taking a higher risk, the expected return should also be higher.
     In this section, we would dive deeper to how to depict the risk in finance."),
  hr(),
  # tags$div(
  #   tags$ul(
  h3("Variance In Action"),
  fluidRow(
  sidebarPanel(
    sliderInput("variance", "Specify the Variance", 0, 1, 0.5, step = 0.01),
    p("The less of your variance, the more certain you know what value you are going to have.
      Likewise, if the variance of your model is high, the distribution is more spread out,
      meaning, there are more chance of uncertainties."),
    p("The risk in a stock is its response to the stock market movement.
               In this example, if you have a riskier stock, hence, a higher variance,
      it will have a larger movement compared to the stock market.
      In finance world, people called this variance: beta")
  ),
  mainPanel(
    plotOutput("plotNormal"),
    plotOutput("stock")
  )),
 
  p("The variance of a random variable X is the expected value of the squared deviation from the mean of 
        X. It somehow measures how \"spread out\" the data is. Standard deviation is the square root of variance. 
        Variance is important in investment because by understanding variance, we can know how much return we 
        ought to expect. The most common technique we use is the Capital Asset Pricing Model (CAPM)."),
  hr(),
  h3("Capital Asset Pricing Model"),
  #plotOutput("capm"),
  img(src='capm.png', align = "middle"),
      withMathJax(
        p("$$r_a = r_f + \\beta \\cdot(r_m - r_f)$$ ")
        ),
      p("This is the model to predict the expected return.", align = "center"),
  sliderInput("capm", "Specify beta", 0, 10, 5, step = 0.1),
 p("The risk premium is"), 
 verbatimTextOutput("risk")
    )
)
#   )
# )
server <- function(input, output){
  output$plotxy <- renderPlot({
    x = c(1:100)
    y = c(1:100)
    z = floor(runif(100, min = -input$money, max = 1.2 * input$money))
    y[1] = 10
    for(i in c(2: 100)){
      y[i] = y[i - 1] + y[i - 1] * z[i]/100
    }
    if(input$invest){
      d = data.frame(x, y)
      plot( d$x, d$y )
    }
  })
  
  output$stock <- renderPlot({
    # Define 2 vectors
    price <- c(1:50)
    trucks <- c(2, 5, 4, 5, 12)
    z = floor(rnorm(50, mean = price, sd  = 10 * input$variance)) 
    # Graph cars using a y axis that ranges from 0 to 12
    plot(price, type="o", col="blue", ylim=c(0, 50)) +
      
      # Graph trucks with red dashed line and square points
      lines(z, type="o", pch=22, lty=2, col="red") +
      
      # Create a title with a red, bold/italic font
      title(main="Stock Market", col.main="blue", font.main=4)

      legend(3, 40, legend=c("Riskier stock", "Market Stock"),
             col=c("red", "blue"), lty=1:2, cex=0.8)
  }
  )

  output$capm <- renderPlot({
    # Define 2 vectors
    cars <- c(1, 3, 6, 4, 9)
    trucks <- c(2, 5, 4, 5, 12)
    
    # Graph cars using a y axis that ranges from 0 to 12
   plot(cars, type="o", col="blue", ylim=c(0,12)) +
    
    # Graph trucks with red dashed line and square points
    lines(trucks, type="o", pch=22, lty=2, col="red") +
    
    # Create a title with a red, bold/italic font
    title(main="CAPM", col.main="red", font.main=4) 
  }
  )
  
  output$plotNormal <- renderPlot({
   ggplot( data = data.frame(x = c(-5, 5)), aes(x)) +
      stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = input$variance)) +
      scale_y_continuous(breaks = NULL) +
      theme_bw() +
      ggtitle("Normal Distribution")
  })
  
  #output$beta <- renderPrint({ input$capm})
  output$risk <- renderPrint((0.05 +{input$capm} * 0.085) * 100)
}

shinyApp(ui = ui, server = server)
WUFC Tech Projects.R