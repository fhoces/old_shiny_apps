library(shiny)
library(twang)

ui <- fluidPage(
  sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 500px; position:relative;",
    sliderInput("param1", label = "Gamma1 = ",
                min = 0.2, max = 5, value = 1, step = 0.1), 
    sliderInput("param2", label = "Gamma2 = ",
                min = 0.2, max = 5, value = 1, step = 0.1), 
    sliderInput("param3", label = "Beta = ",
                min = 0.2, max = 5, value = 1, step = 0.1),
    sliderInput("param4", label = "scale = ",
                min = 1, max = 2, value = 1, step = 0.1), 
    sliderInput("param5", label = "sigma_e1 = ",
                min = .1, max = 2, value = 1, step = 0.1), 
    sliderInput("param6", label = "scale_e2 = ",
                min = .01, max = .5, value = .2, step = 0.05), 
    sliderInput("param7", label = "sigma_x = ",
                min = .1, max = 2, value = 1, step = 0.1), 
    sliderInput("param8", label = "sigma_z = ",
                min = .1, max = 2, value = 1, step = 0.1)  ),
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Plot", plotOutput("plot1")), 
                tabPanel("Table", tableOutput("table"))
    )
  )
)

server <- function(input, output) {

  sim.data2 <- function(size = 1000,
                        mu.x = 0, sigma.x = 1, 
                        mu.z = 0, sigma.z = 1, 
                        sigma.eps1 = 1, sigma.eps2 = 1, 
                        scale = 1, 
                        beta = 1, tau = 0, 
                        gamma1 = 1, gamma2 = 1, 
                        prop.treat = 1/2)
  {
    set.seed(123)
    eps1 <- rnorm(size, mean = 0, sd = sigma.eps1)
    eps2 <- rnorm(size, mean = 0, sd = sigma.eps2)
    x.var <- rnorm(size, mean = mu.x, sd = sigma.x)
    z.var <- rnorm(size, mean = mu.z, sd = sigma.z)
    f.x <- exp(x.var/scale) - mean( exp(x.var/scale) )
    true.score <- 1 / ( 1 + exp( -(gamma1 * x.var + gamma2 * z.var) ) )
    treat <- 1*(true.score >= 1/2 + eps2)
    y <- tau * treat + beta * f.x + eps1
    return(data.frame(y, treat, x.var, z.var))
  } 
  
  
  reactive.data2 <- reactive( {
    df <- sim.data2(gamma1 = as.numeric(input$param1), 
                    gamma2 = as.numeric(input$param2),
                    beta = as.numeric(input$param3),
                    scale = as.numeric(input$param4), 
                    sigma.eps1 = as.numeric(input$param5), 
                    sigma.eps2 = as.numeric(input$param6), 
                    sigma.x = as.numeric(input$param7), 
                    sigma.z = as.numeric(input$param8)) 
    df
  }
  )
  
  
  output$plot1 <- renderPlot({
    df <- reactive.data2()
    plot(density(df$x.var[df$treat == 1]), 
         col = "red", lty = 1, 
         xlim =c(-5,5), 
         ylim = c(0,.6), 
         main = "Distribution of X by Treatment Status")
    lines(density(df$x.var[df$treat == 0]), col = "blue", lty = 2)
    legend("topleft", c("Treatment", "Control"), col=c("red", "blue") , lty=c(1,2) )
  })

  # Generate an HTML table view of the data
  output$table <- renderTable({
    df <- reactive.data2()
    weight.ps = rep(1,length(df$y))
    bal = bal.table(dx.wts(x = weight.ps, data=as.data.frame(df), 
                           vars = c("x.var","z.var"), estimand= "ATE", 
                           x.as.weights = T, treat.var = "treat")
    )
    as.data.frame(bal)
  })
  
}

shinyApp(ui = ui, server = server)
