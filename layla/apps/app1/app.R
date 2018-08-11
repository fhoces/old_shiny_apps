library(shiny)
library(twang)
#install.packages(c("ISLR", "tree", "randomForest", "ipred", "CBPS"), repos = 'http://cran.us.r-project.org')

ui <- fluidPage(
  sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 500px; position:relative;",
    sliderInput("param1", label = "Gamma1 = ",
                min = 0.2, max = 5, value = 1, step = 0.1), 
    sliderInput("param2", label = "Gamma2 = ",
                min = 0.2, max = 5, value = 1, step = 0.1), 
    sliderInput("param3", label = "Gamma3 = ",
                min = 0.2, max = 5, value = 1, step = 0.1),   
    sliderInput("param4", label = "sigma_t = ",
                min = 0.2, max = 5, value = 1, step = 0.1),
    sliderInput("param5", label = "sigma_c = ",
                min = 1, max = 2, value = 1, step = 0.1), 
    sliderInput("param6", label = "beta_1 = ",
                min = .1, max = 2, value = 1, step = 0.1), 
    sliderInput("param7", label = "beta_2 = ",
                min = .01, max = .5, value = .2, step = 0.05),
    sliderInput("param8", label = "scale = ",
                min = .1, max = 2, value = 1, step = 0.1)  ),
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Plot", plotOutput("plot1")), 
                tabPanel("Table", tableOutput("table"))
    )
  )
)
server <- function(input, output) {

  sim.data1        <- function(nsim = 900, 
                               gamma1 = 1/2, 
                               gamma2 = 2, 
                               gamma3 = 3,
                               sigma1 = 1, 
                               beta1 = 1,
                               beta2 = 0,
                               sigma.c = sqrt(4/3), 
                               sigma.t = sqrt(2/3),
                               tau = 0, 
                               scale = 1/2 
  )
  {
    set.seed(123)
    treat  <-  rbinom(nsim, 1, 0.3)
    x.var  <-  vector(length = nsim)
    z.var  <-  vector(length = nsim)
    x.var <-  rnorm(nsim, 
                    mean = -0.25 + gamma1 * treat , 
                    sd = sigma.c * (1 - treat) + sigma.t * treat)
    z.var[treat == 1]  <-  rexp(length(z.var[treat == 1]),gamma2)
    z.var[treat == 0]  <-  rexp(length(z.var[treat == 0]),gamma3)
    
    y         <-  beta1 * exp(scale * x.var) + 
      beta2 * z.var + tau * treat + 
      rnorm(length(treat), 0, sd=sigma1) 
    df        <- data.frame(y, treat, x.var, z.var)
    return(df)
  }
  
  reactive.data1 <- reactive( {
    sim.data1(gamma1 = as.numeric(input$param1), 
              gamma2 = as.numeric(input$param2),
              gamma3 = as.numeric(input$param3),
              sigma.c = as.numeric(input$param4), 
              sigma.t = as.numeric(input$param5), 
              beta1 = as.numeric(input$param6), 
              beta2 = as.numeric(input$param7), 
              scale = as.numeric(input$param8)) 
  } )

  output$plot1 <- renderPlot({
    df <- reactive.data1()
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
    df <- reactive.data1()
    weight.ps = rep(1,length(df$y))
    bal = bal.table(dx.wts(x = weight.ps, data=as.data.frame(df), 
                           vars = c("x.var","z.var"), estimand= "ATE", 
                           x.as.weights = T, treat.var = "treat")
    )
    as.data.frame(bal)
  })
  
}

shinyApp(ui = ui, server = server)
