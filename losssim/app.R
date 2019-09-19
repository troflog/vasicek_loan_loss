#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(moments)
library(PerformanceAnalytics)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Vasicek loan loss model"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("rho",
                        "Correlation:",
                        min = 0,
                        max = 1,
                        value = 0.3),
            sliderInput("pd",
                        "Probability of default:",
                        min = 0,
                        max = 1,
                        value = 0.1),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 100,
                        value = 20),
            sliderInput("years",
                        "Number of years simulated:",
                        min = 5000,
                        max = 30000,
                        value = 5000),
            sliderInput("corporates",
                        "Number of corporates:",
                        min = 100,
                        max = 10000,
                        value = 1000)
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            textOutput("kurtosis"),
            textOutput("skew")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        sim_port_loss <- function(n,number_of_years,pd,rho){
            loss_lim <- qnorm(pd)
            systematic_factor <-matrix(rep(rnorm(number_of_years),each=n) ,nrow=n)
            idiosyncratic_factor <- matrix(rnorm(n*number_of_years),nrow=n,ncol=number_of_years)  
            firm_value <- sqrt(rho)*systematic_factor+sqrt(1-rho)*idiosyncratic_factor
            sim_port_loss = colSums(firm_value<loss_lim)/n
        }
        pd = min(max(input$pd,1e-6),0.99999999)
        rho = min(max(input$rho,1e-6),0.99999999)
        n=input$corporates
        number_of_years= input$years
        df <-data.frame(loss_rate=sim_port_loss(n,number_of_years,pd,rho))
        kur = round(kurtosis(df$loss_rate,method='sample_excess'),3)
        
        kur_txt = paste('Kurtosis = ',toString(kur))
        
        skew_txt = paste('Skewnes = ',toString(round(skewness(df$loss_rate),3)))
        
        output$kurtosis = renderText(kur_txt)
        output$skew = renderText(skew_txt)
        #Small change
        
        
        ggplot(df, aes(x=loss_rate)) +geom_histogram(color="darkblue", fill="lightblue",bins=input$bins)+labs(x='Loss rate')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
