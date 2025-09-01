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
library(dplyr)
library(gridExtra)
library(grid)
library(fGarch)
options(scipen=9999)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    column(12, align="center", "How results change between paired t-tests and two-sample t-tests"),
    br(),
    br(),
    em("When your data is a Repeated Measurement, or two measurements on the same individual, it is advisable to use a paired t-test instead of a two-sample t-test, but why? Wouldn't a two-sample t-test pick up the differences just as well? Use this simulator to help you find out what sort of scenarios would make it so that a two-sample t-test would not see the differences but a paired t-test would."),
    br(),
    br(),
    br(),
    "Use the input to change standard deviation and mean. The second (blue) distribution is made by taking the first (red) distribution and adding on a randomized amount of data. ",
    br(),

    hr(),
    
    fluidRow(
        column(4,
            sliderInput("sd1",
                        "Standard Deviation of Distributions:",
                        min = 1,
                        max = 50,
                        value = 25),
            sliderInput("m1",
                        "Difference In Means:",
                        min = 1,
                        max = 50,
                        value = 2),
            actionButton("guess", "Submit Changes"),
            tableOutput("ptable")
            
        ),
        
        # Show a plot of the generated distribution
        column(7,
            plotOutput("distPlot"),
           
           
        )
    ),
    br(),
    br(),
    br(),
    column(12, "Below is a chunk of the data you are making in this simulation. This data is a repeated measurement simulation, so the third column is the differences for each observation. For each table, I've changed the column titles to be examples of what sort of data you would collect in a repeated measurements study, so you can use those examples to think about the data."),
    br(),
    br(),
    br(),
    fluidRow(column(6, align="center",
              tableOutput("datatab")),
             column(6, align="center",
                    tableOutput("datatab1")))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dist1 <-  reactiveValues(x = rnorm(n = 100, mean = 22250, sd = 25), Distribution = "Dist 1")
    dist2 <-  reactiveValues(x = rnorm(n = 100, mean = 22252, sd = 25), Distribution = "Dist 2")
    
    observeEvent(input$guess, {
        # generate data based on input$sd1 from ui.R
        dist1$x <- rnorm(n = 100, mean = 22250, sd = input$sd1)
        #     dist2 <- data.frame(x=rnorm(n = 100, mean = input$m2, sd = input$sd2), Distribution = "Dist 2")
        samp1 <- rnorm(n = 100, mean = input$m1, sd =input$sd1)
        dist2$x <- dist1$x +samp1
    })#end of observe code 
    
    output$distPlot <- renderPlot({
        difference = data.frame(difference = dist2$x - dist1$x)
        
        dist2$difference = dist2$x - dist1$x
        together <- rbind(data.frame(x=dist1$x, Distribution =dist1$Distribution), data.frame(x=dist2$x, Distribution =dist2$Distribution)) 
        
        p1 <- ggplot(together, aes(x = x, fill = Distribution))+
            geom_density(alpha=.5)+
            ggtitle("The Two Distributions Together")
        p2 <- ggplot(difference, aes(x = difference))+
            geom_density(alpha=.5)+
            ggtitle("Difference Between Observations")+
            geom_vline(xintercept = 0)
        grid.arrange(p1,p2,nrow=1)
        
    })
    
    output$text <- renderText({
        paired.p <- t.test(dist1$x, dist2$x, paired=TRUE)$p.value
        regular.p <- t.test(dist1$x, dist2$x)$p.value
        
        title <- paste("A two-sample t-test of this data has a p-value of ",regular.p, ",\n and a paired t-test of this data has a p-value of ", paired.p, sep = "")    
    })
    
    output$ptable <- renderTable({
        paired.p <- t.test(dist1$x, dist2$x, paired=TRUE)$p.value
        regular.p <- t.test(dist1$x, dist2$x)$p.value
        
        data.frame(Test = c("A two-sample t-test of this data has a p-value of:", "A paired t-test of this data has a p-value of:"), `p value` =c(regular.p, paired.p))
        
    }, digits=20)
    
    
    output$datatab <- renderTable({
        data.frame(`Person's Strength Originally` = head(dist1$x), `How Strong They Are After Protein Powder` = head(dist2$x), Difference = head(dist2$x)-head(dist1$x), check.names=FALSE)
        
    }, digits=3)
    
    output$datatab1 <- renderTable({
        data.frame(`How Much a Female Elephant Weighs` = head(dist1$x), `How Much They Weigh When Pregnant ` = head(dist2$x), Difference = head(dist2$x)-head(dist1$x), check.names=FALSE)
        
    }, digits=3)
    
    
    
}




# Run the application 
shinyApp(ui = ui, server = server)
