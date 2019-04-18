library(shiny)
library(ggplot2)
library(reshape2)
library(tidyr)


ui <- fluidPage(
  
  titlePanel("Amount calculator"),
  
  fluidRow(
    
    column(4,
           sliderInput(inputId = "initial",
                       label = "Initial Amount",
                       min = 1, max = 100000, value = 1000, step = 500,
                       pre = '$'),
           sliderInput(inputId = "contribution",
                       label = "Annual Contribution",
                       min = 0, max = 50000, value = 2000, step = 500,
                       pre = '$')
    ),
    
    column(4,
           sliderInput(inputId = "return",
                       label = "Return Rate (in %)",
                       min = 0, max = 20, value = 5, step = 0.1),
           sliderInput(inputId = "growth",
                       label = "Growth Rate (in %)",
                       min = 0, max = 20, value = 2, step = 0.1)
    ),
    
    column(4,
           sliderInput(inputId = "year",
                       label = "Years",
                       min = 0, max = 50, value = 20, step = 1),
           selectInput(inputId = "facet",
                       label = "Facet?",
                       choices = c("Yes", "No"), selected = "No")
    )
  ),
  
  hr(),
  
  h3(strong("Timelines")),
  plotOutput("Balanceplot"),
  
  h3(strong("Balances")),
  verbatimTextOutput("table")
)


server <- function(input, output) {
  modalities <- reactive( {
    
    future_value <- function(amount,rate,years) {
      fv <- amount * (1 + rate)^years
      return(fv)
    }
    annuity <- function(contrib, rate, years) {
      FVA <- contrib * {(1+rate)^years - 1} / rate
      return(FVA)
    }
    growing_annuity <- function (contrib, rate, growth, years) {
      FVGA <- contrib * {(1 + rate)^years - (1 + growth)^years} / (rate-growth)
      return(FVGA)
    }
    
    n <- input$initial
    a <- input$contribution
    
    
    fv1 <- vector()
    fv2 <- vector()
    fv3 <- vector()
    
    for (i in 0:input$year) {
      term1 <- future_value(n, input$return / 100, i)
      fv1[i + 1] <- term1
      print(term1)
      
      term2 <- future_value(n, input$return / 100, i) + annuity(a, input$return / 100, i)
      fv2[i + 1] <- term2
      print(term2)
      
      term3 <- future_value(n, input$return / 100, i) + growing_annuity(a, input$return / 100, input$growth / 100, i)
      fv3[i + 1] <- term3
      print(term3)
      
    }
    modalities <- data.frame(
      year = 0:input$year,
      no_contrib = fv1,
      fixed_contrib = fv2,
      growing_contrib = fv3
    )
    
    return(modalities)
  })
  
  
  output$Balanceplot <- renderPlot({
    
    modalities <- gather(modalities(), key = modality, 
                         value = value, no_contrib, fixed_contrib, growing_contrib, factor_key = TRUE)
    colnames(modalities) <- c("year", "modality", "value")
    
    graph <- ggplot(data = modalities, aes(x = year, y = value, col = modality))+
      geom_line()+
      geom_point()+labs(x = "Year", y = "Value", title = "Three modes of investing") 
    
    
    if(input$facet == "Yes") {
    graph <- graph + geom_area(aes(fill = modality), alpha = 0.7, show.legend = TRUE) +
        facet_wrap( ~ modality) + theme_bw()
      
      
    }
    return(graph) 
    
  })
  
  output$table <- renderPrint({modalities()})
}

# Run the application 
shinyApp(ui = ui, server = server)

