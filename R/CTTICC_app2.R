library(psych)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(plotly)
library(shiny)

ctticc <- function(data, items, plot="together", nrow=2, ncol=3) {
  pseudob <- data.frame(qnorm(colMeans(data, na.rm=TRUE)))*-1
  ahat <- function(x) {
    r <- (((2.71828)^x)-(1/(2.71828)^x))/(2.71828-(2.71828)^x)
    ((((0.51+(0.02*abs(pseudob))+(0.301*pseudob^2))*x)+((0.57-(0.009*abs(pseudob))+(0.19*pseudob^2))*r))*1.71633)
  }

  alphas <- psych::alpha(data, check.keys = FALSE)
  citcs <- data.frame(alphas$item.stats$r.drop)
  pseudoA <- data.frame(ahat(citcs))
  pseudoB <- -0.000002895614 + (1.535589 * pseudob)
  df <- as.data.frame(cbind(citcs, pseudoA, pseudoB))
  colnames(df) <- c("CITC", "PseudoA", "PseudoB")
  c <- 0
  df$inum <- row.names(df)

  eq <- function(x, pseudoa, pseudob) {
    c + ((1-c)*(1/(1+2.71828^(-1.7*(pseudoa*(x-pseudob))))))
  }

  if (plot == "together") {
    fun <- function(x, PseudoA, PseudoB) {
      (1 / (1 + 2.71828^(-1.7 * (PseudoA * (x - PseudoB)))))
    }

    df_selected <- df[items, ]
    p <- df_selected %>%
      crossing(x = seq(-4, 4, .1)) %>%
      mutate(y = fun(x, PseudoA, PseudoB)) %>%
      ggplot(aes(x, y, color = inum)) +
      ylim(0, 1) +
      geom_line(linewidth = 1.25) +
      scale_x_continuous(limits = c(-4, 4), labels = c("Low Test Score", "", "Average Test Score", "", "High Test Score")) +
      labs(y = "p(1.0)", x = "")

    q <- ggplotly(p, tooltip = c("colour"))
    return(q)
  }

  return(NULL)
}

as.data.frame(data(testdata))
# data<-data[1:20,1:5]
# colnames(data) <- paste0("Item", 1:5)

ui <- fluidPage(
  fluidRow(
    sidebarPanel(
      checkboxGroupInput("items", "Select Items", choices = colnames(data), selected = colnames(data))
    ),
    mainPanel(
      plotlyOutput('plot1')
    )
  )
)

server <- function(input, output) {
  selectedData <- reactive({
    data[, input$items, drop = FALSE]
  })

  output$plot1 <- renderPlotly({
    # req(input$items)
    # items_indices <- match(input$items, colnames(data))
    ctticc(selectedData(), plot = "together")
  })
}

shinyApp(ui = ui, server = server)
