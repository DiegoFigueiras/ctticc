library(psych)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)

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
      labs(y = "p(1.0)", x = "") +
      theme_minimal(base_family = "Arial", base_size = 14) +
      theme(panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            panel.grid.major = element_line(color = "gray"),
            panel.grid.minor = element_line(color = "gray"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            legend.background = element_rect(fill = "black"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"))

    q <- ggplotly(p, tooltip = c("colour"))
    return(q)
  }

  return(NULL)
}




cttiif <- function(data, items, plot="together") {
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

  if (plot == "together") {
    fun <- function(x, PseudoA, PseudoB) {
      (PseudoA^2) * (1/(1 + exp(-PseudoA*(x - PseudoB)))) * (1-(1/(1 + exp(-PseudoA*(x - PseudoB)))))

    }


    df_selected <- df[items, ]
    p <- df_selected %>%
      crossing(x = seq(-4, 4, .1)) %>%
      mutate(y = fun(x, PseudoA, PseudoB)) %>%
      ggplot(aes(x, y, color = inum)) +
      ylim(0, 1) +
      geom_line(linewidth = 1.25) +
      scale_x_continuous(limits = c(-4, 4), labels = c("Low Test Score", "", "Average Test Score", "", "High Test Score")) +
      labs(y = "p(1.0)", x = "") +
      theme_minimal(base_family = "Arial", base_size = 14) +
      theme(panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            panel.grid.major = element_line(color = "gray"),
            panel.grid.minor = element_line(color = "gray"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            legend.background = element_rect(fill = "black"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"))

    q <- ggplotly(p, tooltip = c("colour"))
    return(q)
  }

  return(NULL)
}


ctttif <- function(data, items, plot="together") {
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

  if (plot == "together") {
    fun <- function(x, PseudoA, PseudoB) {
      (PseudoA^2) * (1/(1 + exp(-PseudoA*(x - PseudoB)))) * (1-(1/(1 + exp(-PseudoA*(x - PseudoB)))))

    }


    df_selected <- df[items, ]
    p <- df_selected %>%
      crossing(x = seq(-4, 4, .1)) %>%
      mutate(y = fun(x, PseudoA, PseudoB)) %>%
      group_by(x)%>%
      summarise(y = sum(y))%>%
      ggplot(aes(x, y)) +
      ylim(0, 1.5) +
      geom_line(linewidth = 1.25, color="blue") +
      scale_x_continuous(limits = c(-4, 4), labels = c("Low Test Score", "", "Average Test Score", "", "High Test Score")) +
      labs(y = "p(1.0)", x = "") +
      theme_minimal(base_family = "Arial", base_size = 14) +
      theme(panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            panel.grid.major = element_line(color = "gray"),
            panel.grid.minor = element_line(color = "gray"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            legend.background = element_rect(fill = "black"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"))

    q <- ggplotly(p, tooltip = c("colour"))
    return(q)
  }

  return(NULL)
}



ui <- dashboardPage(
  dashboardHeader(title = "Item Characteristic Curve Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      fileInput("file", "Upload CSV File", accept = ".csv"),
      actionButton("deselect_all", "Deselect All"),
      checkboxGroupInput("items", "Select Items", choices = NULL, inline=FALSE),
      valueBoxOutput("numItems", width = 12),
      p("Make sure your data is structured such that each column is an item in your assessment and each row a respondent. Scores should be binary, 1 and 0."),
      p("The Item Characteristic Curves are replotted each time you select or de-select an item. User may therefore be interested in gaining visual feedback of item functioning within unique sets of items. When developing subtests this tool should be considered beneficial for making item retention or deletion decisions at the subtest level."),
      p(withMathJax(includeMarkdown("$I_i(\\theta)=a^{2}_iP_i(\\theta)Q_i(\\theta)$"))),
      p(withMathJax(includeMarkdown("where: $a_i$ is the discrimination paramter for item $i$:"))),
      p(withMathJax(includeMarkdown("$P_i(\\theta)=1/(1+EXP(-a_i(\\theta-b_i))),$"))),
      p(withMathJax(includeMarkdown("$Q_i(\\theta)=1-P_i(\\theta),$"))),
      p(withMathJax(includeMarkdown("$\\theta$ is the ability level of interest.")))
    )
  ),
  dashboardBody(
    fluidRow(
      plotlyOutput('plot1')
    ),
    fluidRow(
      column(
        width=6,
        plotlyOutput('tif'),
      ),
      column(
        width=6,
        plotlyOutput('iif')
      )


    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  observe({
    req(data())
    updateCheckboxGroupInput(session, "items", choices = colnames(data()), selected = colnames(data()), inline = TRUE)
  })

  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "items", choices = colnames(data()), selected = character(0), inline=TRUE)
  })

  selectedData <- reactive({
    req(data())
    data()[, input$items, drop = FALSE]
  })

  output$numItems <- renderValueBox({
    req(data())
    num_items <- ncol(selectedData())
    valueBox(
      value = num_items,
      subtitle = "Number of Items",
      icon = icon("list"),
      color = "blue"
    )
  })

  output$plot1 <- renderPlotly({
    req(selectedData())
    ctticc(selectedData(), items = input$items, plot = "together")
  })

  output$tif <- renderPlotly({
    req(selectedData())
    ctttif(selectedData(), items = input$items, plot = "together")
  })

  output$iif <- renderPlotly({
    req(selectedData())
    cttiif(selectedData(), items = input$items, plot = "together")
  })
}

shinyApp(ui = ui, server = server)


# data<-read.csv("testdata2.csv")
# cttiif(data)
# ctticc(data)
#
#
# ctttif(data, plot="together")
#
#
#
# library(ctticc)
# boo<-ctticc::ctticc(data)
