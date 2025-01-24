# install.packages("psych")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("tidyverse")
# install.packages("plotly")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("shinydashboard")
library(psych)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(markdown)
library(downloadthis)

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
      labs(y = "p(1.0)", x = "", color = "Item") +
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
      #ylim(0, 1) +
      geom_line(linewidth = 1.25) +
      scale_x_continuous(limits = c(-4, 4), labels = c("Low Test Score", "", "Average Test Score", "", "High Test Score")) +
      labs(y = "p(1.0)", x = "", color = "Item") +
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
      #    scale_y_continuous(limits = function(lim){c(0,lim[1]+2)}) +
      #    ylim(0, 1.5) +
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
  dashboardHeader(title = "Item Characteristic Curve Dashboard",
                  titleWidth=400,
                  tags$li(a(href = 'https://github.com/DiegoFigueiras/ctticc',
                            icon("github"),
                            title = "Go to GitHub Repo"),
                          class = "dropdown")),
  dashboardSidebar(
    width=350,
    tags$style(".align_class {
                font-size: 1em;
                margin-left: 10px;
                margin-right: 1em !important;}
               .main-sidebar{
                        width:350px;
                        text-wrap: wrap;
                        border: 3px solid #ffffff;}"),
    sidebarMenu(
      br(),
      div(class='align_class',      "Make sure your data is structured such that", br(),
          "each column is an item in your assessment and", br(),
          "each row represents one respondent.", br(),
          br(),
          "Scores should be binary:",br(),
          "1= correct answer.",br(),
          "0= incorrect answer", br(),
          br(),
          br()),
      menuItem("Data Controls:", tabName = "dashboard", icon = icon("dashboard")),
      fileInput("file", "Upload CSV File", accept = ".csv"),
      actionButton("deselect_all", "Deselect All"),
      checkboxGroupInput("items", "Select Items", choices = NULL, inline=FALSE),
      valueBoxOutput("numItems", width = 6),
      valueBoxOutput("numExcl", width = 6),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      div(class='align_class',  "When developing subtests this tool should be", br(),
          "considered beneficial for making item retention", br(),
          "or deletion decisions at the subtest level.",br(),

          br(),
          "The Item Characteristic Curves are replotted", br(),
          "each time the user select or de-select an item.", br(),
          br(),
          "The formulas for the TIF and IFF are below:", br(),
          p(withMathJax(includeMarkdown(" $I_i(\\theta)=a^{2}_iP_i(\\theta)Q_i(\\theta)$"))),
          p(withMathJax(includeMarkdown(" where: $a_i$ is the discrimination paramter for item $i$:"))),
          p(withMathJax(includeMarkdown(" $P_i(\\theta)=1/(1+EXP(-a_i(\\theta-b_i))),$"))),
          p(withMathJax(includeMarkdown(" $Q_i(\\theta)=1-P_i(\\theta),$"))),
          p(withMathJax(includeMarkdown(" $\\theta$ is the ability level of interest."))),
          br(),
          "The formulas for the ICCs can be found here:", br(),
          br(),
          br(),
          download_link(
            link = "https://diegofigueiras.github.io/ctticc/ICC_project.pdf",
            button_label = "ICCs formulas paper",
            button_type = "default",
            has_icon = TRUE,
            icon = "fa fa-print",
            self_contained = FALSE
          )

      )
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title="Item Characteristic Curves",
        solidHeader = TRUE,
        collapsible = TRUE,
        background = "black",
        width=12,
        plotlyOutput('plot1'))
    ),
    fluidRow(
      box(title = "Test Information Function:",
          solidHeader = TRUE,
          collapsible = TRUE,
          background="black",
          plotlyOutput('tif')),
      box(title = "Item Information Functions:",
          solidHeader = TRUE,
          collapsible = TRUE,
          background="black",
          plotlyOutput('iif'))


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
    # Filter columns with non-zero variance
    valid_columns <- colnames(data())[sapply(data(), function(col) sd(col, na.rm = TRUE) > 0)]
    updateCheckboxGroupInput(session, "items", choices = valid_columns, selected = valid_columns, inline = TRUE)
  })

  observeEvent(input$deselect_all, {
    req(data())
    valid_columns <- colnames(data())[sapply(data(), function(col) sd(col, na.rm = TRUE) > 0)]
    updateCheckboxGroupInput(session, "items", choices = valid_columns, selected = character(0), inline = TRUE)
  })

  selectedData <- reactive({
    req(data())
    valid_columns <- colnames(data())[sapply(data(), function(col) sd(col, na.rm = TRUE) > 0)]
    data()[, intersect(input$items, valid_columns), drop = FALSE]
  })

  output$numItems <- renderValueBox({
    req(selectedData())
    num_items <- ncol(selectedData())
    valueBox(
      value = num_items,
      subtitle = "Included Items",
      color = "blue"
    )
  })


  output$numExcl <- renderValueBox({
    req(selectedData())
    req(data())
    num_excl <- ncol(data()) - ncol(selectedData())
    valueBox(
      value = num_excl,
      subtitle = "Excluded Items",
      color = "red"
    )
  })

  output$plot1 <- renderPlotly({
    req(selectedData())
    ctticc(selectedData(), items = colnames(selectedData()), plot = "together")
  })

  output$tif <- renderPlotly({
    req(selectedData())
    ctttif(selectedData(), items = colnames(selectedData()), plot = "together")
  })

  output$iif <- renderPlotly({
    req(selectedData())
    cttiif(selectedData(), items = colnames(selectedData()), plot = "together")
  })
}



shinyApp(ui = ui, server = server)
