library(shiny)
library(ggplot2)
library(shinythemes)
library(tidyverse)

ui <- bootstrapPage(
  navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE,
    "Corvinus Statisztika", id = "nav",
    navbarMenu(
      "Két és több független mintás paraméteres próbák",
      tabPanel(
        "Két várható érték különbségére",
        tags$head(includeCSS("styles.css")),
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Kétmintás t és Z-próba")), style = "color:#045a8d"),
            selectInput(
              inputId = "test_type",
              label = "Próbafüggvény:",
              choices = c("t", "z"),
              selected = "t"
            ),
            selectInput(
              inputId = "test_side",
              label = "Hipotézis megfogalmazása: (jobboldali esetén H1: X-Y eltérés nagyobb, mint a hipozésiben szereplő érték)",
              choices = c("jobboldali", "baloldali", "kétoldali"),
              selected = "jobboldali"
            ),
            numericInput(
              "mu",
              "Feltételezett eltérés: ",
              value = 0
            ),
            numericInput(
              "n_x",
              "x minta elemszáma:",
              min = 1,
              value = 1
            ),
            numericInput(
              "n_y",
              "y minta elemszáma:",
              min = 1,
              value = 1
            ),
            numericInput(
              "mean_x",
              "x minta átlaga:",
              value = 0
            ),
            numericInput(
              "mean_y",
              "y minta átlaga:",
              value = 0
            ),
            numericInput(
              "sigma_x",
              "x minta szórása:",
              min = 0,
              value = 0
            ),
            numericInput(
              "sigma_y",
              "y minta szórása:",
              min = 0,
              value = 0
            ),

            checkboxInput("TF_confidence", "Konfidencia intervallumokat mutat", value = F),
            checkboxInput("TF_Rho_show", "Feltételezett eltérést mutat", value = F)
          ),

          mainPanel(
            plotOutput("plot1")
          )
        )
      ),
      tabPanel(
        "Két szórás összehasonlítása",
        tags$head(includeCSS("styles.css")),
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Két szórás összehasonlítása F-próbával")), style = "color:#045a8d"),
            selectInput(
              inputId = "test_side",
              label = "Hipotézis megfogalmazása: (jobboldali esetén H1: X szórása nagyobb, mint Y szórása)",
              choices = c("jobboldali", "baloldali", "kétoldali"),
              selected = "kétoldali"
            ),
            numericInput(
              "n_x_4_2",
              "x minta elemszáma:",
              min = 1,
              value = 10
            ),
            numericInput(
              "n_y_4_2",
              "y minta elemszáma:",
              min = 1,
              value = 10
            ),
            numericInput(
              "sigma_x_4_2",
              "x minta szórása:",
              min = 0,
              value = 0
            ),
            numericInput(
              "sigma_y_4_2",
              "y minta szórása:",
              min = 0,
              value = 0
            ),
            sliderInput(
              "alpha_4_2",
              "Szignifikanciaszint (%):",
              min = 0,
              max = 20,
              value = 5
            ),
            textOutput("var_test")
          ),

          mainPanel(
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent({
    
    input$test_type
    input$test_side
    input$mu
    input$n_x
    input$n_y
    input$mean_x
    input$mean_y
    input$sigma_x
    input$sigma_y
    input$TF_confidence
    input$TF_Rho_show
    
    input$test_side_4_2
    input$n_x_4_2
    input$n_y_4_2
    input$sigma_x_4_2
    input$sigma_y_4_2
    input$alpha_4_2
    
  }, {
  output$plot1 <- renderPlot(
    {
      #---
      test_type <- input$test_type
      test_side <- input$test_side
      mu <- input$mu
      n_x <- input$n_x
      n_y <- input$n_y
      mean_x <- input$mean_x
      mean_y <- input$mean_y
      sigma_x <- input$sigma_x
      sigma_y <- input$sigma_y
      TF_confidence <- input$TF_confidence
      TF_Rho_show <- input$TF_Rho_show

      A <- rnorm(n_x, mean = mean_x, sd = sigma_x)
      B <- rnorm(n_y, mean = mean_y, sd = sigma_y)

      if (n_x > 1 & sigma_x > 0) {
        A <- A * (sigma_x / var(A)^0.5)
      }

      if (n_y > 1 & sigma_y > 0) {
        B <- B * (sigma_y / var(B)^0.5)
      }

      A <- A + (mean_x - mean(A))
      B <- B + (mean_y - mean(B))

      test_side <- ifelse(test_side == "jobboldali", "greater", test_side)
      test_side <- ifelse(test_side == "baloldali", "less", test_side)
      test_side <- ifelse(test_side == "kétoldali", "two.sided", test_side)

      plot_title <- ""
      try(
        {
          if (test_type == "t") {
            pr_f <- t.test(A, B, alternative = test_side, mu = mu)
          }
          if (test_type == "z") {
            pr_f <- BSDA::z.test(A, B, alternative = test_side, mu = mu, sigma.x = var(A)^0.5, sigma.y = var(B)^0.5)
          }
          plot_title <- paste(
            "Próbafüggvény (",
            test_type,
            ") = ",
            round(pr_f$statistic, 4),
            "             p-érték =  ",
            scales::percent(pr_f$p.value),
            sep = ""
          )
        },
        silent = T
      )
      y_A <- 53
      if (n_x > 1) {
        y_A <- seq(A) * 95 / length(A)
      }
      y_B <- 47
      if (n_y > 1) {
        y_B <- seq(B) * 105 / n_y
      }

      p <- ggplot()
      if (TF_confidence) {
        if (n_y > 1 & sigma_y > 0) {
          p <- p + geom_ribbon(aes(
            x =
              c(
                ifelse(test_type == "t", mean(B) - (qt(0.975, df = n_y - 1) * var(B)^0.5 / sqrt(n_y)), mean(B) - (qnorm(0.975) * var(B)^0.5 / sqrt(n_y))),
                ifelse(test_type == "t", mean(B) + (qt(0.975, df = n_y - 1) * var(B)^0.5 / sqrt(n_y)), mean(B) + (qnorm(0.975) * var(B)^0.5 / sqrt(n_y)))
              ),
            ymin = -Inf,
            ymax = Inf
          ), fill = "#00A3AB", alpha = 0.1)
        }

        if (n_x > 1 & sigma_x > 0) {
          p <- p + geom_ribbon(aes(
            x =
              c(
                ifelse(test_type == "t", mean(A) - (qt(0.975, df = n_x - 1) * var(A)^0.5 / sqrt(n_x)), mean(A) - (qnorm(0.975) * var(A)^0.5 / sqrt(n_x))),
                ifelse(test_type == "t", mean(A) + (qt(0.975, df = n_x - 1) * var(A)^0.5 / sqrt(n_x)), mean(A) + (qnorm(0.975) * var(A)^0.5 / sqrt(n_x)))
              ),
            ymin = -Inf,
            ymax = Inf
          ), fill = "#FF5B6B", alpha = 0.1) +
            geom_ribbon(aes(
              x = c(mean(A), mean(A)),
              ymin = -Inf,
              ymax = -Inf,
              fill = "95%-os konfidencia intervallum"
            ), alpha = 0.1, color = "black") +
            scale_fill_manual(values = c("95%-os konfidencia intervallum" = "#FF5B6B"))
        }

        if ((n_x == 1 | sigma_x == 0) & n_y > 1 & sigma_y > 0) {
          p <- p + geom_ribbon(aes(
            x = c(mean(A), mean(A)),
            ymin = -Inf,
            ymax = -Inf,
            fill = "95%-os konfidencia intervallum"
          ), alpha = 0.1, color = "black") +
            scale_fill_manual(values = c("95%-os konfidencia intervallum" = "#00A3AB"))
        }
      }

      if (mean(A) != mean(B)) {
        p <- p + geom_vline(xintercept = mean(A), color = "#FF5B6B", linetype = "dotted", size = 1.3) +
          geom_vline(xintercept = mean(B), color = "#00A3AB", linetype = "dotted", size = 1.3)
      } else {
        p <- p + geom_vline(xintercept = mean(A), color = "#92D050", linetype = "dotted", size = 1.3)
      }

      p <- p + geom_point(aes(x = A, y = y_A),
        fill = "#FF5B6B", size = 4,
        shape = 21, color = "black", stroke = 2, alpha = 0.7
      ) +
        geom_point(aes(x = B, y = y_B),
          fill = "#00A3AB", size = 4,
          shape = 21, color = "black", stroke = 2, alpha = 0.7
        ) +
        xlab("") +
        ylab("") +
        labs(title = plot_title) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          plot.title = element_text(size = 25, hjust = 0.5),
          axis.text.x = element_text(size = 20),
          legend.position = "bottom",
          legend.justification = 0.9,
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          plot.margin = margin(30, 30, 30, 30, "pt")
        )

      if (TF_Rho_show) {
        p <- p + geom_line(aes(
          x = c(
            mean(c(mean(A), mean(B))) - mu / 2,
            mean(c(mean(A), mean(B))) + mu / 2
          ),
          y = c(50, 50),
          color = "Feltételezett eltérés"
        ), size = 3) +
          scale_color_manual(values = c("Feltételezett eltérés" = "#FFC730"))
      }
      p

      #---      
    },
    width = 1000,
    height = 1000
  )

  output$var_test <- renderText({
    test_side_4_2 <- input$test_side_4_2
    n_x_4_2 <- input$n_x_4_2
    n_y_4_2 <- input$n_y_4_2
    sigma_x_4_2 <- input$sigma_x_4_2
    sigma_y_4_2 <- input$sigma_y_4_2
    alpha_4_2 <- input$alpha_4_2

    test_side_4_2 <- ifelse(test_side_4_2 == "jobboldali", "greater", test_side_4_2)
    test_side_4_2 <- ifelse(test_side_4_2 == "baloldali", "less", test_side_4_2)
    test_side_4_2 <- ifelse(test_side_4_2 == "kétoldali", "two.sided", test_side_4_2)

    x_4_2 <- rnorm(n_x_4_2, mean = 0, sd = sigma_x_4_2)
    y_4_2 <- rnorm(n_y_4_2, mean = 0, sd = sigma_y_4_2)

    if (var(x_4_2) > 0) {
      x_4_2 <- x_4_2 * (sigma_x_4_2 / var(x_4_2)^0.5)
    }
    if (var(y_4_2) > 0) {
      y_4_2 <- y_4_2 * (sigma_y_4_2 / var(y_4_2)^0.5)
    }

    dontes_4_2 <- "nincs"
    try(
      {
    test_4_2 <- var.test(x_4_2, y_4_2, alternative = test_side_4_2)
        if (test_4_2$p.value > alpha_4_2 / 100) {
          if (test_side_4_2 == "two.sided") {
            dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elfogadjuk, a két minta szórása megegyezik."
          }
          if (test_side_4_2 == "greater") {
            dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elfogadjuk, x minta szórása nem nagyobb, mint y minta szórása."
          }
          if (test_side_4_2 == "less") {
            dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elfogadjuk, y minta szórása nem nagyobb, mint x minta szórása."
          }
        } else {
          if (test_side_4_2 == "two.sided") {
            dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elutasítjuk, a két minta szórása nem egyezik meg."
          }
          if (test_side_4_2 == "greater") {
            dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elutasítjuk, x minta szórása nagyobb, mint y minta szórása."
          }
          if (test_side_4_2 == "less") {
            dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elutasítjuk, y minta szórása nagyobb, mint x minta szórása."
          }
        }
      },
      silent = T
    )

    if (dontes_4_2 == "nincs") {
      final_text_4_2 <- "Nem lehet a próbát elvégezni"
    }  
    if (dontes_4_2 != "nincs") {
      final_text_4_2 <- paste(
        "Próbafüggvény (F) = ",
        round(test_4_2$statistic, digits = 4),
        "  P-érték = ",
        scales::percent(
          test_4_2$p.value
        ),
        "  Döntés: ",
        alpha_4_2,
        dontes_4_2,
        sep = ""
      )
    }
    final_text_4_2
  })
  })
}


shinyApp(ui, server)
