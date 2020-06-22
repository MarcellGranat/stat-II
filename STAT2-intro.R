library(shiny)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(ggpubr)

ui <- bootstrapPage(
  navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE,
    "Corvinus Statisztika", id = "nav",
    navbarMenu(
      "Bevezetés",
# ui: 1_1 = conf intervall ------------------------------------------------
      tabPanel(
        "Konfidencia intervallum",
        tags$head(includeCSS("styles.css")),
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Konfidencia intervallumba esés ismételt mintavétel esetén")), style = "color:#045a8d"),
            numericInput(
              "mu_1_1",
              "Várható érték",
              value = 0
            ),
            numericInput(
              "sigma_1_1",
              "Szórás",
              value = 1,
              min = 0
            ),
            numericInput(
              "N_1_1",
              "Sokasság elemszáma",
              value = 1000,
              min = 0
            ),
            numericInput(
              "m_1_1",
              "Mintavétel száma",
              value = 100,
              min = 0
            ),
            numericInput(
              "n_1_1",
              "Minták elemszáma",
              value = 10,
              min = 0
            ),
            sliderInput(
              "alpha_1_1",
              "Szignifikanciaszint (%):",
              min = 1,
              max = 20,
              value = 5
            ),
            actionButton(inputId = "reload_1_1", label = "", icon("refresh"))
          ),

          mainPanel(
            tabsetPanel(
              tabPanel("Ábra", plotOutput("plot_1_1")),
              tabPanel(
                "Eredmény", verbatimTextOutput("answer_1_1"),
                tags$head(tags$style("#answer_1_1{color:black; font-size:40px;font-family: 'Helvetica Neue', Helvetica;}"))
              )
            )
          )
        )
      )
    ),
    navbarMenu(
      "Becslési eljárások",
# ui: 2_1 = Prop estimate -----------------------------------------------------------------
      tabPanel(
        "Arány",
        tags$head(includeCSS("styles.css")),
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Sokassági arány becslése (FAE)")), style = "color:#045a8d"),
            numericInput("p_2_1", "Mintabeli arány (%)", min = 0, max = 100, value = 50),
            numericInput("n_2_1", "Mintanagyság", min = 0, value = 100),
            sliderInput(
              "alpha_2_1",
              "Szignifikanciaszint (%):",
              min = 1,
              max = 20,
              value = 5
            )
          ),
          mainPanel(
            verbatimTextOutput("answer_2_1")
          )
        )
      ),
# ui: 2_2 = var estimate -----------------------------------------------------------------
      tabPanel(
        "Variancia",
        tags$head(includeCSS("styles.css")),
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Variancia becslése FAE mintából")), style = "color:#045a8d"),
            numericInput("ssd_2_2", "Mintabeli szórás", min = 0, value = 1),
            numericInput("n_2_2", "Mintanagyság", min = 0, value = 100),
            sliderInput(
              "alpha_2_2",
              "Szignifikanciaszint (%):",
              min = 1,
              max = 20,
              value = 5
            )
          ),
          mainPanel(
            verbatimTextOutput("answer_2_2")
          )
        )
      )
    ),
    navbarMenu(
      "Két és több független mintás paraméteres próbák",

# ui: 3_1 = 2 sample mean -------------------------------------------------
      tabPanel(
        "Két várható érték különbségére",
        tags$head(includeCSS("styles.css")),
        sidebarLayout(
          sidebarPanel(
            span(tags$i(h6("Kétmintás t és Z-próba")), style = "color:#045a8d"),
            selectInput("type", "Bevitel típusa", c("csúszka", "normál bevitel"), selected = "csúszka"),
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
            uiOutput("mu"),
            uiOutput("n_x"),
            uiOutput("n_y"),
            uiOutput("mean_x"),
            uiOutput("mean_y"),
            uiOutput("sigma_x"),
            uiOutput("sigma_y"),
            sliderInput(
              "alpha",
              "Szignifikanciaszint (%):",
              min = 1,
              max = 20,
              value = 5
            ),
            checkboxInput("TF_confidence", "Konfidencia intervallumokat mutat", value = F),
            checkboxInput("TF_Rho_show", "Feltételezett eltérést mutat", value = F),
            actionButton(inputId = "reload", label = "", icon("refresh"))
          ),

          mainPanel(
            tabsetPanel(
              tabPanel("Ábra", plotOutput("plot1")),
              tabPanel(
                "Döntés", verbatimTextOutput("ketmintas_elteres_answer"),
                tags$head(tags$style("#ketmintas_elteres_answer{color:black; font-size:40px;font-family: 'Helvetica Neue', Helvetica;}"))
              )
            )
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
              inputId = "test_side_4_2",
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
              min = 1,
              max = 20,
              value = 5
            )
          ),

          mainPanel(
            verbatimTextOutput("var_test")
          )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # Server: 1_1 -------------------------------------------------------------
mat_1_1 <- reactive({
  input$reload_1_1
  
  N <- input$N_1_1
  n <- input$n_1_1
  sigma <- input$sigma_1_1
  m <- input$m_1_1
  alpha <- input$alpha_1_1
  mu <- input$mu_1_1

  pop <- rnorm(n = N, mean = mu, sd=sigma)
  if (N > 1 & sigma>0) {
    pop <- pop/var(pop)^0.5*sigma
  }
  pop <- pop - mean(pop) + mu
  mat <- matrix(nrow = n, ncol = m)
  for (i in seq(m)) {
    mat[,i] <- sample(pop, size = n)
  }
  mat
})

output$plot_1_1 <- renderPlot({
  N <- input$N_1_1
  n <- input$n_1_1
  sigma <- input$sigma_1_1
  m <- input$m_1_1
  alpha <- input$alpha_1_1
  mu <- input$mu_1_1
  mat <- mat_1_1()
  
  mat %>% data.frame() %>% gather() %>% group_by(key) %>% summarise(mu = mean(value), sigma = sd(value)) %>% 
    mutate(
      conf_min = mu - qnorm(1-alpha/200)*sigma/n^0.5,
      conf_max = mu + qnorm(1-alpha/200)*sigma/n^0.5
    ) %>% select(key, conf_min, conf_max) %>% 
    rename(y = key) %>% 
    gather(key = "key", value = "value", -y) %>% 
    ggplot() +
    geom_line(aes(y=y, x=value, color = 'Konfidencia-intervallum'), alpha = .5, size=5) +
    geom_vline(aes(xintercept = mu, color = "Sokassági átlag"), size = 1.6) +
    geom_point(data = mat %>% data.frame() %>% gather(), aes(x=value, y = key), size = .9) +
    geom_point(data = mat %>% data.frame() %>% gather() %>% group_by(key) %>% summarise(value = mean(value)),
               aes(x=value, y = key, color = "Adott minta átlaga"), size = 2) +
    scale_color_manual(values = c("Sokassági átlag" = '#FFC730', 'Konfidencia-intervallum' = '#FF5B6B',
                                  "Adott minta átlaga" = "#52CCBF")) +
    labs(x = "", y="Minták") +
    theme_minimal() + theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.text.y = element_blank(),
      axis.title = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
},
width = 1300,
height = 1300)
  
output$answer_1_1 <- renderText({
  N <- input$N_1_1
  n <- input$n_1_1
  sigma <- input$sigma_1_1
  m <- input$m_1_1
  alpha <- input$alpha_1_1
  mu <- input$mu_1_1
  mat <- mat_1_1()
  
  n_outofconf <- mat %>% data.frame() %>% gather() %>% group_by(key) %>% summarise(mu = mean(value), sigma = sd(value)) %>% 
    mutate(
      conf_min = mu - qnorm(1-alpha/200)*sigma/n^0.5,
      conf_max = mu + qnorm(1-alpha/200)*sigma/n^0.5
    ) %>% select(-c(mu, sigma)) %>% mutate(
      inconf = ifelse(conf_min < mu & conf_max > mu, 0, 1)
    ) %>% pull() %>% sum()
  str_c(
    "A(z) ",m, " db mintavétel során összesen ", n_outofconf, " alkalommal volt\na sokassági átlag a mintából számított konfidencia-intervallumon kívül.\n",
    "(", scales::percent(n_outofconf/m, accuracy = .01), ")"
  )
})
# Server: 2_1 -------------------------------------------------------------
output$answer_2_1 <- renderText({

  p <- input$p_2_1/100
  n <- input$n_2_1
  alpha <- input$alpha_2_1
  
  varp <- (p*(1-p))/(n-1)
  sdp <- varp^0.5
  zstat <- qnorm(1-alpha/200)
  conf_min <- p-sdp*zstat
  conf_max <- p+sdp*zstat
  needed_n <- ifelse(n*p < 10 | n*(1-p) < 10, "Szükséges mintaelemszám feltétele sérül!", "Mintaelemszám elégséges.")

    paste(
    "Mintabeli arány = ", scales::percent(p, accuracy = .01), "\n",
    needed_n, "\n",
    "Standard hiba = ", round(sdp, digits = 4), "\n",
    "Z = ", round(zstat, digits = 4), "\n",
    "Konfidencia intervallum alsó határa = ", scales::percent(conf_min, accuracy = .01), "\n",
    "Konfidencia intervallum felső határa = ", scales::percent(conf_max, accuracy = .01),
    sep = ""
  )
   
})

# Server: 2_2 -------------------------------------------------------------
output$answer_2_2 <- renderText({
  conf_var_min <- (input$n_2_2-1)*input$ssd_2_2^2/qchisq(1-input$alpha_2_2/200, df = input$n_2_2-1)
  conf_var_max <- (input$n_2_2-1)*input$ssd_2_2^2/qchisq(input$alpha_2_2/200, df = input$n_2_2-1)

  paste(
    "Mintabeli szórás = ", round(input$ssd_2_2, digits = 4), "\n",
    "Variancia konfidencia-intervallumának alsó határa = ", round(conf_var_min, digits = 4), "\n",
    "Variancia konfidencia-intervallumának felső határa = ", round(conf_var_max, digits = 4), "\n",
    "Szórás konfidencia-intervallumának alsó határa = ",  round(conf_var_min^0.5, digits = 4), "\n",
    "Szórás konfidencia-intervallumának felső határa = ",round(conf_var_max^0.5, digits = 4),
    sep = ""
  )
  })

  

  # Server: ketmintas varhato -----------------------------------------------

  output$mu <- renderUI({
    if (input$type == "csúszka") {
      sliderInput(
        "mu",
        "Feltételezett eltérés: ",
        value = 0,
        min = -100,
        max = 100
      )
    } else {
      numericInput(
        "mu",
        "Feltételezett eltérés: ",
        value = 0
      )
    }
  })

  output$n_x <- renderUI({
    if (input$type == "csúszka") {
      sliderInput(
        "n_x",
        "x minta elemszáma:",
        value = 1,
        min = 1,
        max = 200
      )
    } else {
      numericInput(
        "n_x",
        "x minta elemszáma:",
        value = 1,
        min = 1
      )
    }
  })

  output$n_y <- renderUI({
    if (input$type == "csúszka") {
      sliderInput(
        "n_y",
        "y minta elemszáma:",
        value = 1,
        min = 1,
        max = 200
      )
    } else {
      numericInput(
        "n_y",
        "y minta elemszáma:",
        min = 1,
        value = 1
      )
    }
  })

  output$mean_x <- renderUI({
    if (input$type == "csúszka") {
      sliderInput(
        "mean_x",
        "x minta átlaga",
        value = 0,
        min = -100,
        max = 100
      )
    } else {
      numericInput(
        "mean_x",
        "x minta átlaga",
        value = 0
      )
    }
  })


  output$mean_y <- renderUI({
    if (input$type == "csúszka") {
      sliderInput(
        "mean_y",
        "y minta átlaga",
        value = 0,
        min = -100,
        max = 100
      )
    } else {
      numericInput(
        "mean_y",
        "y minta átlaga",
        value = 0
      )
    }
  })

  output$sigma_x <- renderUI({
    if (input$type == "csúszka") {
      sliderInput(
        "sigma_x",
        "x minta szórása",
        value = 0,
        min = 0,
        max = 50
      )
    } else {
      numericInput(
        "sigma_x",
        "x minta szórása",
        value = 0,
        min = 0
      )
    }
  })

  output$sigma_y <- renderUI({
    if (input$type == "csúszka") {
      sliderInput(
        "sigma_y",
        "y minta szórása",
        value = 0,
        min = 0,
        max = 50
      )
    } else {
      numericInput(
        "sigma_y",
        "y minta szórása",
        value = 0,
        min = 0
      )
    }
  })

  x <- reactive({
    input$reload
    rnorm(input$n_x, mean = 0, sd = 1)
  })
  y <- reactive({
    input$reload
    rnorm(input$n_y, mean = 0, sd = 1)
  })


  output$ketmintas_elteres_answer <- renderText({
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
    alpha <- input$alpha
    x <- x()
    y <- y()

    if (input$n_x > 1 & input$sigma_x > 0) {
      x <- x * (input$sigma_x / var(x)^0.5)
    }
    if (input$sigma_x == 0) {
      x <- rep(input$mean_x, length(x))
    }

    x <- x + (input$mean_x - mean(x))

    if (n_y > 1 & sigma_y > 0) {
      y <- y * (sigma_y / var(y)^0.5)
    }

    if (sigma_y == 0) {
      y <- rep(mean_y, length(y))
    }

    y <- y + (mean_y - mean(y))

    test_side <- ifelse(test_side == "jobboldali", "greater", test_side)
    test_side <- ifelse(test_side == "baloldali", "less", test_side)
    test_side <- ifelse(test_side == "kétoldali", "two.sided", test_side)

    plot_title <- "Ezekkel a paraméterekkel a hipotézis tesztelése nem végezhető."

    try(
      {
        if (test_type == "t") {
          pr_f <- t.test(x, y, alternative = test_side, mu = mu)
        }
        if (test_type == "z") {
          pr_f <- BSDA::z.test(x, y, alternative = test_side, mu = mu, sigma.x = var(x)^0.5, sigma.y = var(y)^0.5)
        }




        if (test_side == "greater") {
          answer <- ifelse(pr_f$p.value < alpha / 100, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist, tehát a két sokassági átlag között lévő különbség nagyobb, mint a feltételezettként megadott.", sep = ""),
            paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist, miszerint a két sokassági átlag között lévő különbség nem nagyobb, mint a feltételezettként megadott.", sep = "")
          )
        }

        if (test_side == "less") {
          answer <- ifelse(pr_f$p.value < alpha / 100, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist, tehát a két sokassági átlag között lévő különbség kisebb, mint a feltételezettként megadott.", sep = ""),
            paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist, miszerint a két sokassági átlag között lévő különbség nem kisebb, mint a feltételezettként megadott.", sep = "")
          )
        }

        if (test_side == "two.sided") {
          answer <- ifelse(pr_f$p.value < alpha / 100, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist, tehát a két sokassági átlag között lévő különbség nem egyenlő a feltételezettként megadottal.", sep = ""),
            paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist, miszerint a két sokassági átlag között lévő különbség egyenlő a feltételezettként megadott.", sep = "")
          )
        }


        plot_title <- paste(
          "Próbafüggvény (",
          test_type,
          ") = ",
          round(pr_f$statistic, 4),
          "\n",
          "p-érték =  ",
          scales::percent(pr_f$p.value),
          "\n",
          answer,
          sep = ""
        )
      },
      silent = T
    )


    plot_title
  })


  output$plot1 <- renderPlot(
    {
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
      alpha <- input$alpha
      x <- x()
      y <- y()

      if (input$n_x > 1 & input$sigma_x > 0) {
        x <- x * (input$sigma_x / var(x)^0.5)
      }
      if (input$sigma_x == 0) {
        x <- rep(input$mean_x, length(x))
      }
      x <- x + (input$mean_x - mean(x))

      if (n_y > 1 & sigma_y > 0) {
        y <- y * (sigma_y / var(y)^0.5)
      }



      if (sigma_y == 0) {
        y <- rep(mean_y, length(y))
      }


      y <- y + (mean_y - mean(y))

      test_side <- ifelse(test_side == "jobboldali", "greater", test_side)
      test_side <- ifelse(test_side == "baloldali", "less", test_side)
      test_side <- ifelse(test_side == "kétoldali", "two.sided", test_side)

      y_A <- 53
      if (n_x > 1) {
        y_A <- seq(x) * 95 / length(x)
      }
      y_B <- 47
      if (n_y > 1) {
        y_B <- seq(y) * 105 / n_y
      }

      p1 <- ggplot()
      if (TF_confidence) {
        if (n_y > 1 & sigma_y > 0) {
          p1 <- p1 + geom_ribbon(aes(
            x =
              c(
                ifelse(test_type == "t", mean(y) - (qt(0.975, df = n_y - 1) * var(y)^0.5 / sqrt(n_y)), mean(y) - (qnorm(0.975) * var(y)^0.5 / sqrt(n_y))),
                ifelse(test_type == "t", mean(y) + (qt(0.975, df = n_y - 1) * var(y)^0.5 / sqrt(n_y)), mean(y) + (qnorm(0.975) * var(y)^0.5 / sqrt(n_y)))
              ),
            ymin = -Inf,
            ymax = Inf
          ), fill = "#00A3AB", alpha = 0.1)
        }

        if (n_x > 1 & sigma_x > 0) {
          p1 <- p1 + geom_ribbon(aes(
            x =
              c(
                ifelse(test_type == "t", mean(x) - (qt(0.975, df = n_x - 1) * var(x)^0.5 / sqrt(n_x)), mean(x) - (qnorm(0.975) * var(x)^0.5 / sqrt(n_x))),
                ifelse(test_type == "t", mean(x) + (qt(0.975, df = n_x - 1) * var(x)^0.5 / sqrt(n_x)), mean(x) + (qnorm(0.975) * var(x)^0.5 / sqrt(n_x)))
              ),
            ymin = -Inf,
            ymax = Inf
          ), fill = "#FF5B6B", alpha = 0.1) +
            geom_ribbon(aes(
              x = c(mean(x), mean(x)),
              ymin = -Inf,
              ymax = -Inf,
              fill = "Konfidencia intervallum"
            ), alpha = 0.1, color = "black") +
            scale_fill_manual(values = c("Konfidencia intervallum" = "#FF5B6B"))
        }

        if ((n_x == 1 | sigma_x == 0) & n_y > 1 & sigma_y > 0) {
          p1 <- p1 + geom_ribbon(aes(
            x = c(mean(x), mean(x)),
            ymin = -Inf,
            ymax = -Inf,
            fill = "Konfidencia intervallum"
          ), alpha = 0.1, color = "black") +
            scale_fill_manual(values = c("Konfidencia intervallum" = "#00A3AB"))
        }
      }

      if (mean(x) != mean(y)) {
        p1 <- p1 + geom_vline(xintercept = mean(x), color = "#FF5B6B", linetype = "dotted", size = 1.3) +
          geom_vline(xintercept = mean(y), color = "#00A3AB", linetype = "dotted", size = 1.3)
      } else {
        p1 <- p1 + geom_vline(xintercept = mean(x), color = "#92D050", linetype = "dotted", size = 1.3)
      }

      p1 <- p1 + geom_point(aes(x = x, y = y_A),
        fill = "#FF5B6B", size = 4,
        shape = 21, color = "black", stroke = 2, alpha = 0.7
      ) +
        geom_point(aes(x = y, y = y_B),
          fill = "#00A3AB", size = 4,
          shape = 21, color = "black", stroke = 2, alpha = 0.7
        ) +
        xlab("") +
        ylab("") +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          plot.title = element_blank(),
          axis.text.x = element_text(size = 20),
          legend.position = "bottom",
          legend.justification = 0.9,
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          plot.margin = margin(30, 30, 30, 30, "pt")
        )

      if (TF_Rho_show) {
        p1 <- p1 + geom_line(aes(
          x = c(
            mean(c(mean(x), mean(y))) - mu / 2,
            mean(c(mean(x), mean(y))) + mu / 2
          ),
          y = c(50, 50),
          color = "Feltételezett eltérés"
        ), size = 3) +
          scale_color_manual(values = c("Feltételezett eltérés" = "#FFC730"))
      }



      plot_title <- ""

      try(
        {
          if (test_type == "t") {
            pr_f <- t.test(x, y, alternative = test_side, mu = mu)
          }
          if (test_type == "z") {
            pr_f <- BSDA::z.test(x, y, alternative = test_side, mu = mu, sigma.x = var(x)^0.5, sigma.y = var(y)^0.5)
          }




          if (test_side == "greater") {
            answer <- ifelse(pr_f$p.value < alpha, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist, tehát a két sokassági átlag között lévő különbség nagyobb, mint a feltételezettként megadott.", sep = ""),
              paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist, miszerint a két sokassági átlag között lévő különbség nem nagyobb, mint a feltételezettként megadott.", sep = "")
            )
          }

          if (test_side == "less") {
            answer <- ifelse(pr_f$p.value < alpha, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist, tehát a két sokassági átlag között lévő különbség kisebb, mint a feltételezettként megadott.", sep = ""),
              paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist, miszerint a két sokassági átlag között lévő különbség nem kisebb, mint a feltételezettként megadott.", sep = "")
            )
          }

          if (test_side == "two.sided") {
            answer <- ifelse(pr_f$p.value < alpha, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist, tehát a két sokassági átlag között lévő különbség nem egyenlő a feltételezettként megadottal.", sep = ""),
              paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist, miszerint a két sokassági átlag között lévő különbség egyenlő a feltételezettként megadott.", sep = "")
            )
          }


          plot_title <- paste(
            "Próbafüggvény (",
            test_type,
            ") = ",
            round(pr_f$statistic, 4),
            "             p-érték =  ",
            scales::percent(pr_f$p.value),
            "\n",
            answer,
            sep = ""
          )
        },
        silent = T
      )

      if (plot_title == "" | sigma_x == 0 | sigma_y == 0) {
        p1
      } else {
        if (test_side == "greater") {
          cf <- ifelse(test_type == "z", qnorm(1 - alpha / 100), qt(1 - alpha / 100, df = n_y + n_x - 2))
          p2 <- ggplot() +
            geom_ribbon(aes(xmin = -Inf, xmax = cf, y = c(-1, 1), fill = "Elfogadási tartomány"), alpha = 0.6) +
            geom_ribbon(aes(xmin = cf, xmax = Inf, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
            geom_hline(yintercept = 0, size = 3, linetype = "dotted", color = "black") +
            geom_line(aes(x = c(cf, cf), y = c(-0.5, 0.5)), size = 2, color = "black") +
            geom_point(aes(x = pr_f$statistic, y = 0), size = 9, shape = 21, color = "black", fill = "white", stroke = 1) +
            annotate("text",
              x = cf, y = 0.7,
              label = expression(paste(c[f])), size = 10, colour = "black"
            ) +
            annotate("text",
              x = pr_f$statistic, y = 0.4,
              label = test_type, size = 10, colour = "black"
            ) +
            annotate("text",
              x = cf, y = -0.7,
              label = round(cf, digits = 2), size = 10, colour = "black"
            ) +
            annotate("text",
              x = pr_f$statistic, y = -0.4,
              label = round(pr_f$statistic, digits = 2), size = 10, colour = "black"
            )
        }

        if (test_side == "less") {
          ca <- ifelse(test_type == "z", qnorm(1 - alpha / 100), qt(1 - alpha / 100, df = n_y + n_x - 2)) * -1
          p2 <- ggplot() +
            geom_ribbon(aes(xmin = ca, xmax = Inf, y = c(-1, 1), fill = "Elfogadási tartomány"), alpha = 0.6) +
            geom_ribbon(aes(xmin = -Inf, xmax = ca, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
            geom_hline(yintercept = 0, size = 3, linetype = "dotted", color = "black") +
            geom_line(aes(x = c(ca, ca), y = c(-0.5, 0.5)), size = 2, color = "black") +
            geom_point(aes(x = pr_f$statistic, y = 0), size = 9, shape = 21, color = "black", fill = "white", stroke = 1) +
            annotate("text",
              x = ca, y = 0.7,
              label = expression(paste(c[a])), size = 10, colour = "black"
            ) +
            annotate("text",
              x = pr_f$statistic, y = 0.4,
              label = test_type, size = 10, colour = "black"
            ) +
            annotate("text",
              x = ca, y = -0.7,
              label = round(ca, digits = 2), size = 10, colour = "black"
            ) +
            annotate("text",
              x = pr_f$statistic, y = -0.4,
              label = round(pr_f$statistic, digits = 2), size = 10, colour = "black"
            )
        }

        if (test_side == "two.sided") {
          ca <- ifelse(test_type == "z", qnorm(1 - alpha / 200), qt(1 - alpha / 200, df = n_y + n_x - 2)) * -1
          cf <- ifelse(test_type == "z", qnorm(1 - alpha / 200), qt(1 - alpha / 200, df = n_y + n_x - 2))
          p2 <- ggplot() +
            geom_ribbon(aes(xmin = ca, xmax = cf, y = c(-1, 1), fill = "Elfogadási tartomány"), alpha = 0.6) +
            geom_ribbon(aes(xmin = -Inf, xmax = ca, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
            geom_ribbon(aes(xmin = cf, xmax = Inf, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
            geom_hline(yintercept = 0, size = 3, linetype = "dotted", color = "black") +
            geom_line(aes(x = c(ca, ca), y = c(-0.5, 0.5)), size = 2, color = "black") +
            geom_line(aes(x = c(cf, cf), y = c(-0.5, 0.5)), size = 2, color = "black") +
            geom_point(aes(x = pr_f$statistic, y = 0), size = 9, shape = 21, color = "black", fill = "white", stroke = 1) +
            annotate("text",
              x = ca, y = 0.7,
              label = expression(paste(c[a])), size = 10, colour = "black"
            ) +
            annotate("text",
              x = pr_f$statistic, y = 0.4,
              label = test_type, size = 10, colour = "black"
            ) +
            annotate("text",
              x = ca, y = -0.7,
              label = round(ca, digits = 2), size = 10, colour = "black"
            ) +
            annotate("text",
              x = pr_f$statistic, y = -0.4,
              label = round(pr_f$statistic, digits = 2), size = 10, colour = "black"
            ) +
            annotate("text",
              x = cf, y = -0.7,
              label = round(cf, digits = 2), size = 10, colour = "black"
            ) +
            annotate("text",
              x = cf, y = 0.7,
              label = expression(paste(c[f])), size = 10, colour = "black"
            )
        }

        if (test_side == "greater" | test_side == "two.sided") {
          if (cf < 3.1) {
            if (pr_f$statistic < 3 & pr_f$statistic > -3) {
              p2 <- p2 + scale_x_continuous(limits = c(-3.1, 3.1), expand = c(0, 0))
            } else {
              p2 <- p2 + scale_x_continuous(limits = sort(c(-pr_f$statistic * 1.1, pr_f$statistic * 1.1)), expand = c(0, 0))
            }
          }
        } else {
          if (ca > 3.1) {
            if (pr_f$statistic < 3 & pr_f$statistic > -3) {
              p2 <- p2 + scale_x_continuous(limits = c(-3.1, 3.1), expand = c(0, 0))
            } else {
              p2 <- p2 + scale_x_continuous(limits = sort(c(-pr_f$statistic * 1.1, pr_f$statistic * 1.1)), expand = c(0, 0))
            }
          }
        }


        p2 <- p2 + scale_fill_manual(values = c("Elfogadási tartomány" = "#92D050", "Elutasítási tartomány" = "#FF5B6B")) +
          theme_void() + theme(
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 20),
            plot.margin = margin(30, 30, 30, 30, "pt"),
            plot.title = element_text(size = 20, hjust = 0.5),
          )

        ggarrange(p1, p2, ncol = 1, heights = c(5, 2))
      }
    },
    width = 1300,
    height = 1300
  )



  # Server: ketmintas-szoras --------------------------------------------------------

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
    test_4_2 <- var.test(x_4_2, y_4_2, alternative = test_side_4_2)
    dontes_4_2 <- "nincs"

    if (!is.na(test_4_2$p.value)) {
      if (test_4_2$p.value > alpha_4_2 / 100) {
        print("0")
        if (test_side_4_2 == "two.sided") {
          print("1")
          dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elfogadjuk, a két minta szórása megegyezik."
        }
        if (test_side_4_2 == "greater") {
          print("2")
          dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elfogadjuk, x minta szórása nem nagyobb, mint y minta szórása."
        }
        if (test_side_4_2 == "less") {
          print("3")
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
    }

    if (dontes_4_2 == "nincs") {
      final_text_4_2 <- "Ezekkel az adatokkal nem lehet a próbát elvégezni."
    }
    if (dontes_4_2 != "nincs") {
      final_text_4_2 <- paste(
        "Próbafüggvény (F) = ",
        round(test_4_2$statistic, digits = 4),
        "\n",
        "P-érték = ",
        scales::percent(
          test_4_2$p.value
        ),
        "\n",
        "Döntés: ",
        alpha_4_2,
        dontes_4_2,
        sep = ""
      )
    }
    final_text_4_2
  })
}

shinyApp(ui, server)
