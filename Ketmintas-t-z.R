test_type <- "t"
test_side <- "jobboldali"
mu <- 1
n_x <- 10
n_y <- 12
mean_x <- 10
mean_y <- 12
sigma_x <- 1
sigma_y <- 1
TF_confidence <- T
TF_Rho_show <- T
alpha <- 5


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






y_A <- 53
if (n_x > 1) {
  y_A <- seq(A) * 95 / length(A)
}
y_B <- 47
if (n_y > 1) {
  y_B <- seq(B) * 105 / n_y
}

p1 <- ggplot()
if (TF_confidence) {
  if (n_y > 1 & sigma_y > 0) {
    p1 <- p1 + geom_ribbon(aes(
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
    p1 <- p1 + geom_ribbon(aes(
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
        fill = "Konfidencia intervallum"
      ), alpha = 0.1, color = "black") +
      scale_fill_manual(values = c("Konfidencia intervallum" = "#FF5B6B"))
  }
  
  if ((n_x == 1 | sigma_x == 0) & n_y > 1 & sigma_y > 0) {
    p1 <- p1 + geom_ribbon(aes(
      x = c(mean(A), mean(A)),
      ymin = -Inf,
      ymax = -Inf,
      fill = "Konfidencia intervallum"
    ), alpha = 0.1, color = "black") +
      scale_fill_manual(values = c("Konfidencia intervallum" = "#00A3AB"))
  }
}

if (mean(A) != mean(B)) {
  p1 <- p1 + geom_vline(xintercept = mean(A), color = "#FF5B6B", linetype = "dotted", size = 1.3) +
    geom_vline(xintercept = mean(B), color = "#00A3AB", linetype = "dotted", size = 1.3)
} else {
  p1 <- p1 + geom_vline(xintercept = mean(A), color = "#92D050", linetype = "dotted", size = 1.3)
}

p1 <- p1 + geom_point(aes(x = A, y = y_A),
                      fill = "#FF5B6B", size = 4,
                      shape = 21, color = "black", stroke = 2, alpha = 0.7
) +
  geom_point(aes(x = B, y = y_B),
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
      mean(c(mean(A), mean(B))) - mu / 2,
      mean(c(mean(A), mean(B))) + mu / 2
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
      pr_f <- t.test(A, B, alternative = test_side, mu = mu)
    }
    if (test_type == "z") {
      pr_f <- BSDA::z.test(A, B, alternative = test_side, mu = mu, sigma.x = var(A)^0.5, sigma.y = var(B)^0.5)
    }
    
    
    
    
    if (test_side == "greater") {
      answer <- ifelse(pr_f$p.value<alpha, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist,\ntehát a két sokassági átlag között lévő különbség nagyobb, mint a feltételezettként megadott.", sep=""),
                       paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist,\nmiszerint a két sokassági átlag között lévő különbség nem nagyobb, mint a feltételezettként megadott.", sep="")
      )
    }
    
    if (test_side == "less") {
      answer <- ifelse(pr_f$p.value<alpha, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist,\ntehát a két sokassági átlag között lévő különbség kisebb, mint a feltételezettként megadott.", sep=""),
                       paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist,\nmiszerint a két sokassági átlag között lévő különbség nem kisebb, mint a feltételezettként megadott.", sep="")
      )
    }
    
    if (test_side == "two.sided") {
      answer <- ifelse(pr_f$p.value<alpha, paste(alpha, "%-os szignifikanciaszinten elutasítjuk a nullhipotézist,\ntehát a két sokassági átlag között lévő különbség nem egyenlő a feltételezettként megadottal.", sep=""),
                       paste(alpha, "%-os szignifikanciaszinten elfogadjuk a nullhipotézist,\nmiszerint a két sokassági átlag között lévő különbség egyenlő a feltételezettként megadott.", sep="")
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

if (plot_title == "" | sigma_x==0 | sigma_y==0) {
  p1
} else {
  
  
  if (test_side == "greater") {
    cf <- ifelse(test_type == "z", qnorm(1 - alpha / 100), qt(1 - alpha / 100, df = n_y + n_x - 2))
    p2 <- ggplot() +
      geom_ribbon(aes(xmin = -Inf, xmax = cf, y = c(-1, 1), fill = "Elfogadási tartomány"), alpha = 0.6) +
      geom_ribbon(aes(xmin = cf, xmax = Inf, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
      geom_hline(yintercept = 0, size = 3, linetype = "dotted", color = "black") +
      geom_line(aes(x = c(cf, cf), y = c(-0.5, 0.5)), size = 2, color = "black") +
      geom_point(aes(x = pr_f$statistic, y = 0), size = 9, shape = 21, color = "black",fill="white", stroke=1) +
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
    ca <- ifelse(test_type == "z", qnorm(1 - alpha / 100), qt(1 - alpha / 100, df = n_y + n_x - 2))*-1
    p2 <- ggplot() +
      geom_ribbon(aes(xmin = ca, xmax = Inf, y = c(-1, 1), fill = "Elfogadási tartomány"), alpha = 0.6) +
      geom_ribbon(aes(xmin = -Inf, xmax = ca, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
      geom_hline(yintercept = 0, size = 3, linetype = "dotted", color = "black") +
      geom_line(aes(x = c(ca, ca), y = c(-0.5, 0.5)), size = 2, color = "black") +
      geom_point(aes(x = pr_f$statistic, y = 0), size = 9, shape = 21, color = "black",fill="white", stroke=1) +
      annotate("text",
               x = ca, y = 0.7,
               label = expression(paste(c[a])), size = 10, colour = "black"
      ) +
      annotate("text",
               x = pr_f$statistic, y = 0.4,
               label = test_type, size = 10, colour = "black"
      )+
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
    ca <- ifelse(test_type == "z", qnorm(1 - alpha / 200), qt(1 - alpha / 200, df = n_y + n_x - 2))*-1
    cf <- ifelse(test_type == "z", qnorm(1 - alpha / 200), qt(1 - alpha / 200, df = n_y + n_x - 2))
    p2 <- ggplot() +
      geom_ribbon(aes(xmin = ca, xmax = cf, y = c(-1, 1), fill = "Elfogadási tartomány"), alpha = 0.6) +
      geom_ribbon(aes(xmin = -Inf, xmax = ca, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
      geom_ribbon(aes(xmin = cf, xmax = Inf, y = c(-1, 1), fill = "Elutasítási tartomány"), alpha = 0.6) +
      geom_hline(yintercept = 0, size = 3, linetype = "dotted", color = "black") +
      geom_line(aes(x = c(ca, ca), y = c(-0.5, 0.5)), size = 2, color = "black") +
      geom_line(aes(x = c(cf, cf), y = c(-0.5, 0.5)), size = 2, color = "black") +
      geom_point(aes(x = pr_f$statistic, y = 0), size = 9, shape = 21, color = "black",fill="white", stroke=1) +
      annotate("text",
               x = ca, y = 0.7,
               label = expression(paste(c[a])), size = 10, colour = "black"
      ) +
      annotate("text",
               x = pr_f$statistic, y = 0.4,
               label = test_type, size = 10, colour = "black"
      )+
      annotate("text",
               x = ca, y = -0.7,
               label = round(ca, digits = 2), size = 10, colour = "black"
      ) +
      annotate("text",
               x = pr_f$statistic, y = -0.4,
               label = round(pr_f$statistic, digits = 2), size = 10, colour = "black"
      )  +
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
    if (cf<3.1) {
      if (pr_f$statistic<3 & pr_f$statistic>-3) {
        p2 <- p2 + scale_x_continuous(limits = c(-3.1, 3.1), expand = c(0,0))
      } else {
        p2 <- p2 + scale_x_continuous(limits = sort(c(-pr_f$statistic*1.1, pr_f$statistic*1.1)), expand = c(0,0))
      }
    }
  } else {
    if (ca>3.1) {
      if (pr_f$statistic<3 & pr_f$statistic>-3) {
        p2 <- p2 + scale_x_continuous(limits = c(-3.1, 3.1), expand = c(0,0))
      } else {
        p2 <- p2 + scale_x_continuous(limits = sort(c(-pr_f$statistic*1.1, pr_f$statistic*1.1)), expand = c(0,0))
      }
    }
  }
  
  
  p2 <- p2 + scale_fill_manual(values = c("Elfogadási tartomány" = "#92D050", "Elutasítási tartomány" = "#FF5B6B")) + 
    labs(title = plot_title) +
    theme_void() + theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 20),
      plot.margin = margin(30, 30, 30, 30, "pt"),
      plot.title = element_text(size = 20, hjust = 0.5),
    )
  
  ggarrange(p1,p2, ncol = 1, heights=c(5,2))
  
}