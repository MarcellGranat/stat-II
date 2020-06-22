# input -------------------------------------------------------------------
mu <- 0
sigma <- 1
n <- 10
ntraject <- 10
alpha <- 0.05
traject_select <- 2
n_hist_group <- 5
# matrix of random variables ---> separeta reactive -----------------------
random_matrix <- rnorm(n * ntraject, mean = mu, sd = sigma) %>%
  matrix(nrow = n, ncol = ntraject)
# plot of sample means ----------------------------------------------------
p <- ggplot()
if (sigma > 0 & n > 1) { # confidence interval
  p <- p + geom_ribbon(aes(
    x = c(
      mu - (qnorm(1 - alpha / 2) * sigma / sqrt(n)),
      mu + (qnorm(1 - alpha / 2) * sigma / sqrt(n))
    ),
    ymin = -Inf, ymax = Inf
  , fill = "Konfidencia intervallum"), alpha = .1) + 
    scale_fill_manual(values = c("Konfidencia intervallum" = "#FF5B6B"))
}

p <- p + geom_vline(xintercept = mu, linetype = "dashed", color = "#FF5B6B", size = 1.2) +
  geom_point(aes(y = seq(from = 0, to = 100, length.out = ntraject), x = colMeans(random_matrix)),
    fill = "#00A3AB", size = 2, shape = 21, color = "black", stroke = 1, alpha = 0.7
  )

p <- p + scale_y_continuous(breaks = NULL) +
  labs(x = "Mintaátlagok", y = "", title = "Mintaátlagok és konfidenciaintervallum") +
  theme_minimal() + theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
# histogram of sample means ------------------------------------------------------
p <- ggplot() +
  geom_histogram(aes(colMeans(random_matrix)), color = "black", fill = "#52CCBF", bins = n_hist_group) +
  labs(x = "", y = "", title = "Mintaátlagok hisztogramja") +
  theme_minimal() 
  
# normalized histogram + normal distribution ------------------------------
x <- seq(from = min(colMeans(random_matrix)), to = max(colMeans(random_matrix)), length.out = n_hist_group + 1)
x
normalised_his <- table(cut(colMeans(random_matrix),
  breaks = x, right = F, include.lowest = T
)) %>%
  tibble() %>%
  mutate(
    x = (x[-1] + x[1:(length(x) - 1)]) / 2
  )
names(normalised_his) <- c("y", "x")
normalised_his
normalised_his <- normalised_his %>%
  mutate(
    y = as.numeric(y / sum(y))
  ) 
normalised_his
diff(normalised_his$x)
p <- ggplot() +
  stat_function(
    data = data.frame(x = c(
      min(colMeans(random_matrix)) - min(colMeans(random_matrix)) * 0.1,
      max(colMeans(random_matrix)) + max(colMeans(random_matrix)) * 0.1
    )), aes(x = x, fill = "Normál eloszlás"), color = "black",
    fun = dnorm, args = list(mu, sigma), geom = "area"
  ) +
  geom_bar(
    data = normalised_his, aes(y = y, x = x), stat = "identity", color = "black", alpha = .8,
    width = x[2] - x[1]
  ) +
  labs(x = "", y = "", title = "Arányokat ismertető hisztogram") +
  scale_fill_manual(values = c("Normál eloszlás" = "#FFC730")) +
  scale_x_continuous(breaks = round(x, digits = 2)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
p
# results -----------------------------------------------------------------
if (sigma > 0 & n > 1) {
  conf_min <- mu - (qnorm(1 - alpha / 2) * sigma / sqrt(n)) # confidence interval
  conf_max <- mu + (qnorm(1 - alpha / 2) * sigma / sqrt(n))
  n_conf_out <- sum(colMeans(random_matrix) > conf_max | colMeans(random_matrix) < conf_min)
  results <- paste(
    "Alfa = ", scales::percent(alpha), "\n",
    "Konfidencia intervallum alsó határa = ", round(conf_min, digits = 4), "\n",
    "Konfidencia intervallum felső határa = ", round(conf_max, digits = 4), "\n",
    "Konfidencia intervallumon kívül eső átlaggal rendelkező minták száma = ", n_conf_out, "\n",
    "Konfidencia intervallumon kívül eső átlaggal rendelkező minták aránya = ",
    scales::percent(n_conf_out / ntraject),
    sep = ""
  )
} else {
  results <- "Ezen paraméterekkel konfidencia intervallum nem szerkeszthető."
}
# individual sample plot --------------------------------------------------
p <-  ggplot()
if (sigma>0 & n>1) {
  p <- p+ geom_ribbon(aes(
    x = c(
      mu - (qnorm(1 - alpha / 2) * sigma / sqrt(n)),
      mu + (qnorm(1 - alpha / 2) * sigma / sqrt(n))
    ),
    ymin = -Inf, ymax = Inf,
    fill = "Konfidencia intervallum"
  ), alpha = .1) 
}
 
  p <- p+ geom_vline(aes(xintercept = mu, color = "Várható érték"), linetype = "dashed", size = 1.3) +
  geom_vline(aes(
    xintercept = mean(as.numeric(random_matrix[, traject_select])),
    color = "Adott minta átlaga"
  ), linetype = "dashed", size = 1.3) +
  geom_point(aes(
    x = as.numeric(random_matrix[, traject_select]),
    y = seq(from = 1, to = 100, length.out = n)
  ),
  fill = "#52CCBF", size = 4, shape = 21, color = "black", stroke = 2, alpha = 0.7
  ) +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(values = c(
    "Adott minta átlaga" = "#52CCBF",
    "Várható érték" = "#FF5B6B"
  )) +
  scale_fill_manual(values = c(
    "Konfidencia intervallum" = "#FF5B6B"
  )) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.title = element_blank()
  )
  
