# input -------------------------------------------------------------------
N <- 1000
n <- 100
m <- 50
alpha <- 5
mu <- 50
sigma <- 2
# random population & sampling ---> reactive ------------------------------------------------------------------
pop <- rnorm(n = N, mean = mu, sd=sigma)
if (N > 1 & sigma>0) {
  pop <- pop/var(pop)^0.5*sigma
}
pop <- pop - mean(pop) + mu
mat <- matrix(nrow = n, ncol = m)
for (i in seq(m)) {
  mat[,i] <- sample(pop, size = n)
}
# plot --------------------------------------------------------------------
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
# results -----------------------------------------------------------------
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