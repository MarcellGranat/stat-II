test_side_4_2 <- "kétoldali"
n_x_4_2 <- 100
n_y_4_2 <- 100
sigma_x_4_2 <- 3
sigma_y_4_2 <- 3.1
alpha_4_2 <- 5

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

x_4_2 <- x_4_2 - mean(x_4_2)
y_4_2 <- y_4_2 - mean(y_4_2)

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
      print("4")
      dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elutasítjuk, a két minta szórása nem egyezik meg."
    }
    if (test_side_4_2 == "greater") {
      print("5")
      dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elutasítjuk, x minta szórása nagyobb, mint y minta szórása."
    }
    if (test_side_4_2 == "less") {
      print("6")
      dontes_4_2 <- "%-os szignifikanciazinten a nullhipotézist elutasítjuk, y minta szórása nagyobb, mint x minta szórása."
    }
  }
}

if (dontes_4_2 == "nincs") {
  final_text_4_2 <- "Ezekkel az adatokkal nem lehet a próbát elvégezni."
}  
if (dontes_4_2 != "nincs") {
  final_text_4_2 <- paste(
    "<b> Próbafüggvény (F) = <br>",  
    round(test_4_2$statistic, digits = 4),
    "\nP-érték = ",
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


# plot --------------------------------------------------------------------

df <- rbind(
data.frame(x=x_4_2) %>% 
  mutate(
    xmin=ifelse(x<0,x,0),
    xmax=ifelse(x>0,x,0),
    y = c(1:length(x_4_2))/length(x_4_2)*50,
    variable = rep("x", length(x_4_2))
  ),
data.frame(x=y_4_2) %>% 
  mutate(
    xmin=ifelse(x<0,x,0),
    xmax=ifelse(x>0,x,0),
    y = 100 - c(1:length(y_4_2))/length(y_4_2)*50,
    variable = rep("y", length(y_4_2))
  )
)


p1 <- ggplot(df) + 
  geom_vline(xintercept = 0, color="black", size=1) + 
  geom_point(aes(x = x, y = y,fill = variable) , size = 2,shape = 21, stroke = 0, alpha = 0.7)

df <- df %>% select(-x) %>% 
  gather(key = key, value = value, -c(y, variable))
  

p1 <- p1 + geom_line(data=df,aes(x=value, y=y, size=as.character(y), color=variable), alpha = 0.7) + 
  scale_size_manual(values=rep(0.1, n_x_4_2 + n_y_4_2), guide = F)

if (sigma_x_4_2 > 0 & n_x_4_2>1) {
  p1 <- p1 + geom_vline(xintercept = c(-sigma_x_4_2,sigma_x_4_2), color = "red")
}

if (sigma_y_4_2 > 0 & n_y_4_2>1) {
  p1 <- p1 + geom_vline(xintercept = c(-sigma_y_4_2,sigma_y_4_2), color = "blue")
}



p1
  




















