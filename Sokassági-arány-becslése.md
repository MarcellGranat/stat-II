Sokassági arány becslése
================
Granát Marcell

``` r
p <- 50
n <- 100
alpha <- 5
```

``` r
p <- p/100
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
```

    [1] "Mintabeli arány = 50.00%\nMintaelemszám elégséges.\nStandard hiba = 0.0503\nZ = 1.96\nKonfidencia intervallum alsó határa = 40.15%\nKonfidencia intervallum felső határa = 59.85%"
