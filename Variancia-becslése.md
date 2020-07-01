Variancia becslése
================
Granát Marcell

``` r
ssd <- 1 #Mintabeli szórás
n <- 100 #Mintanagyság
alpha <- 5 #Szignifikanciaszint (%)
```

``` r
 conf_var_min <- (n-1)*ssd^2/qchisq(1-alpha/200, df = n-1)
  conf_var_max <- (n-1)*ssd^2/qchisq(alpha/200, df = n-1)

  str_c(
    "Mintabeli szórás = ", round(ssd, digits = 4), "\n",
    "Variancia konfidencia-intervallumának alsó határa = ", round(conf_var_min, digits = 4), "\n",
    "Variancia konfidencia-intervallumának felső határa = ", round(conf_var_max, digits = 4), "\n",
    "Szórás konfidencia-intervallumának alsó határa = ",  round(conf_var_min^0.5, digits = 4), "\n",
    "Szórás konfidencia-intervallumának felső határa = ",round(conf_var_max^0.5, digits = 4)
  )
```

    [1] "Mintabeli szórás = 1\nVariancia konfidencia-intervallumának alsó határa = 0.7709\nVariancia konfidencia-intervallumának felső határa = 1.3495\nSzórás konfidencia-intervallumának alsó határa = 0.878\nSzórás konfidencia-intervallumának felső határa = 1.1617"
