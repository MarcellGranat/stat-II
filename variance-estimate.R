# input ------------------------------------------------------------------
ssd <- 0.58
n <- 100
alpha <- 5
# confidence interval -----------------------------------------------------
conf_var_min <- (n-1)*ssd^2/qchisq(1-alpha/200, df = n-1)
conf_var_max <- (n-1)*ssd^2/qchisq(alpha/200, df = n-1)
# output ------------------------------------------------------------------
paste(
  "Mintabeli szórás = ", round(ssd, digits = 4),
  "Variancia konfidencia-intervallumának alsó határa = ", round(conf_var_min, digits = 4), "\n",
  "Variancia konfidencia-intervallumának felső határa = ", round(conf_var_max, digits = 4), "\n",
  "Szórás konfidencia-intervallumának alsó határa = ",  round(conf_var_min^0.5, digits = 4), "\n",
  "Szórás konfidencia-intervallumának felső határa = ",round(conf_var_max^0.5, digits = 4)
)

