# Carga el archivo CSV con punto y coma como delimitador
datosf <- read.csv("BONOS - FOMC.csv")
datos <- read.csv("BONOS - 3-meses.csv")

# Muestra las primeras filas del dataframe
head(datosf)
head(datos)

# Verifica que la conversión se haya realizado correctamente
str(datosf$FOMC)
str(datos$X3.month)


# Convierte la columna X20.year a tipo numérico
datosf$FOMC <- as.numeric(datosf$FOMC)
datos$X3.month <- as.numeric(datos$X3.month)


# Elimina filas con valores ND en X20.year
datosf <- datosf[!is.na(datosf$FOMC), ]
datos <- datos[!is.na(datos$X3.month), ]









# Calcula estadísticas descriptivas, NO SIRVEN
summary(datosf$FOMC)
summary(datos$X3.month)








# Correlación de Pearson
correlacion_pearson <- cor(datosf$FOMC, datos$X3.month, method = "pearson")

# Correlación de Kendall
correlacion_kendall <- cor(datosf$FOMC, datos$X3.month, method = "kendall")

# Correlación de Spearman
correlacion_spearman <- cor(datosf$FOMC, datos$X3.month, method = "spearman")

# Imprimir los resultados
print(paste("Correlación de Pearson:", correlacion_pearson))
print(paste("Correlación de Kendall:", correlacion_kendall))
print(paste("Correlación de Spearman:", correlacion_spearman))








# Desviación estándar de FOMC
sd_fomc <- sd(datosf$FOMC)

# Desviación estándar de X3.month
sd_x3_month <- sd(datos$X3.month)

# Imprimir los resultados
print(paste("Desviación estándar de FOMC:", sd_fomc))
print(paste("Desviación estándar de X3.month:", sd_x3_month))


#GRAFICOS

plot(datosf$FOMC, type = "l", col = "blue", main = "Gráfico de FOMC", xlab = "Índice", ylab = "Valor")

plot(datos$X3.month, type = "l", col = "red", main = "Gráfico de X3.month", xlab = "Índice", ylab = "Valor")




datos_stan <- list(N = nrow(datos),
                   r = datos$X3.month,
                   dt = 1) # Ajusta dt según la frecuencia de tus datos




library(rstan)

# Define el modelo de Stan como una cadena de texto
modelo_vasicek_stan <- "
data {
  int<lower=0> N; // Número de observaciones
  vector[N] r; // Tasas de interés observadas
  real<lower=0> dt; // Paso de tiempo entre observaciones
}


parameters {
  real alpha; // Velocidad de reversión
  real b; // Nivel de largo plazo
  real<lower=0> sigma; // Volatilidad
}


model {
  // Priors
  alpha ~ normal(10, 3);
  b ~ normal(5.33, 0.5);
  sigma ~ inv_gamma(3, 3.9);
  
  // Modelo de Vasicek
  for (i in 2:N) {
    r[i] ~ normal(alpha * (b - r[i-1]) * dt + r[i-1], sigma * sqrt(dt));
  }
}


  generated quantities {
  vector[N] future_rates;
  future_rates[1] = r[N];
  for (i in 2:N) {
    future_rates[i] = normal_rng(alpha * (b - future_rates[i-1]) * dt + future_rates[i-1], sigma * sqrt(dt));
  }
}
"

# Compila el modelo
modelo_vasicek <- stan_model(model_code = modelo_vasicek_stan)

# Ajusta el modelo a los datos
ajuste_vasicek <- sampling(modelo_vasicek, data = datos_stan, iter = 1000, chains = 1)

# Revisa los resultados
print(ajuste_vasicek)
































library(bayesplot)

# Traceplots para todos los parámetros utilizando mcmc_trace
mcmc_trace(ajuste_vasicek, pars = c("b"))

# Densidades posteriores para todos los parámetros
mcmc_areas(ajuste_vasicek, pars = c( "b"))

# Pair plot para parámetros seleccionados
mcmc_pairs(ajuste_vasicek, pars = c("b"))

# Extraer estadísticas de diagnóstico directamente
stan_summary <- summary(ajuste_vasicek))

# Extraer Rhat y n_eff
rhats <- stan_summary$summary[,"Rhat"]
neffs <- stan_summary$summary[,"n_eff"]

# Cargar la librería ggplot2 para hacer los gráficos
library(ggplot2)

# Gráfico para Rhat
ggplot(data = data.frame(Parameter = names(rhats), Rhat = rhats), aes(x = Parameter, y = Rhat)) +
  geom_point() +
  geom_hline(yintercept = 1.1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Rhat Diagnostic Plot", y = "Rhat", x = "Parameter") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Gráfico para n_eff
ggplot(data = data.frame(Parameter = names(neffs), Neff = neffs), aes(x = Parameter, y = Neff)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Effective Sample Size (n_eff) Diagnostic Plot", y = "n_eff", x = "Parameter") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


