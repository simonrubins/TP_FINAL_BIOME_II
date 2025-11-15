#SCRIPT DONDE CORREMOS LOS MODELOS SELECCIONADOS
# 1. Cargar las librerías correctas
library(nlme)      # <--- Esta es la librería para gls()
library(ggeffects) # Para ggpredict()

#SÓLO DARLE BOLA A ESTOS PRIMEROS DOS MODELOS (SSS^2 y gamma)####
m_SSS2 = gls(model = riqueza ~ scale(SSS) + I(scale(SSS)^2),
             data = datos, 
             weights = varIdent(form = ~1|source))
summary(m_SSS2)
# 3. Predecir desde el modelo (tu código)
model_plot <- ggpredict(m_SSS2, 
                        terms = c("SSS [all]"),
                        interval = "confidence")

# 4. Graficar (versión manual corregida)
plot(model_plot) + 
  
  # Capa de puntos
  ggplot2::geom_point(data = datos, 
                      aes(x = SSS, y = riqueza), 
                      alpha = 0.7) +
  
  # Etiquetas
  ggplot2::xlab("SSS [PSU]") +
  ggplot2::ylab("Riqueza")


#AHORA CON gamma

m_gamma= gls(model = riqueza ~ scale(gamma),
             data = datos, 
             weights = varIdent(form = ~1|source))
summary(m_gamma)
# 3. Predecir desde el modelo (tu código)
model_plot <- ggpredict(m_gamma, 
                        terms = c("gamma [all]"),
                        interval = "confidence")

# 4. Graficar (versión manual corregida)
plot(model_plot) + 
  
  # Capa de puntos
  ggplot2::geom_point(data = datos, 
                      aes(x = gamma, y = riqueza), 
                      alpha = 0.7) +
  
  # Etiquetas
  ggplot2::xlab("SSS [PSU]") +
  ggplot2::ylab("Riqueza") 
####A PARTIR DE ACÁ NO DARLE BOLA####
#AHORA CON gamma

m_gamma_SSS= gls(model = riqueza ~ scale(SSS) + I(scale(SSS)^2)+scale(gamma),
             data = datos, 
             weights = varIdent(form = ~1|source))
summary(m_gamma_SSS)
# 3. Predecir desde el modelo (tu código)
model_plot <- ggpredict(m_gamma_SSS, 
                        terms = c("gamma [all]"),
                        interval = "confidence")

# 4. Graficar (versión corregida)
plot(model_plot) + 
  
  # Capa de puntos
  ggplot2::geom_point(data = datos, 
                      aes(x = gamma, y = riqueza), 
                      alpha = 0.7,
                      inherit.aes = FALSE) + # <--- AÑADE ESTO
  
  # Etiquetas
  ggplot2::xlab("gamma") +
  ggplot2::ylab("Riqueza")

modelo<- glm(
  riqueza ~ Chl + SSS + SST + POC + bbp + gamma + source,
  family = poisson,
  data = datos
)

modelo_comp<-glmmTMB(riqueza ~ Chl + SSS + SST + POC + bbp + gamma + source,
family = compois,
data = datos)
drop1(modelo_comp)
check_collinearity(modelo_comp)

modelo_2<-glmmTMB(riqueza ~ Chl + SSS + SST + POC + bbp + gamma,
                     family = compois,
                     data = datos)
check_collinearity(modelo_2)
summary(modelo_2)
drop1(modelo_2)

modelo_3<-glmmTMB(riqueza ~ SSS + SST + POC + bbp + gamma,
                  family = compois,
                  data = datos)
check_collinearity(modelo_3)
summary(modelo_3)
drop1(modelo_3)

modelo_4<-glmmTMB(riqueza ~ bbp + SST + POC + gamma,
                  family = compois,
                  data = datos)
check_collinearity(modelo_4)
summary(modelo_4)
drop1(modelo_4)

modelo_5<-glmmTMB(riqueza ~ SST + POC + gamma,
                  family = compois,
                  data = datos)
check_collinearity(modelo_5)
summary(modelo_5)
drop1(modelo_5)

modelo_5_con_int<-glmmTMB(riqueza ~ SST * POC + gamma,
                  family = compois,
                  data = datos)
summary(modelo_5_con_int)
drop1(modelo_5_con_int)
m_con_int_2<-glmmTMB(riqueza ~ SST+ POC * gamma,
                     family = compois,
                     data = datos)
summary(m_con_int_2)
drop1(m_con_int_2)
m_con_int_3<-glmmTMB(riqueza ~ SST * gamma+POC,
                     family = compois,
                     data = datos)
summary(m_con_int_3)
drop1(m_con_int_3)

anova(modelo_5_con_int,m_con_int_2,m_con_int_3, modelo_5)
# Guardar configuración original
op <- par()
# Configurar tamaño de letra
par(cex.main = 2,    # Título principal
    cex.lab = 1.3,     # Etiquetas de ejes
    cex.axis = 1.3)    # Números en ejes
### fin Sección tamaño de letra

# 1. Generar los residuales
sim <- simulateResiduals(fittedMod = modelo_5_con_int, plot = T)
# 2. genero  plotResiduals y lo guardo en p
plotResiduals(sim, datos$source)

# Restaurar configuración original (tamaño letra)
par(op)


# Gráfico de cuartiles
testDispersion(modelo_5_con_int, type = "DHARMa")



#GRAFICOS
p_sst <- ggpredict(modelo_5_con_int, terms = c("SST", "POC"))
ggplot(p_sst, aes(x, predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  labs(x = "SST", y = "Riqueza predicha", color = "POC", fill = "POC") +
  theme_minimal(base_size = 14)


p_poc <- ggpredict(modelo_5_con_int, terms = c("POC", "SST"))
ggplot(p_poc, aes(x, predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  labs(x = "POC", y = "Riqueza predicha", color = "SST", fill = "SST") +
  theme_minimal(base_size = 14)
p_gamma <- ggpredict(modelo_5_con_int, terms = "gamma")
plot(p_gamma)

ggplot(p_gamma, aes(x, predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Gamma", y = "Riqueza predicha") +
  theme_minimal(base_size = 14)

library(plotly)

# Elegimos tres niveles representativos de gamma
gamma_vals <- quantile(datos$gamma, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

plots <- list()

for(i in 1:3) {
  
  gamma_i <- gamma_vals[i]
  
  # GRID SST–POC para cada gamma
  pred_grid <- expand.grid(
    SST = seq(min(datos$SST), max(datos$SST), length = 50),
    POC = seq(min(datos$POC), max(datos$POC), length = 50),
    gamma = gamma_i
  )
  
  # Predicciones
  pred_grid$pred <- predict(modelo_5_con_int,
                            newdata = pred_grid,
                            type = "response")
  
  # Matriz Z para add_surface()
  zmat <- xtabs(pred ~ SST + POC, data = pred_grid)
  
  # Crear plot
  p <- plot_ly(
    x = unique(pred_grid$SST),
    y = unique(pred_grid$POC),
    z = zmat
  ) %>%
    add_surface() %>%
    layout(
      title = paste0("Superficie 3D — gamma = ", round(gamma_i, 3)),
      scene = list(
        xaxis = list(title = "SST"),
        yaxis = list(title = "POC"),
        zaxis = list(title = "Riqueza predicha")
      )
    )
  
  plots[[i]] <- p
}

# Mostrar cada uno
plots[[1]]
plots[[2]]
plots[[3]]

plot(x=datos$SST, y=datos$POC)
plot(x=datos$POC, y=datos$riqueza)
plot(x=datos$SST, y=datos$riqueza)
