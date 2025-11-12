#####LIBRERIAS####
install.packages("terra")
install.packages("marmap")
library(terra)
library(marmap)
library(pastecs)
library(patchwork)
library(ggplot2)
library(ggpubr)
library(GGally)
library(ggcorrplot)
library(corrplot) 
library(car) 
library(caret)
library(sjPlot)
library(faraway)
library(ggeffects)
library(lm.beta)
library(MuMIn) 
library(performance)
library(ggeffects)
library(dplyr)
library(pastecs)
library(reshape2)
library(lme4)
library(lmerTest)
library(nlme)
library(knitr)
library(emmeans)
library(DHARMa)
library(geosphere)
if(!require(multcompView)) install.packages("multcompView")
if(!require(multcomp)) install.packages("multcomp")
options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE))))
####CARGAR DATOS####
getwd()
datos<-read.csv("D:/Carrera/Trabajos Finales/Biometría II/TP_FINAL_BIOME_II/datos/complete_data.csv", header=T)
datos$source<-as.factor(datos$source)
summary(datos)
names(datos)
summary(datos$source)

# Cambiamos los nombres de los frentes
library(dplyr)
datos$source <- recode_factor(datos$source,
                              "bloom" = "transitorio",
                              "talud" = "estable")


#### GRAFICOS EXPLORATORIOS####
plot_chl <- ggplot(datos, aes(x = Chl, y = riqueza, colour = source)) +
  xlab("Chl") +
  ylab("Riqueza") +
  geom_point(size = 4, shape = 21) + 
  theme_bw()

plot + theme(axis.text.x = element_text(size=12),
             axis.text.y = element_text(size=12), 
             axis.title.x = element_text(size = 14), 
             axis.title.y = element_text(size = 14))

plot_sss <- ggplot(datos, aes(x = SSS, y = riqueza, colour = source)) +
  xlab("SSS") +
  ylab("Riqueza") +
  geom_point(size = 4, shape = 21) + 
  theme_bw()

plot_sss + theme(axis.text.x = element_text(size=12),
             axis.text.y = element_text(size=12), 
             axis.title.x = element_text(size = 14), 
             axis.title.y = element_text(size = 14))

plot_gamma <- ggplot(datos, aes(x = gamma, y = riqueza, colour = source)) +
  xlab("gamma") +
  ylab("Riqueza") +
  geom_point(size = 4, shape = 21) + 
  theme_bw()

plot_gamma + theme(axis.text.x = element_text(size=12),
             axis.text.y = element_text(size=12), 
             axis.title.x = element_text(size = 14), 
             axis.title.y = element_text(size = 14))
plot_POC <- ggplot(datos, aes(x = POC, y = riqueza, colour = source)) +
  xlab("POC") +
  ylab("Riqueza") +
  geom_point(size = 4, shape = 21) + 
  theme_bw()

plot_POC + theme(axis.text.x = element_text(size=12),
             axis.text.y = element_text(size=12), 
             axis.title.x = element_text(size = 14), 
             axis.title.y = element_text(size = 14))


plot_sst <- ggplot(datos, aes(x = SST, y = riqueza, colour = source)) +
  xlab("SST") +
  ylab("Riqueza") +
  geom_point(size = 4, shape = 21) + 
  theme_bw()

plot_sst + theme(axis.text.x = element_text(size=12),
                 axis.text.y = element_text(size=12), 
                 axis.title.x = element_text(size = 14), 
                 axis.title.y = element_text(size = 14))
plot_bbp <- ggplot(datos, aes(x = bbp, y = riqueza, colour = source)) +
  xlab("bbp") +
  ylab("Riqueza") +
  geom_point(size = 4, shape = 21) + 
  theme_bw()

plot_bbp + theme(axis.text.x = element_text(size=12),
                 axis.text.y = element_text(size=12), 
                 axis.title.x = element_text(size = 14), 
                 axis.title.y = element_text(size = 14))

####MAPA ESTACIONES####
bathy_data<-getNOAA.bathy(lon1=-70, lon2 = -52, lat1 = -55, lat2 = -45, resolution = 1 )
map_rast <- rast("Temperature.nc")
map_rast_mean<- mean(map_rast)
pdf("D:/Carrera/Trabajos Finales/Biometría II/statation_map.pdf")

plot(map_rast_mean,
     col = "lightblue",
     main = "",
     xlab = "Longitude", ylab = "Latitude",
     xlim=c(-70, -52), ylim =c(-55,-45),
     legend= FALSE
)

# Add isobaths
plot(bathy_data, deep = 0, shallow = 0, lwd=0.6, col="black", add=T)
plot(bathy_data, deep = -200, shallow = -200, lwd = 0.4, col = "black",drawlabels=T, add = TRUE)
plot(bathy_data, deep = -2000, shallow = -2000, lwd = 0.4, col = "black",drawlabels=T, add = TRUE)
points(datos$lon, datos$lat,pch = 20, cex = 1, col = 'red')

dev.off()
####GRÁFICO EXPLORATORIO####
# Paquetes

library(tidyr)
library(patchwork) # opcional, para combinar gráficos

# Suponiendo que tu data.frame se llama:
datos <- datos_distancia

# Variables predictoras (ajustá según tu modelo)
vars_cont <- c("Chl", "SSS", "SST", "POC", "bbp", "gamma")
var_cat <- "source"

# 1️⃣ Gráficos individuales para las variables continuas
plots_cont <- lapply(vars_cont, function(var) {
  ggplot(datos, aes_string(x = var, y = "riqueza")) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 0.8) +
    theme_minimal(base_size = 13) +
    labs(
      x = var,
      y = "Riqueza",
      title = paste("Riqueza vs", var)
    )
})

# 2️⃣ Gráfico para la variable categórica
plot_cat <- ggplot(datos, aes_string(x = var_cat, y = "riqueza", fill = var_cat)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "gray40") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  labs(
    x = var_cat,
    y = "Riqueza",
    title = paste("Riqueza por", var_cat)
  )

# 3️⃣ Mostrar los gráficos en conjunto
# (podés ajustarlo según cuántas variables tengas)
combined_plot <- wrap_plots(c(plots_cont, list(plot_cat)), ncol = 3)
combined_plot

#SEPARADO POR SOURCE
# 1️⃣ Gráficos para las variables continuas, con color por 'source'
plots_cont <- lapply(vars_cont, function(var) {
  ggplot(datos, aes_string(x = var, y = "riqueza", color = var_cat)) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    theme_minimal(base_size = 13) +
    labs(
      x = var,
      y = "Riqueza",
      title = paste("Riqueza vs", var)
    ) +
    theme(legend.position = "bottom")
})

# 2️⃣ Boxplot para la variable categórica 'source'
plot_cat <- ggplot(datos, aes_string(x = var_cat, y = "riqueza", fill = var_cat)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "gray40") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  labs(
    x = var_cat,
    y = "Riqueza",
    title = paste("Riqueza por", var_cat)
  )

# 3️⃣ Combinar todos los gráficos en una grilla
combined_plot <- wrap_plots(c(plots_cont, list(plot_cat)), ncol = 3)
combined_plot

####MODELO####
library(glmmTMB)
modelo<- glm(
  riqueza ~ Chl + SSS + SST + POC + bbp + gamma + source,
  family = poisson,
  data = datos
)

# Resumen del modelo
summary(modelo)

car::vif(modelo)

# Test dispersión y simulación de residuos con DHARMa
testDispersion(modelo, type = "PearsonChisq")

### Sección tamaño de letra
# Guardar configuración original
op <- par()
# Configurar tamaño de letra
par(cex.main = 2,    # Título principal
    cex.lab = 1.3,     # Etiquetas de ejes
    cex.axis = 1.3)    # Números en ejes
### fin Sección tamaño de letra

# 1. Generar los residuales
sim <- simulateResiduals(fittedMod = modelo, plot = T)
# 2. genero  plotResiduals y lo guardo en p
plotResiduals(sim, datos$source)

# Restaurar configuración original (tamaño letra)
par(op)


# Gráfico de cuartiles
testDispersion(modelo, type = "DHARMa")

####DISTANCIA####
# Coordenadas iniciales por localidad
ref_points <- data.frame(
  source = c("bloom", "talud"),
  ref_lat = c(-48.2656, -45.6659),
  ref_lon = c(-63.7595, -59.7381)
)

# Calcular distancia al punto inicial de cada localidad
datos_distancia <- datos %>%
  left_join(ref_points, by = "source") %>%
  mutate(
    dist_m = distHaversine(
      p1 = cbind(ref_lon, ref_lat),
      p2 = cbind(lon, lat)
    ),
    dist_km = dist_m / 1000
  )
  plot_distancia <- ggplot(datos_distancia, aes(x = dist_m, y = riqueza, colour = source)) +
  xlab("Distancia") +
  ylab("Riqueza") +
  geom_point(size = 4, shape = 21) + 
  theme_bw()
plot_distancia+ theme(axis.text.x = element_text(size=12),
                   axis.text.y = element_text(size=12), 
                   axis.title.x = element_text(size = 14), 
                   axis.title.y = element_text(size = 14))
plot(datos_distancia)

ggpairs(datos_distancia[4:11], aes(colour = source))
?ggpairs
vars_num <- datos %>%
  select(where(is.numeric)) %>%
  select(-lon, -lat, -dist_km) %>%
  colnames()

# Calcular media y desviación estándar por source
resumen <- datos %>%
  group_by(source) %>%
  summarise(across(all_of(vars_num),
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Pasar a formato largo para ggplot
resumen_long <- resumen %>%
  pivot_longer(
    cols = -source,
    names_to = c("variable", ".value"),
    names_sep = "_"
  )

# Graficar
ggplot(resumen_long, aes(x = source, y = mean, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Localidad",
    y = "Media ± Desvío estándar",
    fill = "Source"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
?nlme

#MODELO GEE####

install.packages("geepack")
library(geepack)
datos_distancia <- datos_distancia %>%
  arrange(object_id)

m0<-geeglm(formula= riqueza~Chl+SSS+source+gamma+bbp+POC+SST, family = poisson, data=datos_distancia,id=source,corstr = "ar1")

vif(m0)
summary(m0)
testDispersion(m0, type = "PearsonChisq")

### Sección tamaño de letra
# Guardar configuración original
op <- par()
# Configurar tamaño de letra
par(cex.main = 2,    # Título principal
    cex.lab = 1.3,     # Etiquetas de ejes
    cex.axis = 1.3)    # Números en ejes
### fin Sección tamaño de letra

# 1. Generar los residuales
sim <- simulateResiduals(fittedMod = m0, plot = T)
summary(m0)

#PLAN B: IR A UNA NORMAL####
m2 <- nlme(model = riqueza ~ Chl + SSS + source + gamma + bbp + POC + SST,data = datos_distancia,correlation = corCAR1(form = ~ dist_km))

####MODELO 3#######
m3<-gls(model = riqueza ~ Chl+ source + gamma + bbp + SST+SSS,data = datos_distancia, weights = varIdent(form = ~1|source),correlation = corCAR1(form = ~ dist_km|source))
AIC(m3)

m3_tmb_ar1 <- glmmTMB(
  riqueza ~ Chl + SSS + source + gamma + bbp + POC + SST +
    ar1(factor(dist_bin)| source),
  dispformula = ~ source,
  data = datos_distancia,
  family = gaussian()
)
summary(m3)
Anova(m3_tmb_ar1)
drop1(m3_tmb_ar1)
plot(m3_tmb_ar1)
check_model(m3_tmb_ar1)
r<-resid(m3_tmb_ar1, type="pearson")
plot(predict(m3_tmb_ar1),r)
car::qqPlot(r)
summary(m3)
shapiro.test(r)
hist(r)
####CHEQUEO MODELOS DE A UNA VARIABLE####
#SALINIDAD
modelo_SSS<-gls(model = riqueza ~ SSS,data = datos_distancia, weights = varIdent(form = ~1|source),correlation = corCAR1(form = ~ dist_km|source))
summary(modelo_SSS)
plot(modelo_SSS)
check_model(modelo_SSS)
r<-resid(modelo_SSS, type="pearson")
car::qqPlot(r)
shapiro.test(r)
hist(r)
#NOS QUEDAMOS CON EL M3 CON VARIDENT y CORCAR1. 
AIC(m3)
car::Anova(m3)
m4<-gls(model = riqueza ~ Chl + SSS + source + gamma + bbp+ SST,data = datos_distancia, weights = varIdent(form = ~1|source),correlation = corCAR1(form = ~ dist_km|source))
summary(m4)
m5<-gls(model = riqueza ~ Chl + SSS+source + gamma+bbp,data = datos_distancia, weights = varIdent(form = ~1|source),correlation = corCAR1(form = ~ dist_km|source))
AIC(m3,m4,m5) #EL m4 Es el de menor AIC, HASTA PROBANDO SACANDO MÁS VARIABLES

####PLANTEO MODELOS CON INTERACCIÓN####
m4_con_int<-gls(model = riqueza ~ Chl+(SSS+ gamma +bbp+SST) * source ,data = datos_distancia, weights = varIdent(form = ~1|source),correlation = corCAR1(form = ~ dist_km|source)) #NOS QUEDAMOS CON ESTE MODELO POR AHORA AIC=327
AIC(m4_con_int)
check_model(m4_con_int)
BIC()
anova(m4, m4_con_int)
plot(m4_con_int)

check_model(m4_con_int)
r<-resid(m4_con_int, type="pearson")
qqPlot(r)
plot(m4_con_int$fitted, r)
summary(m4_con_int)
shapiro.test(r)
hist(r)

compsimples<-emmeans(m4_con_int, pairwise~source|Chl)
summary(compsimples)

plot(compsimples, comparisons = T)
pred_modelo<-as.data.frame(compsimples$emmeans)
ggplot(pred_modelo, aes(x = Chl, y = emmean, fill = source)) +
  geom_point(aes(colour = source)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, colour = source), width = 0.2) +
  labs(x = "Tiempo") + labs(y = "Biomasa microbiana (μg C/g sustrato)") +
  ggtitle("Comparación de Biomasa microbiana en el tiempo para cada tratamiento")

m5_sin_int<-gls(model = riqueza ~ Chl +gamma+SST+SSS+bbp+source,data = datos_distancia, weights = varIdent(form = ~1|source),correlation = corLin(form = ~ dist_km|source))
m5_con_int<-gls(model = riqueza ~ (Chl +gamma+SST+SSS+bbp)*source,data = datos_distancia, weights = varIdent(form = ~1|source),correlation = corLin(form = ~ dist_km|source))
?gls
anova(m4, m5_sin_int, m4_con_int, m5_con_int)
AIC(m5_con_int)
summary(m5_con_int)

install.packages("randomForest")  # Solo la primera vez
library(randomForest)
# Convertir 'source' a factor si es categórica
datos_distancia$source <- as.factor(datos_distancia$source)

# Entrenamiento del modelo Random Forest
set.seed(123)  # Para reproducibilidad
rf_model <- randomForest(
  riqueza ~ SSS + gamma + bbp + SST + source,
  data = datos_distancia,
  ntree = 500,      # número de árboles
  mtry = 3,         # número de variables seleccionadas por división
  importance = TRUE # para medir la importancia de las variables
)

# Ver resumen del modelo
print(rf_model)



# Mostrar importancia
importance(rf_model)

# Visualizar
varImpPlot(rf_model)

pred_rf <- predict(rf_model, newdata = datos_distancia)
# Comparar con los valores observados
plot(datos_distancia$riqueza, pred_rf,
     xlab = "Observado", ylab = "Predicho", main = "Random Forest")
abline(0, 1, col = "red")

####HACEMOS MODELOS DE A UNA VARIABLE####
