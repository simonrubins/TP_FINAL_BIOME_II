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
if(!require(multcompView)) install.packages("multcompView")
if(!require(multcomp)) install.packages("multcomp")
options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE))))
####CARGAR DATOS####
datos<-read.csv("complete_data.csv", header=T)
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

