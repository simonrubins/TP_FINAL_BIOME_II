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
if(!require(multcompView)) install.packages("multcompView")
if(!require(multcomp)) install.packages("multcomp")
options(emmeans= list(emmeans = list(infer = c(TRUE, F)),
                      contrast = list(infer = c(TRUE, TRUE))))

datos<-read.csv("complete_data.csv", header=T)
datos$source<-as.factor(datos$source)
summary(datos)

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
