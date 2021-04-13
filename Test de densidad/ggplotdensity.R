library(ggplot2)
library(dplyr)

# Experimental data
Exp303 <- read.delim("~/Escritorio/Tesis/Figuras/Test de densidad/Exp-303.15K", header=T)
Exp333 <- read.delim("~/Escritorio/Tesis/Figuras/Test de densidad/Exp-333.15K", header=T)
Exp373 <- read.delim("~/Escritorio/Tesis/Figuras/Test de densidad/Exp-373.15K", header=T)

Exp303_vapor <- filter(Exp303, Phase=='vapor')
Exp303_liquid <- filter(Exp303, Phase=='liquid')
Exp333_vapor <- filter(Exp333, Phase=='vapor')
Exp333_supercritical <- filter(Exp333, Phase=='supercritical')
Exp373_vapor <- filter(Exp373, Phase=='vapor')
Exp373_supercritical <- filter(Exp373, Phase=='supercritical')

#Simulations data
Sim303 <- read.delim("~/Escritorio/Tesis/Figuras/Test de densidad/Sim-303.15K", header=T)
Sim333 <- read.delim("~/Escritorio/Tesis/Figuras/Test de densidad/Sim-333.15K", header=T)
Sim373 <- read.delim("~/Escritorio/Tesis/Figuras/Test de densidad/Sim-373.15K", header=T)

Sim303_TraPPE <- filter(Sim303, Potencial=='TraPPE')
Sim303_NN <- filter(Sim303, Potencial=='NN')
Sim333_TraPPE <- filter(Sim333, Potencial=='TraPPE')
Sim333_NN <- filter(Sim333, Potencial=='NN')
Sim373_TraPPE <- filter(Sim373, Potencial=='TraPPE')
Sim373_NN <- filter(Sim373, Potencial=='NN')

FF <- c('TraPPE', 'NN')
Phase <- c('Liquid / Supercritical', 'Vapor')
x <- c(-2,-3)
y <- c(-2,-3)
data <- data.frame(x,y,Phase,FF)

# Plot with legend - English
ggplot() + geom_line(data=data, aes(x=x, y=y,linetype=Phase),size=0.4,color='black') + 
          geom_point(data=data, aes(x=x, y=y,shape=FF),size=1.5,color='black') + 
          geom_line(data=Exp303_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='purple',linetype = "dashed") + 
          geom_line(data=Exp303_liquid, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='purple') + 
          geom_point(data=Sim303_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='purple', shape=23) + 
          geom_point(data=Sim303_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='purple', shape=21) + 
          geom_line(data=Exp333_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='darkorange1',linetype = "dashed") + 
          geom_line(data=Exp333_supercritical, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='darkorange1') + 
          geom_point(data=Sim333_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='darkorange1', shape=21) + 
          geom_point(data=Sim333_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,,color='black', fill='darkorange1', shape=23) + 
          geom_line(data=Exp373_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='chartreuse3',linetype = "dashed") + 
          geom_line(data=Exp373_supercritical, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='chartreuse3') + 
          geom_point(data=Sim373_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='chartreuse3', shape=21) + 
          geom_point(data=Sim373_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='chartreuse3', shape=23) + 
            labs(x='Pressure (bar)', y=expression(paste('Density (',kg/m^3,')'))) +
              theme(panel.grid.major  = element_line(color = 'black', linetype="dotted", size=0.2), panel.background = element_rect(fill = "white"), 
              panel.border = element_rect(color = "black", fill = NA, size=0.8),  axis.line = element_line(color = "black", size=0), 
              axis.title =  element_text(color = "black", size=15), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=15),
              legend.position = c(.8, .3), legend.background = element_rect(color = "black", size = 0.4, linetype = "solid"),legend.title = element_blank(), legend.text = element_text(color = "black", size=14))  + 
                scale_y_continuous(limits=c(0,1200), expand=c(0,0)) + scale_x_continuous(limits=c(0,1170),expand=c(0,0), breaks = seq(0, 1000, by = 200)) +
                  annotate("text", x = 1090, y = 1135, label = "303.15 K", size=4, color='purple', fontface='bold') +
                  annotate("text", x = 1090, y = 1065, label = "333.15 K", size=4, color='darkorange1', fontface='bold') +
                  annotate("text", x = 1090, y = 985, label = "373.15 K", size=4, color='chartreuse3', fontface='bold')+
                  annotate("rect", xmin = 590, xmax = 1010, ymin = 1180, ymax = 800, alpha = .1) + 
                    scale_shape_manual(values=c(23, 21)) + scale_linetype_manual(values=c('solid', 'dashed'))


# Plot with legend - Spanish
FF <- c('TraPPE', 'NN')
Phase <- c('Líquido / Supercrítico', 'Vapor')
x <- c(-2,-3)
y <- c(-2,-3)
data <- data.frame(x,y,Phase,FF)

ggplot() + geom_line(data=data, aes(x=x, y=y,linetype=Phase),size=0.4,color='black') + 
  geom_point(data=data, aes(x=x, y=y,shape=FF),size=1.5,color='black') + 
  geom_line(data=Exp303_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='purple',linetype = "dashed") + 
  geom_line(data=Exp303_liquid, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='purple') + 
  geom_point(data=Sim303_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='purple', shape=23) + 
  geom_point(data=Sim303_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='purple', shape=21) + 
  geom_line(data=Exp333_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='darkorange1',linetype = "dashed") + 
  geom_line(data=Exp333_supercritical, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='darkorange1') + 
  geom_point(data=Sim333_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='darkorange1', shape=21) + 
  geom_point(data=Sim333_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,,color='black', fill='darkorange1', shape=23) + 
  geom_line(data=Exp373_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='chartreuse3',linetype = "dashed") + 
  geom_line(data=Exp373_supercritical, aes(x=Pressure..bar., y=Density..kg.m3.),size=1,color='chartreuse3') + 
  geom_point(data=Sim373_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='chartreuse3', shape=21) + 
  geom_point(data=Sim373_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=3,color='black', fill='chartreuse3', shape=23) + 
  labs(x='Presión (bar)', y=expression(paste('Densidad (',kg/m^3,')'))) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),panel.grid.major  = element_line(color = 'black', linetype="dotted", size=0.2), panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size=0.8),  axis.line = element_line(color = "black", size=0), 
        axis.title =  element_text(color = "black", size=15), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=15),
        legend.position = c(.8, .3), legend.background = element_rect(color = "black", size = 0.4, linetype = "solid"),legend.title = element_blank(), legend.text = element_text(color = "black", size=14))  + 
  scale_y_continuous(limits=c(0,1200), expand=c(0,0)) + scale_x_continuous(limits=c(0,1200),expand=c(0,0), breaks = seq(0, 1200, by = 200)) +
  annotate("text", x = 1090, y = 1135, label = "303,15 K", size=5, color='purple', fontface='bold', family="Times") +
  annotate("text", x = 1090, y = 1065, label = "333,15 K", size=5, color='darkorange1', fontface='bold', family="Times") +
  annotate("text", x = 1090, y = 985, label = "373,15 K", size=5, color='chartreuse3', fontface='bold', family="Times")+
  annotate("rect", xmin = 590, xmax = 1010, ymin = 1180, ymax = 800, alpha = .1) + 
  scale_shape_manual(values=c(23, 21)) + scale_linetype_manual(values=c('solid', 'dashed')) + theme(text=element_text(family="Times"))


# Plot without legend
ggplot() + geom_line(data=Exp303_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=0.7,color='purple',linetype = "dashed") + 
  geom_line(data=Exp303_liquid, aes(x=Pressure..bar., y=Density..kg.m3.),size=0.7,color='purple') + 
  geom_point(data=Sim303_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=1.5,color='purple', shape=1) + 
  geom_point(data=Sim303_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=1.5,color='purple', shape=8) + 
  geom_line(data=Exp333_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=0.7,color='darkorange1',linetype = "dashed") + 
  geom_line(data=Exp333_supercritical, aes(x=Pressure..bar., y=Density..kg.m3.),size=0.7,color='darkorange1') + 
  geom_point(data=Sim333_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=1.5,color='darkorange1', shape=1) + 
  geom_point(data=Sim333_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=1.5,color='darkorange1', shape=8) + 
  geom_line(data=Exp373_vapor, aes(x=Pressure..bar., y=Density..kg.m3.),size=0.7,color='chartreuse3',linetype = "dashed") + 
  geom_line(data=Exp373_supercritical, aes(x=Pressure..bar., y=Density..kg.m3.),size=0.7,color='chartreuse3') + 
  geom_point(data=Sim373_TraPPE, aes(x=Presion..bar., y=Densidad..kg.m3.),size=1.5,color='chartreuse3', shape=1) + 
  geom_point(data=Sim373_NN, aes(x=Presion..bar., y=Densidad..kg.m3.),size=1.5,color='chartreuse3', shape=8) + 
  labs(x='Pressure (bar)', y=expression(paste('Density (',kg/m^3,')'))) +
  theme(panel.grid.major  = element_line(color = 'black', linetype="dotted", size=0.2), panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, size=0.8),  axis.line = element_line(color = "black", size=0), 
        axis.title =  element_text(color = "black", size=12), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=12),
        legend.position = c(.85, .35), legend.background = element_rect(color = "black", size = 0.4, linetype = "solid"),legend.title = element_blank(), legend.text = element_text(color = "black", size=8.5))  + 
  scale_y_continuous(limits=c(0,1200), expand=c(0,0)) + scale_x_continuous(limits=c(0,1170),expand=c(0,0), breaks = seq(0, 1000, by = 200)) +
  annotate("text", x = 1090, y = 1135, label = "303.15 K", size=3.5, color='purple', fontface='bold') +
  annotate("text", x = 1090, y = 1065, label = "333.15 K", size=3.5, color='darkorange1', fontface='bold') +
  annotate("text", x = 1090, y = 985, label = "373.15 K", size=3.5, color='chartreuse3', fontface='bold')+
  annotate("rect", xmin = 590, xmax = 1010, ymin = 1180, ymax = 800, alpha = .1) 

