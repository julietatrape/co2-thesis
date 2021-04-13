library(ggplot2)
library(dplyr)

# Load experimental data
Cut333 <- read.delim("Cut-333.15K", header=T)

# Set plot's legend
Type <- c('Experimental value', 'Simulation')
Type2 <- c(1,2)
x <- c(-2,-3)
y <- c(-2,-3)
data <- data.frame(x,y,Type,Type2)

# Plot with ggplot
ggplot() +  geom_line(data=data, aes(x=x, y=y,linetype=Type, color=Type2),size=0.4,color='black') + 
          geom_line(data=Cut333, aes(x=Cutoff..sigma.units., y=Experimental.density..kg.m..),linetype='dashed',size=0.5,color='black') +
          geom_line(data=Cut333, aes(x=Cutoff..sigma.units., y=Density..kg.m..),size=0.5,color='#FC4E07')  +        
          geom_point(data=Cut333, aes(x=Cutoff..sigma.units., y=Density..kg.m..),size=2.5,color='#FC4E07', shape=21, fill='lightsalmon')  +
          geom_point(aes(x=2.393, y=1035.77),size=2.5,color='black', shape=21, fill='lightsalmon')  +
          geom_segment(aes(x = 2.393, y = 850, xend = 2.393, yend = 1000), arrow = arrow(length = unit(0.1, "cm"))) +             labs(x=expression(paste('Cutoff (',sigma,')')), y=expression(paste('Density (',kg/m^3,')'))) +
  annotate("text", x = 2.393, y = 820, label = expression(paste("7 ", ring(A))), size=3.5, color='black', fontface='bold') +
              theme(panel.grid.major  = element_line(color = 'black', linetype="dotted", size=0.2), panel.background = element_rect(fill = "white"), 
              panel.border = element_rect(color = "black", fill = NA, size=0.8),  axis.line = element_line(color = "black", size=0), 
              axis.title =  element_text(color = "black", size=12), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=12),
              legend.position = c(.8, .38), legend.background = element_rect(color = "black", size = 0.4, linetype = "solid"),legend.title = element_blank(), legend.text = element_text(color = "black", size=8.5))  + 
                scale_y_continuous(limits=c(0,1200), expand=c(0,0)) + scale_x_continuous(limits=c(0.95,4.05),expand=c(0,0), breaks = seq(1, 4, by = 1)) + 
                  scale_linetype_manual(values=c('dashed', 'solid')) + scale_color_manual(values=c('black', 'purple'))


