library(ggplot2)
library(ggpubr)
library(scales)

# Data from CO2 simulations with potential TraPPE
LiqTrappe <- read.table("Liq-2GPa-350K-TraPPE", quote="\"")
ITrappe <- read.table("I-2GPa-350K-TraPPE", quote="\"")

# Data from CO2 simulations with potential OPLS
LiqOPLS <- read.table("Liq-2GPa-350K-OPLS", quote="\"")
IOPLS <- read.table("I-2GPa-350K-OPLS", quote="\"")

# Data from CO2 simulations with Neural Network potential 
LiqNN <- read.table("Liq-2GPa-350K-NN", quote="\"")
INN <- read.table("I-2GPa-350K-NN", quote="\"")

# Transparent colors: color = color name, percent = % transparency, name = name for the color
t_col <- function(color, percent = 50, name = NULL) {
  # Get RGB values for named color
  rgb.val <- col2rgb(color)
  # Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  # Save the color
  invisible(t.col)
}

# Create some transparent colors
lred <- t_col("coral", perc = 70, name = "lred")
lblue <- t_col("deepskyblue2", perc = 80, name = "lblue")

# Make plot of Q6 with potential Trappe
q6_trappe <- ggplot() + geom_density(data=LiqTrappe,aes(x=V4), fill=lred, color='orangered', size=0.5) + geom_density(data=ITrappe,aes(x=V4), fill=lblue, color='dodgerblue1', size=0.5) + 
  labs(x=expression(Q[6]), y="Probability") + 
  theme(plot.margin = margin(0.5, 1, 0.3, 1, "cm"),text=element_text(family="Times"),panel.grid.major  = element_line(size=0.0), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, size=1),  axis.line = element_line(color = "black", size=0), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=10), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()) + 
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0), limits=c(0,0.601),labels = comma_format(big.mark = ".",decimal.mark = ","))  

# Make plot of k with potential Trappe
k_trappe <- ggplot() + geom_density(data=LiqTrappe,aes(x=V2), fill=lred, color='orangered', size=0.5) + geom_density(data=ITrappe,aes(x=V2), fill=lblue, color='dodgerblue1', size=0.5) + 
  labs(x=expression(k), y="TraPPE") + 
  theme(plot.margin = margin(0.5, 1, 0.2, 1, "cm"),text=element_text(family="Times"),panel.grid.major  = element_line(size=0.0), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, size=1),  axis.line = element_line(color = "black", size=0), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=10), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_text(color = "black", size=14,face='bold')) + 
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0), limits=c(0,1),labels = comma_format(big.mark = ".",decimal.mark = ",")) 


# Make plot of Q6 with potential OPLS
q6_opls <- ggplot() + geom_density(data=LiqOPLS,aes(x=V4),  fill=lred, color='orangered', size=0.5) + geom_density(data=IOPLS,aes(x=V4), fill=lblue, color='dodgerblue2', size=0.5) + 
  labs(x=expression(Q[6]), y="Probability") + 
  theme(plot.margin = margin(0.45, 1, 0.3, 1, "cm"),text=element_text(family="Times"),panel.grid.major  = element_line(size=0.0), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, size=1),  axis.line = element_line(color = "black", size=0), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=10), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()) + 
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0), limits=c(0,0.601),labels = comma_format(big.mark = ".",decimal.mark = ",")) 
  
# Make plot of k with potential OPLS
k_opls <- ggplot() + geom_density(data=LiqOPLS,aes(x=V2), fill=lred, color='orangered', size=0.5) + geom_density(data=IOPLS,aes(x=V2), fill=lblue, color='dodgerblue2', size=0.5) + 
  labs(x=expression(tilde(k)[chi[o]](chi)), y="OPLS") + 
  theme(plot.margin = margin(0.5, 1, 0.2, 1, "cm"),text=element_text(family="Times"),panel.grid.major  = element_line(size=0.0), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, size=1),  axis.line = element_line(color = "black", size=0), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=10), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_text(color = "black", size=14,face='bold')) + 
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0), limits=c(0,1),labels = comma_format(big.mark = ".",decimal.mark = ",")) 
  
# Make plot of Q6 with Neural Network potential
q6_nn <- ggplot() + geom_density(data=LiqNN,aes(x=V4), fill=lred, color='orangered', size=0.5) + geom_density(data=INN,aes(x=V4), fill=lblue, color='dodgerblue2', size=0.5) + 
  labs(x=expression(Q[6]), y="Probability") + 
  theme(plot.margin = margin(0.4, 1, 0.5, 1, "cm"),text=element_text(family="Times"),panel.grid.major  = element_line(size=0.0), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, size=1),  axis.line = element_line(color = "black", size=0), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=14), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank()) + 
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0), limits=c(0,0.601),labels = comma_format(big.mark = ".",decimal.mark = ",")) 

# Make plot of k with Neural Network potential
k_nn <- ggplot() + geom_density(data=LiqNN,aes(x=V2), fill=lred, color='orangered', size=0.5) + geom_density(data=INN,aes(x=V2), fill=lblue, color='dodgerblue2', size=0.5) + 
  labs(x=expression(tilde(k)[chi[o]](chi)), y="NN") + 
  theme(plot.margin = margin(0.5, 1, 0.2, 1, "cm"),text=element_text(family="Times"),panel.grid.major  = element_line(size=0.0), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, size=1),  axis.line = element_line(color = "black", size=0), axis.ticks = element_line(color = "black", size=0.5), axis.text = element_text(color = "black", size=14), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_text(color = "black", size=14,face='bold')) + 
  scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0), limits=c(0,1),labels = comma_format(big.mark = ".",decimal.mark = ",")) 
  
# Make plot with the 3 subplots of k
k <- ggarrange(k_trappe+rremove("x.title")+rremove("x.text"),k_opls+rremove("x.title")+rremove("x.text"),k_nn+rremove("x.title"),heights = c(2,2,2.2),ncol=1,nrow = 3) 
k <- annotate_figure(k,
                bottom=text_grob(expression(tilde(k)[chi[o]](chi)),face = "bold",hjust = 1,x=0.54,size = 14,family="Times"))

# Make plot with the 3 subplots of Q6
q6 <- ggarrange(q6_trappe+rremove("x.title")+rremove("x.text"),q6_opls+rremove("x.title")+rremove("x.text"),q6_nn+rremove("x.title"),heights = c(2,2,2.2),ncol=1,nrow = 3)
q6 <- annotate_figure(q6,
                bottom=text_grob(expression(Q[6]),face = "bold",hjust = 1,x=0.51,size = 14,family="Times"))

# Make plot with all 6 subplots
ggarrange(k,q6,ncol=2,nrow = 1,heights = c(2,1.6))
