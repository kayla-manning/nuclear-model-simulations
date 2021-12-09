library(tidyverse)
library(extrafont)
# font_import()

# creating Unif(0,100) plot to represent \mu_{a,j}

img_name <- 'graphs/unif_0_100_pdf.png'
png(img_name, width = 634, height = 316)
data.frame(x = c(-4, 104)) %>% 
  ggplot(aes(x)) +
  stat_function(fun = dunif, n = 101, 
                args = list(0, 100))  +
  
  theme_classic(base_size = 12) +
  labs(x = 'Value',
       y = 'Probability of drawing the value',
       title = 'Unif(0, 100) Probability Density Function') +
  scale_x_continuous(limits = c(-10, 110),
                     breaks = seq(-10, 110, 20)) +
  scale_y_continuous(limits = c(-0.001, 0.011),
                     breaks = seq(0, 0.01, 0.005)) +
  geom_vline(aes(xintercept = 50, color = 'Mean'), 
             linetype='dotted') +
  scale_color_manual(name = 'Values',
                     values = c(Mean = '#1b9e77')) +
  theme(text=element_text(family="Times New Roman", size=12))
dev.off()

# creating Unif(25, 75) plot for illustrative purposes

img_name <- 'graphs/unif_25_75_pdf.png'
png(img_name, width = 634, height = 316)
data.frame(x = c(-4, 104)) %>% 
  ggplot(aes(x)) +
  stat_function(fun = dunif, n = 101, 
                args = list(25, 75))  +
  theme_classic(base_size = 12) +
  labs(x = 'Value',
       y = 'Probability of drawing the value',
       title = 'Unif(25, 75) Probability Density Function') +
  geom_vline(aes(xintercept = 50, color = 'Mean'), 
             linetype='dotted') +
  scale_color_manual(name = 'Values',
                     values = c(Mean = '#1b9e77')) +
  theme(text=element_text(family="Times New Roman", size=12))
dev.off()

# creating N(50,400) plot

img_name <- 'graphs/n_50_400_pdf.png'
png(img_name, width = 634, height = 316)
data.frame(x = c(-4, 104)) %>% 
  ggplot(aes(x)) +
  stat_function(fun = dnorm, n = 101, 
                args = list(50, 20))  +
  
  theme_classic(base_size = 12) +
  labs(x = 'Value',
       y = 'Probability of drawing the value',
       title = 'N(50, 400) Probability Density Function') +
  geom_vline(aes(xintercept = 70, color = '+-1SD'), linetype = 'dashed') +
  geom_vline(aes(xintercept = 30, color = '+-1SD'), linetype = 'dashed') +
  geom_vline(aes(xintercept = 50, color = 'Mean'), linetype = 'dotted') +
  geom_vline(aes(xintercept = 10, color = '+-2SD'), linetype = 'dotted') +
  geom_vline(aes(xintercept = 90, color = '+-2SD'), linetype = 'dotted') +
  scale_x_continuous(breaks = seq(10, 100, 20)) +
  scale_color_manual(name = 'Values',
                     values = c('+-1SD'='#1b9e77', 
                                Mean='#d95f02',
                                '+-2SD'='#7570b3')) +
  theme(text=element_text(family="Times New Roman", size=12))
dev.off()

# creating Cauchy(50, 50) plot

img_name <- 'graphs/cauchy_50_50_pdf.png'
png(img_name, width = 634, height = 316)
data.frame(x = c(-100, 200)) %>% 
  ggplot(aes(x)) +
  stat_function(fun = dcauchy, n = 101, 
                args = list(50, 50),
                aes(color = 'Cauchy'))  +
  stat_function(fun = dnorm, n = 101, 
                args = list(50, 50),
                aes(color = 'Normal')) +
  theme_classic(base_size = 12) +
  labs(x = 'Value',
       y = 'Probability of drawing the value',
       title = 'Cauchy(50, 50) Probability Density Function',
       subtitle = 'compared to a N(50, 2500) distribution') +
  geom_vline(aes(xintercept = 50, color = 'Mean'), linetype = 'dotted') +
  scale_x_continuous(breaks = seq(-100, 200, 50)) +
  scale_color_manual(name = 'Values', values = c(Cauchy = '#1b9e77',
                                                 Normal = 'gray',
                                                 Mean = '#d95f02')) +
  theme(text=element_text(family="Times New Roman", size=12))

dev.off()

