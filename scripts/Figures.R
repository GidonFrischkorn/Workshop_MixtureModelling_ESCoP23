library(ggplot2)
library(brms)
library(plotly)
library(tidyverse)
# clean up plots
clean_plot <- function(...){
  clean_plot <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line.x = element_line(color = 'black'),
                      axis.line.y = element_line(color = 'black'),
                      legend.key = element_rect(fill = 'white'),
                      text = element_text(size = 15),
                      line = element_line(linewidth = 1),
                      axis.ticks = element_line(linewidth = 1),
                      ...)
  
  return(clean_plot)
}

# settings for mixture model plot
mus <- c(0,0)
kappas <- c(5,0)
weights <- c(2)
probs <- utilities::softmax(weights)
sum(probs)

ggplot() +
  xlim(c(-pi,pi)) +
  ylim(c(0, dvon_mises(0, mu = mus[1], kappa = kappas[1]))) +
  geom_function(fun = dvon_mises, args = list(mu = mus[1], kappa = kappas[1]), color = "blue") +
  annotate(geom = "text",label = "Precision", x = 0, y = 0.2, size = 6) +
  annotate("segment", x = -0.7, xend = 0.7, 
           y = dvon_mises(-0.7, mu = mus[1], kappa = kappas[1]),
           yend = dvon_mises(0.7, mu = mus[1], kappa = kappas[1]),
           arrow = arrow(ends = "both", angle = 40, length = unit(.2,"cm"))) +
  clean_plot(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot() +
  xlim(c(-pi,pi)) +
  ylim(c(0, dvon_mises(0, mu = mus[1], kappa = kappas[1]))) +
  geom_function(fun = dvon_mises, args = list(mu = mus[2], kappa = kappas[2]), color = "red") +
  clean_plot(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# weighted density function for the vonMises distribution
weighted_vonMises_density <- function(x, mu, kappa, log = F, weight = 1){
  weighted_vonMises_density <- dvon_mises(x = x,mu = mu , kappa = kappa, log = log) * weight
  return(weighted_vonMises_density)
}

# sum of three von Mises densities for plotting the mixture density
sum_vonMises_density <- function(x,mus,kappas,log = F, probs) {
  sum_density <- dvon_mises(x, mu = mus[1], kappa = kappas[1]) * probs[1] +
    dvon_mises(x, mu = mus[2], kappa = kappas[2]) * probs[2]
  return(sum_density)
}

ggplot() +
  xlim(c(-pi,pi)) +
  ylim(c(0, dvon_mises(0, mu = mus[1], kappa = kappas[1]))) +
  stat_function(fun = sum_vonMises_density,
                args = list(mus = mus, kappas = kappas, probs = probs), 
                lwd = 1.5) +
  clean_plot(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

# 2) illustrate link functions

ggplot() +
  geom_function(fun = exp) +
  xlim(-3,3) +
  labs(x = "parameter space", y = "native scale") +
  clean_plot() +
  annotate(geom = "segment",
           x = 2.81, xend = 2.81,
           y = 0, yend = exp(2.81),
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"), type = "closed")) +
  annotate(geom = "segment",
           x = 2.81, xend = -3,
           y = exp(2.81), yend = exp(2.81),
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"), type = "closed")) +
  annotate(geom = "text",
           label = "Par. est = 2.81",
           x = 2.1, y = 0.5) +
  annotate(geom = "text",
           label = paste("Value on native Scale =",round(exp(2.81),1)),
           x = -2, y =18)

ggplot() +
  geom_function(fun = inv_logit_scaled) +
  xlim(-5,5) +
  labs(x = "Theta", y = "Probabilitiy") +
  clean_plot() +
  annotate(geom = "segment",
           x = 3.37, xend = 3.37,
           y = 0, yend = inv_logit_scaled(3.37),
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"), type = "closed")) +
  annotate(geom = "segment",
           x = 3.37, xend = -5,
           y = inv_logit_scaled(3.37), yend = inv_logit_scaled(3.37),
           arrow = arrow(ends = "last", angle = 30, length = unit(.2,"cm"), type = "closed")) +
  annotate(geom = "text",
           label = "Par. est = 3.37",
           x = 2, y = 0.1) +
  annotate(geom = "text",
           label = paste("Value on native Scale =",round(inv_logit_scaled(3.37),2)),
           x = -3, y = 0.9)


# 3 dimensional plot of the softmax
x1 <- seq(from = -6, to = 4, by = 0.1)
x2 <- seq(from = -6, to = 8, by = 0.1)

df_softmax <- expand.grid(
  x1 = x1,
  x2 = x2
) %>% 
  rowwise() %>% 
  mutate(p1 = exp(x1)/(sum(exp(c(x1,x2,0)))),
         p2 = exp(x2)/(sum(exp(c(x1,x2,0)))),
         p3 = exp(0)/(sum(exp(c(x1,x2,0)))))


fig <- plot_ly(data = df_softmax, x = ~x1, y = ~x2, z = ~p1, color = ~p2) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Theta 1'),
                      yaxis = list(title = 'Theta 2'),
                      zaxis = list(title = 'Prob 1')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Miles/(US) gallon',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
fig
