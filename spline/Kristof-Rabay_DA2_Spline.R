library(tidyverse)
library(ggplot2)
library(lspline)
library(viridis)

x <- c(0:100)

y <- ifelse(x < 20, x*0.2+10 + rnorm(x),
             ifelse(x < 40,  x*0.7 + rnorm(x),
                    ifelse(x <= 100, 27 + rnorm(x), 0)))

data <- data.frame(x,y)

regr <- lm(y ~ x, data = data)
regr_spl <- lm(y ~ lspline(x, knots = c(20,40)), data = data)
data$linear_regression <- predict(regr)
data$spline <- predict(regr_spl)

View(data)

colors <- c("Linear regression"="#95D840FF","Linear piecewise spline"="#33638DFF","Lowess"="#55C667FF")
line_types <- c("Linear regression"="longdash","Linear piecewise spline"="solid", "Lowess"="solid")

options(digits=2)

data %>% ggplot(aes(x=x,y=y)) +
  geom_jitter(size = 0.3) +
  geom_line(aes(x=x, y=linear_regression, color = "Linear regression", linetype = "Linear regression")) +
  geom_line(aes(x=x, y=spline, color = "Linear piecewise spline", linetype = "Linear piecewise spline"), size = 1.2) +
  geom_smooth(aes(x=x, y=y, color = "Lowess", linetype = "Lowess"), se = F, size = 0.7) +
  geom_vline(xintercept = 20, lwd = 0.5, alpha = 0.5, linetype = 'dotted') +
  geom_vline(xintercept = 40, lwd = 0.5, alpha = 0.5, linetype = 'dotted') +
  labs(title = 'Piecewise linear spline with 2 knots',
       subtitle = 'Y corresponds to X unit changes differently in 3 ranges of X',
       x = 'X',
       y = 'Y') + 
  geom_text(x = 8, y = 17, label = paste0("beta[x<=20]==",regr_spl$coefficients[[2]][1]), parse = T, size = 3.5, color = "#33638DFF") +
  geom_text(x = 22, y = 25, label = paste0("beta[x<=40]==",regr_spl$coefficients[[3]][1]), parse = T, size = 3.5,color = "#33638DFF") +
  geom_text(x = 70, y = 31, label = paste0("beta[x>40]==",regr_spl$coefficients[[4]][1]), parse = T, size = 3.5,color = "#33638DFF") +
  geom_text(x = 50, y = 20, label = paste0("beta[x]==",regr$coefficients[2]), parse = T, size = 3.5,color = "#95D840FF") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(5,33, by = 5), limits = c(5,33)) +
  scale_color_manual(name="X-Y relationship",values=colors) +
  scale_linetype_manual(name="X-Y relationship",values=line_types) +
  theme_classic()+
  theme(
    legend.position = c(0.95, .4),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.background = element_rect(colour = "transparent", fill = "transparent")
    )


ggsave("C:/Users/Krisz/Desktop/ceu/materials/fall_2/DA2/Kristof-Rabay_DA2_Spline.png", plot = last_plot(), device = 'png')


