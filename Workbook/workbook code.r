setwd("D:/UofA/W2/Workbook")
mn <- 10
x <- seq(from = 0, to = 20, by = 0.2)
y <- dnorm(x, mean = mn, sd = 2, log = FALSE)

dat <- data.frame(x,y)

ggplot(data = dat, aes(y = y, x = x))+ geom_line() +
geom_vline(xintercept = mn, linetype = "dashed") +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
scale_x_continuous(breaks = c(mn), labels = c(substitute(mu))) +
scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5), labels = c(rep("",6))) +
labs(x = "y", y = "Probability Density")

ggsave("CRDnullNormDensity.png", width = 10, height = 6, units = "cm")

x <- seq(from = -10, to = 20, by = 0.2)

mn1 <- 4
y1 <- dnorm(x, mean = mn1, sd = 2, log = FALSE)
mn2 <- 6
y2 <- dnorm(x, mean = mn2, sd = 2, log = FALSE)
mn3 <- 0
y3 <- dnorm(x, mean = mn3, sd = 2, log = FALSE)
mn4 <- 12.5
y4 <- dnorm(x, mean = mn4, sd = 2, log = FALSE)

mn <- c(mn1, mn2, mn3, mn4)
dat <- data.frame(x, y1, y2, y3, y4)

ggplot(data = dat, aes(y = y1, x = x))+ geom_line() +
 geom_line(aes(y = y2, x = x)) +
  geom_line(aes(y = y3, x = x)) +
   geom_line(aes(y = y4, x = x)) +
geom_vline(xintercept = mn, linetype = "dashed") +
theme_bw()  + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
scale_x_continuous(breaks = c(mn), labels = c(expression(mu[A]),
expression(mu[B]), expression(mu[C]), expression(mu[D]))) +
scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5), labels = c(rep("",6))) +
labs(x = "y", y = "Probability Density")

ggsave("CRDAlternativeNormDensity.png", width = 10, height = 6, units = "cm")
