
library(agricolae)
library(ggplot2)
library(emmeans)
#library(asreml)
library(asremlPlus)
library(gridExtra)
install.packages("ggpubr")
library(ggpubr)
library(dplyr)

######################################################################
# Example 1
######################################################################

dat <- read.csv("example1.csv")

str(dat)

dat$trt <- factor(dat$trt)

ggplot(data = dat, mapping = aes(x = trt, y = RL)) + 
geom_boxplot() +
theme_bw()

######################################################################
#CRD analysis

dat.aov <- aov(RL ~ trt, data = dat)            # fitting the model

shapiro.test(dat.aov$residuals)
resplt(dat.aov)

summary(dat.aov)
anova(dat.aov)

library(emmeans)


pred.out <- emmeans(dat.aov, "trt")

str(pred.out)
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tk.out <- HSD.test(dat.aov, trt = "trt", 
                    console = TRUE)


tk.out$groups$trt <- factor(row.names(tk.out$groups),
                 levels = c("1", "5", "10", "20"))

str(tk.out$groups)


library(dplyr)

tk.out$groups
pred.out


pred.out <- left_join(pred.out, tk.out$groups, 
                      by = "trt")



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci


# graph the predicted values
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 0.1) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Calcium Concentration", y = "Predicted Root Length (cm)")

ggsave("example1pred.jpg")

write.csv(pred.out, "example1pred.csv", row.names = FALSE)

######################################################################
# Example 2
######################################################################

dat <- read.csv("example2.csv")
str(dat)

#dat$trt <- factor(dat$trt)

ggplot(data = dat, aes(x = trt, y = TuberLengthGrowth)) + 
geom_boxplot() +
theme_bw() + 
labs(y = "Growth in Tuber Length (mm)", x = NULL)


######################################################################
#CRD analysis

dat.aov <- aov(TuberLengthGrowth ~ trt, data = dat)            # fitting the model

resplt(dat.aov)
shapiro.test(dat.aov$residuals)

anova(dat.aov)

pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)



tk.out <- HSD.test(dat.aov, trt = "trt", console = TRUE)
tk.out$groups$trt <- factor(row.names(tk.out$groups))


pred.out <- left_join(pred.out, tk.out$groups, by = "trt")
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci



# order the treatment by growth
pred.out <- pred.out[order(pred.out$emmean),]
pred.out
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))


# graph the predicted values
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 0.1) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Treatment", y = "Predicted Growth in Tuber Length (mm)")

ggsave("example2pred.jpg")

write.csv(pred.out, "example2pred.csv", row.names = FALSE)



######################################################################
# Example 3
######################################################################

dat <- read.csv("example3.csv")
str(dat)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Variety, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

dat.aov <- aov(Yield ~ Block + Variety, data = dat)            # fitting the model

resplt(dat.aov)

anova(dat.aov)
shapiro.test(dat.aov$residuals)

pred.out <- emmeans(dat.aov, "Variety")
pred.out
pred.out <- data.frame(pred.out)

tk.out <- HSD.test(dat.aov, trt = "Variety", console = TRUE)
tk.out$groups$Variety <- factor(row.names(tk.out$groups))

pred.out <- left_join(pred.out, tk.out$groups, by = "Variety")

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by yield size
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$Variety <- factor(as.character(pred.out$Variety),
                levels = as.character(pred.out$Variety))

# graph the predicted values
ggplot(data = pred.out, aes(x = Variety)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Variety", y = "Predicted Yield (t/ha)")



#######################################################################
# Example 4
#######################################################################

dat <- read.csv("example4.csv")

str(dat)

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)

ggplot(data = dat, aes(x = trt, y = DM)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = row, y = DM)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = col, y = DM)) + geom_boxplot() +
theme_bw()

# fitting the model
dat.aov <- aov(DM ~ row + col + trt, data = dat)

resplt(dat.aov)

anova(dat.aov)
shapiro.test(dat.aov$residuals)

pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)


tk.out <- HSD.test(dat.aov, trt = "trt", console = TRUE)
tk.out$groups$trt <- factor(row.names(tk.out$groups))

pred.out <- left_join(pred.out, tk.out$groups, by = "trt")
pred.out



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$ci <-  1*pred.out$SE    #1 standard error
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by DM

pred.out <- pred.out[order(pred.out$emmean),]
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))

# graph the predicted values
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 20) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Soil Type", y = "Predicted Dry Matter (kg/ha)")

######################################################################
# LMM Example 3
######################################################################

library(asreml)

dat <- read.csv("example3.csv")
str(dat)
dat$Block <- factor(dat$Block)
dat$Plot <- factor(dat$Plot)

dat.asr <- asreml(Yield ~ Variety, random = ~ Block, 
residual = ~ id(Plot), data = dat)

resplt(dat.asr)
shapiro.test(dat.asr$residuals)


anova(dat.asr)

dat.ww <- wald(dat.asr, denDF = "default")$Wald
dat.ww
#The ANOVA table
round(dat.ww,3)

dat.pred <- predict(dat.asr, classify = "Variety",
sed = TRUE)

dat.pred
pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred, 
                    data = dat, pred = "Variety", sig = 0.95)

pred.out


pred.out$ci <- qt(p = 0.975, dat.asr$nedf) * pred.out$standard.error
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Treatments by Yield size
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Variety <- factor(as.character(pred.out$Variety),
                levels = as.character(pred.out$Variety))


# graph the predicted values
ggplot(data = pred.out, aes(x = Variety)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")


library(asremlPlus)
dat.current <- asreml(Yield ~ Variety, random = ~ Block, rcov = ~ id(Plot), data = dat)
dat.reduced <- asreml(Yield ~ Variety, rcov = ~ id(Plot), data = dat)

REMLRT(h1.asreml.obj = dat.current, 
       h0.asreml.obj = dat.reduced)


#######################################################################
# LMM Example 4
#######################################################################

dat <- read.csv("example4.csv")

str(dat)
dat$row <- factor(dat$row)
dat$col <- factor(dat$col)
dat$plots <- factor(dat$plots)


# fitting the model
dat.asr <- asreml(DM ~ trt, random = ~ row + col, 
rcov = ~ id(plots), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)
shapiro.test(dat.asr$residuals)


#The ANOVA table
round(dat.ww,3)

dat.pred <- predict(dat.asr, classify = "trt",
sed = TRUE)

pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred, data = dat, 
                    pred = "trt", sig = 0.95)
pred.out



pred.out$ci <- qt(p = 0.975, dat.asr$nedf) * pred.out$standard.error
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Treatments by DM
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))


# graph the predicted values
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 5) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Soil Type", y = "Predicted Dry Matter (kg/ha)")


summary(dat.asr)$varcomp

#########################################################################################
# LogLRT - col
#########################################################################################
dat.current <- asreml(DM ~ trt, random = ~ row + col, 
rcov = ~ id(plots), data = dat)
dat.reduced <- asreml(DM ~ trt, random = ~ row, 
rcov = ~ id(plots), data = dat)

REMLRT(h1.asreml.obj = dat.current, 
       h0.asreml.obj = dat.reduced)


######################################################################
# Example 5
######################################################################

dat <- read.csv("example5.csv")
str(dat)

dat$WholePlot <- factor(dat$WholePlot)

ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 6))

ggplot(data = dat, aes(x = Fungicide, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
  facet_wrap(~ Fungicide, dir = "v") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 6))

ggplot(data = dat, aes(x = Fungicide, y = Yield)) + geom_boxplot() +
  facet_wrap(~Genotype) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 6))


ggplot(data = dat, 
       aes(x = Genotype, y = Yield, colour = Fungicide, group = Fungicide)) +
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.y = mean, geom = "line")  + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))


ggplot(data = dat, 
       aes(x = Fungicide, y = Yield, colour = Genotype, group = Genotype)) +
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.y = mean, geom = "line")  + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))



######################################################################
# Split-plot analysis
######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
random = ~ Block + Block:WholePlot, rcov = ~ units, data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.asr <- asreml(Yield ~ Fungicide + Genotype,
random = ~ Block + Block:WholePlot, 
rcov = ~ units, data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.pred <- predict(dat.asr, classify = "Genotype",
sed = TRUE)

pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred, 
                    data = dat, pred = "Genotype", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# graph the predicted values
ggplot(data = pred.out, aes(x = Genotype)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Genotype, y = up, label = groups), vjust = 0, 
          hjust = 0, nudge_y = 0.05, angle = 90, size = 3) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))

dat.pred <- predict(dat.asr, classify = "Fungicide",
sed = TRUE)

pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred, 
                    data = dat, pred = "Fungicide", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# graph the predicted values
ggplot(data = pred.out, aes(x = Fungicide)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Fungicide, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")




summary(dat.asr)$varcomp

# Likelihood Ratio test for Block:WholePlot
dat.current <- asreml(Yield ~ Genotype + Fungicide,
random = ~ Block + Block:WholePlot, rcov = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Fungicide,
random = ~ Block, rcov = ~ units, data = dat)

REMLRT(h1.asreml.obj = dat.current, 
       h0.asreml.obj = dat.reduced)


# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Genotype + Fungicide,
random = ~ Block, rcov = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Fungicide,
rcov = ~ units, data = dat)

REMLRT(h1.asreml.obj = dat.current, 
       h0.asreml.obj = dat.reduced)


######################################################################
# Example 6
######################################################################

dat <- read.csv("example6.csv")
str(dat)
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Treatment, y = Yield)) + geom_boxplot() +
theme_bw()

######################################################################
# fitting the model

dat <- dat[order(dat$Row, dat$Column),]

dat.asr <- asreml(Yield ~ Treatment, random = ~ Block,
rcov = ~ id(Column):ar1(Row), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

# Predict
dat.pred <- predict(dat.asr, classify = "Treatment",
sed = TRUE)

pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred, data = dat, pred = "Treatment", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# graph the predicted values
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, 
          angle = 0, nudge_y = 0.3) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")  + 
theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 8))


plot(variogram(dat.asr))

summary(dat.asr)$varcomp


# Likelihood Ratio test for ar1(Row)

dat.current <- asreml(Yield ~ Treatment,
random = ~ Block, rcov = ~ id(Column):ar1(Row), data = dat)

dat.reduced <- asreml(Yield ~ Treatment,
random = ~ Block, rcov = ~ id(Column):id(Row), data = dat)

REMLRT(h1.asreml.obj = dat.current, 
       h0.asreml.obj = dat.reduced)



######################################################################
# Example 7
######################################################################

dat <- read.csv("example7.csv")
str(dat)

dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Herbicide, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Rate, y = Yield)) + geom_boxplot() +
theme_bw()

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

######################################################################
# fitting the model

dat.asr <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
random = ~ Block,  rcov = ~ id(Column):ar1(Row), data = dat)


dat.ww <- wald(dat.asr, denDF = "default")$Wald

shapiro.test(dat.asr$residuals)

round(dat.ww,3)

plot(variogram(dat.asr))





summary(dat.asr)$varcomp

# Likelihood Ratio test for ar1(Row)

dat.current <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate + lin(Row),
                      random = ~ Block, rcov = ~ id(Column):ar1(Row), data = dat)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate +lin(Row),
random = ~ Block, rcov = ~ id(Column):id(Row), data = dat)

REMLRT(h1.asreml.obj = dat.current, 
       h0.asreml.obj = dat.reduced)




dat.pred <- predict(dat.asr, classify = "Herbicide",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred, data = dat, pred = "Herbicide", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted value
ggplot(data = pred.out, aes(x = Herbicide)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Herbicide, y = up, label = groups), vjust = 0, nudge_y = 0.01) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")

dat.pred <- predict(dat.asr, classify = "Herbicide:Rate",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(model.obj = dat.asr, pred.obj = dat.pred, data = dat, pred = "Herbicide:Rate", sig = 0.95)
pred.out


pred.out$Treatment <- paste(pred.out$Herbicide, pred.out$Rate, sep = "_")



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# graph the predicted values
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, nudge_y = 0.1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")  + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 8))



##################################################################################################

dat.asr <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate + lin(Row),
                  random = ~ Block + spl(Row) + Row,  rcov = ~ id(Column):ar1(Row), data = dat)


dat.asr2 <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate + lin(Row),
                  random = ~ Block,  rcov = ~ id(Column):ar1(Row), data = dat)



dat.asr <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate + lin(Row),
                  random = ~ Block,  rcov = ~ id(Column):ar1(Row), data = dat)


resplt(dat.asr)

dat.ww <- wald(dat.asr, denDF = "default")$Wald
round(dat.ww,3)



plot(variogram(dat.asr))

dat.ww <- wald(dat.asr, denDF = "default")$Wald

resplt(dat.asr)


REMLRT(h1.asreml.obj = dat.asr, 
       h0.asreml.obj = dat.asr2)



