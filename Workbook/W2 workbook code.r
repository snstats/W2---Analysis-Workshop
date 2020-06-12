

#a <- round(rnorm(4,5,1),2)
#b <- round(rnorm(4,7,1),2)
#c <- round(rnorm(4,5.5,1),2)

dat <- data.frame(treatment = rep(c("A","B","C"), each = 4), response = c(a,b,c))
dat$treatment <-  factor(dat$treatment)

setwd("D:/UofA/W2/Workbook")
write.csv(dat, "hypcrd.csv", row.names = FALSE, quote = FALSE)

dat <- read.csv("hypcrd.csv")

ggplot(data = dat, aes(x = treatment, y = response)) + geom_boxplot() +
theme_bw()

ggsave("hypcrd_boxplot.pdf")


mn <- mean(dat$response)

col <- c("orangered", "forestgreen", "steelblue","black")
ggplot(data = dat, aes(x = treatment, y = response)) + geom_point(aes(colour = treatment), size = 3) +
scale_colour_manual(values = col) +
geom_hline(yintercept = mn, color = "orange2", size = 1, linetype = "solid") +
theme_bw()
ggsave("hypcrd_overallmean.pdf")



ggplot(data = dat, aes(x = treatment, y = response)) + geom_point((aes(colour = treatment)), size = 3) +
scale_colour_manual(values = col) +
geom_hline(yintercept = mn, color = "orange2", size = 1, linetype = "solid") +
geom_segment(aes(x = 1.1, y = dat$response[1], xend = 1.1, yend = mn, colour = "residual")) +
geom_segment(aes(x = 0.8, y = dat$response[2], xend = 0.8, yend = mn, colour = "residual")) +
geom_segment(aes(x = 0.9, y = dat$response[3], xend = 0.9, yend = mn, colour = "residual")) +
geom_segment(aes(x = 1.2, y = dat$response[4], xend = 1.2, yend = mn, colour = "residual")) +
geom_segment(aes(x = 1.8, y = dat$response[5], xend = 1.8, yend = mn, colour = "residual")) +
geom_segment(aes(x = 1.9, y = dat$response[6], xend = 1.9, yend = mn, colour = "residual")) +
geom_segment(aes(x = 2.1, y = dat$response[7], xend = 2.1, yend = mn, colour = "residual")) +
geom_segment(aes(x = 2.2, y = dat$response[8], xend = 2.2, yend = mn, colour = "residual")) +
geom_segment(aes(x = 2.8, y = dat$response[9], xend = 2.8, yend = mn, colour = "residual")) +
geom_segment(aes(x = 2.9, y = dat$response[10], xend = 2.9, yend = mn, colour = "residual")) +
geom_segment(aes(x = 3.1, y = dat$response[11], xend = 3.1, yend = mn, colour = "residual")) +
geom_segment(aes(x = 3.2, y = dat$response[12], xend = 3.2, yend = mn, colour = "residual")) +

theme_bw()
ggsave("hypcrd_totss.pdf")


trta <- tapply(dat$response, list(dat$treatment), mean)[1]
trtb <- tapply(dat$response, list(dat$treatment), mean)[2]
trtc <- tapply(dat$response, list(dat$treatment), mean)[3]

ggplot(data = dat, aes(x = treatment, y = response)) + geom_point((aes(colour = treatment)), size = 3) +
scale_colour_manual(values = col) +
geom_hline(yintercept = mn, color = "orange2", size = 1, linetype = "solid") +
geom_hline(yintercept = trta, colour = col[1], size = 1, linetype = "solid") +
geom_hline(yintercept = trtb, color = col[2], size = 1, linetype = "solid") +
geom_hline(yintercept = trtc, color = col[3], size = 1, linetype = "solid") +
theme_bw()

ggsave("hypcrd_trtmean.pdf")




ggplot(data = dat, aes(x = treatment, y = response)) + geom_point((aes(colour = treatment)), size = 3) +
geom_hline(yintercept = mn, color = "orange2", size = 1, linetype = "solid") +
geom_hline(yintercept = trta, colour = col[1], size = 1, linetype = "solid") +
geom_hline(yintercept = trtb, color = col[2], size = 1, linetype = "solid") +
geom_hline(yintercept = trtc, color = col[3], size = 1, linetype = "solid") +
scale_colour_manual(values = col) +

geom_segment(aes(x = 1.1, y = dat$response[1], xend = 1.1, yend = trta, colour = "residual")) +
geom_segment(aes(x = 0.8, y = dat$response[2], xend = 0.8, yend = trta, colour = "residual")) +
geom_segment(aes(x = 0.9, y = dat$response[3], xend = 0.9, yend = trta, colour = "residual")) +
geom_segment(aes(x = 1.2, y = dat$response[4], xend = 1.2, yend = trta, colour = "residual")) +
geom_segment(aes(x = 1.8, y = dat$response[5], xend = 1.8, yend = trtb, colour = "residual")) +
geom_segment(aes(x = 1.9, y = dat$response[6], xend = 1.9, yend = trtb, colour = "residual")) +
geom_segment(aes(x = 2.1, y = dat$response[7], xend = 2.1, yend = trtb, colour = "residual")) +
geom_segment(aes(x = 2.2, y = dat$response[8], xend = 2.2, yend = trtb, colour = "residual")) +
geom_segment(aes(x = 2.8, y = dat$response[9], xend = 2.8, yend = trtc, colour = "residual")) +
geom_segment(aes(x = 2.9, y = dat$response[10], xend = 2.9, yend = trtc, colour = "residual")) +
geom_segment(aes(x = 3.1, y = dat$response[11], xend = 3.1, yend = trtc, colour = "residual")) +
geom_segment(aes(x = 3.2, y = dat$response[12], xend = 3.2, yend = trtc, colour = "residual")) +

theme_bw()
ggsave("hypcrd_trtss.pdf")





######################################################################
######################################################################


#a <- round(rnorm(4,5,1),2)
#b <- round(rnorm(4,5,1),2)
#c <- round(rnorm(4,5,1),2)

dat <- data.frame(treatment = rep(c("A","B","C"), each = 4), response = c(a,b,c))
dat$treatment <-  factor(dat$treatment)

setwd("D:/UofA/W2/Workbook")
write.csv(dat, "compAcrd.csv", row.names = FALSE, quote = FALSE)

dat <- read.csv("compAcrd.csv")

ggplot(data = dat, aes(x = treatment, y = response)) + geom_boxplot() +
theme_bw()

ggsave("compAcrd_boxplot.pdf")



######################################################################
######################################################################

# Example 1

library(agricolae)


trt <- c(1, 5, 10, 20)
rep <- 5
outdesign <- design.crd(trt, r = rep, serie = 0)
des.out <- outdesign$book
des.out


write.csv(des.out, "example1.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example1.csv")

dat$trt <- factor(dat$trt)
dd <- c(12,17,18,10)
dat$RL <- round(rnorm(nrow(dat), mean = dd[dat$trt], sd = 1),2)

ggplot(data = dat, aes(x = trt, y = RL)) + geom_boxplot() +
theme_bw()

write.csv(dat, "example1all.csv", quote = FALSE, row.names = FALSE)



dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example1.csv")
dat$trt <- factor(dat$trt)


ggplot(data = dat, aes(x = trt, y = RL)) + geom_boxplot() +
theme_bw()

ggsave("example1_boxplot.pdf")


######################################################################
#CRD analysis

dat <- read.csv("example1all.csv")


dat$trt <- factor(dat$trt)                      # convert trt to a factor

dat.aov <- aov(RL ~ trt, data = dat)            # fitting the model

par(mfrow = c(2, 2))

plot(dat.aov)
ggsave("Example1Resplot.png")

library(emmeans)

pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "trt", console = TRUE)
tuk.out$groups$trt <- factor(row.names(tuk.out$groups),
                 levels = c("1", "5", "10", "20"))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "trt")



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

ggsave("Example1Pred.png")


######################################################################
# Example 2
######################################################################


library(agricolae)


trt <- paste("T", 1:7, sep = "")
rep <- 12
outdesign <- design.crd(trt, r = rep, serie = 0)
des.out <- outdesign$book
des.out


write.csv(des.out, "example2.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example2.csv")

dat$trt <- factor(dat$trt)
dd <- round(runif(min = 10, max = 18, n = 7),1)
dat$TuberLengthGrowth <- round(rnorm(nrow(dat), mean = dd[dat$trt], sd = 1.2),2)

ggplot(data = dat, aes(x = trt, y = TuberLengthGrowth)) + geom_boxplot() +
theme_bw()

write.csv(dat, "example2all.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("example2all.csv")
dat$trt <- factor(dat$trt)


ggplot(data = dat, aes(x = trt, y = TuberLengthGrowth)) + geom_boxplot() +
theme_bw() + labs(y = "Growth in Tuber Length (mm)", x = NULL)

ggsave("example2_boxplot.pdf")


######################################################################
#CRD analysis

dat <- read.csv("example2all.csv")


dat$trt <- factor(dat$trt)                      # convert trt to a factor

dat.aov <- aov(TuberLengthGrowth ~ trt, data = dat)            # fitting the model

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Example2Resplot.png")


library(emmeans)

pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "trt", console = TRUE)
tuk.out$groups$trt <- factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "trt")



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

 
# order the treatment by growth
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))
 


# graph the predicted values 
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 0.1) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Treatment", y = "Predicted Growth in Tuber Length (mm)")

ggsave("Example2Pred.png")


######################################################################
# Exercise 1
######################################################################

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise1.csv")

str(dat)

summary(dat)

ggplot(data = dat, aes(x = Variety, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("Exercise1_boxplot.pdf")


dat.aov <- aov(Yield ~ Variety, data = dat)            # fitting the model

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Exercise1Resplot.png")

anova(dat.aov)

library(emmeans)

pred.out <- emmeans(dat.aov, "Variety")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "Variety", console = TRUE)


tuk.out$groups$Variety <-
  factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "Variety")



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

ggsave("Exercise1Pred.png")

######################################################################
# Exercise 2
######################################################################


library(agricolae)


Treatment <- c("CN", "CP", "KC", "PE", "HL", "HE")
rep <- 4
outdesign <- design.crd(Treatment, r = rep, serie = 0)
des.out <- outdesign$book
des.out


write.csv(des.out, "exercise2.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise2.csv")

dat$Treatment <- factor(dat$Treatment)
dd <- round(runif(min = 1.7, max = 3.9, n = 6),1)
dat$Time <- round(rnorm(nrow(dat), mean = dd[dat$Treatment], sd = 0.5),2)

ggplot(data = dat, aes(x = Treatment, y = Time)) + geom_boxplot() +
theme_bw()

write.csv(dat, "exercise2all.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("exercise2all.csv")
dat$Treatment <- factor(dat$Treatment)




dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise2.csv")

str(dat)

summary(dat)

ggplot(data = dat, aes(x = Treatment, y = Time)) + geom_boxplot() +
theme_bw()

ggsave("Exercise2_boxplot.pdf")


dat.aov <- aov(Time ~ Treatment, data = dat)            # fitting the model

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Exercise2Resplot.png")

anova(dat.aov)

library(emmeans)

pred.out <- emmeans(dat.aov, "Treatment")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "Treatment", console = TRUE)
tuk.out$groups$Treatment <- factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "Treatment")



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by yield size
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$Treatment <- factor(as.character(pred.out$Treatment),
                levels = as.character(pred.out$Treatment))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Treatment", y = "Predicted Time (days)")

ggsave("Exercise2Pred.png")

######################################################################
# Example 3
######################################################################

setwd("D:/UofA/W2")
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example3.csv")
dat$Block <- factor(dat$Block)
str(dat)

summary(dat)

ggplot(data = dat, aes(x = Variety, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("Example3Var_boxplot.pdf")

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("Example3Blk_boxplot.pdf")



dat.aov <- aov(Yield ~ Block + Variety, data = dat)            # fitting the model

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Example3Resplot.png")

anova(dat.aov)
shapiro.test(dat.aov$residuals)


library(emmeans)

pred.out <- emmeans(dat.aov, "Variety")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "Variety", console = TRUE)
tuk.out$groups$Variety <- factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "Variety")



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

ggsave("Example3Pred.png")

######################################################################
# Exercise 3
######################################################################

setwd("D:/UofA/W2/Workbook")
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise3.csv")
dat$Replicate <- factor(dat$Replicate)
str(dat)

summary(dat)

ggplot(data = dat, aes(x = Variety, y = AverageFruitSize)) + geom_boxplot() +
theme_bw()

ggsave("Exercise3Var_boxplot.pdf")

ggplot(data = dat, aes(x = Replicate, y = AverageFruitSize)) + geom_boxplot() +
theme_bw()

ggsave("Exercise3Blk_boxplot.pdf")



dat.aov <- aov(AverageFruitSize ~ Replicate + Variety, data = dat)            # fitting the model

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Exercise3Resplot.png")

anova(dat.aov)
shapiro.test(dat.aov$residuals)


library(emmeans)

pred.out <- emmeans(dat.aov, "Variety")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "Variety", console = TRUE)
tuk.out$groups$Variety <- factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "Variety")



# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by AverageFruitSize size
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$Variety <- factor(as.character(pred.out$Variety),
                levels = as.character(pred.out$Variety))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Variety)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Variety", y = "Predicted AverageFruitSize (kg)")

ggsave("Exercise3Pred.png")


######################################################################
# Exercise 4
######################################################################

setwd("D:/UofA/W2/Workbook")
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise4.csv")
dat$Block <- factor(dat$Block)
dat$SeedingRate <- factor(dat$SeedingRate)
str(dat)

summary(dat)

ggplot(data = dat, aes(x = SeedingRate, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("Exercise4Var_boxplot.pdf")

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("Exercise4Blk_boxplot.pdf")



dat.aov <- aov(Yield ~ Block + SeedingRate, data = dat)            # fitting the model

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Exercise4Resplot.png")

anova(dat.aov)
shapiro.test(dat.aov$residuals)


#######################################################################
# Example 4
#######################################################################


trt <- c("S1", "S2", "S3", "S4")
outdesign <- design.lsd(trt, serie=1)
des.out <- outdesign$book

write.csv(des.out, "example4.csv", quote = FALSE, row.names = FALSE)

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example4.csv")

dd <- c(2000,1800,2180,1750)
dat$DM <- round(rnorm(nrow(dat), mean = dd[dat$trt], sd = 200),2)

ggplot(data = dat, aes(x = trt, y = DM)) + geom_boxplot() +
theme_bw()

write.csv(dat, "example4.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example4.csv")

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)

str(dat)
summary(dat)

ggplot(data = dat, aes(x = trt, y = DM)) + geom_boxplot() +
theme_bw()

ggsave("Example4trt_boxplot.pdf")

ggplot(data = dat, aes(x = row, y = DM)) + geom_boxplot() +
theme_bw()

ggsave("Example4row_boxplot.pdf")

ggplot(data = dat, aes(x = col, y = DM)) + geom_boxplot() +
theme_bw()

ggsave("Example4col_boxplot.pdf")

# fitting the model
dat.aov <- aov(DM ~ row + col + trt, data = dat)

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Example4Resplot.png")

anova(dat.aov)
shapiro.test(dat.aov$residuals)


library(emmeans)

pred.out <- emmeans(dat.aov, "trt")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "trt", console = TRUE)
tuk.out$groups$trt <- factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "trt")
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by AverageFruitSize size
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$trt <- factor(as.character(pred.out$trt),
                levels = as.character(pred.out$trt))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = trt)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = trt, y = up, label = groups), vjust = 0, nudge_y = 5) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "Soil Type", y = "Predicted Dry Matter (kg/ha)")

ggsave("Example4Pred.png")


######################################################################
# Exercise 5
######################################################################
Treatment <- c("Seed", "Silk", "Stalk", "Root", "Damage")
outdesign <- design.lsd(Treatment, serie=1)
des.out <- outdesign$book

write.csv(des.out, "exercise5.csv", quote = FALSE, row.names = FALSE)

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise5.csv")

dd <- c(40.1,29.7,34.1,45.9,36.8)
dat$EarInfect <- round(rnorm(nrow(dat), mean = dd[dat$Treatment], sd = 2),2)
dd <- c(1, 2,  4,3,0)
dat$EarInfect <- dat$EarInfect + round(rnorm(nrow(dat), mean = dd[dat$row], sd = 0.001),2)
ggplot(data = dat, aes(x = Treatment, y = EarInfect)) + geom_boxplot() +
theme_bw()

write.csv(dat, "exercise5.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise5.csv")

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)

str(dat)
summary(dat)

ggplot(data = dat, aes(x = Treatment, y = EarInfect)) + geom_boxplot() +
theme_bw()

ggsave("Exercise5trt_boxplot.pdf")

ggplot(data = dat, aes(x = row, y = EarInfect)) + geom_boxplot() +
theme_bw()

ggsave("Exercise5row_boxplot.pdf")

ggplot(data = dat, aes(x = col, y = EarInfect)) + geom_boxplot() +
theme_bw()

ggsave("Exercise5col_boxplot.pdf")

# fitting the model
dat.aov <- aov(EarInfect ~ row + col + Treatment, data = dat)

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Exercise5Resplot.png")

anova(dat.aov)
shapiro.test(dat.aov$residuals)


library(emmeans)

pred.out <- emmeans(dat.aov, "Treatment")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "Treatment", console = TRUE)
tuk.out$groups$Treatment <- factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "Treatment")
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by EarInfect size
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$Treatment <- factor(as.character(pred.out$Treatment),
                levels = as.character(pred.out$Treatment))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, nudge_y = 1) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Ear Infection (%)")

ggsave("Exercise5Pred.png")


######################################################################
# Exercise 6
######################################################################
Treatment <- c("T0", "T4", "T8", "T12")
outdesign <- design.lsd(Treatment, serie=1)
des.out <- outdesign$book

write.csv(des.out, "exercise6.csv", quote = FALSE, row.names = FALSE)

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise6.csv")

dd <- c(20.1, 17.9, 14.8, 15.1)
dat$SugarYield <- round(rnorm(nrow(dat), mean = dd[dat$Treatment], sd = 2),2)
dd <- c(1, 2, 4, 3)
dat$SugarYield <- dat$SugarYield + round(rnorm(nrow(dat), mean = dd[dat$col], sd = 0.01),2)
ggplot(data = dat, aes(x = Treatment, y = SugarYield)) + geom_boxplot() +
theme_bw()

write.csv(dat, "exercise6.csv", quote = FALSE, row.names = FALSE)
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise6.csv")

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)

str(dat)
summary(dat)

ggplot(data = dat, aes(x = Treatment, y = SugarYield)) + geom_boxplot() +
theme_bw()

ggsave("Exercise6trt_boxplot.pdf")

ggplot(data = dat, aes(x = row, y = SugarYield)) + geom_boxplot() +
theme_bw()

ggsave("Exercise6row_boxplot.pdf")

ggplot(data = dat, aes(x = col, y = SugarYield)) + geom_boxplot() +
theme_bw()

ggsave("Exercise6col_boxplot.pdf")

# fitting the model
dat.aov <- aov(SugarYield ~ row + col + Treatment, data = dat)

par(mfrow = c(2, 2))
plot(dat.aov)
ggsave("Exercise6Resplot.png")

anova(dat.aov)
shapiro.test(dat.aov$residuals)


library(emmeans)

pred.out <- emmeans(dat.aov, "Treatment")
pred.out
pred.out <- data.frame(pred.out)

library(agricolae)

tuk.out <- HSD.test(dat.aov, trt = "Treatment", console = TRUE)
tuk.out$groups$Treatment <- factor(row.names(tuk.out$groups))

library(dplyr)

pred.out <- left_join(pred.out, tuk.out$groups, by = "Treatment")
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$SE    #95% Confidence Interval
pred.out$low <- pred.out$emmean - pred.out$ci
pred.out$up <- pred.out$emmean + pred.out$ci

# order the Treatments by Yield size
pred.out <- pred.out[order(pred.out$emmean),]
pred.out$Treatment <- factor(as.character(pred.out$Treatment),
                levels = as.character(pred.out$Treatment))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, nudge_y = 0.2) +
geom_point(aes(y = emmean), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Sugar Yield (t/ha)")

ggsave("Exercise6Pred.png")


######################################################################
# LMM Example 3
######################################################################

library(asreml)

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example3.csv")
dat$Block <- factor(dat$Block)
dat$Plot <- factor(dat$Plot)

dat.asr <- asreml(Yield ~ Variety, random = ~ Block,
residual = ~ id(Plot), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)

ggsave("Example3LMMResplot.png")
#The ANOVA table
round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.pred <- predict(dat.asr, classify = "Variety",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Variety", sig = 0.95)
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

ggsave("Example3LMMPred.png")


#######################################################################
# LMM Example 4
#######################################################################

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example4.csv")
str(dat)

dat$plots <- factor(dat$plots)

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)


# fitting the model
dat.asr <- asreml(DM ~ trt, random = ~ row + col,
residual = ~ id(plots), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)

ggsave("Example4LMMResplot.png")

#The ANOVA table
round(dat.ww,3)
shapiro.test(dat.asr$residuals)

dat.pred <- predict(dat.asr, classify = "trt",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "trt", sig = 0.95)
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

ggsave("Example4LMMPred.png")

summary(dat.asr)$varcomp

library(asremlPlus)
dat.current <- asreml(DM ~ trt, random = ~ row + col, residual =  ~ id(plots), data = dat)
dat.reduced <- asreml(DM ~ trt, random = ~ row, residual =  ~ id(plots), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


######################################################################
# Exercise 7
######################################################################

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise1.csv")
dat$Plot <- factor(dat$Plot)

dat.asr <- asreml(Yield ~ Variety, residual =  ~ units, data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise7Resplot.png")

#The ANOVA table
round(dat.ww,3)
shapiro.test(dat.asr$residuals)

dat.pred <- predict(dat.asr, classify = "Variety",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, model.obj = dat.asr, pred = "Variety", sig = 0.95)
pred.out

# order the Varieties by yield size
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Variety <- factor(as.character(pred.out$Variety),
                levels = as.character(pred.out$Variety))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Variety)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Variety", y = "Predicted Yield (t/ha)")

ggsave("Exercise7Pred.png")

######################################################################
# Exercise 8
######################################################################
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise2.csv")
dat$plots <- factor(dat$plots)

dat.asr <- asreml(Time ~ Treatment, residual =  ~ id(plots), data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise8Resplot.png")

#The ANOVA table
round(dat.ww,3)
shapiro.test(dat.asr$residuals)

dat.pred <- predict(dat.asr, classify = "Treatment",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Treatment", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Treatment by Time
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Treatment <- factor(as.character(pred.out$Treatment),
                levels = as.character(pred.out$Treatment))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Treatment", y = "Predicted Time (days)")

ggsave("Exercise8Pred.png")


######################################################################
# Exercise 9
######################################################################
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise3.csv")
dat$Replicate <- factor(dat$Replicate)
dat$Plot <- factor(dat$Plot)

dat.asr <- asreml(AverageFruitSize ~ Variety, random = ~ Replicate, residual =  ~ id(Plot), data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise9Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)



dat.pred <- predict(dat.asr, classify = "Variety",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Variety", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Variety by AverageFruitSize
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Variety <- factor(as.character(pred.out$Variety),
                levels = as.character(pred.out$Variety))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Variety)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Variety", y = "Predicted Average Fruit Size (kg)")

ggsave("Exercise9Pred.png")

library(asremlPlus)
dat.current <- asreml(AverageFruitSize ~ Variety, random = ~ Replicate, residual =  ~ id(Plot), data = dat)
dat.reduced <- asreml(AverageFruitSize ~ Variety, residual =  ~ id(Plot), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


######################################################################
# Exercise 10
######################################################################

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise4.csv")
dat$Block <- factor(dat$Block)
dat$SeedingRate <- factor(dat$SeedingRate)
dat$Plot <- factor(dat$Plot)

dat.asr <- asreml(Yield ~ SeedingRate, random =~ Block, residual =  ~ id(Plot), data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise10Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

library(asremlPlus)
dat.current <- asreml(Yield ~ SeedingRate, random =~ Block, residual =  ~ id(Plot), data = dat)
dat.reduced <- asreml(Yield ~ SeedingRate, residual =  ~ id(Plot), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

######################################################################
# Exercise 11
######################################################################
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise5.csv")

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)
dat$plots <- factor(dat$plots)

# fitting the model
dat.asr <- asreml(EarInfect ~ Treatment, random =~ row + col, residual =  ~ id(plots), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise11Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

dat.pred <- predict(dat.asr, classify = "Treatment",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Treatment", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Treatment by EarInfect
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Treatment <- factor(as.character(pred.out$Treatment),
                levels = as.character(pred.out$Treatment))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Treatment", y = "Predicted Ear Infection (%)")

ggsave("Exercise11Pred.png")

library(asremlPlus)
dat.current <- asreml(EarInfect ~ Treatment, random =~ row + col, residual =  ~ id(plots), data = dat)
dat.reduced <- asreml(EarInfect ~ Treatment, random =~ row, residual =  ~ id(plots), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

dat.reduced <- asreml(EarInfect ~ Treatment, random =~ col, data = residual =  ~ id(plots), dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


######################################################################
# Exercise 12
######################################################################
dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise6.csv")

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)
dat$plots <- factor(dat$plots)

# fitting the model
dat.asr <- asreml(SugarYield ~ Treatment, random = ~ row + col, residual =  ~ id(plots), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise12Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

dat.pred <- predict(dat.asr, classify = "Treatment",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Treatment", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci

# order the Treatment by SugarYield
pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Treatment <- factor(as.character(pred.out$Treatment),
                levels = as.character(pred.out$Treatment))
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, nudge_y = 0.05) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Treatment", y = "Predicted Sugar Yield (t/ha)")

ggsave("Exercise12Pred.png")

library(asremlPlus)
dat.current <- asreml(SugarYield ~ Treatment, random = ~ row + col, data = dat)
dat.reduced <- asreml(SugarYield ~ Treatment, random = ~ row, residual =  ~ id(plots), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

dat.reduced <- asreml(SugarYield ~ Treatment, random = ~ col, residual =  ~ id(plots), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

######################################################################
# Example 5
######################################################################

library(agridat)
dat <- durban.splitplot
dat$wholeplot <- factor(rep(rep(rep(1:2, each = 7), 4),10))
write.csv(dat, "example5.csv", quote = FALSE, row.names = FALSE)

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/example5.csv")
dat$WholePlot <- factor(dat$WholePlot)

ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 6))

ggsave("example5_Genoboxplot.pdf")

ggplot(data = dat, aes(x = Fungicide, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("example5_Fungboxplot.pdf")

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("example5_Blockboxplot.pdf")

######################################################################
# Split-plot analysis
######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
random = ~ Block + Block:WholePlot, residual = ~ units, data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Example5Resplot1.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.asr <- asreml(Yield ~ Genotype + Fungicide,
random = ~ Block + Block:WholePlot, residual = ~ units, data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Example5Resplot2.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.pred <- predict(dat.asr, classify = "Genotype",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Genotype", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Genotype)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Genotype, y = up, label = groups), vjust = 0, hjust = 0, nudge_y = 0.05, angle = 90, size = 2) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))

ggsave("Example5GenoPred.png")



dat.pred <- predict(dat.asr, classify = "Fungicide",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Fungicide", sig = 0.95)
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

ggsave("Example5FungPred.png")

summary(dat.asr)$varcomp

library(asremlPlus)

# Likelihood Ratio test for Block:WholePlot
dat.current <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
random = ~ Block + Block:WholePlot, residual = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
random = ~ Block, residual = ~ units, data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
random = ~ Block, residual = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Fungicide + Genotype:Fungicide,
residual = ~ units, data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

######################################################################
# Exercise 13
######################################################################

library(agridat)
dat <- yates.oats
dat$WholePlot <- factor(rep(1:3, 24))
write.csv(dat, "exercise13.csv", quote = FALSE, row.names = FALSE)

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise13.csv")
dat$WholePlot <- factor(dat$WholePlot)
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Nitrogen <- factor(dat$Nitrogen)


ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise13_Genoboxplot.pdf")

ggplot(data = dat, aes(x = Nitrogen, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise_Nitroboxplot.pdf")

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise13_Blockboxplot.pdf")

######################################################################
# Split-plot analysis
######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block + Block:WholePlot, residual = ~ units, data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise13Resplot1.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.asr <- asreml(Yield ~ Genotype + Nitrogen,
random = ~ Block + Block:WholePlot, data = dat)            # fitting the model
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise13Resplot2.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.pred <- predict(dat.asr, classify = "Genotype",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Genotype", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Genotype)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Genotype, y = up, label = groups), vjust = 0, nudge_y = 1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (number of 1/4 lbs)")

ggsave("Exercise13GenoPred.png")



dat.pred <- predict(dat.asr, classify = "Nitrogen",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Nitrogen", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Nitrogen)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.05) +
geom_text(aes(x = Nitrogen, y = up, label = groups), vjust = 0, nudge_y = 1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Nitrogen (hundredweight per acre)", y = "Predicted Yield (number of 1/4 lbs)")

ggsave("Exercise13NitroPred.png")

summary(dat.asr)$varcomp

library(asremlPlus)

# Likelihood Ratio test for Block:WholePlot
dat.current <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block + Block:WholePlot, residual = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block, residual = ~ units, data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block, residual = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
residual = ~ units, data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

######################################################################
# Exercise 14
######################################################################

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise14.csv")
dat$WholePlot <- factor(dat$WholePlot)
dat$Block <- factor(dat$Block)
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)

ggplot(data = dat, aes(x = Variety, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise14_Varboxplot.pdf")

ggplot(data = dat, aes(x = Irrigation, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise14_Irriboxplot.pdf")

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise14_Blockboxplot.pdf")

######################################################################
# Split-plot analysis
######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Variety + Irrigation + Variety:Irrigation,
random = ~ Block + Block:WholePlot, residual = ~ units, data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise14Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.pred <- predict(dat.asr, classify = "Variety:Irrigation",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Variety:Irrigation", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Variety)) + facet_wrap(~Irrigation) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Variety, y = up, label = groups), vjust = 0, nudge_y = 1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (number of 1/4 lbs)")

ggsave("Exercise14Pred.png")

summary(dat.asr)$varcomp

library(asremlPlus)

# Likelihood Ratio test for Block:WholePlot
dat.current <- asreml(Yield ~ Variety + Irrigation + Variety:Irrigation,
random = ~ Block + Block:WholePlot, residual = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Variety + Irrigation + Variety:Irrigation,
random = ~ Block, residual = ~ units, data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Variety + Irrigation + Variety:Irrigation,
random = ~ Block, residual = ~ units, data = dat)

dat.reduced <- asreml(Yield ~ Variety + Irrigation + Variety:Irrigation,
residual = ~ units, data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

######################################################################
# Example 6
######################################################################

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/D:/UofA/W2/Workbook/example6.csv")

str(dat)
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("example6_Blockboxplot.pdf")

ggplot(data = dat, aes(x = Treatment, y = Yield)) + geom_boxplot() +
theme_bw() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
setwd("D:/UofA/W2/Workbook")
ggsave("example6_Treatboxplot.pdf")


######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Treatment, random = ~ Block,
residual = ~ id(Column):ar1(Row), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Example6Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

# Predict
dat.pred <- predict(dat.asr, classify = "Treatment",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Treatment", sig = 0.95)
pred.out


# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, angle = 90, nudge_y = 0.1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")  + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 8))

ggsave("Example6Pred.png")



summary(dat.asr)$varcomp

# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Treatment,
random = ~ Block,
residual = ~ id(Column):ar1(Row), data = dat)

dat.reduced <- asreml(Yield ~ Treatment, residual = ~ id(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Row)

dat.reduced <- asreml(Yield ~ Treatment,
random = ~ Block, residual = ~ id(Column):id(Row), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


######################################################################
# Exercise 15 (Spatial of Exercise 13)
######################################################################

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise13.csv")
dat$WholePlot <- factor(dat$WholePlot)
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Nitrogen <- factor(dat$Nitrogen)


ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise15_Genoboxplot.pdf")

ggplot(data = dat, aes(x = Nitrogen, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise_Nitroboxplot.pdf")

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise15_Blockboxplot.pdf")

######################################################################
# Split-plot analysis
######################################################################
# fitting the model

dat <- dat[order(dat$Column, dat$Row),]
dat.asr <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block + Block:WholePlot,
residual = ~ id(Column):ar1(Row), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise15Resplot1.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


dat.asr <- asreml(Yield ~ Genotype + Nitrogen,
random = ~ Block + Block:WholePlot,
residual = ~ id(Column):ar1(Row), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise15Resplot2.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

summary(dat.asr)$varcomp

plot(variogram(dat.asr))

library(asremlPlus)

# Likelihood Ratio test for Block:WholePlot
dat.current <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block + Block:WholePlot,
residual = ~ id(Column):ar1(Row), data = dat)


dat.reduced <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block,
residual = ~ id(Column):ar1(Row), data = dat)


reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block, residual = ~ id(Column):ar1(Row), data = dat)


dat.reduced <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
residual = ~ id(Column):ar1(Row), data = dat)

reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)

# Likelihood Ratio test for ar1(Row)
dat.current <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block + Block:WholePlot,
residual = ~ id(Column):ar1(Row), data = dat)


dat.reduced <- asreml(Yield ~ Genotype + Nitrogen + Genotype:Nitrogen,
random = ~ Block + Block:WholePlot,
residual = ~ id(Column):id(Row), data = dat)


reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


dat.asr <- asreml(Yield ~ Genotype + Nitrogen,
random = ~ Block + Block:WholePlot,
residual = ~ id(Column):ar1(Row), data = dat)


dat.pred <- predict(dat.asr, classify = "Genotype",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, model.obj = dat.asr, data = dat, pred = "Genotype", sig = 0.95)
pred.out

 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Genotype)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Genotype, y = up, label = groups), vjust = 0, nudge_y = 1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (number of 1/4 lbs)")

ggsave("Exercise15GenoPred.png")



dat.pred <- predict(dat.asr, classify = "Nitrogen",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Nitrogen", sig = 0.95)
pred.out

 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Nitrogen)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.05) +
geom_text(aes(x = Nitrogen, y = up, label = groups), vjust = 0, nudge_y = 1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Nitrogen (hundredweight per acre)", y = "Predicted Yield (number of 1/4 lbs)")

ggsave("Exercise15NitroPred.png")


######################################################################
# Exercise 16
######################################################################
library(agridat)
data(besag.elbatan)
dat <- besag.elbatan

names(dat) <- c("Yield", "Genotype", "Block", "Row")
dat$Column <- dat$Block
dat <- dat[,c(3,4,5,2,1)]

setwd("D:/UofA/W2/Workbook")
write.csv(dat, "exercise16.csv", row.names = FALSE, quote = FALSE)



dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise16.csv")
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

str(dat)

library(car)
leveneTest(Yield ~ Genotype, data = dat)


ggplot(data = dat, aes(x = Genotype, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise16_Genoboxplot.pdf")


ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("exercise16_Blockboxplot.pdf")

######################################################################
# RCBD analysis
######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Genotype,
random = ~ Block,
residual = ~ id(Column):ar1(Row), data = dat)
dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise16Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

summary(dat.asr)$varcomp

plot(variogram(dat.asr))

library(asremlPlus)

# Likelihood Ratio test for Block
dat.current <- asreml(Yield ~ Genotype,
random = ~ Block,
residual = ~ id(Column):ar1(Row), data = dat)


dat.reduced <- asreml(Yield ~ Genotype,
residual = ~ id(Column):ar1(Row), data = dat)


reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Row)
dat.current <- asreml(Yield ~ Genotype,
random = ~ Block,
residual = ~ id(Column):ar1(Row), data = dat)


dat.reduced <- asreml(Yield ~ Genotype,
random = ~ Block,
residual = ~ id(Column):id(Row), data = dat)

dat.reduced <- update(dat.reduced)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


dat.asr <- asreml(Yield ~ Genotype,
random = ~ Block,
residual = ~ id(Column):ar1(Row), data = dat)


dat.pred <- predict(dat.asr, classify = "Genotype",
sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Genotype", sig = 0.95)
pred.out

pred.out <- pred.out[order(pred.out$predicted.value),]
pred.out$Genotype <- factor(as.character(pred.out$Genotype),
                levels = as.character(pred.out$Genotype))

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Genotype)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Genotype, y = up, label = groups), vjust = 0, nudge_y = 0.2, angle = 90, size = 3) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "Genotype", y = "Predicted Yield") +
theme(axis.text.x = element_text(angle = 90, vjust = 0, size = 8))

ggsave("Exercise16GenoPred.png")




######################################################################
# Example 7
######################################################################

dat <- read.csv("D:/UofA/W2/Workbook/example7.csv")
str(dat)
dat$Row <- factor(dat$Row)
dat$Column <- factor(dat$Column)
dat$Block <- factor(dat$Block)

ggplot(data = dat, aes(x = Herbicide, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("example7_Herbboxplot.pdf")

ggplot(data = dat, aes(x = Rate, y = Yield)) + geom_boxplot() +
theme_bw() +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("example7_Rateboxplot.pdf")

ggplot(data = dat, aes(x = Block, y = Yield)) + geom_boxplot() +
theme_bw()

ggsave("example7_Blockboxplot.pdf")


######################################################################
# fitting the model
dat.asr <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
random = ~ Block,  residual = ~ id(Column):ar1(Row), data = dat)

dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Example7Resplot.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)


summary(dat.asr)$varcomp

plot(variogram(dat.asr))


# Likelihood Ratio test for Block

dat.current <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
random = ~ Block, residual = ~ id(Column):ar1(Row), data = dat)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate, residual = ~ id(Column):ar1(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


# Likelihood Ratio test for ar1(Row)

dat.reduced <- asreml(Yield ~ Control + Herbicide + Herbicide:Rate,
random = ~ Block, residual = ~ id(Column):id(Row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


dat.pred <- predict(dat.asr, classify = "Control",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Control", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Control)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Control, y = up, label = groups), vjust = 0, nudge_y = 0.01) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")

ggsave("Example7ControlPred.png")



dat.pred <- predict(dat.asr, classify = "Herbicide",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Herbicide", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Herbicide)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Herbicide, y = up, label = groups), vjust = 0, nudge_y = 0.01) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")

ggsave("Example7HerbicidePred.png")



dat.pred <- predict(dat.asr, classify = "Herbicide:Rate",
present = c("Control", "Herbicide", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Herbicide:Rate", sig = 0.95)
pred.out
pred.out$Treatment <- paste(pred.out$Herbicide, pred.out$Rate, sep = "_")

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Treatment)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Treatment, y = up, label = groups), vjust = 0, angle = 90, nudge_y = 0.1) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield (t/ha)")  + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 8))

ggsave("Exercise16HerbRatePred.png")


######################################################################
# Exercise 17
######################################################################

library(agridat)
data(cochran.crd)
dat <- cochran.crd


#The  experiment  was  conducted  to  investigate  the  effect  of  sulfur  on  controlling  scab  disease  in
#potatoes. There were seven treatments. Control, plus spring and fall application of 300, 600, 1200
#lbs/acre of sulfur. The response variable was infection as a percent of the surface area covered with
#scab. A completely randomized design was used with 8 replications of the control and 4 replications
#of the other treatments.

dat$row <- factor(dat$row)
dat$col <- factor(dat$col)
dat <- dat[order(dat$col, dat$row),]

dat$Control <- rep("No", nrow(dat))
dat$Control[dat$trt == "O"] <- "Yes"
dat$Control <- factor(dat$Control)
dat$Season <- factor(substring(as.character(dat$trt),1,1))
dat$Rate <- factor(substring(as.character(dat$trt),2))

setwd("D:/UofA/W2/Workbook")
write.csv(dat, "exercise17.csv", quote = FALSE,row.names = FALSE)

dat <- read.csv("D:/UofA/W2/W2 Files for Workshop/exercise17.csv")

# Set up factors
dat$row <- factor(dat$row)
dat$col <- factor(dat$col)
dat$Control <- factor(dat$Control)
dat$Season <- factor(dat$Season)
dat$Rate <- factor(dat$Rate)

# Order the data
dat <- dat[order(dat$col, dat$row),]

dat.asr <- asreml(inf ~ Control + Season + Rate + Season:Rate,
residual = ~ ar1(col):id(row), data = dat)

dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise17Resplot1.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

plot(variogram(dat.asr))

summary(dat.asr)$varcomp

# Drop the interaction

dat.asr <- asreml(inf ~ Control + Season + Rate,
residual = ~ ar1(col):id(row), data = dat)

dat.ww <- wald(dat.asr, denDF = "default")$Wald

plot(dat.asr)
ggsave("Exercise17Resplot2.png")

round(dat.ww,3)
shapiro.test(dat.asr$residuals)

summary(dat.asr)$varcomp


# Likelihood Ratio test for ar1(Column)

dat.reduced <- asreml(inf ~ Control + Season + Rate,
residual = ~ id(col):id(row), data = dat)
reml.lrt.asreml(full.asreml.obj = dat.current, reduced.asreml.obj = dat.reduced)


dat.pred <- predict(dat.asr, classify = "Control",
present = c("Control", "Season", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Control", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Control)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Control, y = up, label = groups), vjust = 0, nudge_y = 0.3) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield")

ggsave("Exercise17ControlPred.png")

dat.pred <- predict(dat.asr, classify = "Season",
present = c("Control", "Season", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Season", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Season)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Season, y = up, label = groups), vjust = 0, nudge_y = 0.01) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield")

ggsave("Exercise17SeasonPred.png")



dat.pred <- predict(dat.asr, classify = "Rate",
present = c("Control", "Season", "Rate"), sed = TRUE)

pred.out <- tuk.out(pred.obj = dat.pred, data = dat, pred = "Rate", sig = 0.95)
pred.out

# Calculate the upper and lower limits of the confidence intervals
pred.out$ci <-  qnorm(0.975)*pred.out$standard.error    #95% Confidence Interval
pred.out$low <- pred.out$predicted.value - pred.out$ci
pred.out$up <- pred.out$predicted.value + pred.out$ci
 
# graph the predicted values 
ggplot(data = pred.out, aes(x = Rate)) +
geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
geom_text(aes(x = Rate, y = up, label = groups), vjust = 0, nudge_y = 0.5) +
geom_point(aes(y = predicted.value), color = "black", shape = 16) + theme_bw() +
labs(x = "", y = "Predicted Yield")

ggsave("Exercise17RatePred.png")
