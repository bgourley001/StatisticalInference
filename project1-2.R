#packages
library(dplyr)
library(ggplot2)

#Load data
data("ToothGrowth")

#data structure
str(ToothGrowth)

#get dose values
unique(ToothGrowth$dose)

#convert dose from num to factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

#data summary
summary(ToothGrowth)

#boxplot
g <- ggplot(aes(x = supp, y = len), data = ToothGrowth) +
  geom_boxplot(aes(fill = supp)) + facet_wrap(~ dose) +
  ggtitle("Comparison of Tooth Growth by Supplement and Dosage") +
  labs(x = "Supplement", y = "Tooth Length")

print(g)

#summary table of length
group_len <- group_by(ToothGrowth,dose,supp)
group_len.summary <- summarize(group_len,
          count = n(),
          mean = mean(len),
          median = median(len),
          std.dev = sd(len),
          variance = var(len))

group_len.summary

#data subsets
#OJ against VC
OJ <- subset(ToothGrowth,supp == "OJ")
VC <- subset(ToothGrowth,supp == "VC")

#histograms
hist(OJ$len)
hist(VC$len)

#sd and var OJ
OJ.sd <- sd(OJ$len)
OJ.var <- var(OJ$len)
OJ.sd
OJ.var

#sd and var OJ
VC.sd <- sd(VC$len)
VC.var <- var(VC$len)
VC.sd
VC.var

#t.test OJ against VC
#H0 : Difference in tooth growth means = 0
#Ha : Difference in tooth growth means != 0
t.test(OJ$len,VC$len,paired = FALSE,var.equal = FALSE)

#p-value of 0.06 is higher than 0.05, therefore we fail to reject H0, although the result is marginal.
#The 95% confidence interval of [-0.17,7.57] contains 0

#OJ subsets for dose levels 0.5, 1.0, 2.0
OJ_0.5 <- subset(OJ,dose == 0.5)
OJ_1.0 <- subset(OJ,dose == 1.0)
OJ_2.0 <- subset(OJ,dose == 2.0)

#histograms
hist(OJ_0.5$len)
hist(OJ_1.0$len)
hist(OJ_2.0$len)

t.test(OJ_1.0$len,OJ_0.5$len,paired = FALSE,var.equal = FALSE)
#reject H0, p-value is much less than 0.05 and 95% confidence interval does not contain 0

t.test(OJ_2.0$len,OJ_0.5$len,paired = FALSE,var.equal = FALSE)
#reject H0, p-value is much less than 0.05 and 95% confidence interval does not contain 0

t.test(OJ_2.0$len,OJ_1.0$len,paired = FALSE,var.equal = FALSE)
#reject H0, p-value is less than 0.05 and 95% confidence interval does not contain 0

VC_0.5 <- subset(VC,dose == 0.5)
VC_1.0 <- subset(VC,dose == 1.0)
VC_2.0 <- subset(VC,dose == 2.0)

t.test(VC_1.0$len,VC_0.5$len,paired = FALSE,var.equal = FALSE)
#reject H0, p-value is much less than 0.05 and 95% confidence interval does not contain 0

t.test(VC_2.0$len,VC_0.5$len,paired = FALSE,var.equal = FALSE)
#reject H0, p-value is much less than 0.05 and 95% confidence interval does not contain 0

t.test(VC_2.0$len,VC_1.0$len,paired = FALSE,var.equal = FALSE)
#reject H0, p-value is much less than 0.05 and 95% confidence interval does not contain 0

################################################################
t.test(OJ_0.5$len,VC_0.5$len,paired = FALSE,var.equal = FALSE)

t.test(OJ_1.0$len,VC_1.0$len,paired = FALSE,var.equal = FALSE)

t.test(OJ_2.0$len,VC_2.0$len,paired = FALSE,var.equal = FALSE)
################################################################

