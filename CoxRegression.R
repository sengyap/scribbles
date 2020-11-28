# working directory
setwd("/Users/sengyap/Downloads/NICF-Complex Predictive Modelling & Forecasting/Day 5")

# read files in excel
library("readxl")
cox = read_excel("DemoCox.xls")
str(cox)
cox$priorthrp = as.factor(cox$priorthrp)

# exploratory analysis
# correlation matrix
coef_cor = cor(cox[, sapply(cox, is.numeric)], use="complete.obs") # using only complete observations x, y

corrplot::corrplot(coef_cor, method = "number", type='lower')

pairs(daysurv~perfstatus+month+age,data=cox, 
      main="Simple Scatterplot Matrix")

# fit a KM model
fit <- survfit(Surv(daysurv, censoring) ~ priorthrp, data=cox)
summary(fit)
fit
ggsurvplot(fit)

# This shows the fitting of Kaplan Meier graphs to the 
# data showing the variation of the survivor function with different values of Priortherap

# significance test
survdiff(Surv(daysurv, censoring) ~ priorthrp, data=cox) # p-value=0.5
# There seems to be little impact on survival time

# fit a Cox model

coxmodel <- coxph(Surv(daysurv, censoring) ~ perfstatus + month + age + priorthrp , data=cox)
summary(coxmodel)
extractAIC(coxmodel)  #976.7001

# This shows that the model is quite successful in explaining the data (logrank test p-value <0.001)
# And that perfstatus is highly significant in determining survival time 
# The higher perfstatus; the longer the survival time, p-value <0.001
# All other variables and factors seem insignificant, p-value >0.05
