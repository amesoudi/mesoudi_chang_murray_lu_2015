
#summary of findings:
#mainland chinese copy more than other groups in S1 and S2, no differences in S3

library(Hmisc)
library(ggplot2)
library(car)

#load data HKdata.csv into dataframe HKdata-------------------------------


library(foreign, pos=4)
HKdata <- read.csv("HKdata.csv")

#re-order so that UK is ref group
HKdata$culture <- factor(HKdata$culture, levels=c('UK','HK','CI','CM'))


#descriptive stats----------------------------

library(pastecs)
stat.desc(HKdata$season1_copies/29)
stat.desc(HKdata$season2_copies/29)
stat.desc(HKdata$season3_copies/29)

by(HKdata$season1_copies/29, HKdata$culture, stat.desc)
by(HKdata$season2_copies/29, HKdata$culture, stat.desc)
by(HKdata$season3_copies/29, HKdata$culture, stat.desc)
by(HKdata$s1s2_copies/29, HKdata$culture, stat.desc)



#cross cultural comparison of season 1 copy rates-----------------------------------

#now same for S1
#plot data, barchart - indicates higher mean for chinese rural, all others look equal
ggplot(HKdata, aes(culture, season1_copies/29)) + stat_summary(fun = mean, geom = "bar", fill = "White", color = "Black", size = 1)  + coord_cartesian(ylim = c(0, 0.42)) + labs(x = "", y = "frequency of copying") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) + theme(axis.text.x = element_text(family="Times", color = "Black", size = 28), axis.text.y = element_text(family="Times", color = "Black", size = 24), axis.title.y = element_text(family="Times", size = 28, vjust = 1.7), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 1, color = "black"))

#boxplots might be better, but suggests non-normally distributed vars
plot(season1_copies ~ culture, data = HKdata)

#plot log(season1_copies): suggests brit and HK are identical, chinese immigrant and rural are higher
clog <- function(x) log(x + 0.5)
plot(clog(season1_copies) ~ culture, data = HKdata)

#histograms show data is non-normally distributed, lots of zeroes; confirms brit & HK are similar
ggplot(HKdata, aes(season1_copies), fill = "white", color = "black") + facet_wrap(~ culture) + geom_histogram()

#test for normality indicates it is not normal (sig = not normal)
shapiro.test(HKdata$season1_copies)

#levene's test for homogeneity of variance also indicates no homogeneity (sig = heterogenous var)
leveneTest(HKdata$season1_copies, HKdata$culture, center=median)


#quasibinomial

summary(nullmodelS1bin <- glm(copy_frequency_s1 ~ 1, family = quasibinomial, data = HKdata))

#goodness of fit test is non-sig, indicating good overall fit
with(nullmodelS1bin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#add cultural group
summary(culturemodelS1bin <- glm(copy_frequency_s1 ~ culture, family = quasibinomial, data = HKdata))

#goodness of fit test is still non-sig; model fits well
with(culturemodelS1bin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#compare null and culture models; sig diff, so fit is improved by culture
anova(nullmodelS1bin, culturemodelS1bin, test = "Chisq")

#do post-hoc tests: indicate that Hk, Brit and immigrant are identical
#mainland sig diff to every other group
library(multcomp)
summary(posthocs <- glht(culturemodelS1bin, linfct = mcp(culture = "Tukey")))


#add sex, age; neither significant
summary(agesexmodelS1bin <- glm(copy_frequency_s1 ~ culture + age + sex, family = quasibinomial, data = HKdata))

#compare agesex and culture-only models; no sig diff so drop age & sex
anova(agesexmodelS1bin, culturemodelS1bin, test = "Chisq")

#culture+sex - sigificant
summary(sexculturemodelS1bin <- glm(copy_frequency_s1 ~ culture + sex, family = quasibinomial, data = HKdata))
anova(sexculturemodelS1bin, culturemodelS1bin, test = "Chisq")

library(multcomp)
summary(posthocs <- glht(sexculturemodelS1bin, linfct = mcp(culture = "Tukey")))


#just age
summary(ageonlymodelS1bin <- glm(copy_frequency_s1 ~ age, family = quasibinomial, data = HKdata))
anova(ageonlymodelS1bin, nullmodelS1bin, test = "Chisq")


#add IND and COL; neither sig
summary(indcolmodelS1bin <- glm(copy_frequency_s1 ~ culture + individualist_reversed + collectivist_reversed, family = quasibinomial, data = HKdata))

#compare ind/col and culture-only models; no sig diff
anova(indcolmodelS1bin, nullmodelS1bin, test = "Chisq")


#culture x sex interaction? No
summary(sexXculturemodelS1bin <- glm(copy_frequency_s1 ~ culture*sex, family = quasibinomial, data = HKdata))
anova(sexXculturemodelS1bin, culturemodelS1bin, test = "Chisq")


#just for interest, the full model; culture still sig but sex also now marginally sig
summary(fullmodelS1bin <- glm(copy_frequency_s1 ~ culture + age + sex + individualist_reversed + collectivist_reversed, family = quasibinomial, data = HKdata))

#although full model is no better fit than culture model, so ignore sig sex in full
anova(fullmodelS1bin, sexculturemodelS1bin, test = "Chisq")


#get confidence intervals
confint(sexculturemodelS1bin)
confint.default(sexculturemodelS1bin)


#conclusion from quasibinomial model for S1: HK, British and immigrant groups are identical, mainland Chinese group copy sig more than others
#specifically, for every copy done by a Brit, Chinese mainlanders copy exp(0.7612)=2.14 times (over twice as likely to copy). Other groups are similarly different.

#cross cultural comparison of season 2 copy rates--------------------------------------

#graphs

#plot data, barchart - indicates higher mean for chinese rural, all others look equal
library(ggplot2)
ggplot(HKdata, aes(culture, copy_frequency_all)) + stat_summary(fun = mean, geom = "bar", fill = "White", color = "Black", size = 1)  + coord_cartesian(ylim = c(0, 0.42)) + labs(x = "", y = "frequency of copying") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) + theme(axis.text.x = element_text(family="Times", color = "Black", size = 28), axis.text.y = element_text(family="Times", color = "Black", size = 24), axis.title.y = element_text(family="Times", size = 28, vjust = 1.7), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 1, color = "black"))


#boxplots might be better, but suggests non-normally distributed vars
plot(season2_copies ~ culture, data = HKdata)

#plot log(season2_copies): suggests brit and HK are identical, chinese immigrant and rural are higher
clog <- function(x) log(x + 0.5)
plot(clog(season2_copies) ~ culture, data = HKdata)

#histograms show data is non-normally distributed, lots of zeroes; confirms brit & HK are similar
ggplot(HKdata, aes(season2_copies), fill = "white", color = "black") + facet_wrap(~ culture) + geom_histogram()


#assumption tests

#test for normality indicates it is not normal (sig = not normal)
shapiro.test(HKdata$copy_frequency_s2)

#levene's test for homogeneity of variance also indicates no homogeneity (sig = heterogenous var)
library(car)
leveneTest(HKdata$copy_frequency_s2, HKdata$culture, center=median)

#quasibinomial

summary(nullmodelS2bin <- glm(copy_frequency_s2 ~ 1, family = quasibinomial, data = HKdata))

#goodness of fit test is non-sig, indicating good overall fit
with(nullmodelS2bin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#add cultural group
summary(culturemodelS2bin <- glm(copy_frequency_s2 ~ culture, family = quasibinomial, data = HKdata))

#goodness of fit test is still non-sig; model fits well
with(culturemodelS2bin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#compare null and culture models; sig diff, so fit is improved by culture
anova(nullmodelS2bin, culturemodelS2bin, test = "Chisq")

#do post-hoc tests: indicate that Hk, Brit and immigrant are identical
#mainland sig diff to every other group
library(multcomp)
summary(posthocs <- glht(culturemodelS2bin, linfct = mcp(culture = "Tukey")))


#add sex, age; neither significant
summary(agesexmodelS2bin <- glm(copy_frequency_s2 ~ culture + age + sex, family = quasibinomial, data = HKdata))

#compare agesex and culture-only models; no sig diff so drop age & sex
anova(agesexmodelS2bin, culturemodelS2bin, test = "Chisq")


#just sex, ns
summary(sexonlymodelS2bin <- glm(copy_frequency_s2 ~ sex, family = quasibinomial, data = HKdata))

#sex + culture, marginal
summary(sexculturemodelS2bin <- glm(copy_frequency_s2 ~ culture + sex, family = quasibinomial, data = HKdata))
anova(culturemodelS2bin, sexculturemodelS2bin, test = "Chisq")

summary(posthocs <- glht(sexculturemodelS2bin, linfct = mcp(culture = "Tukey")))


#get confidence intervals
confint(sexculturemodelS2bin)
confint.default(sexculturemodelS2bin)


#add IND and COL; neither sig
summary(indcolmodelS2bin <- glm(copy_frequency_s2 ~ culture + individualist_reversed + collectivist_reversed, family = quasibinomial, data = HKdata))

#compare ind/col and culture-only models; no sig diff
anova(indcolmodelS2bin, culturemodelS2bin, test = "Chisq")


#just for interest, the full model; culture still sig but sex also now marginally sig
summary(fullmodelS2bin <- glm(copy_frequency_s2 ~ culture + age + sex + individualist_reversed + collectivist_reversed, family = quasibinomial, data = HKdata))

#although full model is no better fit than culture model, so ignore sig sex in full
anova(fullmodelS2bin, sexculturemodelS2bin, test = "Chisq")


#conclusion from quasibinomial model: HK, British and immigrant groups are identical, mainland Chinese group copy sig more than others
#specifically, for every copy done by a Brit, Chinese mainlanders copy exp(0.79964)=2.22 times (over twice as likely to copy). Other groups are similarly different.


#cross cultural comparison of season 3 copy rates---------------------------------------------

#plot data, barchart - indicates higher mean for chinese rural, all others look equal
ggplot(HKdata, aes(culture, season3_copies/29)) + stat_summary(fun = mean, geom = "bar", fill = "White", color = "Black", size = 1)  + coord_cartesian(ylim = c(0, 0.42)) + labs(x = "", y = "frequency of copying") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) + theme(axis.text.x = element_text(family="Times", color = "Black", size = 28), axis.text.y = element_text(family="Times", color = "Black", size = 24), axis.title.y = element_text(family="Times", size = 28, vjust = 1.7), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), axis.ticks = element_line(size = 1, color = "black"))

#boxplots might be better, but suggests non-normally distributed vars
plot(season3_copies ~ culture, data = HKdata)

#plot log(season3_copies): suggests brit and HK are identical, chinese immigrant and rural are higher
clog <- function(x) log(x + 0.5)
plot(clog(season3_copies) ~ culture, data = HKdata)

#histograms show data is non-normally distributed, lots of zeroes; confirms brit & HK are similar
ggplot(HKdata, aes(season3_copies), fill = "white", color = "black") + facet_wrap(~ culture) + geom_histogram()

#test for normality indicates it is not normal (sig = not normal)
shapiro.test(HKdata$season3_copies)

#levene's test for homogeneity of variance also indicates no homogeneity (sig = heterogenous var)
leveneTest(HKdata$season3_copies, HKdata$culture, center=median)


#quasibinomial

summary(nullmodelS3bin <- glm(copy_frequency_s3 ~ 1, family = quasibinomial, data = HKdata))

#goodness of fit test is non-sig, indicating good overall fit
with(nullmodelS3bin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#add cultural group
summary(culturemodelS3bin <- glm(copy_frequency_s3 ~ culture, family = quasibinomial, data = HKdata))

#goodness of fit test is still non-sig; model fits well
with(culturemodelS3bin, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#compare null and culture models; no sig diff, so fit is not improved by culture
anova(nullmodelS3bin, culturemodelS3bin, test = "Chisq")

#do post-hoc tests: again no sig diffs
library(multcomp)
summary(posthocs <- glht(culturemodelS3bin, linfct = mcp(culture = "Tukey")))


#add sex, age; neither significant
summary(agesexmodelS3bin <- glm(copy_frequency_s3 ~ culture + age + sex, family = quasibinomial, data = HKdata))

#compare agesex and culture-only models; no sig diff so drop age & sex
anova(agesexmodelS3bin, culturemodelS3bin, test = "Chisq")


#just sex, also not sig (unlike S1 and S2)
summary(sexmodelS3bin <- glm(copy_frequency_s3 ~ culture + sex, family = quasibinomial, data = HKdata))


#add IND and COL; neither sig
summary(indcolmodelS3bin <- glm(copy_frequency_s3 ~ culture + individualist_reversed + collectivist_reversed, family = quasibinomial, data = HKdata))

#compare ind/col and culture-only models; no sig diff
anova(indcolmodelS3bin, culturemodelS3bin, test = "Chisq")


#just for interest, the full model; culture still sig but sex also now marginally sig
summary(fullmodelS3bin <- glm(copy_frequency_s3 ~ culture + age + sex + individualist_reversed + collectivist_reversed, family = quasibinomial, data = HKdata))

#although full model is no better fit than culture model, so ignore sig sex in full
anova(fullmodelS3bin, culturemodelS3bin, test = "Chisq")
anova(fullmodelS3bin, nullmodelS3bin, test = "Chisq")

#conclusion from quasibinomial model for S3: No differences between any groups


#conclusion: S3 shows no or marginal effect of culture; same trend as before (mainland diff to others)

