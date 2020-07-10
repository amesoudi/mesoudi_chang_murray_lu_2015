
#summary of findings (does copy freq predict score?):

#Overall: yes, even controlling for culture
#S1: Brit, mainland yes; HK, immigrant no
#S2: Yes all
#S3: Only Brit


library(grid)
library(ggplot2)

#load data HKdata.csv into dataframe HKdata-------------------------------

# (change path below)

library(foreign, pos=4)
HKdata <- read.csv("HKdata.csv")

#re-order so that UK is ref group
HKdata$culture <- factor(HKdata$culture, levels=c('UK','HK','CI','CM'))


#graphs of relationship between copy frequency and score--------------------------
library(ggplot2)

#combined data, all cultures
ggplot(HKdata, aes(season1_copies, score_season1))  + geom_point() + geom_smooth(method=glm, color = "Red", alpha = 0.1)
ggplot(HKdata, aes(season2_copies, score_season2))  + geom_point() + geom_smooth(method=glm, color = "Blue", alpha = 0.1)
ggplot(HKdata, aes(season2_copies, score_season3))  + geom_point() + geom_smooth(method=glm, color = "Blue", alpha = 0.1)


#graphs by culture
ggplot(HKdata, aes(season1_copies, score_season1)) + facet_wrap(~ culture)  + geom_point() + geom_smooth(method=glm, color = "Red", alpha = 0.1)
ggplot(HKdata, aes(season2_copies, score_season2)) + facet_wrap(~ culture)  + geom_point() + geom_smooth(method=glm, color = "Blue", alpha = 0.1)
ggplot(HKdata, aes(season3_copies, score_season2)) + facet_wrap(~ culture)  + geom_point() + geom_smooth(method=glm, color = "Blue", alpha = 0.1)


#for season 1 only
ggplot(HKdata, aes(season1_copies/29, score_season1, colour = culture)) + geom_point(size = 2.5, aes(shape = culture)) + geom_smooth(method=glm, alpha = 0.0, size=1.4) + scale_y_continuous(limits = c(8000,30000), breaks = seq(10000,30000,by=5000)) + scale_x_continuous(breaks = seq(0,1,by=0.2)) + labs(x = "copy frequency", y = "score") + theme(axis.text.x = element_text(family="Times",color = "Black", size = 18), axis.text.y = element_text(family="Times",color = "Black", size = 18), axis.title.y = element_text(family="Times",size = 26, vjust = 1.5), axis.title.x = element_text(family="Times",size = 26, vjust = 0.0), strip.text = element_text(family="Times",size = 18), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), legend.title = element_blank(), legend.text = element_text(family="Times",size = 18), legend.key.width = unit(1, 'cm'), legend.background = element_rect(color = "black", size = 0.7), legend.justification = c(1,0), legend.position = c(0.95,0.05), legend.key = element_blank(), axis.ticks = element_line(size = 1, color = "black")) + guides(colour = guide_legend(override.aes = list(size=1.5))) + scale_colour_manual(values=c("indianred3", "darkolivegreen4", "Orange", "royalblue")) + scale_shape_manual(values=c(15,16,17,18))

#for season 2 only
ggplot(HKdata, aes(season2_copies/29, score_season2, colour = culture)) + geom_point(size = 2.5, aes(shape = culture))  + geom_smooth(method=glm, alpha = 0.0, size=1.3) + scale_y_continuous(limits = c(8000,30000), breaks = seq(10000,30000,by=5000)) + scale_x_continuous(breaks = seq(0,1,by=0.2)) + labs(x = "copy frequency", y = "score") + theme(axis.text.x = element_text(family="Times",color = "Black", size = 18), axis.text.y = element_text(family="Times",color = "Black", size = 18), axis.title.y = element_text(family="Times",size = 26, vjust = 1.5), axis.title.x = element_text(family="Times",size = 26, vjust = 0.0), strip.text = element_text(family="Times",size = 18), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), legend.title = element_blank(), legend.text = element_text(family="Times",size = 18), legend.key.width = unit(1, 'cm'), legend.background = element_rect(color = "black", size = 0.7), legend.justification = c(1,0), legend.position = c(0.95,0.05), legend.key = element_blank(), axis.ticks = element_line(size = 1, color = "black")) + guides(colour = guide_legend(override.aes = list(size=1.3))) + scale_colour_manual(values=c("indianred3", "darkolivegreen4", "Orange", "royalblue")) + scale_shape_manual(values=c(15,16,17,18))

#now same graph, diff colors (season 3)
ggplot(HKdata, aes(season3_copies/29, score_season3, colour = culture)) + geom_point(size = 2.5, aes(shape = culture)) + geom_smooth(method=glm, alpha = 0.0, size=1.4) + scale_y_continuous(limits = c(8000,30000), breaks = seq(10000,30000,by=5000)) + scale_x_continuous(breaks = seq(0,1,by=0.2)) + labs(x = "copy frequency", y = "score") + theme(axis.text.x = element_text(family="Times",color = "Black", size = 18), axis.text.y = element_text(family="Times",color = "Black", size = 18), axis.title.y = element_text(family="Times",size = 26, vjust = 1.5), axis.title.x = element_text(family="Times",size = 26, vjust = 0.0), strip.text = element_text(family="Times",size = 18), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), legend.title = element_blank(), legend.text = element_text(family="Times",size = 18), legend.key.width = unit(1, 'cm'), legend.background = element_rect(color = "black", size = 0.7), legend.justification = c(1,0), legend.position = c(0.95,0.05), legend.key = element_blank(), axis.ticks = element_line(size = 1, color = "black")) + guides(colour = guide_legend(override.aes = list(size=1.5))) + scale_colour_manual(values=c("indianred3", "darkolivegreen4", "Orange", "royalblue")) + scale_shape_manual(values=c(15,16,17,18))



#now weighted by best dem score (season 1)
ggplot(HKdata, aes(season1_copies/29, s1_score_weighted, colour = culture)) + geom_point(size = 2.4, aes(shape = culture)) + geom_smooth(method=glm, alpha = 0.0, size=2, aes(linetype=culture)) + scale_y_continuous(limits = c(0.3,1.3), breaks = seq(0.3,1.3,by=0.1)) + scale_x_continuous(breaks = seq(0,1,by=0.2)) + labs(x = "copy frequency", y = "relative score") + theme(axis.text.x = element_text(family="Times",color = "Black", size = 18), axis.text.y = element_text(family="Times",color = "Black", size = 18), axis.title.y = element_text(family="Times",size = 26, vjust = 1.5), axis.title.x = element_text(family="Times",size = 26, vjust = 0.0), strip.text = element_text(family="Times",size = 18), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), legend.title = element_blank(), legend.text = element_text(family="Times",size = 18), legend.key.width = unit(4, 'cm'), legend.background = element_rect(color = "black", size = 0.7), legend.justification = c(1,0), legend.position = c(0.95,0.05), legend.key = element_blank(), axis.ticks = element_line(size = 1, color = "black")) + guides(colour = guide_legend(override.aes = list(size=1.5))) + scale_colour_manual(values=c("indianred3", "darkolivegreen4", "Orange", "royalblue")) + scale_shape_manual(values=c(15,16,17,18)) + scale_linetype_manual(values=c(1,2,5,6)) + guides(colour = guide_legend(override.aes = list(shape = NA)))

#now weighted by best dem score (season 2)
ggplot(HKdata, aes(season2_copies/29, s2_score_weighted, colour = culture)) + geom_point(size = 2.4, aes(shape = culture)) + geom_smooth(method=glm, alpha = 0.0, size=2, aes(linetype=culture)) + scale_y_continuous(limits = c(0.3,1.3), breaks = seq(0.3,1.3,by=0.1)) + scale_x_continuous(breaks = seq(0,1,by=0.2)) + labs(x = "copy frequency", y = "relative score") + theme(axis.text.x = element_text(family="Times",color = "Black", size = 18), axis.text.y = element_text(family="Times",color = "Black", size = 18), axis.title.y = element_text(family="Times",size = 26, vjust = 1.5), axis.title.x = element_text(family="Times",size = 26, vjust = 0.0), strip.text = element_text(family="Times",size = 18), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), legend.title = element_blank(), legend.text = element_text(family="Times",size = 18), legend.key.width = unit(4, 'cm'), legend.background = element_rect(color = "black", size = 0.7), legend.justification = c(1,0), legend.position = c(0.95,0.05), legend.key = element_blank(), axis.ticks = element_line(size = 1, color = "black")) + guides(colour = guide_legend(override.aes = list(size=1.5))) + scale_colour_manual(values=c("indianred3", "darkolivegreen4", "Orange", "royalblue")) + scale_shape_manual(values=c(15,16,17,18)) + scale_linetype_manual(values=c(1,2,5,6)) + guides(colour = guide_legend(override.aes = list(shape = NA)))

#now weighted by best dem score (season 3)
ggplot(HKdata, aes(season3_copies/29, s3_score_weighted, colour = culture)) + geom_point(size = 2.4, aes(shape = culture)) + geom_smooth(method=glm, alpha = 0.0, size=2, aes(linetype=culture)) + scale_y_continuous(limits = c(0.3,1.3), breaks = seq(0.3,1.3,by=0.1)) + scale_x_continuous(breaks = seq(0,1,by=0.2)) + labs(x = "copy frequency", y = "relative score") + theme(axis.text.x = element_text(family="Times",color = "Black", size = 18), axis.text.y = element_text(family="Times",color = "Black", size = 18), axis.title.y = element_text(family="Times",size = 26, vjust = 1.5), axis.title.x = element_text(family="Times",size = 26, vjust = 0.0), strip.text = element_text(family="Times",size = 18), panel.background = element_rect(fill = "white"), axis.line = element_line(size = 1), panel.grid.minor = element_blank(), legend.title = element_blank(), legend.text = element_text(family="Times",size = 18), legend.key.width = unit(4, 'cm'), legend.background = element_rect(color = "black", size = 0.7), legend.justification = c(1,0), legend.position = c(0.95,0.05), legend.key = element_blank(), axis.ticks = element_line(size = 1, color = "black")) + guides(colour = guide_legend(override.aes = list(size=1.5))) + scale_colour_manual(values=c("indianred3", "darkolivegreen4", "Orange", "royalblue")) + scale_shape_manual(values=c(15,16,17,18)) + scale_linetype_manual(values=c(1,2,5,6)) + guides(colour = guide_legend(override.aes = list(shape = NA)))



#glm regressions --------------------
#all 3 significant, S3 less so
summary(scoreXcopyS1 <- glm(score_season1 ~ season1_copies, data = HKdata))
summary(scoreXcopyS2 <- glm(score_season2 ~ season2_copies, data = HKdata))
summary(scoreXcopyS3 <- glm(score_season3 ~ season3_copies, data = HKdata))
summary(scoreXcopyS3 <- glm(s1s2_score ~ s1s2_copies, data = HKdata))


#separate linear regressions for each cultural group - used weighted scores in the paper-------------------

#british: all sig
HKdata_brit <- HKdata[HKdata$culture == "UK", ]
#season 1
summary(brit_scoreXcopyS1 <- glm(score_season1 ~ season1_copies, data = HKdata_brit))
#season 2
summary(brit_scoreXcopyS2 <- glm(score_season2 ~ season2_copies, data = HKdata_brit))
#season 3
summary(brit_scoreXcopyS3 <- glm(score_season3 ~ season3_copies, data = HKdata_brit))
#weighted S1
summary(brit_scoreXcopyS1W <- glm(s1_score_weighted ~ season1_copies, data = HKdata_brit))
#weighted S2
summary(brit_scoreXcopyS2W <- glm(s2_score_weighted ~ season2_copies, data = HKdata_brit))
#weighted S3
summary(brit_scoreXcopyS3W <- glm(s3_score_weighted ~ season3_copies, data = HKdata_brit))

#mainland: S1, S2 sig
HKdata_main <- HKdata[HKdata$culture == "CM", ]
#season 1
summary(main_scoreXcopyS1 <- glm(score_season1 ~ season1_copies, data = HKdata_main))
#season 2
summary(main_scoreXcopyS2 <- glm(score_season2 ~ season2_copies, data = HKdata_main))
#season 3
summary(main_scoreXcopyS3 <- glm(score_season3 ~ season3_copies, data = HKdata_main))
#weighted S1
summary(main_scoreXcopyS1W <- glm(s1_score_weighted ~ season1_copies, data = HKdata_main))
#weighted S2
summary(main_scoreXcopyS2W <- glm(s2_score_weighted ~ season2_copies, data = HKdata_main))
#weighted S3
summary(main_scoreXcopyS3W <- glm(s3_score_weighted ~ season3_copies, data = HKdata_main))


#immigrant: S2 sig
HKdata_immigrant <- HKdata[HKdata$culture == "CI", ]
#season 1
summary(immigrant_scoreXcopyS1 <- glm(score_season1 ~ season1_copies, data = HKdata_immigrant))
#season 2
summary(immigrant_scoreXcopyS2 <- glm(score_season2 ~ season2_copies, data = HKdata_immigrant))
#season 3
summary(immigrant_scoreXcopyS3 <- glm(score_season3 ~ season3_copies, data = HKdata_immigrant))
#weighted S1
summary(immigrant_scoreXcopyS1W <- glm(s1_score_weighted ~ season1_copies, data = HKdata_immigrant))
#weighted S2
summary(immigrant_scoreXcopyS2W <- glm(s2_score_weighted ~ season2_copies, data = HKdata_immigrant))
#weighted S3
summary(immigrant_scoreXcopyS3W <- glm(s3_score_weighted ~ season3_copies, data = HKdata_immigrant))


#hongkong: only S2 sig
HKdata_hongkong <- HKdata[HKdata$culture == "HK", ]
#season 1
summary(hongkong_scoreXcopyS1 <- glm(score_season1 ~ season1_copies, data = HKdata_hongkong))
#season 2
summary(hongkong_scoreXcopyS2 <- glm(score_season2 ~ season2_copies, data = HKdata_hongkong))
#season 3
summary(hongkong_scoreXcopyS3 <- glm(score_season3 ~ season3_copies, data = HKdata_hongkong))
#weighted S1
summary(hongkong_scoreXcopyS1W <- glm(s1_score_weighted ~ season1_copies, data = HKdata_hongkong))
#weighted S2
summary(hongkong_scoreXcopyS2W <- glm(s2_score_weighted ~ season2_copies, data = HKdata_hongkong))
#weighted S3
summary(hongkong_scoreXcopyS3W <- glm(s3_score_weighted ~ season3_copies, data = HKdata_hongkong))


