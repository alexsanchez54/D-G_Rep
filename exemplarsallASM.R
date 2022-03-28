if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(emmeans)) {install.packages("emmeans"); require(emmeans)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

study1 <- read.delim("myData2.txt")
study2 <- read.delim("myData3.txt")
study3 <- read.delim("myData4.txt")
study4 <- read.delim("myData5.txt")
study5 <- read.delim("myData6.txt")
study6 <- read.delim("mydatarep.txt")
study7 <- read.delim("myData7.txt")

myColNames <- Reduce(intersect, list(colnames(study1), colnames(study2), colnames(study3),
                                     colnames(study4), colnames(study5), colnames(study6),
                                     colnames(study7)))

myColNames2 <- unique(c(colnames(study1), colnames(study2), colnames(study3),
                        colnames(study4), colnames(study5), colnames(study6),
                        colnames(study7)))

study2[, myColNames2[!(myColNames2 %in% colnames(study2))]] <- NA
study2 <- study2[, myColNames2]
study2$study <- "Study 2"

study3[, myColNames2[!(myColNames2 %in% colnames(study3))]] <- NA
study3 <- study3[, myColNames2]
study3$study <- "Study 3"

study4[, myColNames2[!(myColNames2 %in% colnames(study4))]] <- NA
study4 <- study4[, myColNames2]
study4$study <- "Study 4"

study5[, myColNames2[!(myColNames2 %in% colnames(study5))]] <- NA
study5 <- study5[, myColNames2]
study5$study <- "Study 5"

study6[, myColNames2[!(myColNames2 %in% colnames(study6))]] <- NA
study6 <- study6[, myColNames2]
study6$study <- "Study 6"

study7[, myColNames2[!(myColNames2 %in% colnames(study7))]] <- NA
study7 <- study7[, myColNames2]
study7$study <- "Study 7"

studyrep[, myColNames2[!(myColNames2 %in% colnames(studyrep))]] <- NA
studyrep <- studyrep[, myColNames2]
studyrep$study <- "Study Rep"

myDataAll <- rbind(study2, study3, study4, study5, study6, study7, studyrep)

head(myDataAll)
table(myDataAll$citizenship)
table(myDataAll$residence)

myDataAll <- myDataAll[myDataAll$citizenship == "us", ]

head(myDataAll)

table(myDataAll$exp_check6)

tapply(myDataAll$D_Score, myDataAll$exp_check6, mean, na.rm = TRUE)
tapply(myDataAll$D_Score, myDataAll$exp_check6, sd, na.rm = TRUE)
myDataAll$exp_check6 <- factor(myDataAll$exp_check6)
summary(aov(myDataAll$D_Score ~ myDataAll$exp_check6))
summary(lm(myDataAll$D_Score ~ myDataAll$exp_check6))

lmFit <- lm(myDataAll$D_Score ~ myDataAll$exp_check6)
pairs(emmeans(lmFit, ~ exp_check6), adjust = "none")

lmerFit0 <- lmer(D_Score ~ 1 + (1 | study), myDataAll[!is.na(myDataAll$exp_check6), ])
lmerFit1 <- lmer(D_Score ~ exp_check6 + (1 | study), myDataAll[!is.na(myDataAll$exp_check6), ])
anova(lmerFit0, lmerFit1)

lmerFit2 <- lmer(D_Score ~ exp_check6 + (exp_check6 | study), myDataAll[!is.na(myDataAll$exp_check6), ])
anova(lmerFit1, lmerFit2)

summary(lmerFit1)
plot(allEffects(lmerFit1))

emmeans(lmerFit1, ~ exp_check6)
pairs(emmeans(lmerFit1, ~ exp_check6), adjust = "none")

1/lmBF(D_Score ~ learnCond, study6)

tapply(myDataAll$D_Score, list(myDataAll$study, myDataAll$exp_check6), mean)

myDataAll$exp_check6b <- paste(myDataAll$exp_check6)
myDataAll$exp_check6b[myDataAll$learnCond == "Control"] <- 0
myDataAll$exp_check6b[myDataAll$exp_check6b == "NA"] <- NA

myDataAll$exp_check6b <- factor(myDataAll$exp_check6b)
tapply(myDataAll$D_Score, list(myDataAll$study, myDataAll$exp_check6b), mean)

lmerFit0b <- lmer(D_Score ~ 1 + (1 | study), myDataAll[!is.na(myDataAll$exp_check6b), ])
lmerFit1b <- lmer(D_Score ~ exp_check6b + (1 | study), myDataAll[!is.na(myDataAll$exp_check6b), ])
anova(lmerFit0b, lmerFit1b)

summary(lmerFit1b)
plot(allEffects(lmerFit1b))

emmeans(lmerFit1b, ~ exp_check6b)
pairs(emmeans(lmerFit1b, ~ exp_check6b), adjust = "none")

cohensD(myDataAll$D_Score[myDataAll$exp_check6b == "0"], myDataAll$D_Score[myDataAll$exp_check6b == "2"])
cohensD(myDataAll$D_Score[myDataAll$exp_check6b == "0"], myDataAll$D_Score[myDataAll$exp_check6b == "1"])
cohensD(myDataAll$D_Score[myDataAll$exp_check6b == "0"], myDataAll$D_Score[myDataAll$exp_check6b == "3"])

table(myDataAll$study, myDataAll$exp_check6)
round(prop.table(table(myDataAll$study, myDataAll$exp_check6), 1), 3)

cohensD(myDataAll$D_Score[myDataAll$study == "Study 7" & myDataAll$exp_check6b == "0"], myDataAll$D_Score[myDataAll$study == "Study 7" & myDataAll$exp_check6b == "2"])
cohensD(myDataAll$D_Score[myDataAll$study == "Study 7" & myDataAll$exp_check6b == "0"], myDataAll$D_Score[myDataAll$study == "Study 7" & myDataAll$exp_check6b == "1"])
cohensD(myDataAll$D_Score[myDataAll$study == "Study 7" & myDataAll$exp_check6b == "0"], myDataAll$D_Score[myDataAll$study == "Study 7" & myDataAll$exp_check6b == "3"])

cohensD(myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "0"], myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "2"])
cohensD(myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "0"], myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "1"])
cohensD(myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "0"], myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "3"])

t.test(myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "0"], myDataAll$D_Score[!(myDataAll$study == "Study 7") & myDataAll$exp_check6b == "1"])

cohensD(study7$D_Score ~ study7$learnCond)
1/ttestBF(study7$D_Score[study7$learnCond == "Control"], study7$D_Score[study7$learnCond == "Experimental"])
t.test(study7$D_Score ~ study7$learnCond)
