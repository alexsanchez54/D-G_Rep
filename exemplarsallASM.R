if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(emmeans)) {install.packages("emmeans"); require(emmeans)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}

Yessetwd(dirname(rstudioapi::getActiveDocumentContext()$path))

study1 <- read.delim("exemplarsall/myData1.txt")
study2 <- read.delim("exemplarsall/myData2.txt")
study3 <- read.delim("exemplarsall/myData3.txt")
study4 <- read.delim("exemplarsall/myData4.txt") #all White sample
study5 <- read.delim("exemplarsall/myData5.txt")
study6 <- read.delim("exemplarsall/myData6.txt")
study7 <- read.delim("exemplarsall/myData7.txt")
#study8 <- read.delim("exemplarsall/myData8.txt")
#study9 <- read.delim("exemplarsall/myData9.txt")
#study10 <- read.delim("exemplarsall/myData10.txt")
#study11 <- read.delim("exemplarsall/myData11.txt")

study4 <- study4 %>% mutate(raceethnic = recode(raceethnic, "white" = "White"))

myColNames <- Reduce(intersect, list(colnames(study1), colnames(study2), colnames(study3),
                                     colnames(study4), colnames(study5), colnames(study6),
                                     colnames(study7)))

myColNames2 <- unique(c(colnames(study1), colnames(study2), colnames(study3),
                        colnames(study4), colnames(study5), colnames(study6),
                        colnames(study7)))

study1[, myColNames2[!(myColNames2 %in% colnames(study1))]] <- NA
study1 <- study1[, myColNames2]
study1$study <- "Study 1"

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

myDataAll <- rbind(study1, study2, study3, study4, study5, study6, study7)


myDataAll %>% 
  group_by(study) %>% 
  summarise(number = table(learnCond), n = n())


table(study1$raceethnic)
table(study2$raceethnic)
table(study3$raceethnic)
table(study4$raceethnic)
table(study5$raceethnic)
table(study6$raceethnic)
table(study7$raceethnic)



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
