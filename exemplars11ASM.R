if (!require(BayesFactor)) {install.packages("BayesFactor"); require(BayesFactor)}
if (!require(beanplot)) {install.packages("beanplot"); require(beanplot)}
if (!require(car)) {install.packages("car"); require(car)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(pastecs)) {install.packages("pastecs"); require(pastecs)}
if (!require(corrplot)) {install.packages("corrplot"); require(corrplot)}
if (!require(bayesplot)) {install.packages("bayesplot"); require(bayesplot)}
if (!require(rstanarm)) {install.packages("rstanarm"); require(rstanarm)}
if (!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Reading in and cleaning IAT file -----------------------------------------------------

iatLong <- read.delim("iat.txt")

table(iatLong$task_name)

iatLong <- iatLong[iatLong$task_name == "race_iat", ]

head(iatLong)
dim(iatLong)

iatLong <- iatLong[order(iatLong$session_id, iatLong$trial_number), ]
head(iatLong)

str(iatLong)

rownames(iatLong) <- 1:nrow(iatLong)

table(iatLong$block_pairing_definition)
table(iatLong$task_name)
table(iatLong$trial_name)
table(iatLong$session_id)

tapply(iatLong$trial_latency, iatLong$session_id, mean)
hist(tapply(iatLong$trial_latency, iatLong$session_id, mean), col = "white")

stat.desc(tapply(iatLong$trial_latency, iatLong$session_id, mean))
round(stat.desc(tapply(iatLong$trial_latency, iatLong$session_id, mean)), 3)

hist(tapply(iatLong$trial_latency, iatLong$session_id, mean)[tapply(iatLong$trial_latency, iatLong$session_id, mean) < 10000],
     col = "white")

latencyBySubj <- tapply(iatLong$trial_latency, iatLong$session_id, mean)[tapply(iatLong$trial_latency, iatLong$session_id, mean) < 10000]

hist(latencyBySubj,
     main = "Average response latency by participant", xlab = "Latency (ms)",
     xlim = c(0, 8000), breaks = 30, col = "white")
abline(v = mean(latencyBySubj), col = "red")
abline(v = median(latencyBySubj), col = "blue")


incompleteSubjects <- names(table(iatLong$session_id)[!(table(iatLong$session_id) == 146)])
completeIAT <- iatLong[!(iatLong$session_id %in% incompleteSubjects), ]

nrow(completeIAT)/nrow(iatLong)
table(unique(iatLong$session_id) %in% completeIAT$session_id)

table(completeIAT$trial_latency > 10000)
table(completeIAT$trial_latency < 0)

completeIAT$trial_latency[completeIAT$trial_latency > 10000] <- NA
table(completeIAT$trial_latency > 10000)
table(completeIAT$trial_latency > 10000, useNA = "ifany")


table(completeIAT$session_id, completeIAT$trial_latency < 300)
table(completeIAT$session_id, completeIAT$trial_latency < 300)[, "TRUE"]
table(table(completeIAT$session_id, completeIAT$trial_latency < 300)[, "TRUE"] > 14.6)
table(completeIAT$session_id, completeIAT$trial_latency < 300)[, "TRUE"][table(completeIAT$session_id, completeIAT$trial_latency < 300)[, "TRUE"] > 14.6]

tooFast <- names(table(completeIAT$session_id, completeIAT$trial_latency < 300)[, "TRUE"][table(completeIAT$session_id, completeIAT$trial_latency < 300)[, "TRUE"] > 14])

cleanIAT <- completeIAT[!(completeIAT$session_id %in% tooFast), ]

nrow(cleanIAT)/nrow(iatLong)
table(unique(iatLong$session_id) %in% cleanIAT$session_id)

table(cleanIAT$block_pairing_definition, cleanIAT$block_number)

iatCritical <- cleanIAT[cleanIAT$block_number %in% c(3, 5), ]
table(iatCritical$block_pairing_definition, iatCritical$block_number)

iatCritical$block_pairing_definition <- factor(iatCritical$block_pairing_definition)
table(iatCritical$block_pairing_definition, iatCritical$block_number)

table(iatCritical$block_pairing_definition, iatCritical$block_number)

meanRTs <- tapply(iatCritical$trial_latency, list(iatCritical$session_id, iatCritical$block_number),
                  mean, na.rm = TRUE)
meanRTs <- as.data.frame(meanRTs)
colnames(meanRTs) <- c("block3", "block5")

pooledSD <- tapply(iatCritical$trial_latency, iatCritical$session_id, sd, na.rm = TRUE)
pooledSD <- as.data.frame(pooledSD)

meanRTs$session_id <- rownames(meanRTs)
pooledSD$session_id <- rownames(pooledSD)

head(meanRTs)
head(pooledSD)

rownames(meanRTs) <- 1:nrow(meanRTs)
rownames(pooledSD) <- 1:nrow(pooledSD)

meanRTs <- meanRTs[, c("session_id", "block3", "block5")]
pooledSD <- pooledSD[, c("session_id", "pooledSD")]

meanRTandSD <- merge(meanRTs, pooledSD, by = "session_id")
head(meanRTandSD)

meanRTandSD$D_Score <- (meanRTandSD$block5 - meanRTandSD$block3) / meanRTandSD$pooledSD
meanRTandSD$D_Score <- (with(meanRTandSD, (block5 - block3) / pooledSD))

table((meanRTandSD$block5 - meanRTandSD$block3) / meanRTandSD$pooledSD == (with(meanRTandSD, (block5 - block3) / pooledSD)))


# Look at item effects ----------------------------------------------------

tapply(iatLong[iatLong$block_number == 1, ]$trial_latency, iatLong[iatLong$block_number == 1, ]$trial_name, median)
tapply(iatLong[iatLong$block_number == 1, ]$trial_error, iatLong[iatLong$block_number == 1, ]$trial_name, mean)

tapply(iatLong[iatLong$block_number == 2, ]$trial_latency, iatLong[iatLong$block_number == 2, ]$trial_name, median)
tapply(iatLong[iatLong$block_number == 2, ]$trial_error, iatLong[iatLong$block_number == 2, ]$trial_name, mean)

# Reading in and cleaning demographics file -----------------------------------------------------

demLong <- read.delim("demographics.txt")

dem <- reshape(demLong, timevar = "characteristic", idvar = "user_id",
               direction = "wide", drop = c("study_name", "X"))
names(dem) <- gsub("value.", "", names(dem))
rownames(dem) <- 1:nrow(dem)

head(dem)

sessions <- read.delim("sessions.txt", na.strings = ".")

sessionsDem <- merge(sessions, dem, by = "user_id")
head(sessionsDem)

sessionsDem <- sessionsDem[, c("session_id", "gender", "birthyear", "politicalid",
                               "raceethnic","citizenship")]
head(sessionsDem)
str(sessionsDem)

sessionsDem$session_id <- factor(sessionsDem$session_id)

table(sessionsDem$gender)
sessionsDem$gender <- factor(sessionsDem$gender)

sessionsDem$gender[sessionsDem$gender == "null"] <- NA

table(sessionsDem$gender)
sessionsDem$gender <- factor(sessionsDem$gender)
sessionsDem$gender <- factor(sessionsDem$gender, labels = c("Female", "Male", "Other"))

str(sessionsDem)

sessionsDem$birthyear <- as.numeric(as.character(sessionsDem$birthyear))
sessionsDem$age <- 2021 - sessionsDem$birthyear

sessionsDem <- sessionsDem[, !(colnames(sessionsDem) == "birthyear")]

sessionsDem$politicalid <- factor(sessionsDem$politicalid, levels = c(-3:3), 
                                  labels = c("Strongly conservative", "Moderately conservative", "Slightly conservative", 
                                             "Neutral","Slightly liberal", "Moderately liberal", "Strongly liberal"))
table(sessionsDem$politicalid)

str(sessionsDem)

table(sessionsDem$raceethnic)
sessionsDem$raceethnic[sessionsDem$raceethnic == "null"] <- NA
sessionsDem$raceethnic <- factor(sessionsDem$raceethnic)

sessionsDem$raceethnic <- factor(sessionsDem$raceethnic,
                                 labels = c("Asian", "Black", "Hispanic", 
                                            "Middle Eastern", "Multiracial", "Native American", 
                                            "Pacific Islander", "White"))

head(sessionsDem)

sessionsDem$citizenship <- factor(sessionsDem$citizenship)
table(sessionsDem$citizenship)


table(sessionsDem$gender)
table(sessionsDem$politicalid)
table(sessionsDem$raceethnic)
mean(sessionsDem$age, na.rm = TRUE)
table(sessionsDem$citizenship)

includingDemogr <- merge(x = meanRTandSD, y = sessionsDem, 
                         by = "session_id", all.x = TRUE)
includingDemogrAll <- includingDemogr
includingDemogr <- includingDemogr[includingDemogr$citizenship == "us", ]

#Alex code
#includingDemogr <- includingDemogr[includingDemogr$raceethnic == "White", ]

# Reading in and cleaning explicit file -----------------------------------

expLong <- read.delim("explicit.txt", na.strings = "-999")

expLong[expLong$questionnaire_name %in% c("control_attention", "exp_attention"), "question_name"] <- gsub("exp_check6", expLong[expLong$questionnaire_name %in% c("control_attention", "exp_attention"), "question_name"])

exp <- reshape(expLong, timevar = "question_name", idvar = "session_id", direction = "wide",
               drop = c("task_number", "question_number", "questionnaire_name", "attempt",
                        "study_name", "text", "feedback", "IMPTASKTO"))

colnames(exp) <- gsub("question_response.", "", colnames(exp))

head(exp)

semanticDiffItems <- paste0("semantic", c("Beauty", "Good", "Honest", "Pleasant"))
contigencyItems <- paste0("exp_check", 1:6)

explicitItems <- c("thermoBlack", "thermoWhite",
                   paste0(semanticDiffItems, "Black"), paste0(semanticDiffItems, "White"))

exp <- exp[, c("session_id", "d", "block3Cond", explicitItems, contigencyItems)]

includingExplicit <- merge(includingDemogr, exp,
                           by = "session_id", all.x = TRUE)
rownames(includingExplicit) <- 1:nrow(includingExplicit)

includingExplicitAll <- merge(includingDemogrAll, exp, by = "session_id", all.x = TRUE)
rownames(includingExplicitAll) <- 1:nrow(includingExplicitAll)

myData <- includingExplicit
myDataAll <- includingExplicitAll

myData <- myData[!is.na(myData$D_Score), ]
myDataAll <- myDataAll[!is.na(myDataAll$D_Score), ]

# Demographics ------------------------------------------------------------

table(myData$gender, useNA = "always")
barplot(table(myData$gender, useNA = "always"), col = "white", main = "Participant gender",
        names.arg = c("Female", "Male", "Other", "NA"))

myData$age <- as.numeric(paste(myData$age))
mean(myData$age, na.rm = TRUE)
sd(myData$age, na.rm = TRUE)
hist(myData$age, main = "Participant age", xlab = "Age (years)", xlim = c(0, 100), breaks = 20, col = "white")

table(myData$politicalid, useNA = "always")
barplot(table(myData$politicalid, useNA = "always"), col = "white", main = "Participant ideology")

table(myData$raceethnic, useNA = "always")
barplot(table(myData$raceethnic, useNA = "always"), col = "white", main = "Participant race",
        names.arg = c(levels(myData$raceethnic), "NA"))

table(myData$citizenship, useNA = "always")
barplot(table(myData$citizenship, useNA = "always"), col = "white", main = "Participant citizenship",
        names.arg = c(levels(myData$citizenship), "NA"))

myData$citizenship <- factor(myData$citizenship)
table(myData$citizenship, useNA = "always")
barplot(table(myData$citizenship, useNA = "always"), col = "white", main = "Participant citizenship",
        names.arg = c(levels(myData$citizenship), "NA"))


# Retrieve conditon -------------------------------------------------------

sessionTasks <- read.delim("sessionTasks.txt")

myData$learnCond <- NA

for (i in 1:nrow(myData)) {
  if ("control_learn" %in% sessionTasks[sessionTasks$session_id == myData$session_id[i], "task_id"]) {
    myData$learnCond[i] <- 1
  }
  else if ("exp_learn" %in% sessionTasks[sessionTasks$session_id == myData$session_id[i], "task_id"]) {
    myData$learnCond[i] <- 2
  }  
  print(i)
}

table(myData$learnCond)

myData$learnCond <- factor(myData$learnCond, levels = c(1, 2), labels = c("Control", "Experimental"))

myData <- myData[!is.na(myData$learnCond), ]

# IAT analyses------------------------------------------------------------

table(myData$block3Cond)
myData$block3Cond <- factor(myData$block3Cond)
table(myData$block3Cond, useNA = "always")

head(myData)

myData$order <- car::recode(myData$block3Cond, "'Black Americans/Pleasant Words,White Americans/Unpleasant Words' = 'Incongruent first';
                    'White Americans/Unpleasant Words,Black Americans/Pleasant Words' = 'Incongruent first'; else = 'Congruent first'")
table(myData$order)

myData$d <- as.numeric(paste(myData$d))

cor(myData$D_Score, myData$d, use = "pairwise.complete.obs")
plot(myData$D_Score, myData$d)

myData <- myData[!is.na(myData$order), ]
myData$D_Score[myData$order == "Incongruent first"] <- myData$D_Score[myData$order == "Incongruent first"]*(-1)

plot(myData$D_Score, myData$d)
cor(myData$D_Score, myData$d, use = "pairwise.complete.obs")

head(myData)

pdf("D score distribution.pdf", width = 10)
hist(myData$D_Score, main = "Distribution of D scores",
     xlab = "Race attitude D score", breaks = 10, col = "white")
dev.off()

mean(myData$D_Score, na.rm = TRUE)
sd(myData$D_Score, na.rm = TRUE)
head(myData)
table(myData$D_Score > 0)


# Does it significantly differ from 0?
t.test(myData$D_Score, mu = 0)
t.test(myData$D_Score)
ttestBF(myData$D_Score)
cohensD(as.numeric(myData$D_Score))

myTest <- t.test(myData$D_Score, mu = 0)

beanplot(myData$D_Score,
         what = c(1, 1, 1, 1),
         col = rainbow(4, alpha = 0.20)[1],
         main = "Distribution of IAT D scores",
         axes = FALSE, bw = 0.2, xlab = "",
         ylim = c(-2, 2))
axis(2)
mtext("Race implicit attitude")

lmOrder <- lm(D_Score ~ order, data = myData)
summary(lmOrder)


pdf("IAT order effect.pdf", width = 10)
beanplot(myData$D_Score[myData$order == "Congruent first"],
         myData$D_Score[myData$order == "Incongruent first"],
         what = c(1, 1, 1, 1),
         col = rainbow(4, alpha = 0.20)[1],
         main = "Distribution of IAT D scores",
         axes = FALSE, bw = 0.2, xlab = "",
         ylim = c(-2, 2))
axis(1, at = 1:2, levels(myData$order))
axis(2)
mtext("Race implicit attitude")
dev.off()

tapply(myData$D_Score, myData$order, mean)

t.test(myData$D_Score ~ myData$order)
ttestBF(myData$D_Score[myData$order == "Congruent first"],
        myData$D_Score[myData$order == "Incongruent first"])
cohensD(myData$D_Score ~ myData$order)


t.test(myData[myData$order == "Congruent first", ]$D_Score)
ttestBF(myData[myData$order == "Congruent first", ]$D_Score)
cohensD(as.numeric(myData[myData$order == "Congruent first", ]$D_Score))

t.test(myData[myData$order == "Incongruent first", ]$D_Score)
ttestBF(myData[myData$order == "Incongruent first", ]$D_Score)
cohensD(as.numeric(myData[myData$order == "Incongruent first", ]$D_Score))

lmCond <- lm(D_Score ~ learnCond, data = myData)
summary(lmCond)

1/lmBF(D_Score ~ learnCond, data = myData)

lmCond2 <- lm(D_Score ~ learnCond, data = myData[myData$raceethn == "White", ])
summary(lmCond2)

1/lmBF(D_Score ~ learnCond, data = myData[!is.na(myData$raceethn) & myData$raceethn == "White", ])

lmCond3 <- lm(D_Score ~ learnCond, data = myData[myData$citizenship == "us", ])
summary(lmCond3)

1/lmBF(D_Score ~ learnCond, data = myData[!is.na(myData$raceethn) & myData$raceethn == "White", ])

pdf("IAT condition effect.pdf", width = 10)
beanplot(myData$D_Score[myData$learnCond == "Control"],
         myData$D_Score[myData$learnCond == "Experimental"],
         what = c(1, 1, 1, 1),
         col = rainbow(4, alpha = 0.20)[1],
         main = "Distribution of IAT D scores",
         axes = FALSE, bw = 0.2, xlab = "",
         ylim = c(-2, 2))
axis(1, at = 1:2, levels(myData$learnCond))
axis(2)
mtext("Race implicit attitude")
dev.off()

tapply(myData$D_Score, myData$learnCond, mean)
tapply(myData$D_Score, myData$learnCond, cohensD)

cohensD(myData$D_Score ~ myData$learnCond)

tapply(myData[myData$raceethn == "White", ]$D_Score, myData[myData$raceethn == "White", ]$learnCond, mean)
tapply(myData[myData$raceethn == "White", ]$D_Score, myData[myData$raceethn == "White", ]$learnCond, cohensD)

cohensD(myData[myData$raceethn == "White", ]$D_Score ~ myData[myData$raceethn == "White", ]$learnCond)


# Explicit attitudes ------------------------------------------------------

head(myData)

for (i in 1:length(explicitItems)) {
  myData[, explicitItems[i]][myData[, explicitItems[i]] == "-999"] <- NA
  myData[, explicitItems[i]] <- as.numeric(paste(myData[, explicitItems[i]]))
}

corrplot(cor(myData[, explicitItems], use = "pairwise.complete.obs"))

apply(myData[, explicitItems], 2, mean, na.rm = TRUE)

myData$expBlack <- apply(myData[, explicitItems[grepl("Black", explicitItems)]], 1, mean, na.rm = TRUE)
myData$expWhite <- apply(myData[, explicitItems[grepl("White", explicitItems)]], 1, mean, na.rm = TRUE)

myData$expDiff <- myData$expWhite - myData$expBlack

hist(myData$expDiff, col = "white")

cor.test(myData$D_Score, myData$expDiff)

lmCondExp <- lm(expDiff ~ learnCond, data = myData)
summary(lmCondExp)

1/lmBF(expDiff ~ learnCond, data = myData[!is.na(myData$expDiff), ])

lmCondExp2 <- lm(expDiff ~ learnCond, data = myData[myData$raceethn == "White" & !is.na(myData$expDiff), ])
summary(lmCondExp2)

1/lmBF(expDiff ~ learnCond, data = myData[!is.na(myData$raceethn) & !is.na(myData$expDiff)
                                          & myData$raceethn == "White", ])


# Learning trials ---------------------------------------------------------

learn <- read.delim("iat.txt")

table(learn$task_name)

learn <- learn[learn$task_name %in% c("control_learn", "control_learn2", "exp_learn", "exp_learn2"), ]
learn <- learn[learn$session_id %in% myData$session_id, ]

head(learn)

learn1 <- learn[learn$task_name %in% c("control_learn", "exp_learn"), ]

table(learn1$trial_error)
table(learn1$task_name, learn1$trial_error)
prop.table(table(learn1$task_name, learn1$trial_error), margin = 1)

prop.table(table(learn1$session_id, learn1$trial_error), margin = 1)[, 1]
table(prop.table(table(learn1$session_id, learn1$trial_error), margin = 1)[, 1])


learn2 <- learn[learn$task_name %in% c("control_learn2", "exp_learn2"), ]

table(learn2$trial_error)
table(learn2$task_name, learn2$trial_error)
prop.table(table(learn2$task_name, learn2$trial_error), margin = 1)

prop.table(table(learn2$session_id, learn2$trial_error), margin = 1)[, 1]
table(prop.table(table(learn2$session_id, learn2$trial_error), margin = 1)[, 1])

# fBetweenToD <- function(F, df) {
#      if (F >= 0) cohensD <- 2*(sqrt(F/df))
#      if (F < 0) cohensD <- -2*(sqrt(abs(F)/df))
#      return(cohensD)
# }
# 
# fBetweenToD(6.79, 31)


# # Sample size simulation --------------------------------------------------

sampSizeVec <- seq(100, 1000, by = 100)

bfList <- vector(mode = "list", length = length(sampSizeVec))

set.seed(125)
for (i in 1:length(bfList)) {
  for (j in 1:1000) {
    bfList[[i]][j] <- extractBF(lmBF(D_Score ~ learnCond, myData[sample(1:nrow(myData), sampSizeVec[i], replace = TRUE), ]))$bf
    print(paste(i, j))
  }
}

round(bfList[[10]], 2)

plot(1:10, unlist(lapply(bfList, median)))

sampSizeVec2 <- seq(100, 5000, by = 500)

pValList <- vector(mode = "list", length = length(sampSizeVec))

set.seed(125)
for (i in 1:length(pValList)) {
  for (j in 1:1000) {
    pValList[[i]][j] <- t.test(D_Score ~ learnCond, myData[sample(1:nrow(myData), sampSizeVec[i], replace = TRUE), ])$p.val
    print(paste(i, j))
  }
}


plot(1:10, unlist(lapply(pValList, function(x) sum(x < .05)))/1000)

sampSizeVec2 <- seq(100, 5000, by = 500)

pValList2 <- vector(mode = "list", length = length(sampSizeVec2))

set.seed(125)
for (i in 1:length(pValList2)) {
  for (j in 1:1000) {
    pValList2[[i]][j] <- t.test(D_Score ~ learnCond, myData[sample(1:nrow(myData), sampSizeVec2[i], replace = TRUE), ])$p.val
    print(paste(i, j))
  }
}

pdf("Power by sample size.pdf", width = 10)
plot(1:10, unlist(lapply(pValList, function(x) sum(x < .05)))/1000, axes = FALSE, main = "Power by sample size",
     xlab = "Sample size", ylab = "Power", ylim = c(0, 1), pch = 21, bg = "red")
axis(1, at = 1:10, labels = sampSizeVec)
axis(2)
abline(h = 0.8, lty = 3)
dev.off()

pdf("Power by sample size 2.pdf", width = 10)
plot(1:10, unlist(lapply(pValList2, function(x) sum(x < .05)))/1000, axes = FALSE, main = "Power by sample size",
     xlab = "Sample size", ylab = "Power", ylim = c(0, 1), pch = 21, bg = "red")
axis(1, at = 1:10, labels = sampSizeVec2)
axis(2)
abline(h = 0.8, lty = 3)
dev.off()

bfList2 <- vector(mode = "list", length = length(sampSizeVec2))

set.seed(125)
for (i in 1:length(bfList2)) {
  for (j in 1:1000) {
    bfList2[[i]][j] <- extractBF(lmBF(D_Score ~ learnCond, myData[sample(1:nrow(myData), sampSizeVec2[i], replace = TRUE), ]))$bf
    print(paste(i, j))
  }
}

plot(1:10, 1/unlist(lapply(bfList2, median)))

pdf("Bayes Factor by sample size.pdf", width = 10)
plot(1:10, unlist(lapply(bfList, median)), axes = FALSE, main = "Bayes Factor by sample size",
     xlab = "Sample size", ylab = "Median BF", pch = 21, bg = "red")
abline(h = 10, lty = 3)
axis(1, at = 1:10, labels = sampSizeVec)
axis(2)
dev.off()

pdf("Bayes Factor by sample size 2.pdf", width = 10)
plot(1:10, unlist(lapply(bfList2, median)), axes = FALSE, main = "Bayes Factor by sample size",
     xlab = "Sample size", ylab = "Median BF", pch = 21, bg = "red")
abline(h = 10, lty = 3)
axis(1, at = 1:10, labels = sampSizeVec2)
axis(2)
dev.off()

# 700/(nrow(myData)/nrow(myDataAll))
# 700/(nrow(myData[myData$raceethnic == "White", ])/nrow(myDataAll))

# Bayesian equivalence testing --------------------------------------------

myData$D_ScoreZ <- scale(myData$D_Score)

summary(lm(D_ScoreZ ~ learnCond, myData))

bayesFit <- stan_lm(D_ScoreZ ~ learnCond, data = myData, prior = NULL, seed = 12345)

summary(bayesFit)

posteriorBayesFit <- as.array(bayesFit)
dim(posteriorBayesFit)

mcmc_intervals(posteriorBayesFit)
mcmc_hist(posteriorBayesFit)

# Extract and summarize the posterior draws
postSlopeBayesFit <- as.matrix(bayesFit, pars = "learnCondExperimental")

# Compute mean, sd, and quantiles (2.5% and 97.5%) for intercept parameter
mean(postSlopeBayesFit)
sd(postSlopeBayesFit)
quantile(postSlopeBayesFit, c(0.025, 0.975))

# Compute the proportion of posterior samples that fall into the range consistent with H0 (i.e., below 0.10)

postSlopeBayesFit <- as.data.frame(postSlopeBayesFit)
colnames(postSlopeBayesFit) <- "mean"
postSlopeBayesFit$null <- ifelse(postSlopeBayesFit$mean > -0.1, "Null", "Alt")
table(postSlopeBayesFit$null)/sum(table(postSlopeBayesFit$null)) # Percentage of samples falling within the null range


plot(density(postSlopeBayesFit$mean), main = "Bayesian Equivalence Testing",
     axes = FALSE, xlab = expression(beta),
     col = "transparent", xlim = c(-0.8, 0.8), ylim = c(0, 7))
axis(1)
axis(2, at = seq(0, 7, 1), las = 1)
polygon(x = c(density(postSlopeBayesFit$mean)$x[density(postSlopeBayesFit$mean)$x > -0.1],
              max(density(postSlopeBayesFit$mean)$x[density(postSlopeBayesFit$mean)$x > -0.1]), 
              min(density(postSlopeBayesFit$mean)$x[density(postSlopeBayesFit$mean)$x > -0.1])),
        y = c(density(postSlopeBayesFit$mean)$y[density(postSlopeBayesFit$mean)$x > -0.1], 0, 0),
        col = rainbow(2, alpha = 0.4)[1], border = "transparent")
polygon(x = c(density(postSlopeBayesFit$mean)$x[density(postSlopeBayesFit$mean)$x < -0.1],
              max(density(postSlopeBayesFit$mean)$x[density(postSlopeBayesFit$mean)$x < -0.1]), 
              min(density(postSlopeBayesFit$mean)$x[density(postSlopeBayesFit$mean)$x < -0.1])),
        y = c(density(postSlopeBayesFit$mean)$y[density(postSlopeBayesFit$mean)$x < -0.1], 0, 0),
        col = rainbow(2, alpha = 0.4)[2], border = "transparent") 
abline(v = -0.1)
abline(v = mean(postSlopeBayesFit$mean), lty = 2)
text(0.5, 4, paste0(format(round(table(postSlopeBayesFit$null)/sum(table(postSlopeBayesFit$null))*100, 2), nsmall = 2), "%")[2],
     col = rainbow(2)[1])
text(-0.5, 4, paste0(format(round(table(postSlopeBayesFit$null)/sum(table(postSlopeBayesFit$null))*100, 2), nsmall = 2), "%")[1],
     col = rainbow(2)[2])

# # Retrieve conditon -------------------------------------------------------
# 
# myDataAll$learnCond <- NA
# 
# for (i in 1:nrow(myDataAll)) {
#      if ("control_learn2" %in% sessionTasks[sessionTasks$session_id == myDataAll$session_id[i], "task_id"]) {
#           myDataAll$learnCond[i] <- 1
#      }
#      else if ("exp_learn2" %in% sessionTasks[sessionTasks$session_id == myDataAll$session_id[i], "task_id"]) {
#           myDataAll$learnCond[i] <- 2
#      }  
#      print(i)
# }
# 
# table(myDataAll$learnCond, useNA = "always")
# 
# myDataAll$learnCond <- factor(myDataAll$learnCond, levels = c(1, 2), labels = c("Control", "Experimental"))
# 
# myDataAll <- myDataAll[!is.na(myDataAll$learnCond), ]
# 
# # IAT analyses------------------------------------------------------------
# 
# table(myDataAll$block3Cond)
# myDataAll$block3Cond <- factor(myDataAll$block3Cond)
# table(myDataAll$block3Cond, useNA = "always")
# 
# head(myDataAll)
# 
# myDataAll$order <- car::recode(myDataAll$block3Cond, "'Black Americans/Pleasant Words,White Americans/Unpleasant Words' = 'Incongruent first';
#                             'White Americans/Unpleasant Words,Black Americans/Pleasant Words' = 'Incongruent first'; else = 'Congruent first'")
# table(myDataAll$order)
# 
# myDataAll$d <- as.numeric(paste(myDataAll$d))
# 
# cor(myDataAll$D_Score, myDataAll$d, use = "pairwise.complete.obs")
# plot(myDataAll$D_Score, myDataAll$d)
# 
# myDataAll <- myDataAll[!is.na(myDataAll$order), ]
# myDataAll$D_Score[myDataAll$order == "Incongruent first"] <- myDataAll$D_Score[myDataAll$order == "Incongruent first"]*(-1)
# 
# plot(myDataAll$D_Score, myDataAll$d)
# cor(myDataAll$D_Score, myDataAll$d, use = "pairwise.complete.obs")
# 
# head(myDataAll)
# 
# # pdf("D score distribution.pdf", width = 10)
# hist(myDataAll$D_Score, main = "Distribution of D scores",
#      xlab = "Race attitude D score", breaks = 10, col = "white")
# # dev.off()
# 
# mean(myDataAll$D_Score, na.rm = TRUE)
# sd(myDataAll$D_Score, na.rm = TRUE)
# head(myDataAll)
# table(myDataAll$D_Score > 0)
# 
# 
# # Does it significantly differ from 0?
# t.test(myDataAll$D_Score, mu = 0)
# t.test(myDataAll$D_Score)
# ttestBF(myDataAll$D_Score)
# cohensD(as.numeric(myDataAll$D_Score))
# 
# # myTest <- t.test(myDataAll$D_Score, mu = 0)
# 
# beanplot(myDataAll$D_Score,
#          what = c(1, 1, 1, 1),
#          col = rainbow(4, alpha = 0.20)[1],
#          main = "Distribution of IAT D scores",
#          axes = FALSE, bw = 0.2, xlab = "",
#          ylim = c(-2, 2))
# axis(2)
# mtext("Race implicit attitude")
# 
# lmOrderAll <- lm(D_Score ~ order, data = myDataAll)
# summary(lmOrderAll)
# 
# 
# # pdf("IAT order effect.pdf", width = 10)
# beanplot(myDataAll$D_Score[myDataAll$order == "Congruent first"],
#          myDataAll$D_Score[myDataAll$order == "Incongruent first"],
#          what = c(1, 1, 1, 1),
#          col = rainbow(4, alpha = 0.20)[1],
#          main = "Distribution of IAT D scores",
#          axes = FALSE, bw = 0.2, xlab = "",
#          ylim = c(-2, 2))
# axis(1, at = 1:2, levels(myDataAll$order))
# axis(2)
# mtext("Race implicit attitude")
# # dev.off()
# 
# tapply(myDataAll$D_Score, myDataAll$order, mean)
# 
# t.test(myDataAll$D_Score ~ myDataAll$order)
# ttestBF(myDataAll$D_Score[myDataAll$order == "Congruent first"],
#         myDataAll$D_Score[myDataAll$order == "Incongruent first"])
# cohensD(myDataAll$D_Score ~ myDataAll$order)
# 
# 
# t.test(myDataAll[myDataAll$order == "Congruent first", ]$D_Score)
# ttestBF(myDataAll[myDataAll$order == "Congruent first", ]$D_Score)
# cohensD(as.numeric(myDataAll[myDataAll$order == "Congruent first", ]$D_Score))
# 
# t.test(myDataAll[myDataAll$order == "Incongruent first", ]$D_Score)
# ttestBF(myDataAll[myDataAll$order == "Incongruent first", ]$D_Score)
# cohensD(as.numeric(myDataAll[myDataAll$order == "Incongruent first", ]$D_Score))
# 
# lmCondAll <- lm(D_Score ~ learnCond, data = myDataAll)
# summary(lmCondAll)
# 
# lmBF(D_Score ~ learnCond, data = myDataAll)
# 
# 
# # pdf("IAT condition effect.pdf", width = 10)
# beanplot(myDataAll$D_Score[myDataAll$learnCond == "Control"],
#          myDataAll$D_Score[myDataAll$learnCond == "Experimental"],
#          what = c(1, 1, 1, 1),
#          col = rainbow(4, alpha = 0.20)[1],
#          main = "Distribution of IAT D scores",
#          axes = FALSE, bw = 0.2, xlab = "",
#          ylim = c(-2, 2))
# axis(1, at = 1:2, levels(myDataAll$learnCond))
# axis(2)
# mtext("Race implicit attitude")
# # dev.off()
# 
# tapply(myDataAll$D_Score, myDataAll$learnCond, mean)
# tapply(myDataAll$D_Score, myDataAll$learnCond, cohensD)
# 
# cohensD(myDataAll$D_Score ~ myDataAll$learnCond)
# 
# # Explicit attitudes ------------------------------------------------------
# 
# head(myDataAll)
# 
# for (i in 1:length(explicitItems)) {
#      myDataAll[, explicitItems[i]][myDataAll[, explicitItems[i]] == "-999"] <- NA
#      myDataAll[, explicitItems[i]] <- as.numeric(paste(myDataAll[, explicitItems[i]]))
# }
# 
# corrplot(cor(myDataAll[, explicitItems], use = "pairwise.complete.obs"))
# 
# apply(myDataAll[, explicitItems], 2, mean, na.rm = TRUE)
# 
# myDataAll$expBlack <- apply(myDataAll[, explicitItems[grepl("Black", explicitItems)]], 1, mean, na.rm = TRUE)
# myDataAll$expWhite <- apply(myDataAll[, explicitItems[grepl("White", explicitItems)]], 1, mean, na.rm = TRUE)
# 
# myDataAll$expDiff <- myDataAll$expWhite - myDataAll$expBlack
# 
# hist(myDataAll$expDiff, col = "white")
# 
# cor.test(myDataAll$D_Score, myDataAll$expDiff)
# 
# lmCondExpAll <- lm(expDiff ~ learnCond, data = myDataAll)
# summary(lmCondExpAll)
# 
# 1/lmBF(expDiff ~ learnCond, data = myDataAll[!is.na(myDataAll$expDiff), ])


# New contingency memory items --------------------------------------------

table(myData$exp_check1)

# write.csv(myData[, grepl("exp_check", colnames(myData))], "contingency.csv", row.names = FALSE)
# 
# contingency <- read.csv("contingency_coded.csv")

myData <- cbind(myData, contingency[, c("aware1", "aware2")])
head(myData)

table(myData$aware1)
table(myData$aware2)

tapply(myData$D_Score, myData$aware1, mean, na.rm = TRUE)
tapply(myData$D_Score, myData$aware2, mean, na.rm = TRUE)

t.test(myData$D_Score ~ myData$aware1)

table(myData$exp_check3)

tapply(myData$D_Score, myData$exp_check3, mean, na.rm = TRUE)

t.test(myData$D_Score ~ myData$exp_check3)

table(myData$exp_check4)
tapply(myData$D_Score, myData$exp_check4, mean, na.rm = TRUE)

table(myData$exp_check5)
tapply(myData$D_Score, myData$exp_check5, mean, na.rm = TRUE)

table(myData$exp_check6)
tapply(myData$D_Score, myData$exp_check6, mean, na.rm = TRUE)

tapply(myData$D_Score, myData$learnCond, mean, na.rm = TRUE)
tapply(myData$D_Score, myData$exp_check6, length)

summary(aov(myData$D_Score ~ myData$exp_check6))

cohensD(myData[myData$learnCond == "Control", ]$D_Score,
        myData[myData$learnCond == "Experimental" & myData$exp_check6 == "2", ]$D_Score)

#Alex code

myDataControl <- myData[myData$learnCond == "Control", ] %>% 
  mutate(pass456 = "control")

myDataExpNoAnswer <- myData[myData$learnCond == "Experimental" & is.na(myData$exp_check6), ] %>% 
  mutate(pass456 = "noAnswer")

myDataExpPass <- myData[myData$learnCond == "Experimental" &
                          (!is.na(myData$exp_check6)) &
                          ((myData$exp_check4 == "1" | myData$exp_check5 == "2") & myData$exp_check6 == "2"), ] %>% 
  mutate(pass456 = "pass")

myDataExpFail <- myData[myData$learnCond == "Experimental" &
                          (!is.na(myData$exp_check6) &
                             !((myData$exp_check4 == "1" | myData$exp_check5 == "2") & myData$exp_check6 == "2")), ] %>% 
  mutate(pass456 = "fail")

myData <- rbind(myDataExpFail, myDataExpPass, myDataExpNoAnswer, myDataControl)

#View(myData[myData$pass456 == "pass", ])

table(myData$pass456, useNA = "always")

###

myDataPass <- myData[myData$pass456 == "pass" | myData$pass456 == "control", ]
table(myDataPass$learnCond, useNA = "always")

lmCond <- lm(D_Score ~ learnCond, data = myDataPass)
summary(lmCond)

1/lmBF(D_Score ~ learnCond, data = myDataPass)

lmCond2 <- lm(D_Score ~ learnCond, data = myDataPass[myDataPass$raceethn == "White", ])
summary(lmCond2)

1/lmBF(D_Score ~ learnCond, data = myDataPass[!is.na(myDataPass$raceethn) & myDataPass$raceethn == "White", ])

lmCond3 <- lm(D_Score ~ learnCond, data = myDataPass[myDataPass$citizenship == "us", ])
summary(lmCond3)

1/lmBF(D_Score ~ learnCond, data = myDataPass[!is.na(myDataPass$raceethn) & myDataPass$raceethn == "White", ])

pdf("IAT condition effect.pdf", width = 10)
beanplot(myDataPass$D_Score[myDataPass$learnCond == "Control"],
         myDataPass$D_Score[myDataPass$learnCond == "Experimental"],
         what = c(1, 1, 1, 1),
         col = rainbow(4, alpha = 0.20)[1],
         main = "Distribution of IAT D scores",
         axes = FALSE, bw = 0.2, xlab = "",
         ylim = c(-2, 2))
axis(1, at = 1:2, levels(myDataPass$learnCond))
axis(2)
mtext("Race implicit attitude")
dev.off()

tapply(myDataPass$D_Score, myDataPass$learnCond, mean)
tapply(myDataPass$D_Score, myDataPass$learnCond, cohensD)

cohensD(myDataPass$D_Score ~ myDataPass$learnCond)

tapply(myDataPass[myDataPass$raceethn == "White", ]$D_Score, myDataPass[myDataPass$raceethn == "White", ]$learnCond, mean)
tapply(myDataPass[myDataPass$raceethn == "White", ]$D_Score, myDataPass[myDataPass$raceethn == "White", ]$learnCond, cohensD)

cohensD(myDataPass[myDataPass$raceethn == "White", ]$D_Score ~ myDataPass[myDataPass$raceethn == "White", ]$learnCond)
# Meta-analysis -----------------------------------------------------------

t.test(myData$D_Score ~ myData$learnCond, var.equal = TRUE)
table(myData$learnCond)

tVec <- c(0.75009, 0.29966, 2.3377, 0.7517, 1.3696)
n1Vec <- c(148, 500, 159, 345, 215)
n2Vec <- c(167, 579, 167, 341, 203)

sum(n1Vec) + sum(n2Vec)

1/meta.ttestBF(tVec, n1Vec, n2Vec)
postBF <- meta.ttestBF(tVec, n1Vec, n2Vec, posterior = TRUE, iterations = 1000)

pdf("Posterior.pdf", width = 10)
hist(postBF, col = "white")
abline(v = quantile(postBF, c(0.025, 0.975)), col = "red")
abline(v = mean(postBF), col = "blue")
dev.off()