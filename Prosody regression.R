rm(list=ls())
#install.packages("corrplot")
#install.packages("ggplot2")
library(ggplot2)
library(corrplot)
library(languageR)
library(lme4)
library(lmerTest)
library(grid)

# ------------------------------SET UP INPUT FILE(S)----------------------------
d.base      <- read.csv("data/d_prosody.csv",colClasses="character")
d.base$subject  <- as.factor(d.base$subject)
d.base$item    <- as.factor(d.base$item)
#d.base$word.type <- as.factor(d.base$word.type)
d.base$seconds  <- as.numeric(d.base$seconds)
# scaling freq or not makes no difference in whether models run, but DOES affect results (a bit)
d.base$freq    <- as.numeric(d.base$freq)
#d.base$freq   <- scale(as.numeric(d.base$freq), center = TRUE, scale = TRUE)
d.base$len.char  <- scale(as.numeric(d.base$len.char),   center = TRUE, scale = TRUE)
d.base$len.phon  <- scale(as.numeric(d.base$len.phon),   center = TRUE, scale = TRUE)
d.base$len.syll  <- scale(as.numeric(d.base$len.syll),   center = TRUE, scale = TRUE)
d.base$relat   <- scale(as.numeric(d.base$related),   center = TRUE, scale = TRUE)
d.base$integ   <- scale(as.numeric(d.base$integrated),  center = TRUE, scale = TRUE)
d.base$plaus   <- scale(as.numeric(d.base$plausibility), center = TRUE, scale = TRUE)
d.base$assoc   <- scale(as.numeric(d.base$association), center = TRUE, scale = TRUE)
d.base$n1.len   <- scale(as.numeric(d.base$n1.len),    center = TRUE, scale = TRUE)
d.base$p1.len   <- scale(as.numeric(d.base$p1.len),    center = TRUE, scale = TRUE)
d.base$a1.len   <- scale(as.numeric(d.base$a1.len),    center = TRUE, scale = TRUE)
d.base$n2.len   <- scale(as.numeric(d.base$n2.len),    center = TRUE, scale = TRUE)

# scaling fr.pre.wd or not makes no difference
d.base$fr.pre.wd <- as.numeric(d.base$pre.word.freq)
#d.base$fr.pre.wd <- scale(as.numeric(d.base$pre.word.freq), center = TRUE, scale = TRUE)

# scaling fr.pos.wd causes computation of residuals from 5 preamble positions to throw a warning
#d.base$fr.pos.wd <- scale(as.numeric(d.base$post.word.freq), center = TRUE, scale = TRUE)
d.base$fr.pos.wd <- as.numeric(d.base$post.word.freq)

View(d.base)


#------------------------------SET UP OUTPUT FILE------------------------------
sink("output/SemRel Prosody Analyses.txt")
cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill = 70)
cat(" ", "\n")




# ------------------------------CREATE SUBSETS-----------------------------------

d.d1 <- subset(d.base, word.type == "D1")
d.n1 <- subset(d.base, word.type == "N1")
d.p1 <- subset(d.base, word.type == "P1")
d.d2 <- subset(d.base, word.type == "D2")
d.a1 <- subset(d.base, word.type == "A1")
d.n2 <- subset(d.base, word.type == "N2")



# ------------------------------------------------------------------------------------------------------------------
# ---------------------------CREATE AND COMPARE MODELS--------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------


# D1 -------------------
cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.i <- lmer(seconds ~ integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.i))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.i.si <- lmer(seconds ~ integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.i.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Relatedness)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.r <- lmer(seconds ~ relat + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.r))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Relatedness) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.r.si <- lmer(seconds ~ relat + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1 + relat|subject) + (1+relat|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.r.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.a <- lmer(seconds ~ assoc + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1 + assoc|subject) + (1 + assoc|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.a))


cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration with Related)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.ir <- lmer(seconds ~ relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.ir))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration with Related -Random Slopes & Intercepts)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.ir.si <- lmer(seconds ~ relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1 +integ +relat|subject) + (1 +integ + relat|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.ir.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration with with Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d1.ia <- lmer(seconds ~ assoc + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(pros.d1.ia))


# N1 -------------------

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.i <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1 |subject) + (1 |item), data = d.n1, REML = TRUE)
print(summary(pros.n1.i))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.i.si <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1 + integ |subject) + (1 + integ |item), data = d.n1, REML = TRUE)
print(summary(pros.n1.i.si))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Relatedness)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.r <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1 |subject) + (1 |item), data = d.n1, REML = TRUE)
print(summary(pros.n1.r))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Relatedness) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.r.si <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1 + relat |subject) + (1 + relat|item), data = d.n1, REML = TRUE)
print(summary(pros.n1.r.si))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.a <- lmer(seconds ~ assoc + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1 |subject) + (1 |item), data = d.n1, REML = TRUE)
print(summary(pros.n1.a))


cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration with Related)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.ir <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.n1, REML = TRUE)
print(summary(pros.n1.ir))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration with Related -Random Slopes & Intercepts)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.ir.si <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1 + relat + integ|subject) + (1 + relat + integ|item), data = d.n1, REML = TRUE)
print(summary(pros.n1.ir.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration with Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n1.ia <- lmer(seconds ~ assoc + integ + plaus + freq + len.char + len.phon + len.syll + p1.len + a1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.n1, REML = TRUE)
print(summary(pros.n1.ia))


# P1 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Integration)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.i <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.i))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Integration) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.i.si <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1 + integ|subject) + (1 + integ|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.i.si))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Relatedness)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.r <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.r))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Relatedness) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.r.si <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1 + relat|subject) + (1 + relat|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.r.si))


cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.a <- lmer(seconds ~ assoc + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.a))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Integration with Related)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.ir <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.ir))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Integration with Related) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.ir.si <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1 + relat + integ|subject) + (1 + relat + integ|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.ir.si))
cat(rep(c("-"), times = 40, quote = F),"\n")

cat("SUBSET P1 (Integration with Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.p1.ia <- lmer(seconds ~ assoc + integ + plaus + freq + len.char + len.phon + n1.len + a1.len + n2.len + fr.pre.wd + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(pros.p1.ia))

# D2 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.i <- lmer(seconds ~ integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.i))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.i.si <- lmer(seconds ~ integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1 + integ|subject) + (1 + integ|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.i.si))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Relatedness)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.r <- lmer(seconds ~ relat + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.r))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Relatedness) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.r.si <- lmer(seconds ~ relat + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1 + relat|subject) + (1+ relat|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.r.si))


cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.a <- lmer(seconds ~ assoc + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.a))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration with Related)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.ir <- lmer(seconds ~ relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.ir))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration with Related) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.ir.si <- lmer(seconds ~ relat + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1 + relat + integ|subject) + (1 + relat + integ|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.ir.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration with Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.d2.ia <- lmer(seconds ~ assoc + integ + plaus + n1.len + p1.len + a1.len + n2.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(pros.d2.ia))

# A1 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.i <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.i))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.i.si <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1 + integ|subject) + (1 + integ|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.i.si))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Relatedness)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.r <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.r))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Relatedness) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.r.si <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1 + relat|subject) + (1 + relat|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.r.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.a <- lmer(seconds ~ assoc + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.a))


cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration with Related)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.ir <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.ir))


cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration with Related) -Random Slopes & Intercept", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.ir.si <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1 + relat + integ|subject) + (1 + relat + integ|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.ir.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration with Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.a1.ia <- lmer(seconds ~ assoc + integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + n2.len + fr.pos.wd + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(pros.a1.ia))

# N2 -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.i <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.i))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.i.si <- lmer(seconds ~ integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1 + integ|subject) + (1 + integ|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.i.si))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Relatedness)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.r <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.r))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Relatedness) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.r.si <- lmer(seconds ~ relat + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1 + relat|subject) + (1 + relat|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.r.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.a <- lmer(seconds ~ assoc + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.a))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration with Related)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.ir <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.ir))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration with Related) -Random Slopes & Intercepts", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.ir.si <- lmer(seconds ~ relat + integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1 + relat + integ|subject) + (1 + relat + integ|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.ir.si))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration with Association)", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
pros.n2.ia <- lmer(seconds ~ assoc + integ + plaus + freq + len.char + len.phon + len.syll + n1.len + p1.len + a1.len + fr.pre.wd + fr.pos.wd + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(pros.n2.ia))

# CORRELATION MATRIX -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("CORRELATION MATRIX", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
d.cor <-d.base[,c(5, 9:12, 19:22, 25:30)]
d.cor <-subset(d.cor, word.type == "D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type=="N2")
d.cor$word.type = NULL

cor.mat <-cor(d.cor, use = "complete")
print(cor.mat, digits = 4)
png(file="figures/correlation_matrix.png")
plot.title = paste("Semrel Prosody Correlation Matrix")
#corrplot(cor.mat, method = "circle", main = plot.title, mar=c(1,1,2,1))
corrplot.mixed(cor.mat, main = plot.title, mar = c(1, 1, 2, 1))
dev.off()


cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("INDIVIDUAL CORRELATIONS", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")


ind.cor <- data.frame(Factor = c("Freq.",
                 "Len. Char.",
                 "Len. Phon.",
                 "Len.Syll.",
                 "Relat.",
                 "Integ.",
                 "Plaus.",
                 "Assoc.",
                 "N1.Len.",
                 "P1.Len.",
                 "A1.Len.",
                 "N2.Len."),

              Seconds = c(cor(d.base$seconds, d.base$freq,   use = "complete"),
                    cor(d.base$seconds, d.base$len.char, use = "complete"),
                    cor(d.base$seconds, d.base$len.phon, use = "complete"),
                    cor(d.base$seconds, d.base$len.syll, use = "complete"),
                    cor(d.base$seconds, d.base$relat,  use = "complete"),
                    cor(d.base$seconds, d.base$integ,  use = "complete"),
                    cor(d.base$seconds, d.base$plaus,  use = "complete"),
                    cor(d.base$seconds, d.base$assoc,  use = "complete"),
                    cor(d.base$seconds, d.base$n1.len,  use = "complete"),
                    cor(d.base$seconds, d.base$p1.len,  use = "complete"),
                    cor(d.base$seconds, d.base$a1.len,  use = "complete"),
                    cor(d.base$seconds, d.base$n2.len,  use = "complete")))
print(ind.cor)
sink()

# COEFFICIENTS TABLE----------------------------------------------------------------------------
library(ggplot2)
options(scipen = 999)


coeff.table.1 <- data.frame (
 POS  = rep(c("D1","N1","P1","D2","A1","N2"), each=2),
 Factor = c(rep(c("Integration","Relatedness"), times = 6)),
 Beta  = c(
  co.d1.i <- cbind(as.data.frame(t(t(colnames(pros.d1.i@pp$X)))),as.data.frame(t(t(pros.d1.i@beta))))[2,2],
  co.d1.r <- cbind(as.data.frame(t(t(colnames(pros.d1.r@pp$X)))),as.data.frame(t(t(pros.d1.r@beta))))[2,2],
  co.n1.i <- cbind(as.data.frame(t(t(colnames(pros.n1.i@pp$X)))),as.data.frame(t(t(pros.n1.i@beta))))[2,2],
  co.n1.r <- cbind(as.data.frame(t(t(colnames(pros.n1.r@pp$X)))),as.data.frame(t(t(pros.n1.r@beta))))[2,2],
  co.p1.i <- cbind(as.data.frame(t(t(colnames(pros.p1.i@pp$X)))),as.data.frame(t(t(pros.p1.i@beta))))[2,2],
  co.p1.r <- cbind(as.data.frame(t(t(colnames(pros.p1.r@pp$X)))),as.data.frame(t(t(pros.p1.r@beta))))[2,2],
  co.d2.i <- cbind(as.data.frame(t(t(colnames(pros.d2.i@pp$X)))),as.data.frame(t(t(pros.d2.i@beta))))[2,2],
  co.d2.r <- cbind(as.data.frame(t(t(colnames(pros.d2.r@pp$X)))),as.data.frame(t(t(pros.d2.r@beta))))[2,2],
  co.a1.i <- cbind(as.data.frame(t(t(colnames(pros.a1.i@pp$X)))),as.data.frame(t(t(pros.a1.i@beta))))[2,2],
  co.a1.r <- cbind(as.data.frame(t(t(colnames(pros.a1.r@pp$X)))),as.data.frame(t(t(pros.a1.r@beta))))[2,2],
  co.n2.i <- cbind(as.data.frame(t(t(colnames(pros.n2.i@pp$X)))),as.data.frame(t(t(pros.n2.i@beta))))[2,2],
  co.n2.r <- cbind(as.data.frame(t(t(colnames(pros.n2.r@pp$X)))),as.data.frame(t(t(pros.n2.r@beta))))[2,2]))


coeff.table.5 <- data.frame (
  POS  = rep(c("D1","N1","P1","D2","A1","N2"), each=2),
  Factor = c(rep(c("Integration","Relatedness"), times = 6)),
  Beta  = c(
    co.d1.i.si <- cbind(as.data.frame(t(t(colnames(pros.d1.i.si@pp$X)))),as.data.frame(t(t(pros.d1.i.si@beta))))[2,2],
    co.d1.r.si <- cbind(as.data.frame(t(t(colnames(pros.d1.r.si@pp$X)))),as.data.frame(t(t(pros.d1.r.si@beta))))[2,2],
    co.n1.i.si <- cbind(as.data.frame(t(t(colnames(pros.n1.i.si@pp$X)))),as.data.frame(t(t(pros.n1.i.si@beta))))[2,2],
    co.n1.r.si <- cbind(as.data.frame(t(t(colnames(pros.n1.r.si@pp$X)))),as.data.frame(t(t(pros.n1.r.si@beta))))[2,2],
    co.p1.i.si <- cbind(as.data.frame(t(t(colnames(pros.p1.i.si@pp$X)))),as.data.frame(t(t(pros.p1.i.si@beta))))[2,2],
    co.p1.r.si <- cbind(as.data.frame(t(t(colnames(pros.p1.r.si@pp$X)))),as.data.frame(t(t(pros.p1.r.si@beta))))[2,2],
    co.d2.i.si <- cbind(as.data.frame(t(t(colnames(pros.d2.i.si@pp$X)))),as.data.frame(t(t(pros.d2.i.si@beta))))[2,2],
    co.d2.r.si <- cbind(as.data.frame(t(t(colnames(pros.d2.r.si@pp$X)))),as.data.frame(t(t(pros.d2.r.si@beta))))[2,2],
    co.a1.i.si <- cbind(as.data.frame(t(t(colnames(pros.a1.i.si@pp$X)))),as.data.frame(t(t(pros.a1.i.si@beta))))[2,2],
    co.a1.r.si <- cbind(as.data.frame(t(t(colnames(pros.a1.r.si@pp$X)))),as.data.frame(t(t(pros.a1.r.si@beta))))[2,2],
    co.n2.i.si <- cbind(as.data.frame(t(t(colnames(pros.n2.i.si@pp$X)))),as.data.frame(t(t(pros.n2.i.si@beta))))[2,2],
    co.n2.r.si <- cbind(as.data.frame(t(t(colnames(pros.n2.r.si@pp$X)))),as.data.frame(t(t(pros.n2.r.si@beta))))[2,2]))


coeff.table.3 <- data.frame (
 POS  = rep(c("D1","N1","P1","D2","A1","N2"), each=2),
 Factor = c(rep(c("Integration","Relatedness"), times = 6)),
 Beta  = c(
  co.d1.ir <- cbind(as.data.frame(t(t(colnames(pros.d1.ir@pp$X)))),as.data.frame(t(t(pros.d1.ir@beta))))[3, 2],
  co.d1.ir <- cbind(as.data.frame(t(t(colnames(pros.d1.ir@pp$X)))),as.data.frame(t(t(pros.d1.ir@beta))))[2, 2],
  co.n1.ir <- cbind(as.data.frame(t(t(colnames(pros.n1.ir@pp$X)))),as.data.frame(t(t(pros.n1.ir@beta))))[3, 2],
  co.n1.ir <- cbind(as.data.frame(t(t(colnames(pros.n1.ir@pp$X)))),as.data.frame(t(t(pros.n1.ir@beta))))[2, 2],
  co.p1.ir <- cbind(as.data.frame(t(t(colnames(pros.p1.ir@pp$X)))),as.data.frame(t(t(pros.p1.ir@beta))))[3, 2],
  co.p1.ir <- cbind(as.data.frame(t(t(colnames(pros.p1.ir@pp$X)))),as.data.frame(t(t(pros.p1.ir@beta))))[2, 2],
  co.d2.ir <- cbind(as.data.frame(t(t(colnames(pros.d2.ir@pp$X)))),as.data.frame(t(t(pros.d2.ir@beta))))[3, 2],
  co.d2.ir <- cbind(as.data.frame(t(t(colnames(pros.d2.ir@pp$X)))),as.data.frame(t(t(pros.d2.ir@beta))))[2, 2],
  co.a1.ir <- cbind(as.data.frame(t(t(colnames(pros.a1.ir@pp$X)))),as.data.frame(t(t(pros.a1.ir@beta))))[3, 2],
  co.a1.ir <- cbind(as.data.frame(t(t(colnames(pros.a1.ir@pp$X)))),as.data.frame(t(t(pros.a1.ir@beta))))[2, 2],
  co.n2.ir <- cbind(as.data.frame(t(t(colnames(pros.n2.ir@pp$X)))),as.data.frame(t(t(pros.n2.ir@beta))))[3, 2],
  co.n2.ir <- cbind(as.data.frame(t(t(colnames(pros.n2.ir@pp$X)))),as.data.frame(t(t(pros.n2.ir@beta))))[2, 2]))


coeff.table.6 <- data.frame (
  POS  = rep(c("D1","N1","P1","D2","A1","N2"), each=2),
  Factor = c(rep(c("Integration","Relatedness"), times = 6)),
  Beta  = c(
    co.d1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.d1.ir.si@pp$X)))),as.data.frame(t(t(pros.d1.ir.si@beta))))[3, 2],
    co.d1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.d1.ir.si@pp$X)))),as.data.frame(t(t(pros.d1.ir.si@beta))))[2, 2],
    co.n1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.n1.ir.si@pp$X)))),as.data.frame(t(t(pros.n1.ir.si@beta))))[3, 2],
    co.n1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.n1.ir.si@pp$X)))),as.data.frame(t(t(pros.n1.ir.si@beta))))[2, 2],
    co.p1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.p1.ir.si@pp$X)))),as.data.frame(t(t(pros.p1.ir.si@beta))))[3, 2],
    co.p1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.p1.ir.si@pp$X)))),as.data.frame(t(t(pros.p1.ir.si@beta))))[2, 2],
    co.d2.ir.si <- cbind(as.data.frame(t(t(colnames(pros.d2.ir.si@pp$X)))),as.data.frame(t(t(pros.d2.ir.si@beta))))[3, 2],
    co.d2.ir.si <- cbind(as.data.frame(t(t(colnames(pros.d2.ir.si@pp$X)))),as.data.frame(t(t(pros.d2.ir.si@beta))))[2, 2],
    co.a1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.a1.ir.si@pp$X)))),as.data.frame(t(t(pros.a1.ir.si@beta))))[3, 2],
    co.a1.ir.si <- cbind(as.data.frame(t(t(colnames(pros.a1.ir.si@pp$X)))),as.data.frame(t(t(pros.a1.ir.si@beta))))[2, 2],
    co.n2.ir.si <- cbind(as.data.frame(t(t(colnames(pros.n2.ir.si@pp$X)))),as.data.frame(t(t(pros.n2.ir.si@beta))))[3, 2],
    co.n2.ir.si <- cbind(as.data.frame(t(t(colnames(pros.n2.ir.si@pp$X)))),as.data.frame(t(t(pros.n2.ir.si@beta))))[2, 2]))



# FIGURES: Betas by part of speech ----------------------------------

positions <- c("D1","N1","P1","D2","A1","N2")
dodge <- position_dodge(width = 0.9)
g1   <- ggplot(data = coeff.table.1, aes(x = POS, y = Beta, fill = Factor)) +
 layer(geom="bar", stat="identity", position = position_dodge()) +
 scale_x_discrete(limits = positions) +
 theme_classic() +
 geom_hline(yintercept=0)+
 theme(text = element_text(size=18.5)) +
 ylab("Standardized Coefficients") +
 theme(axis.title.y=element_text(vjust=1.5)) +
 ggtitle("Coefficients by Part of Speech (No Residuals)") +
 theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15))
p.text = grobTree(textGrob(expression(paste(italic("**p"),"<.05, ", italic("*p"),"<.10")), x=0.02, y=0.09, hjust=0, gp=gpar(col="black", fontsize=14)))
g1 <- g1 + annotation_custom(p.text) +
 annotate("text", x = 2.225, y = .00475, label = "*", size = 8) +
 annotate("text", x = 4.775, y = -.00655, label = "*", size = 8) +
 annotate("text", x = 5.200, y = -.00605, label =" *", size = 8)
g1 <-  g1+scale_y_continuous(limits=c(-.01575,.01575), breaks=c(-.015, -.010, -.005, 0, .005, .01, .015))
ggsave("figures/Coefficients X Part of Speech (No Residuals).png")


positions <- c("D1","N1","P1","D2","A1","N2")
dodge <- position_dodge(width = 0.9)
g5   <- ggplot(data = coeff.table.5, aes(x = POS, y = Beta, fill = Factor)) +
  layer(geom="bar", stat="identity", position = position_dodge()) +
  scale_x_discrete(limits = positions) +
  theme_classic() +
  geom_hline(yintercept=0)+
  theme(text = element_text(size=18.5)) +
  ylab("Standardized Coefficients") +
  theme(axis.title.y=element_text(vjust=1.5)) +
  ggtitle("Coefficients by Part of Speech (No Residuals)\n-Random Slopes & Intercepts") +
  theme(plot.title = element_text(face="bold", color="black", size=15, lineheight=0.95))
p.text = grobTree(textGrob(expression(paste(italic("**p"),"<.05, ", italic("*p"),"<.10")), x=0.02, y=0.09, hjust=0, gp=gpar(col="black", fontsize=14)))
# g1 <- g1 + annotation_custom(p.text) +
#   annotate("text", x = 2.225, y = .00475, label = "*", size = 8) +
#   annotate("text", x = 4.775, y = -.00655, label = "*", size = 8) +
#   annotate("text", x = 5.200, y = -.00605, label =" *", size = 8)
g5 <- g5 +  scale_y_continuous(limits=c(-.01575,.01575), breaks=c(-.015, -.010, -.005, 0, .005, .01, .015))

ggsave("figures/Coefficients X Part of Speech (No Residuals) -Random Slopes & Intercepts.png")

positions <- c("D1","N1","P1","D2","A1","N2")
dodge <- position_dodge(width = 0.9)
g3   <- ggplot(data = coeff.table.3, aes(x = POS, y = Beta, fill = Factor)) +
 layer(geom="bar", stat="identity", position = position_dodge()) +
 scale_x_discrete(limits = positions) +
 theme_classic() +
 geom_hline(yintercept=0)+
 theme(text = element_text(size=18.5)) +
 ylab("Standardized Coefficients") +
 theme(axis.title.y=element_text(vjust=1.5)) +
 ggtitle("Coefficients by Part of Speech\n(No Residuals) (Fixed Effects: Int. & Rel.)") +
 theme(plot.title = element_text(size=17, face="bold", vjust=1, lineheight=0.95))
p.text = grobTree(textGrob(expression(paste(italic("**p"),"<.05, ", italic("*p"),"<.10")), x=0.02, y=0.09, hjust=0, gp=gpar(col="black", fontsize=14)))
g3 <- g3 + annotation_custom(p.text) +
 annotate("text", x = 2.225, y = .011, label = "**", size = 8)+
 annotate("text", x = 3.225, y = .0055, label = "*", size = 8)+
 annotate("text", x = 6.225, y = -.01555, label = "*", size = 8)
g3 <- g3 + scale_y_continuous(limits=c(-.01575,.01575), breaks=c(-.015, -.010, -.005, 0, .005, .01, .015))
ggsave("figures/Coefficients X Part of Speech (No Residuals) (Fixed Effects: Integ & Rel).png")


positions <- c("D1","N1","P1","D2","A1","N2")
dodge <- position_dodge(width = 0.9)
g6   <- ggplot(data = coeff.table.3, aes(x = POS, y = Beta, fill = Factor)) +
  layer(geom="bar", stat="identity", position = position_dodge()) +
  scale_x_discrete(limits = positions) +
  theme_classic() +
  geom_hline(yintercept=0)+
  theme(text = element_text(size=18.5)) +
  ylab("Standardized Coefficients") +
  theme(axis.title.y=element_text(vjust=1.5)) +
  ggtitle("Coefficients by Part of Speech(No Residuals)\n (Fixed Effects: Int. & Rel.) -Random Slopes & Intercepts") +
  theme(plot.title = element_text(size=17, face="bold", vjust=1, lineheight=0.95))
p.text = grobTree(textGrob(expression(paste(italic("**p"),"<.05, ", italic("*p"),"<.10")), x=0.02, y=0.09, hjust=0, gp=gpar(col="black", fontsize=14)))
# g6 <- g6 + annotation_custom(p.text) +
#   annotate("text", x = 2.225, y = .011, label = "**", size = 8)+
#   annotate("text", x = 3.225, y = .0055, label = "*", size = 8)+
#   annotate("text", x = 6.225, y = -.01555, label = "*", size = 8)
g6 <- g6 +  scale_y_continuous(limits=c(-.01575,.01575), breaks=c(-.015, -.010, -.005, 0, .005, .01, .015))
ggsave("figures/Coefficients X Part of Speech (No Residuals) (Fixed Effects: Integ & Relat)-Random Slopes & Intercepts.png")



coeff.table.1$Factor <- c(rep(c("Integration","Relatedness"), times = 6))
coeff.table.5$Factor <- c(rep(c("Integration.SI","Relatedness.SI"), times = 6))
coeff.table.3$Factor <- c(rep(c("Integration.IR","Relatedness.IR"), times = 6))
coeff.table.6$Factor <- c(rep(c("Integration.IR.SI","Relatedness.IR.SI"), times = 6))
coeff.all <- rbind(coeff.table.1,coeff.table.5)
coeff.all <- rbind(coeff.all, coeff.table.3)
coeff.all <- rbind(coeff.all, coeff.table.6)

positions <- c("D1","N1","P1","D2","A1","N2")
dodge <- position_dodge(width = 0.9)
g.all <- ggplot() +
  geom_bar(data = coeff.all, aes(x = POS, y = Beta, fill = Factor), stat = "identity",position = position_dodge()) +
  scale_fill_manual(values=rep(c("#4c4cff","#7f7fff", "#b2b2ff","#e5e5ff", "#ff4500", "#ff6a32", "#ff8f66",  "#ffb499"), times=6)) +
  scale_x_discrete(limits = positions) +
  theme_classic() +
  geom_hline(yintercept=0)+
  theme(text = element_text(size=18.5)) +
  ylab("Standardized Coefficients") +
  theme(axis.title.y=element_text(vjust=1.5)) +
  ggtitle("All models") +
  theme(plot.title = element_text(size=17, face="bold", vjust=1, lineheight=0.95)) +
  scale_y_continuous(limits=c(-.01575,.01575), breaks=c(-.015, -.010, -.005, 0, .005, .01, .015))
g.all
ggsave("figures/ Coefficients X Part of Speech- All Models.png")





#install.packages("gridExtra")
require(gridExtra)
library(gridExtra)
gall <- arrangeGrob(g1, g3, g5, g6, ncol=1)
ggsave(file="figures/Compare Plots.png", gall)













#=================== COMPUTING & USING RESIDUALS from here on ================

sink("output/SemRel Prosody Analyses-Residuals.txt")
cat(" ", "\n")
cat("SEMREL PROSODY ANALYSES with RESIDUALS RUN ON: ", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill= 70)
cat(" ", "\n")


d.preamble.6pos <- subset(d.base, word.type == "D1" | word.type == "N1" | word.type == "P1" | word.type == "D2" | word.type == "A1"| word.type == "N2")
pros.preamble.frln <- lmer( seconds ~ freq + len.phon + (1 + freq + len.phon |subject) + (1 + freq + len.phon |item), data = d.preamble.6pos, REML = TRUE)
d.preamble.6pos$resid.frln <- residuals(pros.preamble.frln)
cat(rep(c("-"), times = 40, quote = F),"\n")
cat("6pos Freq. & Length", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
print(summary(pros.preamble.frln))

pros.preamble.fpos <- lmer( resid.frln ~ fr.pos.wd + (1 + fr.pos.wd|subject) + (1 + fr.pos.wd|item), data = d.preamble.6pos, REML = TRUE)
d.preamble.6pos$resid.frln.fpos <- residuals(pros.preamble.fpos)
cat(rep(c("-"), times = 40, quote = F),"\n")
cat("6pos Freq. Following Word", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
print(summary(pros.preamble.fpos))

d.preamble.5pos <- subset(d.preamble.6pos, word.type != "D1")
pros.preamble.fpre <- lmer( resid.frln.fpos ~ fr.pre.wd + (1 + fr.pre.wd|subject) + (1 + fr.pre.wd|item), data = d.preamble.5pos, REML = TRUE)
d.preamble.5pos$resid.fpre <- residuals(pros.preamble.fpre)
cat(rep(c("-"), times = 40, quote = F),"\n")
cat("6pos Freq. Preceding Word", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
print(summary(pros.preamble.fpre))


# ---------------------- REGRESSION CREATE SUBSETS-----------------------------------

d.d1 <- subset(d.preamble.6pos, word.type == "D1")
d.n1 <- subset(d.preamble.5pos, word.type == "N1")
d.p1 <- subset(d.preamble.5pos, word.type == "P1")
d.d2 <- subset(d.preamble.5pos, word.type == "D2")
d.a1 <- subset(d.preamble.5pos, word.type == "A1")
d.n2 <- subset(d.preamble.5pos, word.type == "N2")

# ------------------------------------------------------------------------------------------------------------------
# ---------------------------CREATE AND COMPARE RESIDUALS MODELS -------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------


# D1 (w/Resids)-------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("D1", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
pros.preamble.d1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.d1, REML = TRUE)
d.d1$resid <- residuals(pros.preamble.d1)
cat(" ", "\n")
cat("D1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.d1))

pros.preamble.d1 <- lmer( resid ~ n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.d1, REML = TRUE)
d.d1$resid <- residuals(pros.preamble.d1)
cat(" ", "\n")
cat("D1:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.d1))

pros.preamble.d1 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.d1, REML = TRUE)
d.d1$resid <- residuals(pros.preamble.d1)
cat(" ", "\n")
cat("D1:: (s/b A1) & N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.d1))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d1.i <- lmer(resid ~ integ + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(res.d1.i))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Relatedness) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d1.r <- lmer(resid ~ relat + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(res.d1.r))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d1.a <- lmer(resid ~ assoc + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(res.d1.a))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d1.ir <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(res.d1.ir))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D1 (Integration with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d1.ia <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.d1, REML = TRUE)
print(summary(res.d1.ia))



# N1 (w/ Resids)-------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("N1", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
pros.preamble.n1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.n1, REML = TRUE)
d.n1$resid <- residuals(pros.preamble.n1)
cat(" ", "\n")
cat("N1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.n1))

pros.preamble.n1 <- lmer( resid ~ p1.len + a1.len + (1 + p1.len + a1.len|subject) + (1 + p1.len + a1.len|item), data = d.n1, REML = TRUE)
d.n1$resid <- residuals(pros.preamble.n1)
cat(" ", "\n")
cat("N1:: P1 & A1 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.n1))

pros.preamble.n1 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.n1, REML = TRUE)
d.n1$resid <- residuals(pros.preamble.n1)
cat(" ", "\n")
cat("N1:: N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.n1))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n1.i <- lmer(resid ~ integ + (1|subject) + (1|item), data = d.n1, REML = TRUE)
print(summary(res.n1.i))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n1.r <- lmer(resid ~ relat + (1|subject) + (1|item), data = d.n1, REML = TRUE)
print(summary(res.n1.r))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n1.a <- lmer(resid ~ assoc + (1|subject) + (1|item), data = d.n1, REML = TRUE)
print(summary(res.n1.a))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n1.ir <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.n1, REML = TRUE)
print(summary(res.n1.ir))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N1 (Integration with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n1.ia <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.n1, REML = TRUE)
print(summary(res.n1.ia))


# P1 (w/ Resids)-------------------

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("P1", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
pros.preamble.p1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.p1, REML = TRUE)
d.p1$resid <- residuals(pros.preamble.p1)
cat(" ", "\n")
cat("P1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.p1))

pros.preamble.p1 <- lmer( resid ~ n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.p1, REML = TRUE)
d.p1$resid <- residuals(pros.preamble.p1)
cat(" ", "\n")
cat("P1:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.p1))

pros.preamble.p1 <- lmer( resid ~ a1.len + n2.len + (1 + a1.len + n2.len|subject) + (1 + a1.len + n2.len|item), data = d.p1, REML = TRUE)
d.p1$resid <- residuals(pros.preamble.p1)
cat(" ", "\n")
cat("P1:: A1 & N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.p1))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Integration) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.p1.i <- lmer(resid ~ integ + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(res.p1.i))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.p1.r <- lmer(resid ~ relat + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(res.p1.r))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.p1.a <- lmer(resid ~ assoc + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(res.p1.a))


cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Integration with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.p1.ir <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(res.p1.ir))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET P1 (Integration with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.p1.ia <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.p1, REML = TRUE)
print(summary(res.p1.ia))

# D2 (w/ Resids) -------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("D2", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
pros.preamble.d2 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.d2, REML = TRUE)
d.d2$resid <- residuals(pros.preamble.d2)
cat(" ", "\n")
cat("D2:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.d2))

pros.preamble.d2 <- lmer( resid ~ n1.len + a1.len + (1 + n1.len + a1.len|subject) + (1 + n1.len + a1.len|item), data = d.d2, REML = TRUE)
d.d2$resid <- residuals(pros.preamble.d2)
cat(" ", "\n")
cat("D2:: N1 & A1 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.d2))

pros.preamble.d2 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.d2, REML = TRUE)
d.d2$resid <- residuals(pros.preamble.d2)
cat(" ", "\n")
cat("D2:: N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.d2))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d2.i <- lmer(resid ~ integ + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(res.d2.i))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d2.r <- lmer(resid ~ relat + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(res.d2.r))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d2.a <- lmer(resid ~ assoc + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(res.d2.a))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d2.ir <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(res.d2.ir))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET D2 (Integration with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.d2.ia <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.d2, REML = TRUE)
print(summary(res.d2.ia))

# A1 (w/ Resids)-------------------
cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("A1", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
pros.preamble.a1 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.a1, REML = TRUE)
d.a1$resid <- residuals(pros.preamble.a1)
cat(" ", "\n")
cat("A1:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.a1))

pros.preamble.a1 <- lmer( resid ~ n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.a1, REML = TRUE)
d.a1$resid <- residuals(pros.preamble.a1)
cat(" ", "\n")
cat("A1:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.a1))

pros.preamble.a1 <- lmer( resid ~ n2.len + (1 + n2.len|subject) + (1 + n2.len|item), data = d.a1, REML = TRUE)
d.a1$resid <- residuals(pros.preamble.a1)
cat(" ", "\n")
cat("A1:: N2 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.a1))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.a1.i <- lmer(resid ~ integ + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(res.a1.i))

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.a1.r <- lmer(resid ~ relat + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(res.a1.r))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.a1.a <- lmer(resid ~ assoc + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(res.a1.a))


cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.a1.ir <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(res.a1.ir))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET A1 (Integration with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.a1.ia <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.a1, REML = TRUE)
print(summary(res.a1.ia))

# N2 (w/ Resids)-------------------

cat(rep(c("\n"), times = 2), rep(c("-"), times = 40, quote = F),"\n")
cat("N2", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F),"\n")
pros.preamble.n2 <- lmer( resid.frln.fpos ~ plaus + (1 + plaus|subject) + (1 + plaus|item), data = d.n2, REML = TRUE)
d.n2$resid <- residuals(pros.preamble.n2)
cat(" ", "\n")
cat("N2:: Plaus", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.n2))

pros.preamble.n2 <- lmer( resid ~ n1.len + p1.len + (1 + n1.len + p1.len|subject) + (1 + n1.len + p1.len|item), data = d.n2, REML = TRUE)
d.n2$resid <- residuals(pros.preamble.n2)
cat(" ", "\n")
cat("N2:: N1 & P1 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.n2))

pros.preamble.n2 <- lmer( resid ~ a1.len + (1 + a1.len|subject) + (1 + a1.len|item), data = d.n2, REML = TRUE)
d.n2$resid <- residuals(pros.preamble.n2)
cat(" ", "\n")
cat("N2:: A1 Length", sep = "", fill = 60)
cat(rep(c("-"), times = 20, quote = F),"\n")
print(summary(pros.preamble.n2))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n2.i <- lmer(resid ~ integ + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(res.n2.i))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Relatedness) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n2.r <- lmer(resid ~ relat + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(res.n2.r))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n2.a <- lmer(resid ~ assoc + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(res.n2.a))


cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration with Related) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n2.ir <- lmer(resid ~ relat + integ + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(res.n2.ir))

cat(rep(c("-"), times = 40, quote = F),"\n")
cat("SUBSET N2 (Integration with Association) residuals", sep = "", fill = 60)
cat(rep(c("-"), times = 40, quote = F), "\n")
res.n2.ia <- lmer(resid ~ assoc + integ + (1|subject) + (1|item), data = d.n2, REML = TRUE)
print(summary(res.n2.ia))
sink()

#
# COEFFICIENTS TABLE----------------------------------------------------------------------------

coeff.table.2 <- data.frame (
POS  = rep(c("D1",
        "N1",
        "P1",
        "D2",
        "A1",
        "N2"),
       each = 3),

Factor = c(rep(c("Integration",
         "Relatedness",
         "Association"),
      times = 6)),

Beta  = c(
 co.d1.i <- cbind( as.data.frame( t( t( colnames( res.d1.i@pp$X)))), as.data.frame( t( t( res.d1.i@beta))))[2, 2],
 co.d1.r <- cbind( as.data.frame( t( t( colnames( res.d1.r@pp$X)))), as.data.frame( t( t( res.d1.r@beta))))[2, 2],
 co.d1.a <- cbind( as.data.frame( t( t( colnames( res.d1.a@pp$X)))), as.data.frame( t( t( res.d1.a@beta))))[2, 2],
 co.n1.i <- cbind( as.data.frame( t( t( colnames( res.n1.i@pp$X)))), as.data.frame( t( t( res.n1.i@beta))))[2, 2],
 co.n1.r <- cbind( as.data.frame( t( t( colnames( res.n1.r@pp$X)))), as.data.frame( t( t( res.n1.r@beta))))[2, 2],
 co.n1.a <- cbind( as.data.frame( t( t( colnames( res.n1.a@pp$X)))), as.data.frame( t( t( res.n1.a@beta))))[2, 2],
 co.p1.i <- cbind( as.data.frame( t( t( colnames( res.p1.i@pp$X)))), as.data.frame( t( t( res.p1.i@beta))))[2, 2],
 co.p1.r <- cbind( as.data.frame( t( t( colnames( res.p1.r@pp$X)))), as.data.frame( t( t( res.p1.r@beta))))[2, 2],
 co.p1.a <- cbind( as.data.frame( t( t( colnames( res.p1.a@pp$X)))), as.data.frame( t( t( res.p1.a@beta))))[2, 2],
 co.d2.i <- cbind( as.data.frame( t( t( colnames( res.d2.i@pp$X)))), as.data.frame( t( t( res.d2.i@beta))))[2, 2],
 co.d2.r <- cbind( as.data.frame( t( t( colnames( res.d2.r@pp$X)))), as.data.frame( t( t( res.d2.r@beta))))[2, 2],
 co.d2.a <- cbind( as.data.frame( t( t( colnames( res.d2.a@pp$X)))), as.data.frame( t( t( res.d2.a@beta))))[2, 2],
 co.a1.i <- cbind( as.data.frame( t( t( colnames( res.a1.i@pp$X)))), as.data.frame( t( t( res.a1.i@beta))))[2, 2],
 co.a1.r <- cbind( as.data.frame( t( t( colnames( res.a1.r@pp$X)))), as.data.frame( t( t( res.a1.r@beta))))[2, 2],
 co.a1.a <- cbind( as.data.frame( t( t( colnames( res.a1.a@pp$X)))), as.data.frame( t( t( res.a1.a@beta))))[2, 2],
 co.n2.i <- cbind( as.data.frame( t( t( colnames( res.n2.i@pp$X)))), as.data.frame( t( t( res.n2.i@beta))))[2, 2],
 co.n2.r <- cbind( as.data.frame( t( t( colnames( res.n2.r@pp$X)))), as.data.frame( t( t( res.n2.r@beta))))[2, 2],
 co.n2.a <- cbind( as.data.frame( t( t( colnames( res.n2.a@pp$X)))), as.data.frame( t( t( res.n2.a@beta))))[2, 2]))

coeff.table.2.ir <- subset(coeff.table.2, Factor != "Association")


coeff.table.4 <- data.frame (
 POS  = rep(c("D1","N1","P1","D2","A1","N2"), each=2),
 Factor = c(rep(c("Integration","Relatedness"), times = 6)),
 Beta  = c(
  co.d1.ir<- cbind(as.data.frame(t(t(colnames(res.d1.ir@pp$X)))),as.data.frame(t(t(res.d1.ir@beta))))[3, 2],
  co.d1.ir<- cbind(as.data.frame(t(t(colnames(res.d1.ir@pp$X)))),as.data.frame(t(t(res.d1.ir@beta))))[2, 2],
  co.n1.ir<- cbind(as.data.frame(t(t(colnames(res.n1.ir@pp$X)))),as.data.frame(t(t(res.n1.ir@beta))))[3, 2],
  co.n1.ir<- cbind(as.data.frame(t(t(colnames(res.n1.ir@pp$X)))),as.data.frame(t(t(res.n1.ir@beta))))[2, 2],
  co.p1.ir<- cbind(as.data.frame(t(t(colnames(res.p1.ir@pp$X)))),as.data.frame(t(t(res.p1.ir@beta))))[3, 2],
  co.p1.ir<- cbind(as.data.frame(t(t(colnames(res.p1.ir@pp$X)))),as.data.frame(t(t(res.p1.ir@beta))))[2, 2],
  co.d2.ir<- cbind(as.data.frame(t(t(colnames(res.d2.ir@pp$X)))),as.data.frame(t(t(res.d2.ir@beta))))[3, 2],
  co.d2.ir<- cbind(as.data.frame(t(t(colnames(res.d2.ir@pp$X)))),as.data.frame(t(t(res.d2.ir@beta))))[2, 2],
  co.a1.ir<- cbind(as.data.frame(t(t(colnames(res.a1.ir@pp$X)))),as.data.frame(t(t(res.a1.ir@beta))))[3, 2],
  co.a1.ir<- cbind(as.data.frame(t(t(colnames(res.a1.ir@pp$X)))),as.data.frame(t(t(res.a1.ir@beta))))[2, 2],
  co.n2.ir<- cbind(as.data.frame(t(t(colnames(res.n2.ir@pp$X)))),as.data.frame(t(t(res.n2.ir@beta))))[3, 2],
  co.n2.ir<- cbind(as.data.frame(t(t(colnames(res.n2.ir@pp$X)))),as.data.frame(t(t(res.n2.ir@beta))))[2, 2]))

# FIGURES: Betas by part of speech ----------------------------------


positions <- c("D1","N1","P1","D2","A1","N2")
dodge <- position_dodge(width = 0.9)
g2   <- ggplot(data = coeff.table.2.ir, aes(x = POS, y = Beta, fill = Factor)) +
 layer(geom="bar", stat="identity", position = position_dodge()) +
 scale_x_discrete(limits = positions) +
 theme_classic() +
 geom_hline(yintercept=0)+
 theme(text = element_text(size=18.5)) +
 ylab("Standardized Coefficients") +
 theme(axis.title.y=element_text(vjust=1.5))+
 ggtitle("Coefficients by Part of Speech") +
 theme(plot.title = element_text(size=17, face="bold", vjust=1, lineheight=0.9))
p.text = grobTree(textGrob(expression(paste(italic("***p"),"<.01")), x=0.02, y=0.09, hjust=0, gp=gpar(col="black", fontsize=14)))
 g2 <- g2 + annotation_custom(p.text) +
 annotate("text", x = 2.775, y = .0062, label = "***", size = 8)
g2

ggsave("figures/Coefficients X Part of Speech.png")

positions <- c("D1","N1","P1","D2","A1","N2")
dodge <- position_dodge(width = 0.9)
g4   <- ggplot(data = coeff.table.4, aes(x = POS, y = Beta, fill = Factor)) +
 layer(geom="bar", stat="identity", position = position_dodge()) +
 scale_x_discrete(limits = positions) +
 theme_classic() +
 geom_hline(yintercept=0)+
 theme(text = element_text(size=18.5)) +
 ylab("Standardized Coefficients") +
 theme(axis.title.y=element_text(vjust=1.5)) +
 ggtitle("Coefficients by Part of Speech\n (Fixed Effects: Int. & Rel.)") +
 theme(plot.title = element_text(size=17, face="bold", vjust=1, lineheight=0.95))
p.text = grobTree(textGrob(expression(paste(italic("***p"),"<.01, ",italic("**p"),"<.05,",italic("*p"), "<.10")), x=0.02, y=0.09, hjust=0, gp=gpar(col="black", fontsize=14)))
g4 <- g4 + annotation_custom(p.text) +
 annotate("text", x = 2.225, y = .0055, label = "**", size = 8) +
 annotate("text", x = 2.775, y = .0235, label = "***", size = 8) +
 annotate("text", x = 3.225, y = -.0220, label = "***", size = 8) +
 annotate("text", x = 3.775, y = -.00475, label = "*", size = 8) +
 annotate("text", x = 4.225, y = .004, label = "**", size = 8)
g4
ggsave("figures/Coefficients X Part of Speech (Fixed Effects: Int. & Rel.).png")


