#DP 2/289/2014 BEGAN WRITING SCRIPT; not working

rm(list=ls())
library(tidyr)
library(dplyr)
library(reshape2)



# ------------------------------SET UP INPUT FILE(S)----------------------------

d <- read.csv("data/duration_data.csv")
View(d)
d.val <- read.csv("data/prosody_items_original.csv")

View(d.val)

# ----------------------------------------------------

d.word <- d.val[,c("subitem","D1","N1","P1","D2","A1","N2","C1","C2","C3","C4","C5")] #SUBSET OF MAIN SHEET
test<-melt(d.word, id.vars=1) #create melted version of data
test$test<-NA
test$test<-paste(test[,1],test[,2],sep="-") #concatenate string
d <-left_join(d,test, by="test") #join b to ad


d$preamb.word <- "NA"

preamb.word <-
  for (i in d$word){
    if (length(subtlex[subtlex$Word==i, 7]) == 0){
      data[data$word==i,12] <- 0}
    else {
      data[data$word==i,12] <- subtlex[subtlex$Word==i, 7]
    }
  }
# data = the datafile you want to import into
# subtlex is the Subtitlexus large excel sheet that has been imported into R
#word = a column in data that has the word you're looking up a value for written in lowercase
#
# the numbers 7 and 12 listed here are the column #s that the script looks up the value in and what column in data it replaces the value for
#
# 7 = column in SUBTlexus that has the frequency info
# 12 = column that you are filling in for the data file

# write.csv(d, file = "output/by_subjects_all_data.csv" )

#------------------------------SET UP OUTPUT FILE------------------------------
sink("output/F2 ELW Regression (subjects ).txt")
cat("__________________________ANALYSES OF SUBJECTS RUN ON:", format(Sys.time(), "%b. %d, %Y at %T"), sep = "", fill=80)



#--------------------PREPARE PREDICTOR VARIABLES FOR MODEL---------------------
# Below, the separate measures for each of the predictors are combined, centered, and scaled to minimize conlinearity where relevent (i.e., all but plausibiltiy and association), and values are added to new columns

d$len.char <- rowSums(d[ ,c("LengthChar.Head","LengthChar.Prep","LengthChar.Adj","LengthChar.Noun")])  # totals char. length measures
d$len.char <- scale(d$len.char, center=TRUE, scale=TRUE)
d$len.phon <- rowSums(d[ ,c("LengthPhon.Head","LengthPhon.Prep","LengthPhon.Adj","LengthPhon.Noun")])  # totals phon. length measures
d$len.phon <- scale(d$len.phon, center=TRUE, scale=TRUE)
d$len.syll <- rowSums(d[ ,c("LengthSylHead","LengthSyll.Prep","LengthSyll.Adj","LengthSyll.Noun")])  # totals syll. length measures
d$len.syll <- scale(d$len.syll, center=TRUE, scale=TRUE)
d$lf.head <- scale(d$LogFr.Head, center=TRUE, scale=TRUE)
d$lf.prep <- scale(d$LogFreq.Prep, center=TRUE, scale=TRUE)
d$lf.adj <- scale(d$LogFreq.Adj, center=TRUE, scale=TRUE)
d$lf.noun <- scale(d$LogFreq.N1, center=TRUE, scale=TRUE)
d$rel.hl <- scale(d$RelatedHL, center=TRUE, scale=TRUE)  # "hl" = head to local
d$rel.lh <- scale(d$RelatedLH, center=TRUE, scale=TRUE)  # "lh" = local to head
d$integ <- scale(d$Integrated, center=TRUE, scale=TRUE)
d$asso <- d$AssArc.H.L
d$plaus <- d$Plausibility
# COMMENT THIS OUT OR REMOVE
write.csv(actual, file = "output/prosody_items_both.csv" )
make.dys(c("len.char", "len.phon", "len.syll", "lf.head", "lf.prep", "lf.adj", "lf.noun", "rel.hl", "rel.lh", "integ", "asso", "plaus"))


#---------------------------CREATE AND COMPARE MODELS--------------------------

# Full model
elogr.subj.dys <- lmer(maincode ~ len.char + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + rel.lh + rel.hl * integ  + asso + plaus + (1|subject), data = d.dys, REML=FALSE)
print(summary(elogr.subj.dys))

# Model with no character length
no.len.char <- lmer(maincode ~ len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + rel.lh + rel.hl * integ  + asso + plaus + (1|subject), data = d.dys,  REML=FALSE)
print(summary(elogr.subj.dys))
step(elogr.subj.dys)
# Model comparison
cat("__________________________ANOVA______________________________", fill=80)
anova(no.len.char, elogr.subj.dys)

# Stepwise analysis (NOTE REDUCED RANDOM EFFECTS)
# cat("__________________________STEP______________________________", fill=80)
# elogr.item.dys <- lmer(elog ~ len.char + len.phon + len.syll + lf.head + lf.prep + lf.adj + lf.noun + rel.lh + rel.hl * integ  + asso + plaus + (1 |item), data = d.dys, weights = (1/v), REML=TRUE)
#
sink()



