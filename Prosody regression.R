#DP 2/289/2014 BEGAN WRITING SCRIPT; not working

rm(list=ls())
library(dplyr)
library(reshape2)



# ------------------------------SET UP INPUT FILE(S)----------------------------


# ----------------------------------------------------
d.val$LengthPhon.Det <-2
d.len.phon <- d.val[,c("subitem","LengthPhon.Det","LengthPhon.Head","LengthPhon.Prep","LengthPhon.Adj","LengthPhon.Det","LengthPhon.Noun")] #SUBSET OF MAIN SHEET
d.len.phon<-melt(d.len.phon, id.vars=1) #create melted version of data
View(d.len.phon)
d.len.phon$test<-NA
d.len.phon$test<-rep(c("D1","N1","P1","A1","D2","N2"), each = 698)
d.len.phon$unique.id <- paste(d.len.phon[,1],d.len.phon[,4],sep="-") #concatenate string
d <-left_join(d,d.len.phon, by="unique.id") #join b to ad
# ----------------------------------------------------
d.val$Length.Syl.Det <-1
d.len.syll <- d.val[,c(1,68,24:25,68,26:27)] #SUBSET OF MAIN SHEET
View(d.len.syll)
d.len.syll<-melt(d.len.syll, id.vars=1) #create melted version of data
View(d.len.syll)
d.len.syll$test<-NA
d.len.syll$test<-rep(c("D1","N1","P1","A1","D2","N2"), each = 698)
d.len.syll$unique.id <- paste(d.len.syll[,1],d.len.syll[,4],sep="-") #concatenate string
names(d)[names(d)=="value"] <- "len.syll"
d.len.syll$test = NULL
d.len.syll$variable = NULL
d <-left_join(d,d.len.syll, by="unique.id") #join b to ad


d.phon <- d.val[,c(1,20:24)]
d.phon$LengthSylHead = NULL
d.phonm <-melt(d.phon, id.vars=1)
d <- read.csv("data/d_words.csv")
View(d)
d.val <- read.csv("data/prosody_items_original.csv")
View(d.val)
subtlex <- read.csv("data/subtlex.csv")
View(subtlex)
d.backup<-d
d$word <- sapply(d$word, as.character)
d$word[is.na(d$word)] <- " "
as.data.frame(d$word)
d$word<-tolower(d$word)

# ---populates column with freq. values----
d$freq<- "NA"
d$freq <- sapply(d$freq, as.character)
d$freq[is.na(d$freq)] <- " "
as.data.frame(d$freq)

freq <-
  for (i in d$word) {
    if(length(subtlex[subtlex$Word == i, 7]) == 0) {
    d[d$word == i, 9] <- 0
}  else {
    d[d$word == i, 9] <- subtlex[subtlex$Word ==i, 7]
  }
}

# ------

d$len.char <- NA
d$len.char <- nchar(d$word)

# -----

d$len.phon <- "NA"
d$len.phon <- sapply(d$len.phon, as.character)
d$len.phon[is.na(d$len.phon)] <- " "
as.data.frame(d$len.phon)

freq <-
  for (i in d$word) {
    if(length(subtlex[subtlex$Word == i, 7]) == 0) {
      d[d$word == i, 9] <- 0
    }  else {
      d[d$word == i, 9] <- subtlex[subtlex$Word ==i, 7]
    }
  }

#5 is work type
#10 is phon
#20 head
#21 prep
#22

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



