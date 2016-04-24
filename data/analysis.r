library(matrixStats)
library(parsedate)

rawDat = read.csv("finalDataClean.csv")
dateVect = mapply(function(x) unlist(as.POSIXlt(parse_iso_8601(x)))['mday'], rawDat$time)
dateDat = cbind(rawDat, day=dateVect)
daySubset = subset(dateDat, select=-c(cs,time,international,name))

daySubset = cbind(daySubset,aggCompScores=rowMeans(daySubset[,6:14]), aggHumanScores=rowMeans(daySubset[,15:23]))
daySubset = daySubset[,-c(6:23)]

day1 = subset(daySubset,day==5)
day2 = subset(daySubset,day==15)

day1_day2_human = t.test(day1$aggHumanScores,day2$aggHumanScores)
cat("T-test for human scores day1 versus day 2\n")
print(day1_day2_human)
day1_day2_comp = t.test(day1$aggCompScores,day2$aggCompScores)
cat("T-test for comp scores day1 versus day 2\n")
print(day1_day2_comp)

dat = subset(dateDat, select=-c(cs,time,international,name))
dat = cbind(dat,aggCompScores=rowMeans(dat[,6:14]), aggHumanScores=rowMeans(dat[,15:23]))
dat = cbind(dat,compSD=rowSds(data.matrix(dat),cols=c(6:14)), humanSD=rowSds(data.matrix(dat),cols=c(15:23)))

#reencode binary variables to categorical
#public computer pen and paper eval
pubPenPaper = subset(dat, pubComp==TRUE & penPaper==TRUE, select=-c(pubComp,penPaper))
newcol = newcol = c(rep("PUBPP",nrow(pubPenPaper)))
pubPenPaper = cbind(pubPenPaper,type=newcol)

#public computer, public computer eval
pubCompPubEval = subset(dat, pubComp==TRUE & penPaper==FALSE, select=-c(pubComp,penPaper))
newcol =  c(rep("PUBCE",nrow(pubPenPaper)))
pubCompPubEval = cbind(pubCompPubEval,type=newcol)

#personal computer, pen and paper eval
personalPenPaper = subset(dat, pubComp==FALSE & penPaper==TRUE, select=-c(pubComp,penPaper))
newcol = c(rep("PCPP",nrow(personalPenPaper)))
personalPenPaper = cbind(personalPenPaper,type=newcol)

#personal computer, computer eval
personalCompEval = subset(dat, pubComp==FALSE & penPaper==FALSE, select=-c(pubComp,penPaper))
newcol = c(rep("PCCE",nrow(personalCompEval)))
personalCompEval = cbind(personalCompEval, type=newcol)

reencoded = rbind(pubPenPaper,pubCompPubEval, personalPenPaper, personalCompEval)
#print.data.frame(reencoded)
#print(dim(reencoded))

fin = reencoded[,-c(4:21)]
row.names(fin) = 1:nrow(fin)

#t-test for human v comp scores
human_comp_standard = t.test(fin$aggCompScores, fin$aggHumanScores)
cat("T test for human versus computer scores\n")
print(human_comp_standard)

homogenous = subset(fin,humanSD <= 0.44)
# cat("\nHOMOGENOUS DATA\n")
# print.data.frame(subset(homogenous,select=-gender))
heterogenous = subset(fin,humanSD > 0.44)
# cat("\nHETEROGENEOUS DATA\n\n")
# print.data.frame(subset(heterogenous,select=-gender))

# cat("Num homogenous members\n")
# print(nrow(homogenous))

# cat("Num heterogenous members\n")
# print(nrow(heterogenous))

# cat("summary stats homogenous\n")
# print(summary(homogenous))

# cat("summary stats heterogenous\n")
# print(summary(heterogenous))

human_comp_rem_homogenous = t.test(heterogenous$aggCompScores, heterogenous$aggHumanScores)
cat("T test for human versus computer scores with homegenous members removed\n")
print(human_comp_rem_homogenous)

heteroD1 = subset(heterogenous, day == 5)
heteroD2 = subset(heterogenous, day == 15)

human_comp_hetero = t.test(heteroD1$aggHumanScores, heteroD2$aggHumanScores)
cat("T test for human day 1 scores versus human day 2 scores with homogenous removed\n")
print(human_comp_hetero)

pexp_model_comp = lm(aggCompScores~progExp,data=dat)
cat("\nLinear Regression model, aggCompScores = progExp\n")
print(summary(pexp_model_comp))
cat("\nAnalysis of variance aggCompScores = progExp\n")
print(summary(aov(pexp_model_comp)))

pexp_model_human = lm(aggHumanScores~progExp,data=dat)
cat("\nLinear Regression model, aggHumanScores = progExp\n")
print(summary(pexp_model_human))
cat("\nAnalysis of variance aggHumanScores = progExp\n")
print(summary(aov(pexp_model_human)))


#PUBLIC VS PRIVATE TESTING
pub = subset(fin,type==("PUBPP") | type==("PUBCE"))
priv = subset(fin,type==("PCPP") | type==("PCCE"))


pub_comp_test = t.test(pub$aggCompScores, priv$aggCompScores)
cat("\nT test for public computer condition v personal computer condition comp scores\n")
print(pub_comp_test)

priv_comp_test = t.test(pub$aggHumanScores, priv$aggHumanScores)
cat("\nT test for public computer condition v personal computer condition comp scores\n")
print(priv_comp_test)

#PEN AND PAPER VS COMPUTER TESTING

pPaper = subset(fin,type==("PUBPP") | type==("PCPP"))
comp = subset(fin,type==("PUBCE") | type==("PCCE"))


ppVComp_comp_test = t.test(pPaper$aggCompScores, comp$aggCompScores)
cat("\nT test for pen and paper condition v computer condition comp scores\n")
print(ppVComp_comp_test)

ppVComp_human_test = t.test(pPaper$aggHumanScores, comp$aggHumanScores)
cat("\nT test for pen and paper condition v computer condition comp scores\n")
print(ppVComp_human_test)

# TESTING ALL FOUR CONDITIONS
cat("\n ANOVA for all four conditions using comp scores.\n")
print(anova(lm(aggCompScores~type,data=fin)))

cat("\n ANOVA for all four conditions using human scores.\n")
print(anova(lm(aggHumanScores~type,data=fin)))

setEPS()
postscript("compBoxPlot.eps")
plot(y=fin$aggCompScores, x=fin$type, ylab="Score", xlab="Condition", main="Aggregated Computer Score by Expirmental Condition")
dev.off()

dev.new()
setEPS()
postscript("humanBoxPlot.eps")
plot(y=fin$aggHumanScores, x=fin$type, ylab="Score", xlab="Condition", main="Aggregated Human Score by Expirmental Condition")
dev.off()


dev.new()
setEPS()
postscript("pexpVaggComp.eps")
plot(y=fin$aggHumanScores, x=fin$progExp, ylab="Score", xlab="Experience in Years", main="Programmer Experience by Computer Scoring")
dev.off()

dev.new()
setEPS()
postscript("pexpVaggHuman.eps")
plot(y=fin$aggHumanScores, x=fin$progExp, ylab="Score", xlab="Experience in Years", main="Programmer Experience by Human Scoring")
dev.off()

pubGraph = rbind(pubPenPaper,pubCompPubEval)
persGraph = rbind(personalCompEval,personalPenPaper)

pubAvgVect = colMeans(pubGraph[,c(4:12)])
persAvgVect = colMeans(persGraph[,c(4:12)])

# print(pubAvgVect)
# print(persAvgVect)

dev.new()
setEPS()

postscript("ScorebyQuestion_persVpub.eps")
plot(pubAvgVect, col="red", bg="grey", ylab="Average Score", pch=19, xlab="Question Number", main="Average Computer Score by Question: Personal v. Public Computer")
lines(pubAvgVect, col="red")
points(persAvgVect, col="blue", pch=19)
lines(persAvgVect, col="blue")
legend("topright", "Public or Personal Computer", c("Public", "Personal"), inset=.05, fill=c("red","blue"))
dev.off()


ppGraph = rbind(pubPenPaper,personalPenPaper)
compGraph = rbind(personalCompEval,pubCompPubEval)

ppAvgVect = colMeans(ppGraph[,c(4:12)])
compAvgVect = colMeans(compGraph[,c(4:12)])

# print(pubAvgVect)
# print(persAvgVect)

dev.new()
setEPS()

postscript("ScorebyQuestion_ppVcomp.eps")
plot(compAvgVect, col="darkorchid4", bg="grey", ylim=c(6,9), ylab="Average Score", pch=19, xlab="Question Number", main="Average Computer Score by Question: Pen and Paper v Computer")
lines(compAvgVect, col="darkorchid4")
points(ppAvgVect, col="darkgreen", pch=19)
lines(ppAvgVect, col="darkgreen")
legend("topright", "Pen and Paper/Computer", c("Computer", "Pen and Paper"), inset=.05, fill=c("purple","green"))
dev.off()

graphics.off()



# print.data.frame(pubGraph)
# print.data.frame(persGraph)


# comp.fit = lm(aggCompScores~type+gender+progExp+age, data=fin)
# cat("LINEAR REGRESSION COMP")
# print(summary(comp.fit))

# human.fit = lm(aggHumanScores~type+gender+progExp+age, data=fin)
# cat("LINEAR REGRESSION COMP")
# print(summary(human.fit))


# par(mfrow=c(5,1))
# hist(fin$aggCompScores)
# hist(fin$aggHumanScores)
# hist(fin$progExp)
# hist(fin$age)
# plot(fin$type)

# dev.new()


# plot(y=fin$aggCompScores, x=fin$type, ylab="Score", xlab="Condition", main="Aggregated Computer Score by Expirmental Condition")
# boxplot(y=fin$aggHumanScores, x=fin$type, ylab="Score", xlab="Condition", main="Aggregated Human Score by Expirmental Condition")

# boxplot(y=fin$aggCompScores, x=fin$progExp, ylab="Score", xlab="Experience", main="Aggregated Computer Score by Programmer Experience")
# boxplot(y=fin$aggHumanScores, x=fin$progExp, ylab="Score", xlab="Experience", main="Aggregated Human Score by Programmer Experience")

# plot(y=fin$aggHumanScores, x=fin$gender, ylab="Score", xlab="Gender", main="Aggregated Computer Score by Gender")
# plot(y=fin$aggHumanScores, x=fin$gender, ylab="Score", xlab="Gender", main="Aggregated Human Score by Gender")

# typ = as.factor(fin$type)
# model = lm(aggCompScores ~ typ, data=fin)
# analysis = Anova(model, idata=fin, design=~typ)


#columns 6:14 in dat are the comp scoring, 14:23 are the author scoring

#bind the aggregate computer and human scores to the data


# logisticModel = glm(pubComp~q1+q2+q3+q4+q5+q6+q7+q8+q9,family=binomial(link='logit'),data=dat)
# aggLogModel = glm(pubComp~aggCompScores,family=binomial(link='logit'),data=agg)


