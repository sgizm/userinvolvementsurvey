######################################################################
# Cross analysis script for User Involvement Survey - Ethics

######################################################################

######################################################################
# Imports and constants
######################################################################

library(foreign)
library(ggplot2)
library(data.table)
library(gmodels)
library(Hmisc)
library(corrplot)
library(reshape2)
library(scales)
library(readr)


POPULATION.SIZE = 397
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year

######################################################################
# Read raw data files
######################################################################

### ERICSSON ###
data1 <- read.csv("raportti_ericsson.csv")
data1 <- data.frame(data1)
data1$jobfunction1 <- factor(data1$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:6),
                           labels = c("Developers", "Developers", "UX designers", "Managers", "Developers", "Developers", "Other"))
data1$jobfunction1.other <- data1$If.other..please.specify
data1$jobfunction1[4] <- "Developers" #Fixing
data1$jobfunction1.other[4] <- ""
data1$jobfunction1[9] <- "Developers" #Fixing custoemr manager to mng
data1$jobfunction1.other[9] <- ""
data1$jobfunction1[15] <- "Managers" #Fixing product owner to mng
data1$jobfunction1.other[15] <- ""
attach(data1)
# Job function
print("Primary job function")
summary(jobfunction1)
ggplot(data1, aes(x=jobfunction1)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 2, 3, 9, 21), labels = c("0", "2", "3", "9", "21"))
data1$jobfunction1.other

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data1$usernotif.S1 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data1$usernotif.S2 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data1$usernotif.S3 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data1$usernotif.S4 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data1$usernotif.S5 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data1$usernotif.S6 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data1$usernotif.S7 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.intentionally.deceive.or.mislead.the.user.if.experiment.results.depend.on.it
usernotif1.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif1.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data1$expinv.S1 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.experiment.results.will.be.correct
data1$expinv.S2 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data1$expinv.S3 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure
data1$expinv.S4 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.experiments
data1$expinv.S5 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data1$expinv.S6 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data1$expinv.S7 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv1.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv1.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")
attach(data1)
# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif1 <- data.frame(Statement=factor(rep(usernotif1.statements, each=length(data1$usernotif.S1))),
                          Rating=c(
                            data1$usernotif.S1,
                            data1$usernotif.S2,
                            data1$usernotif.S3,
                            data1$usernotif.S4,
                            data1$usernotif.S5,
                            data1$usernotif.S6,
                            data1$usernotif.S7), 
                          Jobf = data1$jobfunction1)
ggplot(data=undernotif1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Ericsson")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv1 <- data.frame(Statement=factor(rep(expinv1.statements, each=length(data1$expinv.S1))),
                      Rating=c(
                        data1$expinv.S1,
                        data1$expinv.S2,
                        data1$expinv.S3,
                        data1$expinv.S4,
                        data1$expinv.S5,
                        data1$expinv.S6,
                        data1$expinv.S7),
                      Jobf = data1$jobfunction1)
ggplot(data=expinv1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))




### F-SECURE ###
data2 <- read.csv("raportti_fsecure.csv")
data2 <- data.frame(data2)

data2$jobfunction2 <- factor(data2$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:9),
                           labels = c("Developers", "Managers", "Managers", "Managers", "UX designers", "Developers", "Other", "Other", "Other", "Other"))
data2$jobfunction2.other <- data2$If.other..please.specify
attach(data2)
# Job function
print("Primary job function")
summary(jobfunction2)
ggplot(data2, aes(x=jobfunction2)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 1, 6), labels = c("0", "1", "6"))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data2$usernotif.S1 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data2$usernotif.S2 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data2$usernotif.S3 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data2$usernotif.S4 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data2$usernotif.S5 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data2$usernotif.S6 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data2$usernotif.S7 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.trick.the.user.if.the.validity.of.experiment.results.depend.on.it
usernotif2.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif2.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data2$expinv.S1 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.results.will.be.correct
data2$expinv.S2 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data2$expinv.S3 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure
data2$expinv.S4 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.software.experiments
data2$expinv.S5 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data2$expinv.S6 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data2$expinv.S7 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv2.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv2.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")
attach(data2)
# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif2 <- data.frame(Statement=factor(rep(usernotif2.statements, each=length(data2$usernotif.S1))),
                          Rating=c(
                            data2$usernotif.S1,
                            data2$usernotif.S2,
                            data2$usernotif.S3,
                            data2$usernotif.S4,
                            data2$usernotif.S5,
                            data2$usernotif.S6,
                            data2$usernotif.S7)
                          , Jobf = data2$jobfunction2)
ggplot(data=undernotif2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("F-secure")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv2 <- data.frame(Statement=factor(rep(expinv2.statements, each=length(data2$expinv.S1))),
                      Rating=c(
                        data2$expinv.S1,
                        data2$expinv.S2,
                        data2$expinv.S3,
                        data2$expinv.S4,
                        data2$expinv.S5,
                        data2$expinv.S6,
                        data2$expinv.S7),
                      Jobf = data2$jobfunction2)
ggplot(data=expinv2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))





### VAADIN ###
data3 <- read.csv("raportti_vaadin.csv")
data3 <- data.frame(data3)
data3$jobfunction3 <- factor(data3$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:10),
                           labels = c("Developers", "Developers", "Managers", "Managers", "UX designers", "Developers", "Other" ,"Other", "Other", "Other", "Other"))
#fixing: changed other to sales as both were sales anyways
data3$jobfunction3[11] <- "Managers" #fixing: adding "account manager" to management, others 
data$jobfunction.other <- data$If.other..please.specify
data$jobfunction.other[11] <- ""
attach(data3)
# Job function
print("Primary job function")
summary(jobfunction3)
ggplot(data3, aes(x=jobfunction3)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 1, 6), labels = c("0", "1", "6"))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data3$usernotif.S1 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data3$usernotif.S2 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data3$usernotif.S3 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data3$usernotif.S4 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data3$usernotif.S5 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data3$usernotif.S6 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data3$usernotif.S7 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.trick.the.user.if.the.validity.of.experiment.results.depend.on.it
usernotif3.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif3.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data3$expinv.S1 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.results.will.be.correct
data3$expinv.S2 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data3$expinv.S3 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure
data3$expinv.S4 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.software.experiments
data3$expinv.S5 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data3$expinv.S6 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data3$expinv.S7 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv3.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv3.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")
attach(data3)
# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif3 <- data.frame(Statement=factor(rep(usernotif3.statements, each=length(data3$usernotif.S1))),
                          Rating=c(
                            data3$usernotif.S1,
                            data3$usernotif.S2,
                            data3$usernotif.S3,
                            data3$usernotif.S4,
                            data3$usernotif.S5,
                            data3$usernotif.S6,
                            data3$usernotif.S7)
                          ,Jobf = data3$jobfunction3)
ggplot(data=undernotif3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Vaadin")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv3 <- data.frame(Statement=factor(rep(expinv3.statements, each=length(data3$expinv.S1))),
                      Rating=c(
                        data3$expinv.S1,
                        data3$expinv.S2,
                        data3$expinv.S3,
                        data3$expinv.S4,
                        data3$expinv.S5,
                        data3$expinv.S6,
                        data3$expinv.S7),
                      Jobf = data3$jobfunction3)
ggplot(data=expinv3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))





### REAKTOR ###
data4 <- read.csv("raportti_reaktor.csv", encoding = "UTF-8")
data4 <- data.frame(data4)
data4$jobfunction4 <- factor(data4$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:6),
                           labels = c("Developers", "Managers", "Managers", "UX designers", "UX designers", "Other", "Other"))
data4$jobfunction4[48]
data4$jobfunction4.other <- data$If.other..please.specify
data4$jobfunction4.other[45] <- "" #fixing: This person already marked UX design, but also put in other "UI design". no need for the second one
data4$jobfunction4.other[48] <- "" #fixing: UX, graphic design to UX design
data4$jobfunction4[48] <- "UX designers"
attach(data4)
print("Primary job function")
summary(jobfunction4)
ggplot(data4, aes(x=jobfunction4)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 2, 3, 12, 43), labels = c("0", "2", "3", "12", "43"))

data$jobfunction.other
# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data4$usernotif.S1 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data4$usernotif.S2 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data4$usernotif.S3 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data4$usernotif.S4 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data4$usernotif.S5 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data4$usernotif.S6 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data4$usernotif.S7 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.trick.the.user.if.the.validity.of.experiment.results.depend.on.it
usernotif4.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif4.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data4$expinv.S1 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.results.will.be.correct
data4$expinv.S2 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data4$expinv.S3 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...My.customer.does.not.have.the.needed.technical.infrastructure
data4$expinv.S4 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.software.experiments
data4$expinv.S5 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data4$expinv.S6 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data4$expinv.S7 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.my.customer.s.product.strategy
expinv4.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv4.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")
attach(data4)
# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif4 <- data.frame(Statement=factor(rep(usernotif4.statements, each=length(data4$usernotif.S1))),
                         Rating=c(
                           data4$usernotif.S1,
                           data4$usernotif.S2,
                           data4$usernotif.S3,
                           data4$usernotif.S4,
                           data4$usernotif.S5,
                           data4$usernotif.S6,
                           data4$usernotif.S7)
                         ,Jobf = data4$jobfunction4)
#, Jobf = data4$jobfunction4
ggplot(data=undernotif4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Reaktor")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv4 <- data.frame(Statement=factor(rep(expinv4.statements, each=length(data4$expinv.S1))),
                     Rating=c(
                       data4$expinv.S1,
                       data4$expinv.S2,
                       data4$expinv.S3,
                       data4$expinv.S4,
                       data4$expinv.S5,
                       data4$expinv.S6,
                       data4$expinv.S7),
                     Jobf = data4$jobfunction4)
ggplot(data=expinv4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))




### ALL DATA ###
total_undernotif <- rbind(undernotif1, undernotif2, undernotif3, undernotif4)
total_expinv <- rbind(expinv1, expinv2, expinv3, expinv4)

xx <- rbind(undernotif1, undernotif2, undernotif3, undernotif4) 
yy  <- rbind(expinv1, expinv2, expinv3, expinv4)
  
#4.1
ggplot(data=total_undernotif, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Total") + scale_x_discrete(limits=c("It is ok to trick the user if the validty of experiment results depend on it", "It is ok not to disclose all the experiment details to users involved", "Users should always be notified when they are being involved in an experiment", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "If no laws are being broken, users do not need to be notified", "If we collect personal information, users need to be notified", "Users do not need to know they are involved"))
#Total versus roles:
ggplot(xx, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + ggtitle("Total") + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~xx$Jobf) +  labs(x = "", y = "") + scale_x_discrete(limits=c("It is ok to trick the user if the validty of experiment results depend on it", "It is ok not to disclose all the experiment details to users involved", "Users should always be notified when they are being involved in an experiment", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "If no laws are being broken, users do not need to be notified", "If we collect personal information, users need to be notified", "Users do not need to know they are involved"))
# , labeller = as_labeller(jb_names)

#4.2
ggplot(data=total_expinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Total") + scale_x_discrete(limits=c("Experiments reveal secrets about my customer's strategy", "Experiments give users false expectations", "Users have to be convinced of the benefit before taking part", "Users would not like to be part of software experiments", "My customer does not have the needed technical infrastructure", "Involving users in experiments is time-consuming", "I cannot trust that the experiment results will be correct"))
#Total versus roles:
ggplot(yy, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + ggtitle("Total") + scale_size_continuous(range = c(0, 70)) + facet_wrap(~yy$Jobf) +  labs(x = "", y = "") + scale_x_discrete(limits=c("Experiments reveal secrets about my customer's strategy", "Experiments give users false expectations", "Users have to be convinced of the benefit before taking part", "Users would not like to be part of software experiments", "My customer does not have the needed technical infrastructure", "Involving users in experiments is time-consuming", "I cannot trust that the experiment results will be correct"))

