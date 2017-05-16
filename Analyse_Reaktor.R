######################################################################
# Analysis script for User Involvement Survey - Reaktor

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


POPULATION.SIZE = 397
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year

######################################################################
# Read raw data files
######################################################################

data <- read.csv("raportti_reaktor.csv", encoding = "UTF-8")
data <- data.frame(data)

######################################################################
# Preprocess
######################################################################

###### Demographics #######
data$jobfunction <- factor(data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:6),
                           labels = c("Developing software", "Product management", "Business development", "UX design", "Graphic design", "Coaching", "Other"))
# some fixing required.. new category: data science; and mixed roles e.g., ux + data science
#data$jobfunction[11] <- "Management, other" #fixing: adding "account manager" to management, others 
data$jobfunction.other <- data$If.other..please.specify
#data$jobfunction.other[11] <- ""

# Worktime
data$worktime <- data$X1.2.How.long.have.you.been.working.in.your.current.company.role.
#fixings: 9999 to: 132 for a better histogram 
data$worktime[c(9, 12, 24, 30, 40, 64)] <- 132 

#[15:58, 1/26/2017] Fabian Fagerholm: and the scale will draw better                        
#[15:59, 1/26/2017] Fabian Fagerholm: but we have to remember that when reporting

data$gender <- factor(data$X1.3.Which.of.the.following.best.describes.you..,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))

# Age range
data$age_range <- factor(data$X1.4.What.is.your.age.range..,
                         levels = c(0:4),
                         labels = c("20 or less", "21-30", "31-40", "41-50", "50 or more"))

#Team size
data$teamsize <- factor(data$X1.5.How.many.people.do.you.work.with.on.a.regular.basis.in.the.company..,
                        levels = c(0:4),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20"))
# Work location
data$location <- factor(data$X1.6.Where.is.your.primary.office.located..,
                        levels = c(0:4),
                        labels = c("Helsinki", "Seinäjoki", "New York", "Tokyo", "Amsterdam"))

#### CHAR PROBLEM!


# End user
data$end_user <- factor(data$X1.7.Who.do.you.consider.as.your.primary.user.in.your.job.function..,
                        levels = c(0:1),
                        labels = c("Software developer using Vaadin products", "End user using applications developed with Vaadin tools or frameworks"))

# 2.1 In which development activities are users involved in your company? (click all that apply)
data$useractivities.forming.ideas <- data$Forming.product.or.service.ideas
data$useractivities.gathering.requirements <- data$Gathering.requirements
data$useractivities.software.design <- data$Software.design
data$useractivities.implementation <- data$Implementation
data$useractivities.testing <- data$Testing
data$useractivities.after.release <- data$The.activities.after.release
data$useractivities.commiting.code <- data$Committing.code
data$useractivities.fixes <- data$Providing.fixes
data$useractivities.submitting.bugs <- data$Submitting.bugs
data$useractivities.online.discussion <- data$Participating.in.online.discussion
data$useractivities.other <- data$Other
data$useractivities.other.open <- data$If.other..please.specify..separate.with.commas.
useractivities.options <- c("Forming ideas", "Gathering requirements", "Software design", "Implementing software", "Testing", "The activities after release", "Committing code", "Providing fixes", "Submitting bugs", "Participating in online discussion", "Other")

#Other roles
data$useractivities.other <- data$If.other..please.specify

# 2.2. How much do you agree with the following statements? (User involvement statements)
data$userinv.S1 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data$userinv.S2 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data$userinv.S3 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data$userinv.S4 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users....needs
data$userinv.S5 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work
data$userinv.S6 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
data$userinv.S7 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.would.like.to.get.more.feedback.from.users

userinv.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users' needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date",
  "I would like to get more feedback from users"
)
userinv.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 2.3 In your experience, how easy is it for the following to get information from users?
data$userinf.pro.mng <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Product.managers
data$userinf.mng <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Managers..other
data$userinf.ux.designer <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...UX.designers
data$userinf.dev.frame <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Vaadin.Framework.developers
data$userinf.dev.tool <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Vaadin.Pro.Tools.developers
data$userinf.arc <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Software.architects.
data$userinf.adv <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Product.and.service.advocates
data$userinf.con <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Consultants
data$userinf.cust.sup <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Customer.support
data$userinf.tra <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Trainers
data$userinf.slf <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Myself
userinf.statements <- c("Product managers", "Managers, other", "UX designers", "Vaadin Framework developers", "Vaadin Pro Tools developers", "Software architects", "Product and service advocates", "Consultants", "Customer support", "Trainers", "Myself")
userinf.options <- c("Very difficult", "Difficult", "Neither easy nor difficult", "Easy", "Very easy", "I don't know")

# 2.4 How often do you use the following ways to get information about users?
data$infofreq.O1 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.remotely.observe.users.when.they.are.using.the.software..e.g...screen.sharing.
data$infofreq.O2 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.am.physically.present.with.users.when.they.are.using.the.software..e.g...talk.aloud.study.
data$infofreq.O3 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.the.users.before.they.use.the.software
data$infofreq.O4 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.users.in.person.after.they.used.the.software..e.g...post.use.interview.
data$infofreq.O5 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.recorded.usage.data..e.g...log.data.
data$infofreq.O6 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.online.discussion.channels..e.g..forums.

infofreq.statements <- c(
  "I remotely observe users when they are using the software (e.g., screen sharing)",
  "I am physically present with users when they are using the software (e.g., talk-aloud study)",
  "I interact with the users before they use the software",
  "I interact with users in person after they used the software (e.g., post-use interview)",
  "Through recorded usage data (e.g., log data)",
  "Through online discussion channels")
infofreq.options <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2.5 Try to remember a situation where you knew that involving users in development would be useful, but you could not involve them. Please describe the situation and what challenges you faced.
data$userinf.open <- data$X2.5.Try.to.remember.a.situation.where.you.knew.that.involving.users.in.development.would.be.useful..but.you.could.not.involve.them..Please.describe.the.situation.and.what.challenges.you.faced.

# 3.1 Does your company conduct experiments involving the users?
data$condexp <- factor(data$X3.1.Does.your.company.conduct.experiments.involving.the.users.,
                       levels = c(1:4, "NA"),
                       labels = c("Never", "Rarely", "Occasionally", "Yes, actively", "I don't know"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data$exp.open <- data$X3.2.Please.describe.a.typical.experiment.you.have.seen.or.been.involved.in.in.your.company..including.the.roles...Skip.this.if.you.have.not.seen.or.been.involved.in.any.experiments..

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
data$understanding.S1 <- data$To.understand.users..needs.better.......data.should.always.be.collected.because.it.might.be.needed.later
data$understanding.S2 <- data$To.understand.users..needs.better.......data.should.only.be.collected.when.there.is.a.known.need.or.assumption
data$understanding.S3 <- data$To.understand.users..needs.better.......rich..detailed.data.about.what.users.do.is.useful
data$understanding.S4 <- data$To.understand.users..needs.better.......focused.data.on.a.specific.user.action.or.behaviour.is.useful
data$understanding.S5 <- data$To.understand.users..needs.better.......users.themselves.must.be.actively.involved.in.shaping.the.software
data$understanding.S6 <- data$To.understand.users..needs.better.......we.need.to.measure.user.behaviour.to.decide.what.the.software.should.be.like
understanding.statements <- c(
  "..data should always be collected because it might be needed later",
  "..data should only be collected when there is a known need or assumption",
  "..rich, detailed data about what users do is useful",
  "..focused data on a specific user action or behaviour is useful",
  "..users themselves must be actively involved in shaping the software",
  "..we need to measure user behaviour to decide what the software should be like"
)
understanding.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data$usernotif.S1 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data$usernotif.S2 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data$usernotif.S3 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data$usernotif.S4 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data$usernotif.S5 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data$usernotif.S6 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data$usernotif.S7 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.trick.the.user.if.the.validity.of.experiment.results.depend.on.it
usernotif.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data$expinv.S1 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.results.will.be.correct
data$expinv.S2 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data$expinv.S3 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure
data$expinv.S4 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.software.experiments
data$expinv.S5 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data$expinv.S6 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data$expinv.S7 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "Our company does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about the product strategy")
expinv.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")


attach(data)

######################################################################
# Descriptive statistics
######################################################################

## Data set summary
print(paste("Number of responses after cleaning:", nrow(data)))
print(paste("Response rate:", (nrow(data) / POPULATION.SIZE) * 100, "%"))

## Demographics

# Job function
print("Primary job function")
summary(jobfunction)
ggplot(data, aes(x=jobfunction)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 2, 3, 12, 43), labels = c("0", "2", "3", "12", "43"))

data$jobfunction.other

# Gender
print("Gender")
summary(gender)
ggplot(data, aes(x=gender)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Gender", y="Frequency") + theme(axis.text=element_text(size=15)) + scale_y_continuous(breaks=c(0, 3, 6, 57), labels = c("0", "3", "6", "57"))

# Age range
print("Age range")
summary(age_range)
ggplot(data, aes(x=age_range)) +
  geom_bar(fill="lightgoldenrod2", colour="white") +
  labs(x="Age range", y="Frequency") + scale_y_continuous(breaks=c(0, 14, 16, 36), labels = c("0", "14", "16", "36"))

# Work time
print("How long have you been working in your current role")
summary(worktime)
ggplot(data, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency")  + theme(axis.text=element_text(size=10))+ scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,4,5,6,7,8,9), labels = c("0", "1", "4", "5", "6","7", "8", "9"))

# Team size
print("Team size")
summary(teamsize)
ggplot(data, aes(x=teamsize)) +
  geom_bar(fill="lightgoldenrod2", colour="white") +
  labs(x="Team size", y="Frequency") + theme(axis.text=element_text(size=13)) + scale_y_continuous(breaks=c(0,3,6,10,20,27), labels = c("0", "3", "6", "10", "20","27"))

# Work location
print("Work location")
summary(location)
ggplot(data, aes(x=location)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="Work location", y="Frequency") + theme(axis.text=element_text(size=13)) + scale_y_continuous(breaks=c(0,2,3,10,16), labels = c("0","2", "3", "10", "16"))

# End user
print("End user")
summary(end_user)
ggplot(data, aes(x=end_user)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="End user", y="Frequency") + theme(axis.text=element_text(size=13)) + scale_y_continuous(breaks=c(0,5,7,10,14), labels = c("0", "5", "7", "10", "14"))

# 2.1 In which development activities are users involved in your company? (click all that apply)
useractivities.forming.ideas <- sum(data$useractivities.forming.ideas, na.rm=TRUE)
useractivities.designing.software <- sum(data$useractivities.gathering.requirements, na.rm=TRUE)
useractivities.software.design <- sum(data$useractivities.software.design, na.rm=TRUE)
useractivities.implementation <- sum(data$useractivities.implementation, na.rm=TRUE)
useractivities.testing <- sum(data$useractivities.testing, na.rm=TRUE)
useractivities.after.release <- sum(data$useractivities.after.release, na.rm=TRUE)
useractivities.commiting.code <- sum(data$useractivities.commiting.code, na.rm=TRUE)
useractivities.providing.fixes <- sum(data$useractivities.providing.fixes, na.rm=TRUE)
useractivities.submitting.bugs <- sum(data$useractivities.submitting.bugs, na.rm=TRUE)
useractivities.online.discussion<- sum(data$useractivities.online.discussion, na.rm=TRUE)
useractivities.other <- sum(data$useractivities.other, na.rm=TRUE)
useractivities <- data.frame(Activity=useractivities.options,
                             Frequency=c(
                               useractivities.forming.ideas,
                               useractivities.designing.software,
                               useractivities.software.design,
                               useractivities.implementation,
                               useractivities.testing,
                               useractivities.after.release,
                               useractivities.commiting.code,
                               useractivities.providing.fixes,
                               useractivities.submitting.bugs,
                               useractivities.online.discussion,
                               useractivities.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities)
ggplot(data=useractivities, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="plum4", colour="black") + theme(axis.text=element_text(size=14), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ scale_x_discrete(limits=c("Forming ideas","Gathering requirements","Software design", "Implementing software", "Testing", "The activities after release", "Committing code", "Providing fixes", "Submitting bugs", "Participating in online discussion", "Other")) + scale_y_continuous(breaks=c(0,3,5,8,9,12,13,16,17), labels = c("0", "3", "5", "8", "9", "12", "13", "16", "17"))


# 2.2 How much do you agree with the following statements?
userinv <- data.frame(Statement=factor(rep(userinv.statements, each=length(userinv.S1))),
                      Rating=c(
                        userinv.S1,
                        userinv.S2,
                        userinv.S3,
                        userinv.S4,
                        userinv.S5,
                        userinv.S6,
                        userinv.S7))
ggplot(data=userinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=16))
# + scale_y_discrete(name="Rating", limits=c("1","2", "3","4","5"))

# 2.3 In your experience, how easy is it for the following to get information from users?
userinf <- data.frame(Statement=factor(rep(userinf.statements, each=length(userinf.pro.mng))),
                      Rating=c(
                        userinf.pro.mng,
                        userinf.mng,
                        userinf.ux.designer,
                        userinf.dev.frame,
                        userinf.dev.tool,
                        userinf.arc,
                        userinf.adv,
                        userinf.con,
                        userinf.cust.sup,
                        userinf.tra,
                        userinf.slf))
ggplot(data=userinf, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("Myself", "Trainers", "Customer support", "Consultants", "Product and service advocates", "Software architects", "Vaadin Pro Tools developers", "Vaadin Framework developers", "UX designers", "Managers, other", "Product managers")) + theme(axis.text=element_text(size=16))

# 2.4 How often do you use the following ways to get information about users?
infofreq <- data.frame(Statement=factor(rep(infofreq.statements, each=length(infofreq.O1))),
                       Rating=c(
                         infofreq.O1,
                         infofreq.O2,
                         infofreq.O3,
                         infofreq.O4,
                         infofreq.O5,
                         infofreq.O6))
ggplot(data=infofreq, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))

#2.5 Try to remember a situation
data$userinf.open

# 3.1 Does your company conduct experiments involving the users?
ggplot(data, aes(x=condexp)) +
  geom_bar(fill="burlywood1", colour="white") +
  labs(x="Conducting experiments", y="Frequency") + theme(axis.text=element_text(size=16)) + scale_y_continuous(breaks=c(0,2,6,11), labels = c("0", "2", "6", "11"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data$exp.open

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
understanding <- data.frame(Statement=factor(rep(understanding.statements, each=length(understanding.S1))),
                            Rating=c(
                              understanding.S1,
                              understanding.S2,
                              understanding.S3,
                              understanding.S4,
                              understanding.S5,
                              understanding.S6))
ggplot(data=understanding, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("..we need to measure user behaviour to decide what the software should be like","..users themselves must be actively involved in shaping the software", "..focused data on a specific user action or behaviour is useful", "..rich, detailed data about what users do is useful", "..data should only be collected when there is a known need or assumption", "..data should always be collected because it might be needed later" )) + theme(axis.text=element_text(size=11))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif <- data.frame(Statement=factor(rep(usernotif.statements, each=length(usernotif.S1))),
                         Rating=c(
                           usernotif.S1,
                           usernotif.S2,
                           usernotif.S3,
                           usernotif.S4,
                           usernotif.S5,
                           usernotif.S6,
                           usernotif.S7))
ggplot(data=undernotif, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv <- data.frame(Statement=factor(rep(expinv.statements, each=length(expinv.S1))),
                     Rating=c(
                       expinv.S1,
                       expinv.S2,
                       expinv.S3,
                       expinv.S4,
                       expinv.S5,
                       expinv.S6,
                       expinv.S7))
ggplot(data=expinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))



####CROSS_ANALYSIS###

jb_names <- c(
  'Developing Vaadin framework' = "Framework developers",
  'Developing Vaadin Pro Tools' = "Pro tools developers",
  'Product management' =  'Product managers',
  'Management, other' = "Managers, other",
  'UX Design' = "UX designers",
  'Advocating products and services' = "Advocates",
  'Providing consulting' = "Consultants",
  'Providing customer support' = "Customer support",
  "Sales" = "Sales"
)

#2.1 - useractivities over end-user types 
### Note: cheking the different activities over end-user types did not indicate any remarkable correlation 
# labeller = as_labeller(jb_names) to be added to face_wrap
cc <- table(data$end_user, data$useractivities.submitting.bugs)
cc<-data.frame(cc)
ccc <- cc[cc$Var2 == "TRUE",] 
ggplot(ccc, aes(x=Var1,y=Freq,fill=Var2))+geom_bar(stat="identity") + labs(x="end user type", y="Frequencies", title="People who marked 'submitting bugs' for user involvement activities") + theme(plot.title = element_text(hjust = 0.5))+guides(fill=FALSE) + theme(plot.title = element_text(lineheight=.6))

cd <- table(data$end_user, data$useractivities.online.discussion)
cd<-data.frame(cd)
ccd <- cd[cd$Var2 == "TRUE",] 
ggplot(ccd, aes(x=Var1,y=Freq,fill=Var2))+geom_bar(stat="identity") + labs(x="end user type", y="Frequencies", title="People who marked 'forming ideas' for user involvement activities") + theme(plot.title = element_text(hjust = 0.5))+guides(fill=FALSE) + theme(plot.title = element_text(lineheight=.6))

#2.1 - useractivities over jobfunctions, nothing conclusive 
cx <- table(data$jobfunction, data$useractivities.online.discussion)
cx<-data.frame(cx)
ccx <- cx[cx$Var2 == "TRUE",] 
ggplot(ccx, aes(x=Var1,y=Freq,fill=Var2))+geom_bar(stat="identity") + labs(x="job function", y="Frequencies", title="People who marked 'forming ideas' for user involvement activities") + theme(plot.title = element_text(hjust = 0.5))+guides(fill=FALSE) + theme(plot.title = element_text(lineheight=.6))

#2.2
ggplot(userinv,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction , labeller = as_labeller(jb_names)) +  labs(x = "", y = "") + theme(axis.text=element_text(size=11))

#2.3
ggplot(userinf,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction , labeller = as_labeller(jb_names)) +  labs(x = "", y = "") + theme(axis.text=element_text(size=11))

#2.4
ggplot(infofreq,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction , labeller = as_labeller(jb_names)) +  labs(x = "", y = "") + theme(axis.text=element_text(size=11))

#3.3
ggplot(understanding, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction , labeller = as_labeller(jb_names)) +  labs(x = "", y = "") + theme(axis.text=element_text(size=11)) + scale_x_discrete(limits=c("..we need to measure user behaviour to decide what the software should be like","..users themselves must be actively involved in shaping the software", "..focused data on a specific user action or behaviour is useful", "..rich, detailed data about what users do is useful", "..data should only be collected when there is a known need or assumption", "..data should always be collected because it might be needed later" ))

#4.1
ggplot(undernotif, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction , labeller = as_labeller(jb_names)) +  labs(x = "", y = "") #Bu oldu

#4.2
ggplot(expinv, aes(x=Statement,y=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction, labeller = as_labeller(jb_names)) +  labs(x = "", y = "") + theme(axis.text=element_text(size=11))
