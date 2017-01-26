######################################################################
# Analysis script for User Involvement Survey
# Note: set working directory to the location of the survey CSV file
# Note: run with Rscript analyse.R
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


POPULATION.SIZE = 35
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year

######################################################################
# Read raw data files
######################################################################

data <- read.csv("raporttii.csv")
data <- data.frame(data)
######################################################################
# Preprocess
######################################################################

# Demographics
data$jobfunction <- factor(data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:6),
                           labels = c("Developing software", "Testing software", "UX Design", "Management", "Operations", "Architecture", "Other"))
data$jobfunction.other <- data$If.other..please.specify
data$jobfunction[4] <- "Architecture" #Fixing
data$jobfunction.other[4] <- ""
data$jobfunction[9] <- "Management" #Fixing custoemr manager to mng
data$jobfunction.other[9] <- ""
data$jobfunction[15] <- "Management" #Fixing product owner to mng
data$jobfunction.other[15] <- ""

data$worktime <- data$X1.2.How.long.have.you.been.working.in.your.current.company.role.
data$worktime[5] <- NA #fixing
data$birthyear <- data$X1.3.What.is.your.year.of.birth.
data$birthyear[31] <- data$birthyear[31] + 1900 # Fix data entry error
data$age <- CURRENTYEAR - data$birthyear
data$gender <- factor(data$X1.4.Which.of.the.following.best.describes.you..,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))

data$teamsize <- factor(data$X1.5.What.is.the.size.of.your.primary.work.team..,
                        levels = c(0:4,""),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20", "No team"))


# 2.1 In which development activities are users involved in your company? (click all that apply)
data$useractivities.specifying.requirements <- data$Specifying.requirements
data$useractivities.designing.software <- data$Designing.software
data$useractivities.implementing.software <- data$Implementing.software
data$useractivities.testing <- data$Testing
data$useractivities.after.release <- data$The.activities.after.release
data$useractivities.other <- data$Other
data$useractivities.other.open <- data$If.other..please.specify..separate.with.commas.
useractivities.options <- c("Specifying requirements", "Designing software", "Implementing software", "Testing", "The activities after release", "Other")

# 2.2. How much do you agree with the following statements? (User involvement statements)
data$userinv.S1 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data$userinv.S2 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data$userinv.S3 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data$userinv.S4 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
data$userinv.S5 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work
data$userinv.S6 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
userinv.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users’ needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date"
)
userinv.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 2.3 In your experience, how easy is it for the following to get information from users?
data$userinf.mgr <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Managers
data$userinf.uxd <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...UX.designers
data$userinf.dev <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.developers.
data$userinf.tst <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.testers
data$userinf.arc <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.architects.
data$userinf.ops <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...System.or.network.operators
data$userinf.slf <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Myself
userinf.statements <- c("Managers", "UX designers", "Software developers", "Software testers", "Software architects", "System or network operators", "Myself")
userinf.options <- c("Very difficult", "Difficult", "Neither easy nor difficult", "Easy", "Very easy", "I don't know")

# 2.4 How often do you use the following ways to get information about users?
data$infofreq.O1 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.remotely.observe.users.when.they.are.using.the.software..e.g...screen.sharing.
data$infofreq.O2 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.am.physically.present.with.users.when.they.are.using.the.software..e.g...talk.aloud.study.
data$infofreq.O3 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.users.in.person.after.they.used.the.software..e.g...post.use.interview.
data$infofreq.O4 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.recorded.usage.data..e.g...log.data.or.video.
infofreq.statements <- c(
  "I remotely observe users when they are using the software (e.g., screen sharing)",
  "I am physically present with users when they are using the software (e.g., talk-aloud study)",
  "I interact with users in person after they used the software (e.g., post-use interview)",
  "Through recorded usage data (e.g., log data or video)")
infofreq.options <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2.5 Try to remember a situation where you knew that involving users in development would be useful, but you could not involve them. Please describe the situation and what challenges you faced.
data$userinf.open <- data$X2.5.Try.to.remember.a.situation.where.you.knew.that.involving.users.in.development.would.be.useful..but.you.could.not.involve.them..Please.describe.the.situation.and.what.challenges.you.faced.

# 3.1 Does your company conduct experiments involving the users?
data$condexp <- factor(data$X3.1.Does.your.company.conduct.experiments.involving.the.users..,
                       levels = c(1:4, "NA"),
                       labels = c("Never", "Rarely", "Occasionally", "Yes, actively", "I don't know"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data$exp.open <- data$X3.2.Please.describe.a.typical.experiment.you.have.seen.or.been.involved.in.in.your.company..including.the.roles...Skip.this.if.you.have.not.seen.or.been.involved.in.any.experiments..

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
data$understanding.S1 <- data$For.understanding.user.needs.better.........data.should.always.be.collected.because.it.might.be.needed.later
data$understanding.S2 <- data$For.understanding.user.needs.better.........data.should.only.be.collected.when.there.is.a.known.need.or.assumption.to.test
data$understanding.S3 <- data$For.understanding.user.needs.better.........all.data.about.user.behaviour.is.useful
data$understanding.S4 <- data$For.understanding.user.needs.better.........focused.data.on.a.specifically.chosen.user.action.is.useful
data$understanding.S5 <- data$For.understanding.user.needs.better.........users.themselves.must.be.actively.involved.in.development
data$understanding.S6 <- data$For.understanding.user.needs.better.........we.just.need.to.measure.user.behaviour
understanding.statements <- c(
  "For understanding user needs better data should always be collected because it might be needed later",
  "For understanding user needs better data should only be collected when there is a known need or assumption to test",
  "For understanding user needs better all data about user behaviour is useful",
  "For understanding user needs better focused data on a specifically chosen user action is useful",
  "For understanding user needs better users themselves must be actively involved in development",
  "For understanding user needs better we just need to measure user behaviour"
)
understanding.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data$usernotif.S1 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data$usernotif.S2 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data$usernotif.S3 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data$usernotif.S4 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data$usernotif.S5 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data$usernotif.S6 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data$usernotif.S7 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.intentionally.deceive.or.mislead.the.user.if.experiment.results.depend.on.it
usernotif.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to intentionally deceive or mislead the user if experiment results depend on it")
usernotif.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data$expinv.S1 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.experiment.results.will.be.correct
data$expinv.S2 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data$expinv.S3 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure.to.run.experiments
data$expinv.S4 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.experiments
data$expinv.S5 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part.in.an.experiment
data$expinv.S6 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data$expinv.S7 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "Our company does not have the needed technical infrastructure to run experiments",
  "Users would not like to be part of experiments",
  "Users have to be convinced of the benefit before taking part in an experiment",
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
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Job function", y="Frequency")
#ggplot(data,aes(x = factor(""), fill=jobfunction))+geom_bar()+ coord_polar(theta = "y")  +scale_x_discrete("") #alternative pie chart

# Work time
print("How long have you been working in your current role")
summary(worktime)
ggplot(data, aes(x=worktime)) +
  geom_histogram(binwidth=10) +
  labs(x="Work time", y="Frequency") 

# Birth year
print("Year of birth")
summary(birthyear)
ggplot(data, aes(x=birthyear)) +
  geom_histogram(binwidth=1, fill="#FF9999", colour="#FF9999") +
  labs(x="Year of birth", y="Frequency")

# Age
print("Age")
summary(age)
ggplot(data, aes(x=age)) +
  geom_density(fill="#FF9999", colour="#FF9999") +
  labs(x="Age", y="Density")

ggplot(data, aes(x=age)) + 
  geom_histogram(aes(y=..density..),# Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF9999", colour="#FF9999")  # Overlay with transparent density plot

# Gender
print("Gender")
summary(gender)
ggplot(data, aes(x=gender)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Gender", y="Frequency")

# Team size
print("Team size")
summary(teamsize)
ggplot(data, aes(x=teamsize)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Team size", y="Frequency")


######################################################################
# Analysis
######################################################################

## Section 2

# 2.1 In which development activities are users involved in your company? (click all that apply)
useractivities.specifying.requirements.count <- sum(data$useractivities.specifying.requirements, na.rm=TRUE)
useractivities.designing.software.count <- sum(data$useractivities.designing.software, na.rm=TRUE)
useractivities.implementing.software <- sum(data$useractivities.implementing.software, na.rm=TRUE)
useractivities.testing <- sum(data$useractivities.testing, na.rm=TRUE)
useractivities.after.release <- sum(data$useractivities.after.release, na.rm=TRUE)
useractivities.other <- sum(data$useractivities.other, na.rm=TRUE)
useractivities <- data.frame(Activity=useractivities.options,
                             Frequency=c(
                               useractivities.specifying.requirements.count,
                               useractivities.designing.software.count,
                               useractivities.implementing.software,
                               useractivities.testing,
                               useractivities.after.release,
                               useractivities.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities)
ggplot(data=useractivities, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="#FF9999", colour="#FF9999")

# 2.2 How much do you agree with the following statements?
userinv <- data.frame(Statement=factor(rep(userinv.statements, each=length(userinv.S1))),
                      Rating=c(
                        userinv.S1,
                        userinv.S2,
                        userinv.S3,
                        userinv.S4,
                        userinv.S5,
                        userinv.S6))
ggplot(data=userinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

# 2.3 In your experience, how easy is it for the following to get information from users?
userinf <- data.frame(Statement=factor(rep(userinf.statements, each=length(userinf.mgr))),
                      Rating=c(
                        userinf.mgr,
                        userinf.uxd,
                        userinf.dev,
                        userinf.tst,
                        userinf.arc,
                        userinf.ops,
                        userinf.slf))
ggplot(data=userinf, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

# 2.4 How often do you use the following ways to get information about users?
infofreq <- data.frame(Statement=factor(rep(infofreq.statements, each=length(infofreq.O1))),
                       Rating=c(
                         infofreq.O1,
                         infofreq.O2,
                         infofreq.O3,
                         infofreq.O4))
ggplot(data=infofreq, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

# 3.1 Does your company conduct experiments involving the users?
ggplot(data, aes(x=condexp)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Conducting experiments", y="Frequency")

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
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

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
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

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
  geom_boxplot() + guides(fill=FALSE) + coord_flip()

## CROSS-ANALYSIS ##

#Team size vs. job functions 
#ggplot(data,aes(x=data$jobfunction,y=data$teamsize))+geom_point() + guides(fill=FALSE) + coord_flip() + labs(x="Roles",y="Team size") #bu olmadi
aa <- table(data$jobfunction, data$teamsize)
aa <- data.frame(aa)
ggplot(aa,aes(x=Var2,y=Freq,fill=Var1))+geom_bar(stat="identity") + labs(x="Team size",y="Frequencies")
ggplot(aa,aes(x=Var1,y=Freq,fill=Var2))+geom_bar(stat="identity") + labs(x="Job functions",y="Frequencies") #sanki bu daha iyi


##bb <- aa[-c(36:42), ] no team rowlardan kurtulmaca
##Hala teamsize vs. jobfunctions buble
ggplot(bb, aes(x = Var2, y = Var1, size=Freq)) +
       geom_point(shape=21, colour = "#000000", fill = "#FF9999") + scale_size_continuous(range = c(0, 30)) +
       labs(x = "Teamsize", y = "Job Functions") + scale_fill_continuous(low = "#000000", high = "#FF9999") 

#Gender vs. job functions
ab <- table(data$jobfunction, data$gender)
ab <- data.frame(ab)
ggplot(head(ab,14),aes(x=Var1,y=Freq,fill=Var2))+geom_bar(stat="identity") + labs(x="Job Functions",y="Frequencies") + scale_fill_discrete(name="Genders") #took first 14 row to avoid people who does not tell which gender they are

#Activities
cc <- table(data$jobfunction, data$useractivities.after.release)
cc<-data.frame(cc)
ccc <- cc[cc$Var2 == "TRUE",] 
ggplot(ccc, aes(x=Var1,y=Freq,fill=Var2))+geom_bar(stat="identity") + labs(x="Roles",y="Frequencies") + guides(fill=FALSE) + ggtitle("the ones who marked 'the Activities After Release' for user involvement activities") + theme(plot.title = element_text(lineheight=.6))

#table(data[, 3], data[, 9:14])#boyle yaparak jobfunction columniylan activities boolean columnlarini merge edeip bir seyler yapacaktim ama ayni length degil diyor
#e basit NA leri pairwise omit edeceksin 


#How easy 2.3 (userinf)
yak <- cbind(userinf.mgr, userinf.uxd, userinf.dev, userinf.tst, userinf.arc, userinf.ops, userinf.slf, jobfunction)
#df <- melt(yak, id.vars='jobfunction') #created a 'tall' column with jobfunction on one column and its values vs. other variables i.e. userinf
fff<-melt(yak, id.vars='jobfunction', measure.vars = c('userinf.mgr', 'userinf.uxd', 'userinf.dev', 'userinf.tst', 'userinf.arc', 'userinf.ops', 'userinf.slf')) 
fff <-data.frame(fff)
ggplot(fff, aes(x=jobfunction, y=value, fill=variable)) + geom_bar(stat='identity') # bu calisti ama okumasi cok zor bir graph oldu
ggplot(fff, aes(jobfunction,value, col=variable)) + geom_point() + stat_smooth() + scale_colour_discrete(name  ="how easy it is for", breaks=c("userinf.mgr"), labels=c("manager")) # bu da enteresan bir sey verdi

#ggplot(yak, aes(x=jobfunction,y=Freq,fill=Var2))+geom_bar(stat="identity") + labs(x="Job Functions",y="Frequencies") + scale_fill_discrete(name="Genders")
ggplot(data,aes(x=jobfunction,y=data$userinf.slf, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'How easy it is to get information from users - myself'")
ggplot(data,aes(x=jobfunction,y=data$userinf.dev, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'How easy it is to get information from users - developers'")
ggplot(data,aes(x=jobfunction,y=data$userinf.ops, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'How easy it is to get information from users - operators'")
ggplot(data,aes(x=jobfunction,y=data$userinf.uxd, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'How easy it is to get information from users - UX designers'")
#with(data[data$jobfunction == "Management",], #olmadi sadece managerlarin ne dusundugune bakacaktim. subset eyleyemedim
#ggplot(data,aes(x=jobfunction,y=data$userinf.uxd, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'How easy it is to get information from users - UX designers'")


#How often 2.4 (infofreq)
#bu olmaz. neden cunku rankinglerin onemi var
#aha <- table(data$jobfunction, data$infofreq.O3)
#aha <- data.frame(aha)
#ggplot(aha, aes(x=Var1, y=Freq, fill=Var2))+geom_bar(stat="identity", colour = "#000000", fill = "#FF9999") + labs(x="Roles" ,y="Frequencies") + guides(fill=FALSE)  + ggtitle("the ones who marked 'I interact with users in person after they used the sw") + theme(plot.title = element_text(lineheight=.6))
ggplot(data,aes(x=jobfunction,y=data$infofreq.O3, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'I interact with users in person after they used the software'")
ggplot(data,aes(x=jobfunction,y=data$infofreq.O1, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'I remotely observe users when they are using the sw e.g., screen sharing'")
ggplot(data,aes(x=jobfunction,y=data$infofreq.O4, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'through recorded usage data e.g., log data or video'")
ggplot(data,aes(x=jobfunction,y=data$infofreq.O2, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Their current job function",y="'People who marked: 'I am physically present with users when they are using the sw e.g., talk-aloud study'")


#Roles over "I need to ask permission"
ggplot(data,aes(x=jobfunction,y=data$userinv.S2, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Roles",y="'I need to ask permission to contact users'")

#Roles over "I have sufficient info"
ggplot(data,aes(x=jobfunction,y=data$userinv.S4, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Roles",y="'I have sufficient information about users'")
#ggplot(data,aes(x=jobfunction,y=data$userinv.S4, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip()+ labs(x="Roles",y="'I have sufficient information about users'")  + scale_x_discrete(breaks=c("1", "2", "3", "4", "5"), labels= ("Completely disagree", "2", "3", "4", "Completely agree"))

#yok burda boxplot daha mantikli oluyor ustteki yani
#cd <- table(data$jobfunction, data$userinv.S4)
#cd<-data.frame(cd)
#ccc <- cc[cc$Var2 == "TRUE",] 
#ggplot(cd, aes(x=Var2,y=Freq,fill=Var1))+geom_bar(stat="identity") + labs(x="",y="Roles") + guides(fill=FALSE) + ggtitle("the ones who marked 'the Activities After Release' for user involvement activities") + theme(plot.title = element_text(lineheight=.6))


#How often conduct experiments 3.1.
#iki turlu de olur gibi. dusun. ikinci graphtan i dont know cikarilabilir
ahan <- table(data$jobfunction, data$condexp)
ahan <- data.frame(ahan)
ggplot(ahan, aes(x=Var2, y=Freq, fill=Var1))+geom_bar(stat="identity") + labs(x="Roles" ,y="Frequencies")
ggplot(ahan, aes(x=Var1, y=Freq, fill=Var2))+geom_bar(stat="identity") + labs(x="Roles" ,y="Frequencies") #better


#PAIRS 3.2
#boyle yapacaksan oynaman lazim legendlarla falan ve diger pairlere uygula 
ggplot(data,aes(x=data$understanding.S1,y=data$understanding.S2,color=jobfunction))+geom_point() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~jobfunction, scales="free_y") +  labs(x = "People who ranked 'data should be always collected'", y = "People who ranked 'data should be only collected when there is a known need'") + scale_fill_continuous(low = "#000000", high = "#FF9999") 
ggplot(data,aes(x=data$understanding.S3,y=data$understanding.S4,color=jobfunction))+geom_point() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~jobfunction, scales="free_y") +  labs(x = "People who ranked 'all data about user behaviour is useful'", y = "People who ranked 'focused data on specifically chosen user action is useful'") + scale_fill_continuous(low = "#000000", high = "#FF9999") 
ggplot(data,aes(x=data$understanding.S5,y=data$understanding.S6,color=jobfunction))+geom_point() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~jobfunction, scales="free_y") +  labs(x = "People who ranked 'users themselves must be actively involved in the development'", y = "People who ranked 'we just need to measure user behaviour'") + scale_fill_continuous(low = "#000000", high = "#FF9999") 

ggplot(understanding,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction) +  labs(x = "", y = "") #Bu oldu


#melt lost here... down does not work
hop <- cbind(understanding.S1, understanding.S2, jobfunction) 
data.frame(hop)
d2<-melt(hop, id.vars='jobfunction', variable.name = c('understanding.S1', 'understanding.S2'), 
         value.name = "jobfunction", measure.vars = , na.rm = TRUE)
data.frame(d2)
ggplot(d2, aes(jobfunction,value, col=variable)) + geom_point() + stat_smooth() + scale_colour_discrete(name  ="how easy it is for", breaks=c("userinf.mgr"), labels=c("manager")) # bu da enteresan bir sey verdi

##ETHICS
#4.1 one below is ok but colors to be changed
cz <- table(data$jobfunction, data$usernotif.S1)
cz<-data.frame(cz)
ggplot(cz, aes(x=Var2,y= Freq, fill=Var1))+geom_bar(stat="identity") + labs(x="Roles",y="Frequencies") + scale_fill_discrete(name="Roles")
#the one below is goodie
ggplot(data,aes(x=jobfunction,y=data$usernotif.S1, fill=jobfunction))+geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Roles",y="'Users do not need to know they they are involved'")

#second one below works
#ggplot(undernotif,aes(x=Statement,y=Rating))+ geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction) +  labs(x = "", y = "") + scale_fill_continuous(low = "#000000", high = "#FF9999")
ggplot(undernotif,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction) +  labs(x = "", y = "") #Bu oldu


#4.2
ggplot(expinv,aes(x=Statement,y=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction) +  labs(x = "", y = "") #Bu oldu
#data$jobfunction[jobfunction!="Other"] denedik olmadi

##Correlations##
pp <- table(gender, jobfunction)
pp<- cor(pp, use="pairwise.complete.obs") # works
cor(age, data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..) #gives -0.16 no real correlation
corrplot(pp, method = "color") 

cor(age, worktime, use="pairwise.complete.obs") #0.22

M <-cor(table(age, worktime), use="pairwise.complete.obs")
corrplot(M, method = "color") #plot matrix #does not say much 

cor(data$X1.5.What.is.the.size.of.your.primary.work.team.., X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function.., use="pairwise.complete.obs") #0.53
cor(data$userinv.S2, userinf.slf, use="pairwise.complete.obs") # -0.5516735
cor(userinf.dev, userinf.tst, use="pairwise.complete.obs") #0.8996517 #means that users and testers seem to be similar
cor(infofreq.O3, userinv.S3, use="pairwise.complete.obs") #0.6878795
cor(usernotif.S5, usernotif.S4, use="pairwise.complete.obs") #-0.6001068
cor(userinv.S5, userinv.S6, use="pairwise.complete.obs") #0.7812353
or(understanding.S1, understanding.S3, use="pairwise.complete.obs") #0.7302549

#aggreegated function checking all corr among numerical variables
roles <- X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..
corr_subset <- data[, 58:100]
corr_subset$roles = roles
gender_n <- as.numeric(data$X1.4.Which.of.the.following.best.describes.you..) #converting gender factor to numeric and appending to the dataset
corr_subset$gender = gender_n
corr_subset <- corr_subset[ ,c(ncol(corr_subset),1:(ncol(corr_subset)-1))] #add roles column and bring it to the front
numeric_subset <- corr_subset[sapply(corr_subset, is.numeric)]
numeric_subset_corr<-cor(numeric_subset, use="pairwise.complete.obs")
corrplot(numeric_subset_corr, method= "color", order="hclust", addrect=8)

#scatterplots
ggplot(numeric_subset_corr, aes(x=numeric_subset_corr$userinf.tst, y=numeric_subset_corr$userinf.dev)) +
       geom_point(shape=1) +
       scale_colour_hue(l=50) + # Use a slightly darker palette than normal
       geom_smooth(method=lm,   # Add linear regression lines
                                     se=FALSE)    # Don't add shaded confidence region

pairs(numeric_subset_corr[1:4])
pairs(numeric_subset_corr[6:11]) #2.2. How much do you agree with the following statements? (User involvement statements)
