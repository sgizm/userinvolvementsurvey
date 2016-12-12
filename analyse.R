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

POPULATION.SIZE = 231
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year

######################################################################
# Read raw data files
######################################################################

data <- read.csv("raportti.csv")

######################################################################
# Preprocess
######################################################################

# Demographics
data$jobfunction <- factor(data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function....,
                           levels = c(0:6),
                           labels = c("Developing software", "Testing software", "UX Design", "Management", "Operations", "Architecture", "Other"))
data$jobfunction.other <- data$If.other..please.specify

data$worktime <- data$X1.2.How.long.have.you.been.working.in.your.current.company.role.
data$birthyear <- data$X1.3.What.is.your.year.of.birth.
data$birthyear[31] <- data$birthyear[31] + 1900 # Fix data entry error
data$age <- CURRENTYEAR - data$birthyear
data$gender <- factor(data$X1.4.Which.of.the.following.best.describes.you....,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))

data$teamsize <- factor(data$X1.5.What.is.the.size.of.your.primary.work.team....,
                        levels = c(0:4,"NA"),
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
data$userinv.S5 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work.
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
data$condexp <- factor(data$X3.1.Does.your.company.conduct.experiments.involving.the.users....,
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
  geom_histogram() +
  labs(x="Job function", y="Frequency")

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
  geom_histogram(binwidth=1) +
  labs(x="Year of birth", y="Frequency")

# Age
print("Age")
summary(age)
ggplot(data, aes(x=age)) +
  geom_histogram(binwidth=1) +
  labs(x="Age", y="Frequency")

# Gender
print("Gender")
summary(gender)
ggplot(data, aes(x=gender)) +
  geom_histogram() +
  labs(x="Gender", y="Frequency")

# Team size
print("Team size")
summary(teamsize)
ggplot(data, aes(x=teamsize)) +
  geom_histogram() +
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
  geom_bar(stat="identity")

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
