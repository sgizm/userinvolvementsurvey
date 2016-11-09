######################################################################
# Imports and constants
######################################################################

library(foreign)
library(ggplot2)

CURRENTYEAR <- 2016

######################################################################
# Read raw data files
######################################################################

data <- read.csv("raportti.csv")

######################################################################
# Preprocess
######################################################################

# Demographics
jobfunction <- factor(data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function....,
                      levels = c(0:6),
                      labels = c("Developing software", "Testing software", "UX Design", "Management", "Operations", "Architecture", "Other"))
jobfunction.other <- data$If.other..please.specify

worktime <- data$X1.2.How.long.have.you.been.working.in.your.current.company.role.
birthyear <- data$X1.3.What.is.your.year.of.birth.
age <- CURRENTYEAR - birthyear
gender <- data$X1.4.Which.of.the.following.best.describes.you....

teamsize <- factor(data$X1.5.What.is.the.size.of.your.primary.work.team....,
                   levels = c(0:4,"NA"),
                   labels = c("< 3", "3-5", "6-10", "11-20", ">20", "No team"))

# 2.1 In which development activities are users involved in your company? (click all that apply)
useractivities.specifying.requirements <- data$Specifying.requirements
useractivities.designing.software <- data$Designing.software
useractivities.implementing.software <- data$Implementing.software
useractivities.testing <- data$Testing
useractivities.after.release <- data$The.activities.after.release
useractivities.other <- data$Other
useractivities.other.open <- data$If.other..please.specify..separate.with.commas.

# 2.2. How much do you agree with the following statements? (User involvement statements)
userinv.S1 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
userinv.S2 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
userinv.S3 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
userinv.S4 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
userinv.S5 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work.
userinv.S6 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
userinv.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users’ needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date"
)

# 2.3 In your experience, how easy is it for the following to get information from users?
userinf.mgr <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Managers
userinf.uxd <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...UX.designers
userinf.dev <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.developers.
userinf.tst <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.testers
userinf.arc <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.architects.
userinf.ops <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...System.or.network.operators
userinf.slf <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Myself
userinf.options <- c("Managers", "UX designers", "Software developers", "Software testers", "Software architects", "System or network operators", "Myself")

# 2.4 How often do you use the following ways to get information about users?
infofreq.O1 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.remotely.observe.users.when.they.are.using.the.software..e.g...screen.sharing.
infofreq.O2 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.am.physically.present.with.users.when.they.are.using.the.software..e.g...talk.aloud.study.
infofreq.O3 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.users.in.person.after.they.used.the.software..e.g...post.use.interview.
infofreq.O4 <- data$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.recorded.usage.data..e.g...log.data.or.video.
infofreq.statements <- c(
  "I remotely observe users when they are using the software (e.g., screen sharing)",
  "I am physically present with users when they are using the software (e.g., talk-aloud study)",
  "I interact with users in person after they used the software (e.g., post-use interview)",
  "Through recorded usage data (e.g., log data or video)")
infofreq.options <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2.5 Try to remember a situation where you knew that involving users in development would be useful, but you could not involve them. Please describe the situation and what challenges you faced.
userinf.open <- data$X2.5.Try.to.remember.a.situation.where.you.knew.that.involving.users.in.development.would.be.useful..but.you.could.not.involve.them..Please.describe.the.situation.and.what.challenges.you.faced.

# 3.1 Does your company conduct experiments involving the users?
condexp <- factor(data$X3.1.Does.your.company.conduct.experiments.involving.the.users....,
                  levels = c(1:4, "NA"),
                  labels = c("Never", "Rarely", "Occasionally", "Yes, actively", "I don't know"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
exp.open <- data$X3.2.Please.describe.a.typical.experiment.you.have.seen.or.been.involved.in.in.your.company..including.the.roles...Skip.this.if.you.have.not.seen.or.been.involved.in.any.experiments..

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
understanding.S1 <- data$For.understanding.user.needs.better.........data.should.always.be.collected.because.it.might.be.needed.later
understanding.S2 <- data$For.understanding.user.needs.better.........data.should.only.be.collected.when.there.is.a.known.need.or.assumption.to.test
understanding.S3 <- data$For.understanding.user.needs.better.........all.data.about.user.behaviour.is.useful
understanding.S4 <- data$For.understanding.user.needs.better.........focused.data.on.a.specifically.chosen.user.action.is.useful
understanding.S5 <- data$For.understanding.user.needs.better.........users.themselves.must.be.actively.involved.in.development
understanding.S6 <- data$For.understanding.user.needs.better.........we.just.need.to.measure.user.behaviour
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
usernotif.S1 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
usernotif.S2 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
usernotif.S3 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
usernotif.S4 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
usernotif.S5 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
usernotif.S6 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
usernotif.S7 <- data$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.intentionally.deceive.or.mislead.the.user.if.experiment.results.depend.on.it
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
expinv.S1 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.experiment.results.will.be.correct
expinv.S2 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
expinv.S3 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure.to.run.experiments
expinv.S4 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.experiments
expinv.S5 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part.in.an.experiment
expinv.S6 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
expinv.S7 <- data$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "Our company does not have the needed technical infrastructure to run experiments",
  "Users would not like to be part of experiments",
  "Users have to be convinced of the benefit before taking part in an experiment",
  "Experiments give users false expectations",
  "Experiments reveal secrets about the product strategy")
expinv.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

######################################################################
# Descriptive statistics
######################################################################

## Demographics

# Job function



# Work time
# Birth year
# Age
# Gender
# Team size

