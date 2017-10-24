######################################################################
# Analysis script for User Involvement Survey - F-secure

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


POPULATION.SIZE = 25
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year

######################################################################
# Read raw data files
######################################################################

data <- read.csv("raportti_fsecure.csv")
data <- data.frame(data)
######################################################################
# Preprocess
######################################################################

# Demographics
data$jobfunction <- factor(data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:9),
                           labels = c("Developing software", "Product management or ownership", "Management, other", "Business development", "UX Design", "Architecture", "Consulting", "Customer support", "Training services", "Other"))
data$jobfunction.other <- data$If.other..please.specify

data$worktime <- data$X1.2.How.long.have.you.been.working.in.your.current.company.role.
#[15:58, 1/26/2017] Fabian Fagerholm: and the scale will draw better                        
#[15:59, 1/26/2017] Fabian Fagerholm: but we have to remember that when reporting

data$gender <- factor(data$X1.3.Which.of.the.following.best.describes.you..,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))


data$age_range <- factor(data$X1.4.What.is.your.age.range..,
                        levels = c(0:4),
                        labels = c("20 or less", "21-30", "31-40", "41-50", "50 or more"))

data$teamsize <- factor(data$X1.5.How.many.people.do.you.work.with.on.a.regular.basis.in.the.company..,
                        levels = c(0:4),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20"))

data$end_user <- factor(data$X1.6.Who.do.you.consider.to.be.the.primary.user.in.your.job.function..,
                        levels = c(0:1),
                        labels = c("Business user within B2B domain", "End user using F-secure product and services"))

# 2.1 In which development activities are users involved in your company? (click all that apply)
data$useractivities.forming.ideas <- data$Forming.product.or.service.ideas
data$useractivities.gathering.requirements <- data$Gathering.requirements
data$useractivities.software.design <- data$Software.design
data$useractivities.implementation <- data$Implementation
data$useractivities.testing <- data$Testing
data$useractivities.after.release <- data$The.activities.after.release
data$useractivities.customer.support <- data$Providing.customer.support
data$useractivities.consulting <- data$Providing.consulting
data$useractivities.billing <- data$Billing.services
data$useractivities.other <- data$Other
data$useractivities.other.open <- data$If.other..please.specify..separate.with.commas.
useractivities.options <- c("Forming ideas", "Gathering requirements", "Software design", "Implementing software", "Testing", "The activities after release", "Customer support", "Consulting", "Billing services", "Other")

# 2.2. How much do you agree with the following statements? (User involvement statements)
data$userinv.S1 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data$userinv.S2 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data$userinv.S3 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data$userinv.S4 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
data$userinv.S5 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work
data$userinv.S6 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
data$userinv.S7 <- data$X2.2..How.much.do.you.agree.with.the.following.statements...I.would.like.to.get.more.feedback.from.users

userinv.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users??? needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date",
  "I would like to get more feedback from users"
)
userinv.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 2.3 In your experience, how easy is it for the following to get information from users?
data$userinf.dev <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Developers
data$userinf.mgr <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Product.managers.or.owners
data$userinf.mgr.other <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Managers..other
data$userinf.bus.dev <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Business.developers
data$userinf.ux.designer <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...UX.designers
data$userinf.arc <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Software.architects.
data$userinf.con <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Consultants
data$userinf.cust.sup <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Customer.support
data$userinf.tra <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Trainers
data$userinf.slf <- data$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Myself
userinf.statements <- c("Developers", "Product managers or owners", "Managers, other", "Business developers", "UX designers", "Software architects", "Consultants", "Customer support", "Trainers", "Myself")
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
data$condexp <- factor(data$X3.1.Does.your.company.conduct.experiments.involving.the.users..,
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

# 5.1 How much do you agree with the following statements regarding benefits of experimentation? Please answer according to your personal beliefs.
data$expben.S1 <- data$X5.1.How.much.do.you.agree.with.the.following.statements.regarding.benefits.of.experimentation..Please.answer.according.to.your.personal.beliefs...Experiments.would.give.me.earlier.feedback.on.how.users.respond.to.features
data$expben.S2 <- data$X5.1.How.much.do.you.agree.with.the.following.statements.regarding.benefits.of.experimentation..Please.answer.according.to.your.personal.beliefs...Experiments.would.give.me.an.understanding.of.user.value.before.releases
data$expben.S3 <- data$X5.1.How.much.do.you.agree.with.the.following.statements.regarding.benefits.of.experimentation..Please.answer.according.to.your.personal.beliefs...Experiments.would.mean.that.decisions.are.based.on.empirical.evidence.rather.than.opinions
data$expben.S4 <- data$X5.1.How.much.do.you.agree.with.the.following.statements.regarding.benefits.of.experimentation..Please.answer.according.to.your.personal.beliefs...Experiments.would.allow.me.to.identify.changes.in.user.needs.or.market.demand
expben.statements <- c(
  "Experiments would give me earlier feedback on how users respond to features",
  "Experiments would give me an understanding of user value before releases",
  "Experiments would mean that decisions are based on empirical evidence rather than opinions",
  "Experiments would allow me to identify changes in user needs or market demand")
expben.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 5.2 In order to know the user value, I would be prepared to ...
data$expval.S1 <- data$X5.2.In.order.to.know.the.user.value..I.would.be.prepared.to......meet.users.in.person
data$expval.S2 <- data$X5.2.In.order.to.know.the.user.value..I.would.be.prepared.to......create.a.user.jury
data$expval.S3 <- data$X5.2.In.order.to.know.the.user.value..I.would.be.prepared.to......join.a.user.jury
data$expval.S4 <- data$X5.2.In.order.to.know.the.user.value..I.would.be.prepared.to......spend.work.time.to.design.and.run.experiments
data$expval.S5 <- data$X5.2.In.order.to.know.the.user.value..I.would.be.prepared.to......invest.into.learning.how.to.conduct.systematic..hypothesis.based.experiments
data$expval.S6 <- data$X5.2.In.order.to.know.the.user.value..I.would.be.prepared.to......disseminate.the.experiment.results.and.learnings.in.my.organisation
data$expval.S7 <- data$X5.2.In.order.to.know.the.user.value..I.would.be.prepared.to......Other

expval.statements <- c(
  "meet users in person",
  "create a user jury",
  "join a user jury",
  "spend work time to design and run experiments",
  "invest into learning how to conduct systematic, hypothesis-based experiments",
  "dissaminate the experiment results and learnings in my organisation",
  "other")
expval.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

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
  labs(x="Job functions", y="Frequency")
#ggplot(data,aes(x = factor(""), fill=jobfunction))+geom_bar()+ coord_polar(theta = "y")  +scale_x_discrete("") #alternative pie chart

# Work time
print("How long have you been working in your current role")
summary(worktime)
ggplot(data, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency") + scale_x_continuous(breaks=c(3,12,24,36,84), labels=c("<3m", "1y","2y", "3y","7y")) + scale_y_continuous(breaks=c(0,1,2), labels = c("0", "1", "2"))

#ggplot(data, aes(x=worktime)) + 
#  geom_histogram(aes(y=..density..),# Histogram with density instead of count on y-axis
#                 binwidth=10,
#                 colour="black", fill="white") +
#  geom_density(alpha=.2, fill="#FF9999", colour="#FF9999")  # Overlay with transparent density plot

# Gender
print("Gender")
summary(gender)
ggplot(data, aes(x=gender)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Gender", y="Frequency")

# Age range
print("Age range")
summary(age_range)
ggplot(data, aes(x=age_range)) +
  geom_bar(fill="lightgoldenrod2", colour="white") +
  labs(x="Age range", y="Frequency")

# Team size
print("Team size")
summary(teamsize)
ggplot(data, aes(x=teamsize)) +
  geom_bar(fill="lightgoldenrod2", colour="white") +
  labs(x="Team size", y="Frequency")

# End user
print("End user")
summary(end_user)
ggplot(data, aes(x=end_user)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="End user", y="Frequency")

######################################################################
# Analysis
######################################################################

## Section 2

# 2.1 In which development activities are users involved in your company? (click all that apply)
useractivities.forming.ideas <- sum(data$useractivities.forming.ideas, na.rm=TRUE)
useractivities.gathering.requirements <- sum(data$useractivities.gathering.requirements, na.rm=TRUE)
useractivities.software.design <- sum(data$useractivities.software.design, na.rm=TRUE)
useractivities.implementation <- sum(data$useractivities.implementation, na.rm=TRUE)
useractivities.testing <- sum(data$useractivities.testing, na.rm=TRUE)
useractivities.after.release <- sum(data$useractivities.after.release, na.rm=TRUE)
useractivities.customer.support <- sum(data$useractivities.customer.support, na.rm=TRUE)
useractivities.consulting<- sum(data$useractivities.consulting, na.rm=TRUE)
useractivities.billing <- sum(data$useractivities.billing, na.rm=TRUE)
useractivities.other <- sum(data$useractivities.other, na.rm=TRUE)
useractivities <- data.frame(Activity=useractivities.options,
                             Frequency=c(
                               useractivities.forming.ideas,
                               useractivities.gathering.requirements,
                               useractivities.software.design,
                               useractivities.implementation,
                               useractivities.testing,
                               useractivities.after.release,
                               useractivities.customer.support,
                               useractivities.consulting,
                               useractivities.billing,
                               useractivities.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities)
ggplot(data=useractivities, aes(x=Activity, y=Frequency)) + labs(x="Development activities where users are involved") +
  geom_bar(stat="identity", fill="lightpink2", colour="black") + theme(axis.text=element_text(size=14))+ scale_x_discrete(limits=c("Forming ideas","Gathering requirements","Software design", "Implementing software", "Testing", "The activities after release", "Customer support", "Consulting", "Billing services", "Other"))
  
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
userinf <- data.frame(Statement=factor(rep(userinf.statements, each=length(userinf.mgr))),
                      Rating=c(
                        userinf.dev,
                        userinf.mgr,
                        userinf.mgr.other,
                        userinf.bus.dev,
                        userinf.ux.designer,
                        userinf.arc,
                        userinf.con,
                        userinf.cust.sup,
                        userinf.tra,
                        userinf.slf))
ggplot(data=userinf, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("Myself", "Trainers", "Customer support", "Consultants", "Architects", "UX designers", "Business developers", "Managers, other", "Product managers or owners", "Developers")) + theme(axis.text=element_text(size=16))

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
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=16))

# 3.1 Does your company conduct experiments involving the users?
ggplot(data, aes(x=condexp)) +
  geom_bar(fill="burlywood1", colour="white") +
  labs(x="Conducting experiments", y="Frequency") + theme(axis.text=element_text(size=16))

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

# 5.1 How much do you agree with the following statements regarding benefits of experimentation? Please answer according to your personal beliefs.
expben <- data.frame(Statement=factor(rep(expben.statements, each=length(expben.S1))),
                     Rating=c(
                       expben.S1,
                       expben.S2,
                       expben.S3,
                       expben.S4))
ggplot(data=expben, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))

# 5.2 In order to know the user value, I would be prepared to ...
expval <- data.frame(Statement=factor(rep(expval.statements, each=length(expval.S1))),
                     Rating=c(
                       expval.S1,
                       expval.S2,
                       expval.S3,
                       expval.S4,
                       expval.S5,
                       expval.S6,
                       expval.S7))
ggplot(data=expval, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("other", "dissaminate the experiment results and learnings in my organisation", "invest into learning how to conduct systematic, hypothesis-based experiments", "spend work time to design and run experiments", "join a user jury","create a user jury", "meet users in person")) + theme(axis.text=element_text(size=11))

# 5.3 If other please specify
expval_other <- data$If.other..please.specify.2



## CROSS-ANALYSIS ##

jb_names <- c(
  'Developing software' = "Developers",
  'Product management or ownership' = "Product managers or owners",
  'Management, other' = "Managers, other",
  'Business development' = "Business developers",
  'UX design' = "UX designers",
  'Architecture' = "Architects",
  'Consulting' = "Consultants",
  'Customer support' = "Customer support",
  "Training services" = "Trainers",
  "Other" = "Other"
)

#User access statements over roles
ggplot(userinv,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction, labeller = as_labeller(jb_names)) +  labs(x = "", y = "")

#How easy 2.3 (userinf)
#below is the facet warp
ggplot(userinf,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction, labeller = as_labeller(jb_names)) +  labs(x = "", y = "")

# + scale_x_discrete(limits=c("Myself", "Trainers", "Customer support", "Consultants", "Software architects", "UX designers", "Business developers", "Managers, other", "Product managers or owners", "Developers"))

#interaction methods, 2.4 over roles
ggplot(infofreq,aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~data$jobfunction) +  labs(x = "", y = "") #Bu oldu