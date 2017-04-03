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


POPULATION.SIZE = 135
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year

######################################################################
# Read raw data files
######################################################################

data <- read.csv("raportti_vaadin.csv")
# I was going to rbind pilot survey (filled by Pekka) with the final survey csv, but realized that number of columns differ, because we altered the survey after the pilot. Well, I don't rbind them then I think.
#data_pilot <- read.csv("vaadin_pilot.csv") 
data <- data.frame(data)
#data_pilot <- data.frame(data_pilot)
#data <- rbind(data1, data_pilot)
######################################################################
# Preprocess
######################################################################

###### Demographics #######
data$jobfunction <- factor(data$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                           levels = c(0:10),
                           labels = c("Developing Vaadin framework", "Developing Vaadin Pro Tools", "Product management", "Management, other", "UX Design", "Software architecture", "Advocating products and services" ,"Providing consulting", "Providing customer support", "Providing training services", "Other"))
data$jobfunction.other <- data$If.other..please.specify

# Worktime
data$worktime <- data$X1.2.How.long.have.you.been.working.in.your.current.company.role.
data$worktime[7] <- NA #fixing
#[15:58, 1/26/2017] Fabian Fagerholm: and the scale will draw better                        
#[15:59, 1/26/2017] Fabian Fagerholm: but we have to remember that when reporting

data$gender <- factor(data$X1.4.Which.of.the.following.best.describes.you..,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))

# Age
data$birthyear <- data$X1.3.What.is.your.year.of.birth.
data$birthyear[5] <- data$birthyear[5] + 1900 # Fix data entry error
data$age <- CURRENTYEAR - data$birthyear

#Teams ize
data$teamsize <- factor(data$X1.5.How.many.people.do.you.work.with.on.a.regular.basis.in.the.company..,
                        levels = c(0:4),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20"))
# Work location
data$location <- factor(data$X1.6.Where.is.your.primary.work.location..,
                       levels = c(0:2),
                       labels = c("Finland", "Germany", "USA"))

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

data$useractivities.fixes <- data$Providing.fixes
data$customer.support <- data$Providing.customer.support
data$useractivities.consulting <- data$Providing.consulting
data$useractivities.billing <- data$Billing.services
data$useractivities.other <- data$Other
data$useractivities.other.open <- data$If.other..please.specify..separate.with.commas.
useractivities.options <- c("Forming ideas", "Gathering requirements", "Software design", "Implementing software", "Testing", "The activities after release", "Customer support", "Consulting", "Billing services", "Other")

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
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13))

# Gender
print("Gender")
summary(gender)
ggplot(data, aes(x=gender)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Gender", y="Frequency")

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

# Work time
print("How long have you been working in your current role")
summary(worktime)
ggplot(data, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency") + scale_x_continuous(breaks=c(4,7,12,24,36,60,120), labels=c("<4m", "7m","1y", "2y","3y","5y", "10y")) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6), labels = c("0", "1", "2", "3", "4", "5", "6"))

# Team size
print("Team size")
summary(teamsize)
ggplot(data, aes(x=teamsize)) +
  geom_bar(fill="lightgoldenrod2", colour="white") +
  labs(x="Team size", y="Frequency") + theme(axis.text=element_text(size=13))

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
