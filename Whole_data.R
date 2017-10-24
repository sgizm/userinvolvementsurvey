######################################################################
# Cross analysis script for User Involvement Survey - Whole

######################################################################

######################################################################
# Imports and constants
######################################################################
######################################################################
# Cross analysis script for User Involvement Survey - Whole

######################################################################

######################################################################
# Imports and constants
######################################################################
source(file='Import.R')

# First, make company-specific data frames that all have exactly the same columns
# which( colnames(data4)=="condexp" ) to check column indexes
cols1 <- c(2:5, 7, 42:55, 58, 59, 62:64, 90) # Ericsson
cols2 <- c(2:5, 6, 53:66, 81, 82, 84, 83, 85, 121) # F-secure
cols3 <- c(2:5, 7, 56:69, 72, 73, 77, 74, 78, 118) # Vaadin
cols4 <- c(2:5, 6, 50:63, 77, 78, 80, 79, 81, 115) # Reaktor
clus_colnames  <- c("SUBTIME", "JOBFUNC", "JOBFUNCOTHER", "JOBTIME", "GENDER", "INVA", "INVB", "INVC", "INVD", "INVE", "INVF", "INVG", "INVH", "INVI", "INVJ", "INVK", "INVL", "INVM", "INVN", "ROLE", "worktime", "age_range","gender", "team_size", "condexp" )
clus_data1 <- data1[, cols1]
names(clus_data1) <- clus_colnames
clus_data2 <- data2[, cols2]
names(clus_data2) <- clus_colnames
clus_data3 <- data3[, cols3]
names(clus_data3) <- clus_colnames
clus_data4 <- data4[, cols4]
names(clus_data4) <- clus_colnames

# Second, concatenate the company names' column
clus_data <- rbind(clus_data1, clus_data2, clus_data3, clus_data4)
clus_data[1:35,26] <- "Ericsson"
clus_data[36:43,26] <- "F-secure"
clus_data[44:64,26] <- "Vaadin"
clus_data[65:130,26] <- "Reaktor"
names(clus_data)[26] <- "COMPANY"


## DEMOGRAPHICS ##
# 1.1 job functions total and over companies 
ggplot(clus_data, aes(x=ROLE)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 14, 22, 23, 71), labels = c("0", "14", "22", "23", "71")) 

ggplot(clus_data, aes(x=ROLE)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY)

# 1.2 worktime total (remove the facet_wrap parameter) and worktime over companies 
ggplot(clus_data, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency") + theme(axis.text=element_text(size=13))+ facet_wrap(~clus_data$COMPANY) + scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,4,5,6,7,8,9), labels = c("0", "1", "4", "5", "6","7", "8", "9"))

# 1.3 age range over companies
ggplot(clus_data, aes(x=age_range)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Age range", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY)

# 1.4 gender over companies 
ggplot(clus_data, aes(x=gender)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Gender", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY)

# 1.5 team size
ggplot(clus_data, aes(x=team_size)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Team size", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY) + scale_y_continuous(breaks=c(0,3,6,9,12,20,25), labels=c("0", "3","6", "9","3y","5y", "10y"))

## END OF DEMOGRAPHICS ##

# 2.1
# one data frame for the merged statements
useractivities = cbind(useractivities1[1], useractivities1[-1] + useractivities2[-1] + useractivities3[-1] + useractivities4[-1])
summary(useractivities)
ggplot(useractivities, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="#FF9999", colour="#FF9999") +
  labs(x="Total Activities", y="Frequency") + theme(axis.text=element_text(size=13))

# 2.2 
# note to myself : come back here for facet_wrap function
userinv <- rbind(userinv1, userinv2, userinv3, userinv4)
ggplot(userinv, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() +
  labs(x="Total involvement statements", y="Frequency") + theme(axis.text=element_text(size=13))
data.frame(userinv)
count(userinv[ which(userinv$Statement=='The information I have about users is up to date'), ], "Rating")

# 2.3
userinf <- rbind(userinf1, userinf2, userinf3, userinf4)
ggplot(userinf, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() +
  labs(x="Who is closest to user info?", y="Frequency") + theme(axis.text=element_text(size=13))

# 2.4
infofreq <- rbind(infofreq1, infofreq2, infofreq3, infofreq4)
ggplot(infofreq, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() + labs(x="Methods used", y="Frequency") + theme(axis.text=element_text(size=13))

# 3.1 
ggplot(clus_data, aes(x=condexp)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Total_Conducting experiments", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~clus_data$ROLE)

# 3.3
understanding <- rbind(understanding1, understanding2, understanding3, understanding4)
ggplot(understanding1, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() +
  theme(axis.text=element_text(size=13)) 

# 4.1
undernotif <- rbind(undernotif1, undernotif2, undernotif3, undernotif4)
ggplot(undernotif, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + theme(axis.text =element_text(size=12))  +  ggtitle("Total") + scale_x_discrete(limits=c("Users do not need to know they are involved", "If we collect personal information, users need to be notified", "If no laws are being broken, users do not need to be notified", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "Users should always be notified when they are being involved in an experiment", "It is ok not to disclose all the experiment details to users involved", "It is ok to trick the user if the validty of experiment results depend on it" )) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

# Total versus roles:
ggplot(xx, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + ggtitle("Total") + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~xx$Jobf) +  labs(x = "", y = "") + scale_x_discrete(limits=c("It is ok to trick the user if the validty of experiment results depend on it", "It is ok not to disclose all the experiment details to users involved", "Users should always be notified when they are being involved in an experiment", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "If no laws are being broken, users do not need to be notified", "If we collect personal information, users need to be notified", "Users do not need to know they are involved"))
# , labeller = as_labeller(jb_names)

# 4.2
ggplot(data=total_expinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + theme(axis.text=element_text(size=11)) + ggtitle("Total") + scale_x_discrete(limits=c("I cannot trust that the experiment results will be correct", "Involving users in experiments is time-consuming", "My customer does not have the needed technical infrastructure", "Users would not like to be part of software experiments", "Users have to be convinced of the benefit before taking part", "Experiments give users false expectations", "Experiments reveal secrets about my customer's strategy"))  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
# Total versus roles:
ggplot(yy, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement), fill="white", colour="black") + guides(fill=FALSE) + coord_flip() + ggtitle("Total") + scale_size_continuous(range = c(0, 70)) + facet_wrap(~yy$Jobf) +  labs(x = "", y = "") + scale_x_discrete(limits=c("Experiments reveal secrets about my customer's strategy", "Experiments give users false expectations", "Users have to be convinced of the benefit before taking part", "Users would not like to be part of software experiments", "My customer does not have the needed technical infrastructure", "Involving users in experiments is time-consuming", "I cannot trust that the experiment results will be correct"))



