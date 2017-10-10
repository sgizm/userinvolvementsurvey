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

source("Import.R")

### ALL DATA ###

# worktime
# concatenate worktimes 
total_worktime <- c(data1$worktime, data2$worktime, data3$worktime, data4$worktime)
hist(total_worktime)
#worktime over companies


## Merge data from all companies into a single data set
# First, make company-specific data frames that all have exactly the same columns
cols1 <- c(2:5, 7, 42:55, 58, 59) # Ericsson
cols2 <- c(2:5, 6, 53:66, 81, 82) # F-secure
cols3 <- c(2:5, 7, 56:69, 72, 73) # Vaadin
cols4 <- c(2:5, 6, 50:63, 77, 78) # Reaktor
clus_colnames  <- c("SUBTIME", "JOBFUNC", "JOBFUNCOTHER", "JOBTIME", "GENDER", "INVA", "INVB", "INVC", "INVD", "INVE", "INVF", "INVG", "INVH", "INVI", "INVJ", "INVK", "INVL", "INVM", "INVN", "ROLE", "worktime")
clus_data1 <- data1[, cols1]
names(clus_data1) <- clus_colnames
clus_data2 <- data2[, cols2]
names(clus_data2) <- clus_colnames
clus_data3 <- data3[, cols3]
names(clus_data3) <- clus_colnames
clus_data4 <- data4[, cols4]
names(clus_data4) <- clus_colnames

# Second, concatenate the company-specific data frames into a single data frame
clus_data <- rbind(clus_data1, clus_data2, clus_data3, clus_data4)

ggplot(clus_data, aes(x=clus_data$worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency") + theme(axis.text=element_text(size=10))+ scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,4,5,6,7,8,9), labels = c("0", "1", "4", "5", "6","7", "8", "9"))




total_undernotif <- rbind(undernotif1, undernotif2, undernotif3, undernotif4)
total_expinv <- rbind(expinv1, expinv2, expinv3, expinv4)

xx <- rbind(undernotif1, undernotif2, undernotif3, undernotif4) 
yy  <- rbind(expinv1, expinv2, expinv3, expinv4)

#4.1
ggplot(data=total_undernotif, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + theme(axis.text =element_text(size=12))  +  ggtitle("Total") + scale_x_discrete(limits=c("Users do not need to know they are involved", "If we collect personal information, users need to be notified", "If no laws are being broken, users do not need to be notified", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "Users should always be notified when they are being involved in an experiment", "It is ok not to disclose all the experiment details to users involved", "It is ok to trick the user if the validty of experiment results depend on it" )) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Total versus roles:
ggplot(xx, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + ggtitle("Total") + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~xx$Jobf) +  labs(x = "", y = "") + scale_x_discrete(limits=c("It is ok to trick the user if the validty of experiment results depend on it", "It is ok not to disclose all the experiment details to users involved", "Users should always be notified when they are being involved in an experiment", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "If no laws are being broken, users do not need to be notified", "If we collect personal information, users need to be notified", "Users do not need to know they are involved"))
# , labeller = as_labeller(jb_names)
