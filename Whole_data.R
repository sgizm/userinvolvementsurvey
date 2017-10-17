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
cols1 <- c(2:5, 7, 42:55, 58, 59, 62:64) # Ericsson
cols2 <- c(2:5, 6, 53:66, 81, 82, 84, 83, 85) # F-secure
cols3 <- c(2:5, 7, 56:69, 72, 73, 77, 74, 78) # Vaadin
cols4 <- c(2:5, 6, 50:63, 77, 78, 80, 79, 81) # Reaktor
clus_colnames  <- c("SUBTIME", "JOBFUNC", "JOBFUNCOTHER", "JOBTIME", "GENDER", "INVA", "INVB", "INVC", "INVD", "INVE", "INVF", "INVG", "INVH", "INVI", "INVJ", "INVK", "INVL", "INVM", "INVN", "ROLE", "worktime", "age_range","gender", "team_size" )
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
clus_data[1:35,25] <- "Ericsson"
clus_data[36:43,25] <- "F-secure"
clus_data[44:64,25] <- "Vaadin"
clus_data[65:130,25] <- "Reaktor"
names(clus_data)[25] <- "COMPANY"


## DEMOGRAPHICS ##
# 1.1 job functions total and over companies 
ggplot(clus_data, aes(x=clus_data$ROLE)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 14, 22, 23, 71), labels = c("0", "14", "22", "23", "71")) 

ggplot(clus_data, aes(x=clus_data$ROLE)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY)

# 1.2 worktime total (remove the facet_wrap parameter) and worktime over companies 
ggplot(clus_data, aes(x=clus_data$worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency") + theme(axis.text=element_text(size=10))+ facet_wrap(~clus_data$COMPANY) + scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,4,5,6,7,8,9), labels = c("0", "1", "4", "5", "6","7", "8", "9"))

# 1.3 age range over companies
ggplot(clus_data, aes(x=clus_data$age_range)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY)

# 1.4 gender over companies 
ggplot(clus_data, aes(x=clus_data$gender)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Gender", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY)

# 1.5 team size
ggplot(clus_data, aes(x=clus_data$team_size)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Team size", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~clus_data$COMPANY)

## END OF DEMOGRAPHICS ##

# 2.1

