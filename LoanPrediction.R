# Loan Prediction
# Author : Vana Panagiotou
# Date : 04/12/2017

# Set your working directory
setwd("~/online teaching/Projects from Internet/Loan Prediction in R")

# Load training and test data
# Some of the missing values in the csv are represented by empty spaces "" or " ".
# Replace them with NA values with the argument na.strings = c(""," ",NA)

na.strings=c("",".","NA")
train_set <- read.csv("train_u6lujuX_CVtuZ9i.csv", na.strings = c(""," ",NA))
test_set <- read.csv("test_Y3wMUE5_7gLdaTN.csv", na.strings = c(""," ",NA))


# Display variables and understand their data types
str(train_set)
str(test_set)

# The training set has 614 observations and 13 variables and the test set has 
# 367 observations and 12 variables, which means that the traning set has 1 extra variable. 
# Check which variable is missing from the test set. 

colnames_check <- colnames(train_set) %in% colnames(test_set) 
# colnames_check has a False value for the missing variable
colnames(train_set[colnames_check==FALSE])

# As we can see we are missing the "Loan_Status" variable in the test set, 
# which is something that was expected, since we must predict this by creating a model.

test_set$Loan_Status <- as.factor('NA')


#### EXPLORATORY ANALYSIS


# Examine the number and percentage of customers whose loan was approved
table(train_set$Loan_Status)
#   N   Y 
# 192 422 
prop.table(table(train_set$Loan_Status))
#         N         Y 
# 0.3127036 0.6872964 

# Summarize the number and percentage of customers whose loan was approved
cbind(Amount = table(train_set$Loan_Status), Percentage = prop.table(table(train_set$Loan_Status)))
#   Amount Percentage
# N    192  0.3127036
# Y    422  0.6872964




# Check for missing values (empty or NA) in the training set
library(reshape)
train.missing <- melt(apply(train_set, 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(train.missing)[train.missing$value>0], train.missing[train.missing$value>0,])

#      [,1]               [,2]
# [1,] "Gender"           "13"
# [2,] "Married"          "3" 
# [3,] "Dependents"       "15"
# [4,] "Self_Employed"    "32"
# [5,] "LoanAmount"       "22"
# [6,] "Loan_Amount_Term" "14"
# [7,] "Credit_History"   "50"



# Check for missing values (empty or NA) in the test set
test.missing <- melt(apply(test_set[, -13], 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(test.missing)[test.missing$value>0], test.missing[test.missing$value>0,])

#      [,1]               [,2]
# [1,] "Gender"           "11"
# [2,] "Dependents"       "10"
# [3,] "Self_Employed"    "23"
# [4,] "LoanAmount"       "5" 
# [5,] "Loan_Amount_Term" "6" 
# [6,] "Credit_History"   "29"

# We see that we have missing values in Gender, Married, Dependents, Self_Employed, LoanAmount, 
# Loan_Amount_Term and Credit_History in the training set and 
# Gender, Dependents, Self_Employed, LoanAmount, Loan_Amount_Term and Credit_History in 
# the test set.
    
# To tackle this problem, we are going to predict the missing values with the full data set, 
# which means that we need to combine the training and test sets together.

# Combine training and test sets 
full <- rbind(train_set, test_set)

# Check for missing values (empty or NA) in the full set (training + test)
full.missing <- melt(apply(full[, -13], 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(full.missing)[full.missing$value>0], full.missing[full.missing$value>0,])

#      [,1]               [,2]
# [1,] "Gender"           "24"
# [2,] "Married"          "3" 
# [3,] "Dependents"       "25"
# [4,] "Self_Employed"    "55"
# [5,] "LoanAmount"       "27"
# [6,] "Loan_Amount_Term" "20"
# [7,] "Credit_History"   "79"

# Compute a summary of the full set
summary(full)

# Loan_ID       Gender    Married    Dependents        Education   Self_Employed ApplicantIncome
# LP001002:  1   Female:182   No  :347   0   :545   Graduate    :763   No  :807      Min.   :    0  
# LP001003:  1   Male  :775   Yes :631   1   :160   Not Graduate:218   Yes :119      1st Qu.: 2875  
# LP001005:  1   NA's  : 24   NA's:  3   2   :160                      NA's: 55      Median : 3800  
# LP001006:  1                           3+  : 91                                    Mean   : 5180  
# LP001008:  1                           NA's: 25                                    3rd Qu.: 5516  
# LP001011:  1                                                                       Max.   :81000  
# (Other) :975                                                                                      
# CoapplicantIncome   LoanAmount    Loan_Amount_Term Credit_History     Property_Area Loan_Status
# Min.   :    0     Min.   :  9.0   Min.   :  6.0    Min.   :0.0000   Rural    :290   N :192     
# 1st Qu.:    0     1st Qu.:100.0   1st Qu.:360.0    1st Qu.:1.0000   Semiurban:349   Y :422     
# Median : 1110     Median :126.0   Median :360.0    Median :1.0000   Urban    :342   NA:367     
# Mean   : 1602     Mean   :142.5   Mean   :342.2    Mean   :0.8359                              
# 3rd Qu.: 2365     3rd Qu.:162.0   3rd Qu.:360.0    3rd Qu.:1.0000                              
# Max.   :41667     Max.   :700.0   Max.   :480.0    Max.   :1.0000                              
# NA's   :27      NA's   :20       NA's   :79      


# Before replacing the missing values with sensible values, we will examine each variable
# and create some visualizations in order to better understand the data and their 
# relationships.





####     Univariate visualization


#  Variable "Gender"

# Gender is a factor variable with two levels and has NA's in both the training and test sets.

par(mfrow=c(1,2))
barplot(table(train_set$Gender),main="Gender in Training Set")
barplot(table(test_set$Gender),main="Gender in Test Set")

# Calcualte the percentage of female and male applicants in the training set
prop.table(table(train_set$Gender))
#    Female      Male 
# 0.1863561 0.8136439 

# Calcualte the percentage of female and male applicants in the test set
prop.table(table(test_set$Gender))
#    Female      Male 
# 0.1966292 0.8033708 

# We see that most applicants are males in both the training and test sets.



#  Variable "Married"

# Married is a factor variable with two levels and has NA's only in test set.

par(mfrow=c(1,2))
barplot(table(train_set$Married),main="Married in Training Set")
barplot(table(test_set$Married),main="Married in Test Set")


# Calcualte the percentage of married and unmarried applicants in the training set
prop.table(table(train_set$Married))

#        No       Yes 
# 0.3486088 0.6513912 

# Calcualte the percentage of married and unmarried applicants in the test set
prop.table(table(test_set$Married))

#        No       Yes 
# 0.3651226 0.6348774 


# We see that most applicants are married in both the training and test sets.




#  Variable "Dependents"


# Dependents is a factor variable with four levels and has NA's in both the training and test 
# sets.

par(mfrow=c(1,2))
barplot(table(train_set$Dependents),main="Dependents in Training Set")
barplot(table(test_set$Dependents),main="Dependents in Test Set")

# Calcualte the percentage of dependents associated with the applicants in the training set
prop.table(table(train_set$Dependents))

#         0         1         2        3+ 
# 0.5759599 0.1702838 0.1686144 0.0851419 

# Calcualte the percentage of dependents associated with the applicants in the test set
prop.table(table(test_set$Dependents))

#         0         1         2        3+ 
# 0.5602241 0.1624650 0.1652661 0.1120448 


# We see that most applicants do not have dependents in both the training and test sets.



#  Variable "Education"

# Education is a factor variable with two levels and does not have missing values.

par(mfrow=c(1,2))
barplot(table(train_set$Education),main="Education in Training Set")
barplot(table(test_set$Education),main="Education in Test Set")

# Calcualte the percentage of graduates in the training set
prop.table(table(train_set$Education))

# Graduate Not Graduate 
# 0.781759     0.218241 

# Calcualte the percentage of graduates in the test set
prop.table(table(test_set$Education))

# Graduate Not Graduate 
# 0.7711172    0.2288828


# We see that most applicants are graduates in both the training and test sets.



#  Variable "Self_Employed"

# Self_Employed is a factor variable with two levels and has NA's in both the training and test
# sets.


par(mfrow=c(1,2))
barplot(table(train_set$Self_Employed),main="Self_Employed in Training Set")
barplot(table(test_set$Self_Employed),main="Self_Employed in Test Set")

# Calcualte the percentage of self employed applicants in the training set
prop.table(table(train_set$Self_Employed))

#        No       Yes 
# 0.8591065 0.1408935 

# Calcualte the percentage of self employed applicants in the test set
prop.table(table(test_set$Self_Employed))

#        No       Yes 
# 0.8924419 0.1075581 


# We see that the vast majority of applicants are not self employed in both the training and 
# test sets.



#  Variables "ApplicantIncome" and "CoapplicantIncome"

# They are both numeric variables without missing values.

par(mfrow=c(1,2))
boxplot(train_set$ApplicantIncome,train_set$CoapplicantIncome,names=c("App Income","Coapp Income"),
        main="Incomes in Training Set")
boxplot(test_set$ApplicantIncome,test_set$CoapplicantIncome,names=c("App Income","Coapp Income"),
        main="Incomes in Test Set")


# Compute a summary of the ApplicantIncome in the training set

summary(train_set$ApplicantIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 150    2878    3812    5403    5795   81000 



# Compute a summary of the ApplicantIncome in the test set

summary(test_set$ApplicantIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    2864    3786    4806    5060   72529 


# Compute a summary of the CoapplicantIncome in the training set

summary(train_set$CoapplicantIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0       0    1188    1621    2297   41667


# Compute a summary of the CoapplicantIncome in the test set

summary(test_set$CoapplicantIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0       0    1025    1570    2430   24000 


# We see that there are many outliers and the distributions of both ApplicantIncome and 
# CoapplicantIncome are right-asymmetric.



#  Variable "LoanAmount"

# LoanAmount is a numeric variable and has NA's in both the training and test sets.

par(mfrow=c(1,2))
boxplot(train_set$LoanAmount,main="Loan Amount in Training Set")
boxplot(test_set$LoanAmount,main="Loan Amount in Test Set")


# Compute a summary of the LoanAmount in the training set

summary(train_set$LoanAmount)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  9.0   100.0   128.0   146.4   168.0   700.0      22 


# Compute a summary of the LoanAmount in the test set

summary(test_set$LoanAmount)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 28.0   100.2   125.0   136.1   158.0   550.0       5 


# We see that there are many outliers and the distributions are right-asymmetric.



#  Variable "Loan_Amount_Term"


# Loan_Amount_Term is a numeric variable and has NA's in both the training and test sets.

par(mfrow=c(1,2))
hist(train_set$Loan_Amount_Term,breaks=400,main="Loan_Amount_Term in Training Set")
hist(test_set$Loan_Amount_Term,breaks=400,main="Loan_Amount_Term in Test Set")


# We see that the majority of loan amount terms are 360 months in both the training and 
# test sets.


# Compute a summary of the Loan_Amount_Term in the training set

summary(train_set$Loan_Amount_Term)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  12     360     360     342     360     480      14 


# Compute a summary of the Loan_Amount_Term in the test set

summary(test_set$Loan_Amount_Term)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  6.0   360.0   360.0   342.5   360.0   480.0       6 




#  Variable "Credit_History"

# Credit_History is an integer variable and has NA's in both the training and test sets.

par(mfrow=c(1,2))
barplot(table(train_set$Credit_History),main="Credit_History in Training Set")
barplot(table(test_set$Credit_History),main="Credit_History in Test Set")


# Calcualte the percentage of applicants that have a credit history that meets guidelines
# in the training set

prop.table(table(train_set$Credit_History))

#         0         1 
# 0.1578014 0.8421986 


# Calcualte the percentage of applicants that have a credit history that meets guidelines 
# in the test set

prop.table(table(test_set$Credit_History)) 

#         0         1 
# 0.1745562 0.8254438 


# We see that most applicants have a credit history that meets guidelines in both the training 
# and test sets.




#  Variable "Property_Area"

# Property_Area is a factor variable with three levels and does not have missing values.

par(mfrow=c(1,2))
barplot(table(train_set$Property_Area),main="Property_Area in Training Set")
barplot(table(test_set$Property_Area),main="Property_Area in Test Set")



# Calcualte the percentage of applicants that have Rural, Semiurban or Urban property areas 
# in the training set

prop.table(table(train_set$Property_Area))

#     Rural Semiurban     Urban 
# 0.2915309 0.3794788 0.3289902 


# Calcualte the percentage of applicants that have Rural, Semiurban or Urban property areas 
# in the test set

prop.table(table(test_set$Property_Area))

#     Rural Semiurban     Urban 
# 0.3024523 0.3160763 0.3814714 


# We see that this is the only predictor variable whose distribution looks different 
# between the training and test sets.





####     Variables against response

# or Loan_Status by other variables



#install.packages("ggplot2")
library(ggplot2)

#install.packages("scales")
library(scales)


# Number of approved loans by Gender of Applicant
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) +
       geom_bar() + facet_grid(.~Gender) +
       ggtitle("Loan Status by Gender of Applicant")


# Percentage of approved loans by Gender of Applicant
ggplot(na.omit(train_set),aes(x = Gender, y = as.numeric(as.factor(Gender)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Gender', y='Approved rate', title='Loan Status by Gender of Applicant') + 
       scale_fill_discrete(labels=c("No","Yes"))

# We see that a slightly larger proportion of female applicants are refused than male ones


# Number of approved loans by Marital Status of Applicant
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) + 
       geom_bar() + facet_grid(.~Married) + 
       ggtitle("Loan Status by Marital Status of Applicant")


# Percentage of approved loans by Marital Status of Applicant
ggplot(na.omit(train_set),aes(x = Married, y = as.numeric(as.factor(Married)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Married', y='Approved rate', title='Loan Status by Marital Status of Applicant') + 
       scale_fill_discrete(labels=c("No","Yes"))

# We see that a larger proportion of unmarried applicants are refused than married ones



# Number of approved loans by number of Dependents of Applicant
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) + 
       geom_bar() + facet_grid(.~Dependents) +
       ggtitle("Loan Status by number of Dependents of Applicant")


# Percentage of approved loans by number of Dependents of Applicant
ggplot(na.omit(train_set),aes(x = Dependents, y = as.numeric(as.factor(Dependents)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Dependents', y='Approved rate', title='Loan Status by number of Dependents of Applicant') + 
       scale_fill_discrete(labels=c("No","Yes"))

# We see that a smaller proportion of applicants with 2 dependents are refused than applicants 
# with other number of dependents



# Number of approved loans by Education of Applicant
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) + 
       geom_bar() + facet_grid(.~Education) +
       ggtitle("Loan Status by Education of Applicant")


# Percentage of approved loans by Education of Applicant
ggplot(na.omit(train_set),aes(x = Education, y = as.numeric(as.factor(Education)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Education', y='Approved rate', title='Loan Status by Education of Applicant') + 
       scale_fill_discrete(labels=c("No","Yes"))

# We see that a larger proportion of non graduates are refused than graduates



# Number of approved loans by Employment status of Applicant
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) +
       geom_bar() + facet_grid(.~Self_Employed) +
       ggtitle("Loan Status by Employment status of Applicant")


# Percentage of approved loans by Employment status of Applicant
ggplot(na.omit(train_set),aes(x = Self_Employed, y = as.numeric(as.factor(Self_Employed)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Self_Employed', y='Approved rate', title='Loan Status by Employment status of Applicant') + 
       scale_fill_discrete(labels=c("No","Yes"))

# We see that applicants that are self employed have a slightly worse approved rate



# Number of approved loans by Terms of Loan
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) + 
       geom_bar() + facet_grid(.~Loan_Amount_Term) +
       ggtitle("Loan Status by Terms  of Loan")


# Percentage of approved loans by Terms of Loan
ggplot(na.omit(train_set),aes(x = Loan_Amount_Term, y = as.numeric(as.factor(Loan_Amount_Term)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Loan_Amount_Term', y='Approved rate', title='Loan Status by Terms  of Loan') + 
       scale_fill_discrete(labels=c("No","Yes"))

# From these plots we can only infer that the majority of loans have a term of 360 months 




# Number of approved loans by Credit History of Applicant
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) +
       geom_bar() + facet_grid(.~Credit_History) +
       ggtitle("Loan Status by Credit History of Applicant")



# Percentage of approved loans by Credit History of Applicant
ggplot(na.omit(train_set),aes(x = as.factor(Credit_History), y = as.numeric(as.factor(Credit_History)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Credit_History', y='Approved rate', title='Loan Status by Credit History of Applicant') + 
       scale_fill_discrete(labels=c("No","Yes"))

# Credit_History seems to be a very important predictor variable. 
# The vast majority of applicants whose credit history doesn't meet guidelines are refused.



# Number of approved loans by Property Area
ggplot(train_set, aes(x=Loan_Status, fill = factor(Loan_Status))) +
       geom_bar() + facet_grid(.~Property_Area) +
       ggtitle("Loan Status by Property Area")


# Percentage of approved loans by Property Area
ggplot(na.omit(train_set),aes(x = as.factor(Property_Area), y = as.numeric(as.factor(Property_Area)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format()) +
       labs(x = 'Property_Area', y='Approved rate', title='Loan Status by Property Area') + 
       scale_fill_discrete(labels=c("No","Yes"))

# We see that it's easier to get a loan if the property is Semiurban and harder if it's Rural.



# Number of approved loans by Applicant Income
ggplot(train_set, aes(x=Loan_Status, y=ApplicantIncome)) +
       geom_boxplot() + ggtitle("Loan Status by Applicant Income")


# convert Loan_Status levels to 0 and 1
ll <- train_set$Loan_Status
# Rename by name
levels(ll)[levels(ll)=="N"] <- 0
levels(ll)[levels(ll)=="Y"] <- 1



# Approved loans by Applicant Income with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable ApplicantIncome using linear model (lm)
ggplot( train_set, aes(x=(ApplicantIncome), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'ApplicantIncome', y='Loan Status', title='Loan Status by Applicant Income') 


# We see that ApplicantIncome doesn't seem to have a significant influence on loan approval




# Number of approved loans by Coapplicant Income
ggplot(train_set, aes(x=Loan_Status, y=CoapplicantIncome)) +
       geom_boxplot() + ggtitle("Loan Status by Coapplicant Income")



# Approved loans by Coapplicant Income with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable CoapplicantIncome using linear model (lm)
ggplot( train_set, aes(x=(CoapplicantIncome), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'CoapplicantIncome', y='Loan Status', title='Loan Status by Coapplicant Income') 


# We see that CoapplicantIncome has some influence on loan approval





# Number of approved loans by Loan Amount
ggplot(train_set, aes(x=Loan_Status, y=LoanAmount)) +
       geom_boxplot() + ggtitle("Loan Status by Loan Amount")



# Approved loans by Loan Amount with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable LoanAmount using linear model (lm)
ggplot( train_set, aes(x=(LoanAmount), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'LoanAmount', y='Loan Status', title='Loan Status by Loan Amount') 

# We see that LoanAmount has some influence on loan approval




# Compute Predictor Importance

# For categorical predictor variables: Predictor Importance = 1 - PVal 
# from Pearson Chi-square test 
# For numerical predictor variables: Predictor Importance = 1 - PVal 
# from ANOVA F-Test for Equality of Mean

# Initialize the predictor importance data.frame
pr.imp <- setNames(data.frame(matrix(ncol = ncol(train_set), nrow = 1)), colnames(train_set))
# exclude these columns
drops <- c("Loan_ID","Loan_Status")
pr.imp<-pr.imp[ , !(names(pr.imp) %in% drops)]


# Predictor Importance for categorical variables
pr.imp$Gender <- 1-chisq.test(train_set$Loan_Status, train_set$Gender)$p.value 
pr.imp$Married <- 1-chisq.test(train_set$Loan_Status, train_set$Married)$p.value 
pr.imp$Dependents <- 1-chisq.test(train_set$Loan_Status, train_set$Dependents)$p.value 
pr.imp$Education <- 1-chisq.test(train_set$Loan_Status, train_set$Education)$p.value 
pr.imp$Self_Employed <- 1-chisq.test(train_set$Loan_Status, train_set$Self_Employed)$p.value 
pr.imp$Credit_History <- 1-chisq.test(train_set$Loan_Status, as.factor(train_set$Credit_History))$p.value 
pr.imp$Property_Area <- 1-chisq.test(train_set$Loan_Status, train_set$Property_Area)$p.value 
pr.imp$Loan_Amount_Term <- 1-chisq.test(train_set$Loan_Status, as.factor(train_set$Loan_Amount_Term))$p.value

# Predictor Importance for numerical variables
aov.out <- aov(ApplicantIncome ~ Loan_Status, data=train_set)
pr.imp$ApplicantIncome <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 
aov.out <- aov(CoapplicantIncome ~ Loan_Status, data=train_set)
pr.imp$CoapplicantIncome <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 
aov.out <- aov(LoanAmount ~ Loan_Status, data=train_set)
pr.imp$LoanAmount <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 

# Sort the data frame from the variable with the highest importance to the variable with the 
# lowest importance
pr.imp <- pr.imp[order(pr.imp, decreasing=TRUE)]


# Credit_History 1 
# Property_Area 0.997864  
# Married 0.9656062
# Education 0.9569004
# Loan_Amount_Term 0.8781424 
# CoapplicantIncome 0.8570517 
# LoanAmount 0.6352638
# Dependents 0.6321493 
# Gender 0.291347 
# ApplicantIncome 0.09271219
# Self_Employed 2.553513e-15
        

# From the above univariate analysis, we see that Credit_History, Property_Area, Married and
# Education are the most significant predictors.
# Surprisingly, ApplicantIncome is a very weak predictor, while CoapplicantIncome is a
# significant predictor.


# After this analysis, we will not ignore the outliers from ApplicantIncome, CoapplicantIncome
# and LoanAmount as most significant predictors are all categorical.








####  Missing values Imputation


####  Variable "Married"


# Find which passengers have missing Married variables
married.missing_rows <- which(is.na(full$Married) | full$Married=="")
full[married.missing_rows, ]

#      Loan_ID Gender Married Dependents Education Self_Employed ApplicantIncome CoapplicantIncome
# 105 LP001357   Male    <NA>       <NA>  Graduate            No            3816               754
# 229 LP001760   Male    <NA>       <NA>  Graduate            No            4758                 0
# 436 LP002393 Female    <NA>       <NA>  Graduate            No           10047                 0
#      LoanAmount Loan_Amount_Term Credit_History Property_Area Loan_Status
# 105        160              360              1         Urban           Y
# 229        158              480              1     Semiurban           Y
# 436         NA              240              1     Semiurban           Y


# We will infer the missing Married values based on present data that seem relevant: 
# CoapplicantIncome

# We will consider the Married variable as "No", when the coapplicant income is zero, and
# "Yes" otherwise.

full$Married[is.na(full$Married) & full$CoapplicantIncome==0]<-"No"
full$Married[is.na(full$Married)]<- "Yes"



####  Variables Gender and Dependents


# Replacing the missing values from "Gender" and "Dependents" with the most frequent category 
# might not be the best idea, since they may differ by groups of applicants.



# We will first examine if there are rows with both Gender and Dependents missing
full[is.na(full$Gender) & is.na(full$Dependents),]

#      Loan_ID Gender Married Dependents Education Self_Employed ApplicantIncome CoapplicantIncome
# 753 LP001769   <NA>      No       <NA>  Graduate            No            3333              1250
#      LoanAmount Loan_Amount_Term Credit_History Property_Area Loan_Status
# 753        110              360              1     Semiurban          NA


# We see that there is only one applicant with both these values missing. 
# This applicant is not Married but has higher income than the coapplicant.

# Let's investigate which gender has higher income
aggregate(full$ApplicantIncome, by=list(full$Gender), median)
#   Group.1      x
# 1  Female 3634.5
# 2    Male 3864.0

# Since males have higher income, we will consider the missing value as "Male"
full$Gender[is.na(full$Gender) & is.na(full$Dependents)] <- "Male"

# So all the other missing observations have only one of these variables missing. 

# First, we will examine the missing values of Dependents.



####  Variable Dependents

# Display variables for missing values of Dependents that seem relevant.

data.missing_dep <- subset(full, is.na(Dependents), select=c("Gender","Married", "ApplicantIncome",
                                                      "CoapplicantIncome", "LoanAmount",
                                                      "Loan_Amount_Term", "Property_Area"))

summary(data.missing_dep)


#    Gender   Married  ApplicantIncome CoapplicantIncome   LoanAmount     Loan_Amount_Term
# Female: 5   No :11   Min.   : 2066   Min.   :   0      Min.   : 70.00   Min.   : 84.0   
# Male  :20   Yes:14   1st Qu.: 3250   1st Qu.:   0      1st Qu.: 96.25   1st Qu.:360.0   
#                      Median : 3863   Median :   0      Median :119.50   Median :360.0   
#                      Mean   : 5206   Mean   :1023      Mean   :123.55   Mean   :343.5   
#                      3rd Qu.: 5417   3rd Qu.:1750      3rd Qu.:156.50   3rd Qu.:360.0   
#                      Max.   :14987   Max.   :4490      Max.   :180.00   Max.   :480.0   
#                                                        NA's   :3        NA's   :1   
#
#  Property_Area
#  Rural    : 3   
#  Semiurban:11   
#  Urban    :11   



# We can examine the Dependents variables in relation to the Married variable.
prop.table(table(full$Married, full$Dependents),margin=1)

#              0          1          2         3+
# No  0.81656805 0.10650888 0.04142012 0.03550296
# Yes 0.43527508 0.20064725 0.23624595 0.12783172

# The majority of unmarried people do not have dependents. However, ~ 19% of unmarried people
# have 1 or more dependents, so we can't impute missing dependents with zero for unmarried people.

# We are going to predict missing Dependents variables using three different methods, in order 
# to investigate which one achieves the best results. 

# Modal imputation
# For categorical variables, an easy way to impute the values is to use modal imputation, 
# or impute cases with the mode, or most common value. 


# Find the most common value
mcv.dep <- factor(names(which.max(table(full$Dependents))), levels=levels(full$Dependents))
# [1] 0
# Levels: 0 1 2 3+

tmp1 <- full
# Impute the cases
tmp1$Dependents <- as.factor(ifelse(is.na(tmp1$Dependents)==T, mcv.dep, tmp1$Dependents))
levels(tmp1$Dependents) <- levels(full$Dependents)


summary(full$Dependents)
#   0    1    2   3+ NA's 
# 545  160  160   91   25 

summary(tmp1$Dependents)
#   0   1   2  3+ 
# 570 160 160  91 




# We will use rpart (recursive partitioning for regression) to predict the missing Dependents 
# values

#install.packages("rpart")
library("rpart")

# Set a random seed
set.seed(5)
       
# To handle factor variables, we set the method="class" while calling rpart().
DependentsFit <- rpart(Dependents ~ Gender + Married + Education + ApplicantIncome + 
                              CoapplicantIncome + LoanAmount + Loan_Amount_Term + Property_Area,
                data=full[!is.na(full$Dependents),], method="class")


#accuracy
#p<-predict(DependentsFit,full[!is.na(full$Dependents),],type="class")
#acc=sum(p==full[!is.na(full$Dependents),]$Dependents)/length(p)
#acc
# [1] 0.5700837


tmp2<-full
# Impute the cases
tmp2$Dependents<-as.character(tmp2$Dependents)
tmp2$Dependents[is.na(full$Dependents)] <- predict(DependentsFit, full[is.na(full$Dependents),], type="class")
tmp2$Dependents<-as.factor(tmp2$Dependents)

summary(full$Dependents)
#   0    1    2   3+ NA's 
# 545  160  160   91   25 

summary(tmp2$Dependents)
#   0   1   2  3+ 
# 545 185 160  91 


# Perform mice (Multivariate Imputation by Chained Equations) imputation to predict the 
# missing Dependents values

#install.packages("mice")
library(mice)

set.seed(5)

mice_mod <- mice(full[, !names(full) %in% c('Loan_ID','Self_Employed','Credit_History',
                                            'Loan_Status')]) 
# Find which method was used to predict the missing Dependents values
mice_mod$meth['Dependents']
#Dependents 
#"polyreg" 

# Factor variables with more than two levels are imputed using multinomial logistic regression
# (polyreg)

# Generate the completed data
miceOutput <- complete(mice_mod)  
anyNA(miceOutput)
# [1] FALSE

#pp <- miceOutput[!is.na(full$Dependents), "Dependents"]
#acc=sum(pp==full[!is.na(full$Dependents),]$Dependents)/length(pp)
#acc
# [1] 1



tmp3<-full
# Impute the cases
#tmp$Dependents<-as.character(tmp$Dependents)
tmp3$Dependents[is.na(full$Dependents)] <- miceOutput[is.na(full$Dependents), "Dependents"]
#tmp3$Dependents<-as.factor(tmp$Dependents)


summary(full$Dependents)
#   0    1    2   3+ NA's 
# 545  160  160   91   25 

summary(tmp3$Dependents)
#   0   1   2  3+ 
# 562 164 163  92 


# Compare the original distribution of Dependents with the predicted using modal imputation,
# rpart imputation and mice imputation in order to select the best prediction.

par(mfrow=c(2,2))
x1<-barplot(prop.table(table(full$Dependents)), main='Original Dependents Variable', ylim=c(0, .8))
text(x = x1, y = prop.table(table(full$Dependents)), label = round(prop.table(table(full$Dependents)),3), 
     pos = 3, cex = 0.8, col = "black")
x2<-barplot(prop.table(table(tmp1$Dependents)), main='Imputed Dependents Variable with mode', ylim=c(0, .8))
text(x = x2, y = prop.table(table(tmp1$Dependents)), label = round(prop.table(table(tmp1$Dependents)),3), 
     pos = 3, cex = 0.8, col = "black")
x3<-barplot(prop.table(table(tmp2$Dependents)), main='Imputed Dependents Variable with rpart', ylim=c(0, .8))
text(x = x3, y = prop.table(table(tmp2$Dependents)), label = round(prop.table(table(tmp2$Dependents)),3), 
     pos = 3, cex = 0.8, col = "black")
x4<-barplot(prop.table(table(tmp3$Dependents)), main='Imputed Dependents Variable with mice', ylim=c(0, .8))
text(x = x4, y = prop.table(table(tmp3$Dependents)), label = round(prop.table(table(tmp3$Dependents)),3), 
     pos = 3, cex = 0.8, col = "black")

# We see that the best prediction is achieved using mice imputation, so we will use these values
# to replace the missing Dependents values.

# Replace missing Dependents values with the predicted 
full$Dependents <- tmp3$Dependents



####  Variable Gender 

# We will use mice imputation again to predict the missing Gender values

set.seed(5)

mice_mod <- mice(full[, !names(full) %in% c('Loan_ID','Credit_History','LoanAmount',
                                            'Loan_Amount_Term','Property_Area','Loan_Status')]) 
# Find which method was used to predict the missing Gender values
mice_mod$meth['Gender']
# Gender 
# "logreg"  # Logistic regression


# Generate the completed data
miceOutput <- complete(mice_mod)  
anyNA(miceOutput)
# [1] FALSE


tmp<-full
# Impute missing values
tmp$Gender[is.na(full$Gender)] <- miceOutput[is.na(full$Gender), "Gender"]


summary(full$Gender)
# Female   Male   NA's 
#    182    776     23 

summary(tmp$Gender)
# Female   Male 
#    189    792 

prop.table(table(full$Gender))
#    Female      Male 
# 0.1899791 0.8100209 

prop.table(table(tmp$Gender))
#    Female      Male 
# 0.1926606 0.8073394 


# We see that the percentages of Gender before and after the prediction look very similar,
# which implies that our prediction was correct.

# Replace missing Gender values with the predicted 
full$Gender <- tmp$Gender




# We will use rpart to predict the missing Gender values
#set.seed(5)

# To handle factor variables, we can set the method="class" while calling rpart().
#GenderFit <- rpart(Gender ~ Married + Dependents + Education + Self_Employed + 
#                              ApplicantIncome + CoapplicantIncome ,
#                       data=full[!is.na(full$Gender),], method="class")


#accuracy
#p<-predict(GenderFit,full[!is.na(full$Gender),],type="class")
#acc=sum(p==full[!is.na(full$Gender),]$Gender)/length(p)
#acc
# [1] 0.8350731


#tmp2<-full
#tmp2$Gender[is.na(full$Gender)] <- predict(GenderFit, full[is.na(full$Gender),], type="class")


#summary(full$Gender)
# Female   Male   NA's 
#    182    776     23

#summary(tmp2$Gender)
# Female   Male 
#    184    797



####  Variable Self_Employed

# Examine what percentage of applicants is self employed 
prop.table(table(full$Self_Employed))

#        No       Yes 
# 0.8714903 0.1285097 

# Since ~87% of applicants are not self employed, it would be safe to impute the missing values 
# as "No", as there is a high probability of success.

# However we can investigate if there are any hidden patterns. 

# We will examine the relationship between the Self_Employed variable and the data that seem 
# relevant: Gender and Education

em_ge_ed<-aggregate(full$Self_Employed, by=list(Self_Employed=full$Self_Employed, Gender=full$Gender, 
                                         Education=full$Education), function(x) sum( !is.na(x) ))

#   Self_Employed Gender    Education   x
# 1            No Female     Graduate 127
# 2           Yes Female     Graduate  17
# 3            No   Male     Graduate 499
# 4           Yes   Male     Graduate  77
# 5            No Female Not Graduate  30
# 6           Yes Female Not Graduate   4
# 7            No   Male Not Graduate 151
# 8           Yes   Male Not Graduate  21





ggplot(em_ge_ed, aes(x = factor(Gender), y = x)) + 
       geom_bar(stat='identity',position='dodge') +  facet_grid(Self_Employed~Education) +
       labs(x = 'Gender', y='Self Employed', title='Self Employed Applicants in relation to Gender and Education') 


#em_ge_ed.prop<-em_ge_ed
#em_ge_ed.prop$x<-prop.table(em_ge_ed$x)

#ggplot(em_ge_ed.prop, aes(x = factor(Gender), y = x)) + 
#       geom_bar(stat='identity',position='dodge') +  facet_grid(Self_Employed~Education) +
#       labs(x = 'Gender', y='Self Employed', title='Self Employed Applicants in relation to Gender and Education') 


# We see that the vast majority of applicants are not self employed regardless of their
# gender and education.
# Therefore, we can impute the missing Self_Employed values using the mode="No".

full$Self_Employed[is.na(full$Self_Employed)] <- "No"

# Examine what percentage of applicants is self employed after the replacement
prop.table(table(full$Self_Employed))

#        No       Yes 
# 0.8786952 0.1213048 

# We see that the percentages of self employed applicants before and after the prediction
# look very similar, which implies that our prediction was correct.




####  Variable Credit_History

# Credit History is a high impact variable. If credit history is not available, it possibly
# means that the applicant has not had many credit activities in the past. 
# The safest approach is to treat this variable as a separate category.
# Therefore, we will replace the missing values with "Not Available".

full$Credit_History = as.character(full$Credit_History)
full$Credit_History[is.na(full$Credit_History)] = "Not Available"
full$Credit_History = as.factor(full$Credit_History)



# Modal imputation
# For categorical variables, an easy way to impute the values is to use modal imputation, 
# or impute cases with the mode, or most common value. 

#full$Credit_History <- as.character(full$Credit_History)
#full$Credit_History <- as.factor(full$Credit_History)

#find the most common value
#mcv.cred<-factor(names(which.max(table(full$Credit_History))), levels=levels(full$Credit_History))
# [1] 1
# Levels: 0 1

#tmp1<-full
#impute the cases
#tmp1$Credit_History<-as.factor(ifelse(is.na(tmp1$Credit_History)==T, mcv.cred, tmp1$Credit_History))
#levels(tmp1$Credit_History)<-levels(full$Credit_History)


#summary(full$Credit_History)
#   0    1 NA's 
# 148  754   79 

#summary(tmp1$Credit_History)
#   0   1 
# 148 833 

#full$Credit_History <- tmp1$Credit_History




####  Variable LoanAmount


# Compute a basic summary of LoanAmount
summary(full$LoanAmount)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  9.0   100.0   126.0   142.5   162.0   700.0      27 

# We will examine the relationship between the LoanAmount variable and the data that seem 
# relevant: Education and Self_Employed

# LoanAmount_Education_SelfEmployed
lo_ed_em <- aggregate(LoanAmount ~ Education + Self_Employed, full, median)


# or second approach to compute lo_ed_em
##install.packages("dplyr")
#library(dplyr)
#lo_ed_em <- full %>%
#       group_by(Education,Self_Employed) %>%
#       summarise(GroupMedian = median(LoanAmount,na.rm = TRUE))

# The next data frame provides the median LoanAmount values for all the groups of unique values 
# of Self_Employed and Education variables.

#      Education Self_Employed LoanAmount
# 1     Graduate            No        130  lo_ed_em$LoanAmount[lo_ed_em$Education=='Graduate' & lo_ed_em$Self_Employed=='No']
# 2 Not Graduate            No        117  lo_ed_em$LoanAmount[lo_ed_em$Education=='Not Graduate' & lo_ed_em$Self_Employed=='No']
# 3     Graduate           Yes        150  lo_ed_em$LoanAmount[lo_ed_em$Education=='Graduate' & lo_ed_em$Self_Employed=='Yes']
# 4 Not Graduate           Yes        130  lo_ed_em$LoanAmount[lo_ed_em$Education=='Not Graduate' & lo_ed_em$Self_Employed=='Yes']


# Replace missing values based on their category

ind <- which(is.na(full$LoanAmount))
full[ind,]$LoanAmount[full[ind,]$Education == "Graduate" & full[ind,]$Self_Employed == "No"] <- lo_ed_em$LoanAmount[lo_ed_em$Education=='Graduate' & lo_ed_em$Self_Employed=='No']
full[ind,]$LoanAmount[full[ind,]$Education == "Not Graduate" & full[ind,]$Self_Employed == "No"] <- lo_ed_em$LoanAmount[lo_ed_em$Education=='Not Graduate' & lo_ed_em$Self_Employed=='No']
full[ind,]$LoanAmount[full[ind,]$Education == "Graduate" & full[ind,]$Self_Employed == "Yes"] <- lo_ed_em$LoanAmount[lo_ed_em$Education=='Graduate' & lo_ed_em$Self_Employed=='Yes']
full[ind,]$LoanAmount[full[ind,]$Education == "Not Graduate" & full[ind,]$Self_Employed == "Yes"] <- lo_ed_em$LoanAmount[lo_ed_em$Education=='Not Graduate' & lo_ed_em$Self_Employed=='Yes']



# or second approach
#full$LoanAmount <- 
#       ifelse( (is.na(full$LoanAmount) & full$Education=='Graduate' & full$Self_Employed =='No'),lo_ed_em$LoanAmount[lo_ed_em$Education=='Graduate' & lo_ed_em$Self_Employed=='No'],
#               ifelse( (is.na(full$LoanAmount) & full$Education=='Not Graduate' & full$Self_Employed =='No'),lo_ed_em$LoanAmount[lo_ed_em$Education=='Not Graduate' & lo_ed_em$Self_Employed=='No'],
#                       ifelse( (is.na(full$LoanAmount) & full$Education=='Graduate' & full$Self_Employed =='Yes'),lo_ed_em$LoanAmount[lo_ed_em$Education=='Graduate' & lo_ed_em$Self_Employed=='Yes'],
#                               ifelse( (is.na(full$LoanAmount) & full$Education=='Not Graduate' & full$Self_Employed =='Yes'),lo_ed_em$LoanAmount[lo_ed_em$Education=='Not Graduate' & lo_ed_em$Self_Employed=='Yes'], full$LoanAmount))))



# compute a summary of the LoanAmount variable after prediction to ensure that everything is ok
summary(full$LoanAmount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.0   101.0   128.0   142.1   160.0   700.0 




####  Variable Loan_Amount_Term

# Compute a basic summary of Loan_Amount_Term

summary(full$Loan_Amount_Term)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  6.0   360.0   360.0   342.2   360.0   480.0      20 

# We see that the 1st quartile,median and 3rd quartile values are 360, which means that the
# majority of Loan_Amount_Term values are 360.
# Let's check it out.

table(full$Loan_Amount_Term)

# 6  12  36  60  84 120 180 240 300 350 360 480 
# 1   2   3   3   7   4  66   8  20   1 823  23



# We will examine the relationship between the Loan_Amount_Term variable and the data that seem 
# relevant: LoanAmount 

# LoanAmount_Loan_Amount_Term
lo_te <- aggregate(LoanAmount ~ Loan_Amount_Term, full, median)

#    Loan_Amount_Term LoanAmount
# 1                 6       95.0
# 2                12      185.5
# 3                36      118.0
# 4                60      139.0
# 5                84      108.0
# 6               120       25.0
# 7               180      117.0
# 8               240      100.0
# 9               300      135.5
# 10              350      133.0
# 11              360      130.0
# 12              480      113.0


# There seems to be no linear relationship between LoanAmount and Loan_Amount_Term.

# Create a dataframe with the columns Loan_Amount_Term, median LoanAmount and number of 
# observations for each unique Loan_Amount_Term
t<-table(full$Loan_Amount_Term)

lo_te_nu <- cbind(lo_te, Freq = as.data.frame(t)$Freq)


#    Loan_Amount_Term LoanAmount Freq
# 1                 6       95.0    1
# 2                12      185.5    2
# 3                36      118.0    3
# 4                60      139.0    3
# 5                84      108.0    7
# 6               120       25.0    4
# 7               180      117.0   66
# 8               240      100.0    8
# 9               300      135.5   20
# 10              350      133.0    1
# 11              360      130.0  823
# 12              480      113.0   23


# We see that the LoanAmount does not have a big influence on the Loan_Amount_Term as it
# could be expected.


# Since the vast majority of the loans had a term of 360 months, we will replace the missing
# values with the mode, or most common value, which is 360.
full$Loan_Amount_Term[is.na(full$Loan_Amount_Term)] <- 360
# and since there are only a few unique Loan_Amount_Term values, we will convert this variable
# from integer to factor.
full$Loan_Amount_Term <- as.factor(full$Loan_Amount_Term)






####  Create new variables that will help in the prediction

# Some variables should be combined or transformed someway in order to build a better model.
# We will make several graphs and computations to determine which transformation of variables
# can be added as new features.


####  Variable TotalIncome


# It is possible that some applicants have lower income but strong support co-applicants.
# So it might be a good idea to combine both incomes as total income.


full$TotalIncome <- full$ApplicantIncome + full$CoapplicantIncome


# Number of approved loans by TotalIncome
ggplot(full[1:nrow(train_set),], aes(x=Loan_Status, y=TotalIncome)) +
       geom_boxplot() + ggtitle("Loan Status by TotalIncome")



# Approved loans by TotalIncome with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable TotalIncome using linear model (lm)
ggplot( full[1:nrow(train_set),], aes(x=(TotalIncome), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'TotalIncome', y='Loan Status', title='Loan Status by Total Income') 

# We see that TotalIncome has some influence on loan approval, but it is not very significant.





####  Variable Loan_by_TotalIncome

# Create a variable as the loan amount divided by the sum of applicant and coapplicant income
# (TotalIncome). This variable gives an idea of how well the applicant is suited to pay back
# his loan.


full$Loan_by_TotalIncome <- full$LoanAmount/full$TotalIncome


# Number of approved loans by TotalIncome
ggplot(full[1:nrow(train_set),], aes(x=Loan_Status, y=Loan_by_TotalIncome)) +
       geom_boxplot() + ggtitle("Loan Status by Loan_by_TotalIncome")



# Approved loans by Loan_by_TotalIncome with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable Loan_by_TotalIncome using linear model (lm)
ggplot( full[1:nrow(train_set),], aes(x=(Loan_by_TotalIncome), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'Loan_by_TotalIncome', y='Loan Status', title='Loan Status by Loan_by_TotalIncome') 

# There seems to be a strong negative relationship between loan amount/total income and 
# approved rate.





####  Variable Coapplicant

# Create a variable that indicates whether there is coapplicant or not.


# A coapplicant exists if the CoapplicantIncome is larger than zero, or the applicant is married.
full$Coapplicant <- as.factor(ifelse((full$CoapplicantIncome>0 |full$Married=="Y"), 1, 0))


# Number of approved loans by the existence or not of Coapplicant
ggplot(full[1:nrow(train_set),], aes(x = Coapplicant, fill = factor(Loan_Status))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Coapplicant', y='Approved rate', title='Loan Status by the existence or not of Coapplicant') + 
       scale_fill_discrete(labels=c("N","Y"))

# We see that most applicants have a co-applicant 

# Percentage of approved loans by the existence or not of Coapplicant
ggplot(full[1:nrow(train_set),],aes(x = as.factor(Coapplicant), y = as.numeric(as.factor(Coapplicant)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format()) +
       labs(x = 'Coapplicant', y='Approved rate', title='Loan Status by the existence or not of Coapplicant') + 
       scale_fill_discrete(labels=c("No","Yes"))

# We see that a loan is slightly more likely to be approved if there is a coapplicant.





####  Variable FamilySize


# FamilySize contains the applicant itself, the coapplicant (if exists) and the number of 
# dependents.

#install.packages("dplyr")
library("dplyr")

numDependents <- recode(full$Dependents, '3+' ="3")
numDependents <- as.numeric(as.character(numDependents))
full$FamilySize <- ifelse((full$CoapplicantIncome>0 |full$Married=="Y"),
                          numDependents+2, numDependents+1)

table(full$FamilySize)

#   1   2   3   4   5 
# 259 365 164 147  46 


FamSize <- data.frame(table(full$FamilySize))

# Subset the dataframe to show only the FamilySize>=3 groups
FamSize <- FamSize[as.character(FamSize$Var1) >= 3,]

full$DFamilySize <- full$FamilySize
# Change FamilySize for those with  family sizes (>=3) to 3+ and finally convert it to a factor:
full$DFamilySize[full$FamilySize %in% FamSize$Var1] <- '3+'


table(full$DFamilySize)

#   1   2  3+ 
# 259 365 357

full$DFamilySize <- as.factor(full$DFamilySize)

# Find the levels of FamilySize
levels(full$DFamilySize)
# [1] "1"  "2"  "3+"



# Number of approved loans by FamilySize
ggplot(full[1:nrow(train_set),], aes(x = DFamilySize, fill = factor(Loan_Status))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'DFamilySize', y='Approved rate', title='Loan Status by FamilySize') + 
       scale_fill_discrete(labels=c("N","Y"))


# Percentage of approved loans by FamilySize
ggplot(full[1:nrow(train_set),],aes(x = as.factor(DFamilySize), y = as.numeric(as.factor(DFamilySize)),fill = factor(Loan_Status))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format()) +
       labs(x = 'DFamilySize', y='Approved rate', title='Loan Status by FamilySize') + 
       scale_fill_discrete(labels=c("No","Yes"))



# We see that if an applicant has a coapplicant or one dependent (DFamilySize=2) 
# or has a family of size >=3, it is slightly more likely to take the loan, but 
# if the family size is 1, i.e., if there is only the applicant, it doesn't help to 
# take the loan.





####  Variable FamilyIncome

# Create a variable as the sum of applicant and coapplicant income (TotalIncome) divided by 
# the family size. This variable gives an idea of the actual income of a family.


full$FamilyIncome <- full$TotalIncome/full$FamilySize

# convert FamilySize to factor
full$FamilySize <- as.factor(full$FamilySize)


# Number of approved loans by FamilyIncome
ggplot(full[1:nrow(train_set),], aes(x=Loan_Status, y=FamilyIncome)) +
       geom_boxplot() + ggtitle("Loan Status by FamilyIncome")



# Approved loans by FamilyIncome with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable FamilyIncome using linear model (lm)
ggplot( full[1:nrow(train_set),], aes(x=(FamilyIncome), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'FamilyIncome', y='Loan Status', title='Loan Status by FamilyIncome') 


# Remove the outlier just from the plot
i <- max(full$FamilyIncome) 
family.income.out <- full[which(full$FamilyIncome == i), ]
full.fin.no.outliers <- full[-which(full$FamilyIncome == i),]

ggplot( full.fin.no.outliers[1:nrow(train_set),], aes(x=(FamilyIncome), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'FamilyIncome', y='Loan Status', title='Loan Status by FamilyIncome') 


# There seems to be a strong negative relationship between family income and approved rate.





####  Variable Loan_by_FamilyIncome

# Create a variable as the loan amount divided by family income.
# This variable gives an idea of how well a family is suited to pay back its loan.


full$Loan_by_FamilyIncome <- full$LoanAmount/full$FamilyIncome


# Number of approved loans by Loan_by_FamilyIncome
ggplot(full[1:nrow(train_set),], aes(x=Loan_Status, y=Loan_by_FamilyIncome)) +
       geom_boxplot() + ggtitle("Loan Status by Loan_by_FamilyIncome")



# Approved loans by Loan_by_FamilyIncome with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable Loan_by_FamilyIncome using linear model (lm)
ggplot( full[1:nrow(train_set),], aes(x=(Loan_by_FamilyIncome), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'Loan_by_FamilyIncome', y='Loan Status', title='Loan Status by Loan_by_FamilyIncome') 

# There seems to be a weak negative relationship between loan amount/family income and 
# approved rate.





####  Variable LoanPerMonth 

# Create a variable as the loan amount divided by loan term. It is the monthly installment
# made by a borrower to a lender. We will ignore the interest rate per month here.

full$LoanPerMonth <- full$LoanAmount/as.numeric(as.character(full$Loan_Amount_Term))


# Number of approved loans by LoanPerMonth
ggplot(full[1:nrow(train_set),], aes(x=Loan_Status, y=LoanPerMonth)) +
       geom_boxplot() + ggtitle("Loan Status by LoanPerMonth")


# Approved loans by LoanPerMonth with regression line

# Add a regression line that predicts the response variable Loan_Status as a function of 
# the explanatory variable LoanPerMonth using linear model (lm)
ggplot( full[1:nrow(train_set),], aes(x=(LoanPerMonth), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'LoanPerMonth', y='Loan Status', title='Loan Status by LoanPerMonth') 



# Remove the outlier just from the plot
i <- max(full[1:nrow(train_set),]$LoanPerMonth) 
loan.month.out <- full[which(full$LoanPerMonth == i), ]
full.lom.no.outliers <- full[-which(full$LoanPerMonth == i),]

ggplot( full.lom.no.outliers[1:nrow(train_set),], aes(x=(LoanPerMonth), y=as.numeric(as.character(ll)) ) ) +
       geom_point() + geom_smooth(method="lm", se=FALSE) +
       labs(x = 'LoanPerMonth', y='Loan Status', title='Loan Status by LoanPerMonth') 


# There seems to be a weak negative relationship between loan per month and approved rate.






####  Extreme values treatment



# Extreme values treatment in distribution of LoanAmount, TotalIncome, Loan_by_TotalIncome,
# FamilyIncome, Loan_by_FamilyIncome and LoanPerMonth

# In this problem, the extreme values are practically possible, i.e. some people might apply 
# for high  value loans due to specific needs. So instead of treating them as outliers, 
# it's probably a good idea to take a log transformation of the monetary variables to 
# nullify their effect.



####  Variable LoanAmount 

summary(full$LoanAmount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.0   101.0   128.0   142.1   160.0   700.0 

full$LogLoanAmount <- log(full$LoanAmount)

# Compare the original distribution of LoanAmount with its log version 

par(mfrow=c(1,2))
hist(full$LoanAmount, freq=F, main='Original LoanAmount', col='bisque4')
hist(full$LogLoanAmount, freq=F, main='Log LoanAmount', col='bisque3')


summary(full$LogLoanAmount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.197   4.615   4.852   4.846   5.075   6.551 


# After log transformation the distribution looks much closer to normal and the effect of 
# extreme values has been significantly subsided.



####  Variable TotalIncome

# Since ApplicantIncome and CoapplicantIncome are better predictors when they are combined in 
# one variable, we will take the log transformation of TotalIncome.

summary(full$TotalIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1442    4166    5314    6782    7308   81000


full$LogTotalIncome <- log(full$TotalIncome)

# Compare the original distribution of TotalIncome with its log version 

par(mfrow=c(1,2))
hist(full$TotalIncome, freq=F, main='Original TotalIncome', col='bisque4')
hist(full$LogTotalIncome, freq=F, main='Log TotalIncome', col='bisque3')


summary(full$LogTotalIncome)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.274   8.335   8.578   8.650   8.897  11.302 

# After log transformation the distribution looks much closer to normal and the effect of 
# extreme values has been significantly subsided.



####  Variable Loan_by_TotalIncome

summary(full$Loan_by_TotalIncome)

#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.001905 0.019231 0.024131 0.024076 0.028432 0.102273 

# This one looks symmetric, so we will not take the log transformation.




####  Variable FamilyIncome

summary(full$FamilyIncome)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 457.5  1763.2  2666.7  3582.7  4111.0 63337.0 


full$LogFamilyIncome <- log(full$FamilyIncome)

# Compare the original distribution of TotalIncome with its log version 

par(mfrow=c(1,2))
hist(full$FamilyIncome, freq=F, main='Original FamilyIncome', col='bisque4')
hist(full$LogFamilyIncome, freq=F, main='Log FamilyIncome', col='bisque3')


summary(full$LogFamilyIncome)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.126   7.475   7.889   7.925   8.321  11.056 

# After log transformation the distribution looks much closer to normal and the effect of 
# extreme values has been significantly subsided.




####  Variable Loan_by_FamilyIncome


summary(full$Loan_by_FamilyIncome)

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00349 0.02936 0.04806 0.05634 0.07463 0.33085 


full$LogLoan_by_FamilyIncome <- log(1000*full$Loan_by_FamilyIncome)

# Compare the original distribution of TotalIncome with its log version 

par(mfrow=c(1,2))
hist(full$Loan_by_FamilyIncome, freq=F, main='Original Loan_by_FamilyIncome', col='bisque4')
hist(full$LogLoan_by_FamilyIncome, freq=F, main='Log Loan_by_FamilyIncome', col='bisque3')


summary(full$LogLoan_by_FamilyIncome)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.250   3.380   3.873   3.829   4.313   5.802 


# After log transformation the distribution looks much closer to normal and the effect of 
# extreme values has been significantly subsided.





####  Variable LoanPerMonth 


summary(full$LoanPerMonth)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0250  0.2889  0.3611  0.4903  0.5000 21.6667 


full$LogLoanPerMonth <- log(1000*full$LoanPerMonth)

# Compare the original distribution of TotalIncome with its log version 

par(mfrow=c(1,2))
hist(full$LoanPerMonth, freq=F, main='Original LoanPerMonth', col='bisque4')
hist(full$LogLoanPerMonth, freq=F, main='Log LoanPerMonth', col='bisque3')


summary(full$LogLoanPerMonth)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.219   5.666   5.889   5.953   6.215   9.984 



# After log transformation the distribution looks much closer to normal and the effect of 
# extreme values has been significantly subsided.



# Since log transformations of LoanAmount, TotalIncome, FamilyIncome, Loan_by_FamilyIncome,
# LoanPerMonth make the effect of extreme values less intense we will keep these variables
# and delete their corresponding original values from the data frame. 
# We will also delete the ApplicantIncome and CoapplicantIncome since the new variable
# LogTotalIncome is a better predictor. 
# Finally, we will delete the variable FamilySize, since we will keep its discretized version,
# DFamilySize.  


drop.cols <- c("LoanAmount", "TotalIncome", "FamilyIncome", "Loan_by_FamilyIncome",
               "LoanPerMonth", "ApplicantIncome", "CoapplicantIncome", "FamilySize")
               
full <- full[,!(names(full)) %in% drop.cols]








####  Examine Correlation of Continuous Variables

# Examine the continuous variables separately, and remove any that are highly correlated.

# Determine numeric and integer variables in the data frame
nums <-sapply(colnames(full), function(x) inherits(full[,x], c("numeric","integer")))
numVars <- full[,nums]

# Compute correlation
cor.mat <- cor(numVars)
# Convert it to a vector
cor_vec <- as.vector(cor.mat)
# Get variable names
id1 <- rep(rownames(cor.mat),nrow(cor.mat))
id2 <- as.vector(sapply(rownames(cor.mat),function(x)rep(x,nrow(cor.mat))))
# Create a data frame with three columns: correlation, variable 1, variable 2
cor.final <- data.frame(cor_vec,id1,id2)
# Keep only the variables whose correlation is between 0.8 and 1
cor.final <- cor.final[cor.final$cor_vec>0.8 & cor.final$cor_vec<1,]
# There aren't any variables with cor>0.8 and cor<1 and thus cor.final is an empty matrix.


# In case there were variables with these correlations.

## Half of the rows are symmetric repeats of the other rows. We have to delete them.
## Remove rows with matching column
#cor.final <- cor.final[!cor.final[,1] == cor.final[,2],] 
## Sort by row using apply with MARGIN=1, transpose (t) the output, use duplicated to get the 
## logical index of duplicate rows, negate (!) to get the rows that are not duplicated, 
## and subset the dataset.
#cor.final <- cor.final[!duplicated(t(apply(cor.final, 1, sort))),]

##remove the columns of full with names in id1
#full<-full[,!(names(full) %in% cor.final$id1)]





# Compute Predictor Importance for the new features

# For categorical predictor variables: Predictor Importance = 1 - PVal 
# from Pearson Chi-square test 
# For numerical predictor variables: Predictor Importance = 1 - PVal 
# from ANOVA F-Test for Equality of Mean

train.n <- full[1:nrow(train_set),]

# initialize the predictor importance data.frame
pr.imp <- setNames(data.frame(matrix(ncol = ncol(train.n), nrow = 1)), colnames(train.n))
drops <- c("Loan_ID","Loan_Status")
pr.imp<-pr.imp[ , !(names(pr.imp) %in% drops)]


# Predictor Importance for categorical variables
pr.imp$Gender <- 1-chisq.test(train.n$Loan_Status, train.n$Gender)$p.value 
pr.imp$Married <- 1-chisq.test(train.n$Loan_Status, train.n$Married)$p.value 
pr.imp$Dependents <- 1-chisq.test(train.n$Loan_Status, train.n$Dependents)$p.value 
pr.imp$Education <- 1-chisq.test(train.n$Loan_Status, train.n$Education)$p.value 
pr.imp$Self_Employed <- 1-chisq.test(train.n$Loan_Status, train.n$Self_Employed)$p.value 
pr.imp$Credit_History <- 1-chisq.test(train.n$Loan_Status, as.factor(train.n$Credit_History))$p.value 
pr.imp$Property_Area <- 1-chisq.test(train.n$Loan_Status, train.n$Property_Area)$p.value 
pr.imp$Loan_Amount_Term <- 1-chisq.test(train.n$Loan_Status,train.n$Loan_Amount_Term)$p.value
pr.imp$Coapplicant <- 1-chisq.test(train.n$Loan_Status,train.n$Coapplicant)$p.value
pr.imp$DFamilySize <- 1-chisq.test(train.n$Loan_Status,train.n$DFamilySize)$p.value

# Predictor Importance for numerical variables
aov.out <- aov(Loan_by_TotalIncome ~ Loan_Status, data=train.n)
pr.imp$Loan_by_TotalIncome <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 
aov.out <- aov(LogLoanAmount ~ Loan_Status, data=train.n)
pr.imp$LogLoanAmount <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 
aov.out <- aov(LogTotalIncome ~ Loan_Status, data=train.n)
pr.imp$LogTotalIncome <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 
aov.out <- aov(LogFamilyIncome ~ Loan_Status, data=train.n)
pr.imp$LogFamilyIncome <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 
aov.out <- aov(LogLoan_by_FamilyIncome ~ Loan_Status, data=train.n)
pr.imp$LogLoan_by_FamilyIncome <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 
aov.out <- aov(LogLoanPerMonth ~ Loan_Status, data=train.n)
pr.imp$LogLoanPerMonth <- 1-summary(aov.out)[[1]][["Pr(>F)"]][[1]] 



# Sort the data frame from the variable with the highest importance to the variable with the 
# lowest importance
pr.imp <- pr.imp[order(pr.imp, decreasing=TRUE)]

# Credit_History          1
# Property_Area           0.997864
# Married                 0.9602594
# Education               0.9569004
# Loan_by_TotalIncome     0.956683
# Coapplicant             0.9240891
# Loan_Amount_Term        0.8694148
# Dependents              0.7148474
# DFamilySize             0.651103
# LogLoanAmount           0.6320589
# LogFamilyIncome         0.5625994
# LogLoanPerMonth         0.5038477
# LogTotalIncome          0.1420932
# LogLoan_by_FamilyIncome 0.09906405
# Gender                  0.04015181   
# Self_Employed           2.553513e-15




# From the above analysis, we see that Credit_History, Property_Area, Married and
# Education and the new features Loan_by_TotalIncome and Coapplicant are the most significant 
# predictors.






#### PREDICTION


# Split the data back into the original training and test sets
train_new <- full[1:nrow(train_set), ]
test_new <- full[(nrow(train_set)+1) : nrow(full), ]

# Convert Loan_Status to factor
train_new$Loan_Status <- factor(train_new$Loan_Status)    



#####      Decision Trees

#install.packages("rpart")
# library(rpart)    # it was loaded earlier


#install.packages("e1071")
library(e1071)


#install.packages("caret")
library("caret")


# Model 1

# Tune the model to find the optimal parameter values

# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant")
outcomeName <- 'Loan_Status'


# Search for an optimal value of minsplit (minimum number of observations in a node)
# and cp (complexity parameter)

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(predictors, collapse= " + " )))
rpart.tune <- tune.rpart(my.formula, data=train_new, minsplit = c(5,10,15,20,30), 
                         cp = (1:50)*0.0001, maxdepth = 5:15)


#rpart.tune <- tune.rpart(my.formula, data=train_new, minsplit = c(5,10,15,20,30), 
#                         cp = c(0.005,0.01,0.02,0.03,0.04,0.1, 0.2,0.3,0.4,0.5 ),
#                         maxdepth = 5:15)

# Print the model
print(rpart.tune)

# Get the best model
model.rpart  <- rpart.tune$best.model



# Plot the tree to see how the classification tree looks
#install.packages("rattle")
library(rattle)
# Plot decision tree
fancyRpartPlot(model.rpart) 

# Credit_History governs the decision tree due to its greedy nature.



# Performance on the training set
rpart_train_predict <- predict(model.rpart, newdata=train_new,type="class")
rpart_train_predict.t <- table(train_new$Loan_Status, rpart_train_predict)

# Model Accuracy
rpart_train_accuracy <- (rpart_train_predict.t[1, 1] + rpart_train_predict.t[2, 2]) / sum(rpart_train_predict.t)

# Print Accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", rpart_train_accuracy)
## Model 1 - Accuracy on Training Set:  0.8224756


# Performance on the test set
rpart_test_predict <- predict(model.rpart, newdata=test_new,type="class")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = rpart_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "dec_tree1.csv")
# Accuracy on the public leaderboard: 0.78472




# or second approach

## Set the seed to ensure reproduceability
#set.seed(50)

## Define the predictors and outcome
#predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
#              "Coapplicant")
#outcomeName<-'Loan_Status'

## Fit the model
#model.rpart <- train(x = train_new[,predictors], y = train_new[,outcomeName], 
#                     method = "rpart", metric = "Accuracy")
## Print the model
#print(model.rpart)


## Plot the tree to see how the classification tree looks
##install.packages("rattle")
#library(rattle)
## Plot decision tree
#fancyRpartPlot(model.rpart$finalModel) 

## Credit_History governs the decision tree due to its greedy nature.

## Performance on the training set
#rpart_train_predict <- predict(model.rpart, newdata=train_new)
#rpart_train_predict.t <- table(train_new$Loan_Status, rpart_train_predict)

## Model Accuracy
#rpart_train_accuracy <- (rpart_train_predict.t[1, 1] + rpart_train_predict.t[2, 2]) / sum(rpart_train_predict.t)

## Print Accuracy in Prediction
#cat("Model", 1, "- Accuracy on Training Set: ", rpart_train_accuracy)
## Model 1 - Accuracy on Training Set:  0.8094463



## Performance on the test set
#rpart_test_predict <- predict(model.rpart, newdata=test_new)
## Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
#my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = rpart_test_predict)
## Write the solution to a csv file for submission in Analytics Vidhya
#write.csv(my_solution, file =  "dec_tree1.csv")
## Accuracy on the public leaderboard: 0.77778



# Model 2


# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant", "Loan_Amount_Term", "Dependents", "DFamilySize" , "LogLoanAmount",
              "LogFamilyIncome", "LogLoanPerMonth")

outcomeName <- 'Loan_Status'


# Search for an optimal value of minsplit (minimum number of observations in a node)
# and cp (complexity parameter)

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(predictors, collapse= " + " )))
rpart.tune <- tune.rpart(my.formula, data=train_new, minsplit = c(5,10,15,20,30), 
                         cp = (1:50)*0.0001, maxdepth = 5:15)

# Print the model
print(rpart.tune)

# Get the best model
model.rpart  <- rpart.tune$best.model

# Plot the tree to see how the classification tree looks
fancyRpartPlot(model.rpart) 

# Credit_History governs the decision tree due to its greedy nature.


# Performance on the training set
rpart_train_predict <- predict(model.rpart, newdata=train_new, type="class")
rpart_train_predict.t <- table(train_new$Loan_Status, rpart_train_predict)

# Model Accuracy
rpart_train_accuracy <- (rpart_train_predict.t[1, 1] + rpart_train_predict.t[2, 2]) / sum(rpart_train_predict.t)

# Print Accuracy in Prediction
cat("Model", 2, "- Accuracy on Training Set: ", rpart_train_accuracy)
## Model 2 - Accuracy on Training Set:  0.8306189


# Performance on the test set
rpart_test_predict <- predict(model.rpart, newdata=test_new,type="class")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = rpart_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "dec_tree2.csv")
# Accuracy on the public leaderboard: 0.79167




# Model 3


# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Gender","Married",  "Dependents", "Education", "Self_Employed",
              "Loan_Amount_Term", "Credit_History", "Property_Area", "Loan_by_TotalIncome",
              "Coapplicant", "DFamilySize", "LogLoanAmount", "LogTotalIncome" , 
              "LogFamilyIncome", "LogLoan_by_FamilyIncome", "LogLoanPerMonth")

outcomeName <- 'Loan_Status'

# Search for an optimal value of minsplit (minimum number of observations in a node)
# and cp (complexity parameter)

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(predictors, collapse= " + " )))
rpart.tune <- tune.rpart(my.formula, data=train_new, minsplit = c(5,10,15,20,30), 
                         cp = (1:50)*0.0001, maxdepth = 5:15)

# Print the model
print(rpart.tune)

# Get the best model
model.rpart  <- rpart.tune$best.model

# Plot the tree to see how the classification tree looks
fancyRpartPlot(model.rpart) 

# Credit_History governs the decision tree due to its greedy nature.



# Performance on the training set
rpart_train_predict <- predict(model.rpart, newdata=train_new,type="class")
rpart_train_predict.t <- table(train_new$Loan_Status, rpart_train_predict)

# Model Accuracy
rpart_train_accuracy <- (rpart_train_predict.t[1, 1] + rpart_train_predict.t[2, 2]) / sum(rpart_train_predict.t)

# Print Accuracy in Prediction
cat("Model", 3, "- Accuracy on Training Set: ", rpart_train_accuracy)
## Model 3 - Accuracy on Training Set:  0.8241042



# Performance on the test set
rpart_test_predict <- predict(model.rpart, newdata=test_new,type="class")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = rpart_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "dec_tree3.csv")
# Accuracy on the public leaderboard: 0.75694







#####      Random Forest

# Tune the model to find the optimal parameter values

#install.packages("e1071")
# library(e1071)

#install.packages("randomForest")
library(randomForest)

# Set the seed to ensure reproduceability
set.seed(50)


# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant")

outcomeName <- 'Loan_Status'

# Search for an optimal value of mtry (number of variables used at each split of the tree)
# and ntree (number of trees)


my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(predictors, collapse= " + " )))
rforest.tune<-tune.randomForest(my.formula, data=train_new,
                                mtry=2:7, ntree=c(500,1000,1500,2000), nodesize = 1:10,
                                importance=TRUE)

#rforest.tune<-tune.randomForest(my.formula, data=train_new,
#                                mtry=c(2,7), ntree=c(500,1000,1500,2000), importance=TRUE)

# Print the model
print(rforest.tune)

# Get the best model
model.rforest <- rforest.tune$best.model


# Look at relative variable importance by plotting the mean decrease in Gini calculated 
# across all trees
# Get importance
importance <- model.rforest$importance

varImportance <- data.frame(Variables=row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
       mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
       geom_bar(stat='identity') + 
       geom_text(aes(x = Variables, y = 0.5, label = Rank),
                 hjust=0, vjust=0.55, size = 4, colour="white") +
       labs(x = 'Variables' ,title='Variable Importance') +
       coord_flip() 



# Performance on the training set
rf_model_train_predict <- predict(model.rforest, newdata=train_new)
rf_model_train_predict.t <- table(train_new$Loan_Status, rf_model_train_predict)

# Model Accuracy
rf_model_train_accuracy <- (rf_model_train_predict.t[1, 1] + rf_model_train_predict.t[2, 2]) / sum(rf_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", rf_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8501629



# Performance on the test set
rf_model_test_predict <- predict(model.rforest, newdata=test_new)
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = rf_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "rf1.csv")
# Accuracy on the public leaderboard: 0.79167



# or with train from caret package

## Set the seed to ensure reproduceability
#set.seed(50)

## Define the predictors and outcome
#predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
#              "Coapplicant")

## Fit the model
#model.rforest <- train(x = train_new[,predictors], y = train_new[,outcomeName], 
#                     method = "rf", importance=TRUE,  metric = "Accuracy")
## Print the model
#print(model.rforest)



## Look at relative variable importance by plotting the mean decrease in Gini calculated 
## across all trees
## Get importance
#ImpMeasure<-data.frame(varImp(model.rforest, scale = FALSE)$importance)
#varImportance <- data.frame(Variables=row.names(ImpMeasure), 
#                            Importance = round(ImpMeasure$N,2))

## Create a rank variable based on importance
#rankImportance <- varImportance %>%
#       mutate(Rank = paste0('#',dense_rank(desc(Importance))))
## Negative importance values suggest that the variable can have a detrimental impact 
## on the classification.

## Use ggplot2 to visualize the relative importance of variables
#ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
##                           y = Importance, fill = Importance)) +
##       geom_bar(stat='identity') + 
##       geom_text(aes(x = Variables, y = 0.5, label = Rank),
##                 hjust=0, vjust=0.55, size = 4, colour="white") +
##       labs(x = 'Variables' ,title='Variable Importance') +
##       coord_flip() 




## Performance on the training set
#rf_model_train_predict <- predict(model.rforest, newdata=train_new)
#rf_model_train_predict.t <- table(train_new$Loan_Status, rf_model_train_predict)

## Model Accuracy
#rf_model_train_accuracy <- (rf_model_train_predict.t[1, 1] + rf_model_train_predict.t[2, 2]) / sum(rf_model_train_predict.t)

## Print accuracy in Prediction
#cat("Model", 1, "- Accuracy on Training Set: ", rf_model_train_accuracy)
## Model 1 - Accuracy on Training Set:  0.8469055






# Model 2


# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant", "Loan_Amount_Term", "Dependents", "DFamilySize" , "LogLoanAmount",
              "LogFamilyIncome", "LogLoanPerMonth")
outcomeName <- 'Loan_Status'

# Search for an optimal value of mtry (number of variables used at each split of the tree)
# and ntree (number of trees)

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(predictors, collapse= " + " )))
rforest.tune<-tune.randomForest(my.formula, data=train_new,
                                mtry=2:7, ntree=c(500,1000,1500,2000), nodesize = 1:10,
                                importance=TRUE)
# Print the model
print(rforest.tune)

# Get the best model
model.rforest <- rforest.tune$best.model


# Look at relative variable importance by plotting the mean decrease in Gini calculated 
# across all trees
# Get importance
importance <- model.rforest$importance

varImportance <- data.frame(Variables=row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
       mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
       geom_bar(stat='identity') + 
       geom_text(aes(x = Variables, y = 0.5, label = Rank),
                 hjust=0, vjust=0.55, size = 4, colour="white") +
       labs(x = 'Variables' ,title='Variable Importance') +
       coord_flip() 



# Performance on the training set
rf_model_train_predict <- predict(model.rforest, newdata=train_new)
rf_model_train_predict.t <- table(train_new$Loan_Status, rf_model_train_predict)

# Model Accuracy
rf_model_train_accuracy <- (rf_model_train_predict.t[1, 1] + rf_model_train_predict.t[2, 2]) / sum(rf_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 2, "- Accuracy on Training Set: ", rf_model_train_accuracy)
# Model 2 - Accuracy on Training Set:  0.8566775



# Performance on the test set
rf_model_test_predict <- predict(model.rforest, newdata=test_new)
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = rf_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "rf2.csv")
# Accuracy on the public leaderboard: 0.78472






# Model 3


# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Gender","Married",  "Dependents", "Education", "Self_Employed",
              "Loan_Amount_Term", "Credit_History", "Property_Area", "Loan_by_TotalIncome",
              "Coapplicant", "DFamilySize", "LogLoanAmount", "LogTotalIncome" , 
              "LogFamilyIncome", "LogLoan_by_FamilyIncome", "LogLoanPerMonth")

outcomeName <- 'Loan_Status'

# Search for an optimal value of mtry (number of variables used at each split of the tree)
# and ntree (number of trees)

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(predictors, collapse= " + " )))
rforest.tune<-tune.randomForest(my.formula, data=train_new,
                                mtry=2:7, ntree=c(500,1000,1500,2000), nodesize = 1:10,
                                importance=TRUE)

# Print the model
print(rforest.tune)

# Get the best model
model.rforest <- rforest.tune$best.model


# Look at relative variable importance by plotting the mean decrease in Gini calculated 
# across all trees
# Get importance
importance <- model.rforest$importance

varImportance <- data.frame(Variables=row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
       mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
       geom_bar(stat='identity') + 
       geom_text(aes(x = Variables, y = 0.5, label = Rank),
                 hjust=0, vjust=0.55, size = 4, colour="white") +
       labs(x = 'Variables' ,title='Variable Importance') +
       coord_flip() 



# Performance on the training set
rf_model_train_predict <- predict(model.rforest, newdata=train_new)
rf_model_train_predict.t <- table(train_new$Loan_Status, rf_model_train_predict)

# Model Accuracy
rf_model_train_accuracy <- (rf_model_train_predict.t[1, 1] + rf_model_train_predict.t[2, 2]) / sum(rf_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 3, "- Accuracy on Training Set: ", rf_model_train_accuracy)
# Model 3 - Accuracy on Training Set:  0.8745928



# Performance on the test set
rf_model_test_predict <- predict(model.rforest, newdata=test_new)
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = rf_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "rf3.csv")
# Accuracy on the public leaderboard: 0.79167





#####      Forest of conditional inference trees


# install.packages('party')
library(party)


# Tune the model to find the optimal parameter values

# Model 1


# Set the seed to ensure reproduceability
set.seed(50)


# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant")

outcomeName <- 'Loan_Status'

## Build a Forest of conditional inference trees with cforest to predict Loan_Status
#my.formula <- as.formula(paste(paste(factor(outcomeName))," ~ ", paste(predictors, collapse= " + " )))
#model.cforest <- cforest(my.formula, data = train_new, 
#                         controls=cforest_unbiased(ntree=1000, mtry=2))

# Fit the model
model.cforest <- train(x = train_new[,predictors], y = train_new[,outcomeName], 
                     method = "cforest",  metric = "Accuracy")
# Print the model
print(model.cforest)

       
# Performance on the training set
cf_model_train_predict <- predict(model.cforest, newdata=train_new, OOB=TRUE)
cf_model_train_predict.t <- table(train_new$Loan_Status, cf_model_train_predict)


# Model Accuracy
cf_model_train_accuracy <- (cf_model_train_predict.t[1, 1] + cf_model_train_predict.t[2, 2]) / sum(cf_model_train_predict.t)

# Print accuracy in Prediction
# Model has the number i+1, since the first model was created outside of this loop
cat("Model", 1, "- Accuracy on Training Set: ", cf_model_train_accuracy)
## Model 1 - Accuracy on Training Set:  0.8094463


# Performance on the test set
cf_model_test_predict <- predict(model.cforest, newdata=test_new, OOB=TRUE)
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = cf_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "c_forest1.csv")
# Accuracy on the public leaderboard: 0.77778




# Model 2


# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant", "Loan_Amount_Term", "Dependents", "DFamilySize" , "LogLoanAmount",
              "LogFamilyIncome", "LogLoanPerMonth")

outcomeName <- 'Loan_Status'

# Fit the model
model.cforest <- train(x = train_new[,predictors], y = train_new[,outcomeName], 
                       method = "cforest",  metric = "Accuracy")
# Print the model
print(model.cforest)


# Performance on the training set
cf_model_train_predict <- predict(model.cforest, newdata=train_new, OOB=TRUE)
cf_model_train_predict.t <- table(train_new$Loan_Status, cf_model_train_predict)


# Model Accuracy
cf_model_train_accuracy <- (cf_model_train_predict.t[1, 1] + cf_model_train_predict.t[2, 2]) / sum(cf_model_train_predict.t)

# Print accuracy in Prediction
# Model has the number i+1, since the first model was created outside of this loop
cat("Model", 2, "- Accuracy on Training Set: ", cf_model_train_accuracy)
## Model 2 - Accuracy on Training Set:  0.8094463



# Performance on the test set
cf_model_test_predict <- predict(model.cforest, newdata=test_new, OOB=TRUE)
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = cf_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "c_forest2.csv")
# Accuracy on the public leaderboard: 0.77778






# Model 3


# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Gender","Married",  "Dependents", "Education", "Self_Employed",
              "Loan_Amount_Term", "Credit_History", "Property_Area", "Loan_by_TotalIncome",
              "Coapplicant", "DFamilySize", "LogLoanAmount", "LogTotalIncome" , 
              "LogFamilyIncome", "LogLoan_by_FamilyIncome", "LogLoanPerMonth")

outcomeName <- 'Loan_Status'


# Fit the model
model.cforest <- train(x = train_new[,predictors], y = train_new[,outcomeName], 
                       method = "cforest",  metric = "Accuracy")
# Print the model
print(model.cforest)

# Performance on the training set
cf_model_train_predict <- predict(model.cforest, newdata=train_new, OOB=TRUE)
cf_model_train_predict.t <- table(train_new$Loan_Status, cf_model_train_predict)


# Model Accuracy
cf_model_train_accuracy <- (cf_model_train_predict.t[1, 1] + cf_model_train_predict.t[2, 2]) / sum(cf_model_train_predict.t)

# Print accuracy in Prediction
# Model has the number i+1, since the first model was created outside of this loop
cat("Model", 3, "- Accuracy on Training Set: ", cf_model_train_accuracy)
## Model 3 - Accuracy on Training Set:  0.8094463



# Performance on the test set
cf_model_test_predict <- predict(model.cforest, newdata=test_new, OOB=TRUE)
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = cf_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "c_forest3.csv")
# Accuracy on the public leaderboard: 0.77778







#####      Logistic Regression
##    Generalized Linear Models (GLMs)


#install.packages("stats")
library(stats)



# Model 1

# Set the seed to ensure reproduceability
set.seed(50)


# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant")
outcomeName <- 'Loan_Status'

# Build a Generalized Linear Model with glm to predict Loan_StatuS

# In the glm function, the command "family = binomial" tells R to fit a 
# logistic regression model.
# By specifying binomial(link = "logit"), we declare a binary outcome and that we are estimating
# a logit, rather than probit, model.

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(predictors, collapse= " + " )))

# Build a Generalized Linear Model with glm to predict Loan_Status 
model.glm <- glm(my.formula, data = train_new, family=binomial(link = "logit"))



# Performance on the training set

# By specifying type="response", we clarify that we want our predictions to be on the 
# probability scale.
# We then ask if each probability is greater than 0.5, and by wrapping all of this in the
# as.numeric command, we count all probabilities above 0.5 as predicted values of 1 and all
# that are less than 0.5 as predicted values of 0
glm_model_train_predict <- predict(model.glm , newdata=train_new, type = "response")
glm_model_train_predict <- as.numeric(as.numeric(glm_model_train_predict)>0.5)
glm_model_train_predict.t <- table(train_new$Loan_Status, glm_model_train_predict)


# Model Accuracy
glm_model_train_accuracy <- (glm_model_train_predict.t[1, 1] + glm_model_train_predict.t[2, 2]) / sum(glm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", glm_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8094463



# Performance on the test set
glm_model_test_predict <- predict(model.glm, newdata=test_new, type = "response")
glm_model_test_predict <-  ifelse(glm_model_test_predict > 0.5,"Y","N")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = glm_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "glm1.csv")
# Accuracy on the public leaderboard: 0.77778





# Model 2

# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant", "Loan_Amount_Term", "Dependents", "DFamilySize" , "LogLoanAmount",
              "LogFamilyIncome", "LogLoanPerMonth")
outcomeName <- 'Loan_Status'

# If we train a model with Loan_Amount_Term variable, we get an error, because in the test set
# the (factor) variable Loan_Amount_Term has new levels that are not present in the training set.

# To avoid this, one would like to detect novel levels in new data and encode them in a way that
# the model can understand. This task is much easier when representing categorical variables
# as indicators.
# In vtreat, the procedure is as follows:


train_new2<-train_new
test_new2<-test_new
# convert Loan_Status levels to 0 and 1
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="N"] <- 0
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="Y"] <- 1
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="N"] <- 0
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="Y"] <- 1

train_new2$Loan_Status <- as.numeric(train_new2$Loan_Status)
test_new2$Loan_Status <- as.numeric(test_new2$Loan_Status)

#install.packages("vtreat")
library("vtreat")

# The function designTreatmentsN() takes as input the data frame of training data, 
# the list of input columns, and the (numerical) outcome column.
treatments <- designTreatmentsN(train_new2, varlist= predictors, outcomename = outcomeName)
                                        
                               
# Once we have created the treatment plans using designTreatmentsN(), we can treat the 
# training and test data frames using the function prepare(). 
# This creates new data frames that express the outcome in terms of the new transformed 
# variables. prepare() takes as input a list of treatment plans and a data set to be treated. 
train.treat <- prepare(treatments, train_new2, pruneSig=NULL)
test.treat <- prepare(treatments, test_new2, pruneSig=NULL)

# Now we can fit a model using the transformed variables:

# get the names of the variables
vars <- setdiff(colnames(train.treat), outcomeName)

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(vars, collapse= " + " )))


# In the glm function, the command "family = binomial" tells R to fit a 
# logistic regression model
# By specifying binomial(link = "logit"), we declare a binary outcome and that we are estimating
# a logit, rather than probit, model


# Build a Generalized Linear Model with glm to predict Loan_Status 
model.glm <- glm(my.formula, data = train.treat, family=binomial(link = "logit"))



# Performance on the training set

# By specifying type="response", we clarify that we want our predictions to be on the 
# probability scale.
# We then ask if each probability is greater than 0.5, and by wrapping all of this in the
# as.numeric command, we count all probabilities above 0.5 as predicted values of 1 and all
# that are less than 0.5 as predicted values of 0
glm_model_train_predict <- predict(model.glm , newdata=train.treat, type = "response")
glm_model_train_predict <- as.numeric(as.numeric(glm_model_train_predict)>0.5)
glm_model_train_predict.t <- table(train_new2$Loan_Status, glm_model_train_predict)


# Model Accuracy
glm_model_train_accuracy <- (glm_model_train_predict.t[1, 1] + glm_model_train_predict.t[2, 2]) / sum(glm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 2, "- Accuracy on Training Set: ", glm_model_train_accuracy)
# Model 2 - Accuracy on Training Set:  0.8175896



# Performance on the test set
glm_model_test_predict <- predict(model.glm, newdata=test.treat, type = "response")
glm_model_test_predict <-  ifelse(glm_model_test_predict > 0.5,"Y","N")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new2$Loan_ID, Loan_Status = glm_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "glm2.csv")
# Accuracy on the public leaderboard: 0.77083




# Model 3

# Set the seed to ensure reproduceability
set.seed(50)

# Define the predictors and outcome
predictors<-c("Gender","Married",  "Dependents", "Education", "Self_Employed",
              "Loan_Amount_Term", "Credit_History", "Property_Area", "Loan_by_TotalIncome",
              "Coapplicant", "DFamilySize", "LogLoanAmount", "LogTotalIncome" , 
              "LogFamilyIncome", "LogLoan_by_FamilyIncome", "LogLoanPerMonth")
outcomeName <- 'Loan_Status'



# If we train a model with Loan_Amount_Term variable, we get an error, because in the test set
# the (factor) variable Loan_Amount_Term has new levels that are not present in the training set.

# To avoid this, one would like to detect novel levels in new data and encode them in a way that
# the model can understand. This task is much easier when representing categorical variables
# as indicators.
# In vtreat, the procedure is as follows:


# The function designTreatmentsN() takes as input the data frame of training data, 
# the list of input columns, and the (numerical) outcome column.
treatments <- designTreatmentsN(train_new2, varlist=predictors, outcomename = outcomeName)

# Once we have created the treatment plans using designTreatmentsN(), we can treat the 
# training and test data frames using the function prepare(). 
# This creates new data frames that express the outcome in terms of the new transformed 
# variables. prepare() takes as input a list of treatment plans and a data set to be treated. 
train.treat <- prepare(treatments, train_new2, pruneSig=NULL)
test.treat <- prepare(treatments, test_new2, pruneSig=NULL)

# Now we can fit a model using the transformed variables:

# get the names of the variables
vars <- setdiff(colnames(train.treat), outcomeName)

my.formula <- as.formula(paste(paste(outcomeName)," ~ ", paste(vars, collapse= " + " )))



# In the glm function, the command "family = binomial" tells R to fit a 
# logistic regression model
# By specifying binomial(link = "logit"), we declare a binary outcome and that we are estimating
# a logit, rather than probit, model


# Build a Generalized Linear Model with glm to predict Loan_Status 
model.glm <- glm(my.formula, data = train.treat, 
                 family=binomial(link = "logit"))



# Performance on the training set

# By specifying type="response", we clarify that we want our predictions to be on the 
# probability scale.
# We then ask if each probability is greater than 0.5, and by wrapping all of this in the
# as.numeric command, we count all probabilities above 0.5 as predicted values of 1 and all
# that are less than 0.5 as predicted values of 0
glm_model_train_predict <- predict(model.glm , newdata=train.treat, type = "response")
glm_model_train_predict <- as.numeric(as.numeric(glm_model_train_predict)>0.5)
glm_model_train_predict.t <- table(train_new2$Loan_Status, glm_model_train_predict)


# Model Accuracy
glm_model_train_accuracy <- (glm_model_train_predict.t[1, 1] + glm_model_train_predict.t[2, 2]) / sum(glm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 3, "- Accuracy on Training Set: ", glm_model_train_accuracy)
# Model 3 - Accuracy on Training Set:  0.8159609



# Performance on the test set
glm_model_test_predict <- predict(model.glm, newdata=test.treat, type = "response")
glm_model_test_predict <-  ifelse(glm_model_test_predict > 0.5,"Y","N")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new2$Loan_ID, Loan_Status = glm_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "glm3.csv")
# Accuracy on the public leaderboard: 0.77083




#####      Gradient Boosting Machine (GBM)


# Tune the model to find the optimal parameter values

# Model 1

# Set the seed to ensure reproduceability
set.seed(50)


# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant")
outcomeName <- 'Loan_Status'

# To find the parameters of a model that can be tuned, we can use:

modelLookup(model='gbm')
#   model         parameter                   label forReg forClass probModel
# 1   gbm           n.trees   # Boosting Iterations   TRUE     TRUE      TRUE
# 2   gbm interaction.depth          Max Tree Depth   TRUE     TRUE      TRUE
# 3   gbm         shrinkage               Shrinkage   TRUE     TRUE      TRUE
# 4   gbm    n.minobsinnode Min. Terminal Node Size   TRUE     TRUE      TRUE

# Instead, of specifying the exact values for each parameter for tuning we can simply ask 
# it to use any number of possible values for each tuning parameter through tuneLength. 


# Fit the model using tune length
model.gbm <- train(x = train_new[,predictors], y = factor(train_new[,outcomeName]), 
                   method = "gbm", tuneLength=20, metric = "Accuracy", verbose=FALSE)
# Print the model
print(model.gbm)

# Find variable importance for GBM
varImp(object=model.gbm)

# Plot Variable importance for GBM
plot(varImp(object=model.gbm),main="GBM - Variable Importance")

# Performance on the training set
# By specifying type="prob", we clarify that we want our predictions to be on the 
# probability scale.
# We then ask if each probability is greater than 0.5, and we count all probabilities above 
# 0.5 as predicted values of Y and all that are less than 0.5 as predicted values of N
gbm_model_train_predict <- predict(model.gbm, newdata=train_new,type="prob")$Y
gbm_model_train_predict <- as.factor(ifelse(gbm_model_train_predict>0.5,'Y','N'))
gbm_model_train_predict.t <- table(train_new$Loan_Status, gbm_model_train_predict)


# Model Accuracy
gbm_model_train_accuracy <- (gbm_model_train_predict.t[1, 1] + gbm_model_train_predict.t[2, 2]) / sum(gbm_model_train_predict.t)


# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", gbm_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.7068404



# Performance on the test set
gbm_model_test_predict <- predict(model.gbm, n.trees=model.gbm$bestTune$n.trees, 
                                  newdata=test_new,  type='prob')$Y
gbm_model_test_predict <- as.factor(ifelse(gbm_model_test_predict > 0.5,"Y","N"))
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = gbm_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "gbm1.csv")
# Accuracy on the public leaderboard: 0.74306







#####      Support Vector Machines (SVM) with Radial Basis Function (RBF) Kernel 


# Tune the model to find the optimal parameter values


# Model 1


# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant")
outcomeName <- 'Loan_Status'

# We will tune the model to find the optimal parameter values

# Set the seed to ensure reproduceability
set.seed(50)


# Search for an optimal value of gamma (defines how far the influence of a single 
# training example reaches, with low values meaning 'far' and high values meaning 'close')
# and cost (regularization term that controls the complexity of the model. 
# A high cost value will force the SVM to create a complex enough prediction function
# to missclassify as few training points as possible, while a lower cost parameter will lead
# to a simpler prediction function. )

my.formula <- as.formula(paste(paste(factor(outcomeName))," ~ ", paste(predictors, collapse= " + " )))

fit.tune <- tune.svm(my.formula, data=train_new, kernel="radial",
                     gamma=10^(-2:2),cost=10^(-2:4), probability = TRUE)
# Get the best model                 
model.svm <- fit.tune$best.model                 


# Performance on the training set
svm_model_train_predict <- predict(model.svm, newdata=train_new, type="class")
svm_model_train_predict.t <- table(train_new$Loan_Status, svm_model_train_predict)

# Model Accuracy
svm_model_train_accuracy <- (svm_model_train_predict.t[1, 1] + svm_model_train_predict.t[2, 2]) / sum(svm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", svm_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8517915



# Performance on the test set
svm_model_test_predict <- predict(model.svm, newdata=test_new[,-10], type="class")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = svm_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "svm1.csv")
# Accuracy on the public leaderboard: 0.79861






# Model 2

# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant", "Loan_Amount_Term", "Dependents", "DFamilySize" , "LogLoanAmount",
              "LogFamilyIncome", "LogLoanPerMonth")
outcomeName <- 'Loan_Status'


# Set the seed to ensure reproduceability
set.seed(50)


# Search for an optimal value of gamma (defines how far the influence of a single 
# training example reaches, with low values meaning 'far' and high values meaning 'close')
# and cost (regularization term that controls the complexity of the model. 
# A high cost value will force the SVM to create a complex enough prediction function
# to missclassify as few training points as possible, while a lower cost parameter will lead
# to a simpler prediction function. )


my.formula <- as.formula(paste(paste(factor(outcomeName))," ~ ", paste(predictors, collapse= " + " )))

fit.tune <- tune.svm(my.formula, data=train_new, kernel="radial",
                     gamma=10^(-2:2),cost=10^(-2:4), probability = TRUE)
# Get the best model                 
model.svm <- fit.tune$best.model                 


# Performance on the training set
svm_model_train_predict <- predict(model.svm, newdata=train_new, type="class")
svm_model_train_predict.t <- table(train_new$Loan_Status, svm_model_train_predict)

# Model Accuracy
svm_model_train_accuracy <- (svm_model_train_predict.t[1, 1] + svm_model_train_predict.t[2, 2]) / sum(svm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 2, "- Accuracy on Training Set: ", svm_model_train_accuracy)
# Model 2 - Accuracy on Training Set:  0.8241042



# Performance on the test set
svm_model_test_predict <- predict(model.svm, newdata=test_new[,-10], type="class")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = svm_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "svm2.csv")
# Accuracy on the public leaderboard: 0.78472





# Model 3

# Define the predictors and outcome
predictors<-c("Gender","Married",  "Dependents", "Education", "Self_Employed",
              "Loan_Amount_Term", "Credit_History", "Property_Area", "Loan_by_TotalIncome",
              "Coapplicant", "DFamilySize", "LogLoanAmount", "LogTotalIncome" , 
              "LogFamilyIncome", "LogLoan_by_FamilyIncome", "LogLoanPerMonth")
outcomeName <- 'Loan_Status'


# Set the seed to ensure reproduceability
set.seed(50)


# Search for an optimal value of gamma (defines how far the influence of a single 
# training example reaches, with low values meaning 'far' and high values meaning 'close')
# and cost (regularization term that controls the complexity of the model. 
# A high cost value will force the SVM to create a complex enough prediction function
# to missclassify as few training points as possible, while a lower cost parameter will lead
# to a simpler prediction function. )


my.formula <- as.formula(paste(paste(factor(outcomeName))," ~ ", paste(predictors, collapse= " + " )))

fit.tune <- tune.svm(my.formula, data=train_new, kernel="radial",
                     gamma=10^(-2:2),cost=10^(-2:4), probability = TRUE)
# Get the best model                 
model.svm <- fit.tune$best.model                 


# Performance on the training set
svm_model_train_predict <- predict(model.svm, newdata=train_new, type="class")
svm_model_train_predict.t <- table(train_new$Loan_Status, svm_model_train_predict)

# Model Accuracy
svm_model_train_accuracy <- (svm_model_train_predict.t[1, 1] + svm_model_train_predict.t[2, 2]) / sum(svm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 3, "- Accuracy on Training Set: ", svm_model_train_accuracy)
# Model 3 - Accuracy on Training Set:  0.8110749



# Performance on the test set
svm_model_test_predict <- predict(model.svm, newdata=test_new[,-10], type="class")
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = svm_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "svm3.csv")
# Accuracy on the public leaderboard: 0.77778






#####      kNN (k-Nearest Neighbors)


# The kNN classifier, requires all variables to be numeric. In order to be able to use kNN
# with categorical variables, we will use the knncat algorithm. In this algorithm, continuous
# variables are permitted too.

#install.packages("knncat")
library(knncat)

# Model 1


# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant")
outcomeName <- 'Loan_Status'


# We have first to transform the variables.


# Set the seed to ensure reproduceability
set.seed(50)


train_new2<-train_new
test_new2<-test_new
# convert Loan_Status levels to 0, 1 and 2
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="N"] <- 0
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="Y"] <- 1
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="N"] <- 0
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="Y"] <- 1
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="NA"] <- 2

train_new2$Loan_Status <- as.numeric(as.character(train_new2$Loan_Status))
test_new2$Loan_Status <- as.numeric(as.character(test_new2$Loan_Status))

#install.packages("vtreat")
# library("vtreat") # it was loaded earlier

# The function designTreatmentsN() takes as input the data frame of training data, 
# the list of input columns, and the (numerical) outcome column.
treatments <- designTreatmentsN(train_new2, varlist= predictors, outcomename = outcomeName)


# Once we have created the treatment plans using designTreatmentsN(), we can treat the 
# training and test data frames using the function prepare(). 
# This creates new data frames that express the outcome in terms of the new transformed 
# variables. prepare() takes as input a list of treatment plans and a data set to be treated. 
train.treat <- prepare(treatments, train_new2, pruneSig=NULL)
test.treat <- prepare(treatments, test_new2, pruneSig=NULL)


# Set the seed to ensure reproduceability
set.seed(50)

# knn.model <- knncat(train = train.treat, test = test.treat, classcol=20) 

# Fit the model
model.knn <- knncat(train = train.treat, classcol=20) #class is contained in 20th column      
# Print the model
print(model.knn)
# Training set misclass rate: 19.38%

# best.k=15 gives: Training set misclass rate: 18.24%
# best.k=9 gives: Training set misclass rate: 19.38%


# Performance on the training set
knn_model_train_predict.t <- model.knn$misclass.mat

# Model Accuracy
knn_model_train_accuracy <- (knn_model_train_predict.t[1, 1] + knn_model_train_predict.t[2, 2]) / sum(knn_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", knn_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8061889


# Performance on the test set
knn_model_test_predict <- predict(model.knn, train.treat, test.treat, train.classcol=20, newdata.classcol=20)
levels(knn_model_test_predict)[levels(knn_model_test_predict)=="0"] <- "N"
levels(knn_model_test_predict)[levels(knn_model_test_predict)=="1"] <- "Y"
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = knn_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "knn1.csv")
# Accuracy on the public leaderboard: 0.79167 (k=9)
# knn 1 k=17	0.78472
# knn 1 k=15	0.78472







# Model 2

# Define the predictors and outcome
predictors<-c("Credit_History", "Property_Area", "Married", "Education", "Loan_by_TotalIncome",
              "Coapplicant", "Loan_Amount_Term", "Dependents", "DFamilySize" , "LogLoanAmount",
              "LogFamilyIncome", "LogLoanPerMonth")
outcomeName <- 'Loan_Status'


# We have first to transform the variables.


# Set the seed to ensure reproduceability
set.seed(50)


train_new2<-train_new
test_new2<-test_new
# convert Loan_Status levels to 0, 1 and 2
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="N"] <- 0
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="Y"] <- 1
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="N"] <- 0
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="Y"] <- 1
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="NA"] <- 2

train_new2$Loan_Status <- as.numeric(as.character(train_new2$Loan_Status))
test_new2$Loan_Status <- as.numeric(as.character(test_new2$Loan_Status))

#install.packages("vtreat")
# library("vtreat") # it was loaded earlier

# The function designTreatmentsN() takes as input the data frame of training data, 
# the list of input columns, and the (numerical) outcome column.
treatments <- designTreatmentsN(train_new2, varlist= predictors, outcomename = outcomeName)


# Once we have created the treatment plans using designTreatmentsN(), we can treat the 
# training and test data frames using the function prepare(). 
# This creates new data frames that express the outcome in terms of the new transformed 
# variables. prepare() takes as input a list of treatment plans and a data set to be treated. 
train.treat <- prepare(treatments, train_new2, pruneSig=NULL)
test.treat <- prepare(treatments, test_new2, pruneSig=NULL)


# Set the seed to ensure reproduceability
set.seed(50)

# knn.model <- knncat(train = train.treat, test = test.treat, classcol=43) 
# k = c(1, 3, 5, 7, 9, 12, 15)
# Fit the model
model.knn <- knncat(train = train.treat, classcol=43) #class is contained in 43rd column      
# Print the model
print(model.knn)
# Training set misclass rate: 19.06%

# best.k=15 gives: Training set misclass rate: 19.06%
# best.k=9 gives: Training set misclass rate: 19.06%


# Performance on the training set
knn_model_train_predict.t <- model.knn$misclass.mat

# Model Accuracy
knn_model_train_accuracy <- (knn_model_train_predict.t[1, 1] + knn_model_train_predict.t[2, 2]) / sum(knn_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 2, "- Accuracy on Training Set: ", knn_model_train_accuracy)
# Model 2 - Accuracy on Training Set:  0.8094463


# Performance on the test set
knn_model_test_predict <- predict(model.knn, train.treat, test.treat, train.classcol=43, newdata.classcol=43)
levels(knn_model_test_predict)[levels(knn_model_test_predict)=="0"] <- "N"
levels(knn_model_test_predict)[levels(knn_model_test_predict)=="1"] <- "Y"
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = knn_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "knn2.csv")
# Accuracy on the public leaderboard: 0.77778 (k=9)
# knn 2 k=15	0.77778





# Model 3

# Define the predictors and outcome
predictors<-c("Gender","Married",  "Dependents", "Education", "Self_Employed",
              "Loan_Amount_Term", "Credit_History", "Property_Area", "Loan_by_TotalIncome",
              "Coapplicant", "DFamilySize", "LogLoanAmount", "LogTotalIncome" , 
              "LogFamilyIncome", "LogLoan_by_FamilyIncome", "LogLoanPerMonth")
outcomeName <- 'Loan_Status'



# We have first to transform the variables.


# Set the seed to ensure reproduceability
set.seed(50)


train_new2<-train_new
test_new2<-test_new
# convert Loan_Status levels to 0, 1 and 2
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="N"] <- 0
levels(train_new2$Loan_Status)[levels(train_new2$Loan_Status)=="Y"] <- 1
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="N"] <- 0
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="Y"] <- 1
levels(test_new2$Loan_Status)[levels(test_new2$Loan_Status)=="NA"] <- 2

train_new2$Loan_Status <- as.numeric(as.character(train_new2$Loan_Status))
test_new2$Loan_Status <- as.numeric(as.character(test_new2$Loan_Status))

#install.packages("vtreat")
# library("vtreat") # it was loaded earlier

# The function designTreatmentsN() takes as input the data frame of training data, 
# the list of input columns, and the (numerical) outcome column.
treatments <- designTreatmentsN(train_new2, varlist= predictors, outcomename = outcomeName)


# Once we have created the treatment plans using designTreatmentsN(), we can treat the 
# training and test data frames using the function prepare(). 
# This creates new data frames that express the outcome in terms of the new transformed 
# variables. prepare() takes as input a list of treatment plans and a data set to be treated. 
train.treat <- prepare(treatments, train_new2, pruneSig=NULL)
test.treat <- prepare(treatments, test_new2, pruneSig=NULL)


# Set the seed to ensure reproduceability
set.seed(50)

# knn.model <- knncat(train = train.treat, test = test.treat, classcol=49) 
# k = c(1, 3, 5, 7, 9, 12, 15)
# Fit the model
model.knn <- knncat(train = train.treat, classcol=49) #class is contained in 49th column      
# Print the model
print(model.knn)
# Training set misclass rate: 18.89%

# best.k=15 gives: Training set misclass rate: 19.06%
# best.k=9 gives: Training set misclass rate: 18.89%


# Performance on the training set
knn_model_train_predict.t <- model.knn$misclass.mat

# Model Accuracy
knn_model_train_accuracy <- (knn_model_train_predict.t[1, 1] + knn_model_train_predict.t[2, 2]) / sum(knn_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 3, "- Accuracy on Training Set: ", knn_model_train_accuracy)
# Model 3 - Accuracy on Training Set:  0.8110749


# Performance on the test set
knn_model_test_predict <- predict(model.knn, train.treat, test.treat, train.classcol=49, newdata.classcol=49)
levels(knn_model_test_predict)[levels(knn_model_test_predict)=="0"] <- "N"
levels(knn_model_test_predict)[levels(knn_model_test_predict)=="1"] <- "Y"
# Save the solution to a dataframe with two columns: Loan_ID and Loan_Status (prediction)
my_solution <- data.frame(Loan_ID = test_new$Loan_ID, Loan_Status = knn_model_test_predict)
# Write the solution to a csv file for submission in Analytics Vidhya
write.csv(my_solution, file =  "knn3.csv")
# Accuracy on the public leaderboard: 0.77778 (k=9)
# knn 3 k=15	0.77778


