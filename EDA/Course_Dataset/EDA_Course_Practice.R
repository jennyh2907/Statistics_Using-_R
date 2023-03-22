#' ---
#' title: "HW1_FOA"
#' output: 
#'   html_document: 
#'     fig_caption: yes
#' date: '2022-08-13'
#' ---
#' 
## ----setup, include=FALSE, echo=TRUE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(psych)
library(dplyr)
library(graphics)
library(crosstable)
library(corrplot)
library(plotly)

#' 
#' ### Question 1
#' 
#' #### Question1(a)
#' 
## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------
#Read in the file
StatsQ1<-read_csv("Dow.csv")

## Question (a)
# Compute the return
timeseries <- ts(StatsQ1$`Closing Values`, frequency = 1, start = 1)
Return <- timeseries/stats::lag(timeseries, - 1)

# Calculate the Geometric Mean
Geometric_Mean_Rate <- round(100*(geometric.mean(Return)-1), digits = 2)

## Print out the result
sprintf("The average Dow return over the period given is %.3f.", Geometric_Mean_Rate)

#' 
#' #### Question1(b)
#' 
#' The returns are not normally distributed, but left skewed.
#' 
## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------
#Create a histogram
Arithmetic_Mean <- mean(Return)
hist(Return)

#' 
#' #### Question1(c)
#' 
#' According to the result, the returns do approximately follow empirical rules.(68-95-99.7)
#' 
## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# Find the std dev
stddev <- sd(Return)

# Print out the result
sprintf("The std dev of proportion of defective components is %.4f.", stddev)

TotalSampleSize <- length(Return)

# One std dev bounds
One_SD_Range_Lower <- Arithmetic_Mean - 1*stddev
One_SD_Range_Upper <- Arithmetic_Mean + 1*stddev

# Proportion of observations within 1 SD
Num_Within_One_SD <- length(which(Return >= One_SD_Range_Lower & Return <= One_SD_Range_Upper))
Percent_Within_One_SD <- 100*(Num_Within_One_SD/TotalSampleSize)

# Two std dev bounds
Two_SD_Range_Lower <- Arithmetic_Mean - 2*stddev
Two_SD_Range_Upper <- Arithmetic_Mean + 2*stddev

# Proportion of observations within 2 SD
Num_Within_Two_SD <- length(which(Return >= Two_SD_Range_Lower & Return <= Two_SD_Range_Upper))
Percent_Within_Two_SD <- 100*(Num_Within_Two_SD/TotalSampleSize)

# Three std dev bounds
Three_SD_Range_Lower <- Arithmetic_Mean - 3*stddev
Three_SD_Range_Upper <- Arithmetic_Mean + 3*stddev

# Proportion of observations within 2 SD
Num_Within_Three_SD <- length(which(Return >= Three_SD_Range_Lower & Return <= Three_SD_Range_Upper))
Percent_Within_Three_SD <- 100*(Num_Within_Three_SD/TotalSampleSize)

# Print out the result and interpretation
sprintf("%.1f%% of the shipments are within 1SD from the mean.", Percent_Within_One_SD)
sprintf("%.1f%% of the shipments are within 2SD from the mean.", Percent_Within_Two_SD)
sprintf("%.1f%% of the shipments are within 3SD from the mean.", Percent_Within_Three_SD)

#' 
#' #### Question1(d)
#' 
#' The first quartile is 0.7678409. The second quartile/median is 1.0089782. The third quartile is 1.0324026.
#' 
## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# Create a boxplot
boxplot(Return, data = StatsQ1)

# Five number summary of return
fivenum(Return)

#' 
#' #### Question1(e)
#' 
## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# Calculate IQR
IQR <- IQR(Return, na.rm = TRUE)
Q1 <- 0.7678409
Q3 <- 1.0324026

# Identify mild outliers
mild_outlier <- with(StatsQ1, 
                      Return <- ifelse(Return >= Q3 + 1.5 * IQR | Return <= Q1 - 1.5 * IQR, 
                                                 "Yes", "No"))
Return_df<-data.frame(Return)
Return_df["Mild Outlier"] <- mild_outlier

# Identify extreme outliers
extreme_outlier <- with(StatsQ1, 
                             Return <- ifelse(Return >= Q3 + 3 * IQR | Return <= Q1 - 3 * IQR, 
                                                        "Yes", "No"))
Return_df["Extreme Outlier"] <- extreme_outlier

# Create a data frame
result <- Return_df %>% arrange(desc(`Mild Outlier`))
result

#' 
#' ### Question2
#' 
#' #### Question2(a)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
StatsQ2<-read.csv("Melvyl.csv")

# Make frequency distribution
table(StatsQ2$Items)
table(StatsQ2$Net.Sales)
table(StatsQ2$Method.of.Payment)
table(StatsQ2$Gender)
table(StatsQ2$Marital.Status)
table(StatsQ2$Age)

#' 
#' #### Question2(b)
#' 
#' The mean of net sales for promotional customer is significantly higher than regular ones. But the number of net sales varies more for promotional customer based on the difference in IQR
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Make a crosstable
tab <- crosstable(StatsQ2, c(Net.Sales), by = Type.of.Customer, total="both")%>%
  as_flextable()
tab

#' 
#' #### Question2(c)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Self-define a Function
summary_df <- function(x) {
  c(Count = length(x[!is.na(x)]),
    Count_NA = length(x[is.na(x)]),
    Mean = round(mean(x, na.rm = TRUE), digits = 2),
    Median = round(median(x, na.rm = TRUE), digits = 2),
    Min = round(min(x, na.rm = TRUE), digits = 2),
    Max = round(max(x, na.rm = TRUE), digits = 2),
    Range = round((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)), digits = 2),
    Variance = round(var(x, na.rm = TRUE), digits = 2),
    StdDev = round(sd(x, na.rm = TRUE), digits = 2),
    CV_in_Percent = round(((sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))*100), digits = 2))
}

# Descriptive stats on net sales
Net_Sales <- summary_df(StatsQ2$Net.Sales)

# Use different filters with two types of customer
Filter_Promo <- filter(StatsQ2, Type.of.Customer == "Promotional")
Promo <- summary_df(Filter_Promo$Net.Sales)
Filter_Reg <- filter(StatsQ2, Type.of.Customer == "Regular")
Reg <- summary_df(Filter_Reg$Net.Sales)

#Interpret the result
sprintf("Question2(c)")
# Descriptive Statistics of Net sales and Items
tab_item_net <- crosstable(StatsQ2, c(Net.Sales), by = Items, total="both")%>%
  as_flextable()
tab_item_net

# Descriptive Statistics of Net sales and Method of Payment
tab_pay_net <- crosstable(StatsQ2, c(Net.Sales), by = Method.of.Payment, total="both")%>%
  as_flextable()
tab_pay_net

# Descriptive Statistics of Net sales and Gender
tab_gender_net <- crosstable(StatsQ2, c(Net.Sales), by = Gender, total="both")%>%
  as_flextable()
tab_gender_net

# Descriptive Statistics of Net sales and Marital Status
tab_marital_net <- crosstable(StatsQ2, c(Net.Sales), by = Marital.Status, total="both")%>%
  as_flextable()
tab_marital_net

sprintf("Descriptive Statistics of Net sales")
summary(StatsQ2$Net.Sales)

#' 
#' #### Question2(d)
#' 
#' The correlation of age and net sales is close to 0, so we can say these two variables have no linear relationship to one another.
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Descriptive Stats regarding the relationship between age and net sales
Corr<-cor(StatsQ2$Age, StatsQ2$Net.Sales)

#' 
#' ### Question3
#' 
#' #### Question3(a)
#' 
#' The distribution in Jalisco is left-skewed.And the distribution in Yucatan is right-skewed.The other distributions approximately follows normal distribution.
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
StatsQ3 <- read_csv("SuperMarketTransactions.csv", 
                    col_types = cols(`Purchase Date` = col_date(format = "%m/%d/%Y"), 
        Revenue = col_number()))

# Rename the column
colnames(StatsQ3)[colnames(StatsQ3) == "State or Province"] ="State_or_Province"

# Create a boxplot
boxplot(Revenue ~ State_or_Province, data = StatsQ3, main = "Question3(a) Revenue Broken down by state or province")

#' 
#' #### Question3(b)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Choose states in USA
OnlyUS <- filter(StatsQ3, Country == "USA")
 
# Create a boxplot
boxplot(Revenue ~ State_or_Province, data = OnlyUS, main = "Question3(b) Revenue for states in US")

#' 
#' #### Question3(c)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
StatsQ3 %>% group_by(State_or_Province)%>%summarize(total_revenue = sum(Revenue))

#' 
#' #### Question3(d)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Rename the column
colnames(StatsQ3)[colnames(StatsQ3) == "Product Category"] ="Product_Category"

StatsQ3 %>% group_by(Product_Category)%>%summarize(total_revenue = sum(Revenue))

#' 
#' #### Question3(e)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
StatsQ3 %>%count(State_or_Province)

#' 
#' #### Question3(f)
#' 
#' There are about 71 per cent of customer have more than one child.
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
number_morechild <- nrow(StatsQ3 %>% filter(Children > 1))
Proportion <- (number_morechild/nrow(StatsQ3))*100

#' 
#' #### Question3(g)
#' 
#' The total revenue is 7673
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Rename the column
colnames(StatsQ3)[colnames(StatsQ3) == "Purchase Date"] ="Purchase_Date"

# Filter January and February
Jan_and_Feb <- StatsQ3%>%filter(between(Purchase_Date, as.Date("1/1/2017", format = "%m/%d/%Y"), as.Date("2/28/2017", format = "%m/%d/%Y")))

# Sum of revenue
Jan_and_Feb%>%summarize(total_revenue = sum(Revenue))

#' 
#' #### Question3(h)
#' 
#' There are no significance difference between what female and male buy in all product categories.But female buys a little more drink than male.
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Make a crosstable
crosstab <- crosstable(StatsQ3, c(`Product Family`), by = Gender, total="both")%>%
  as_flextable()
crosstab

#' 
#' #### Question3(i)
#' 
#' There are about 23.45 per cent of people are single while having a house.
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Filter who is single while having a house
Single_and_house <- StatsQ3%>%filter(`Marital Status` == "S" & Homeowner == "Y")

# Calculate the Proportion
answer <- nrow(Single_and_house)/nrow(StatsQ3)*100

#' 
#' #### Question3(j)
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Create a table
Gtable <- table(StatsQ3$Gender)

# Add the percentage
cbind(Gtable, prop.table(Gtable))

#' 
#' ### Question4
#' 
#' #### Question4(1)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
StatsQ4 <- read.csv("CellphoneMarket.csv")

# Turn character to number
StatsQ4$International.Plan<-ifelse(StatsQ4$International.Plan=="yes",1,0)
StatsQ4$Voice.Mail.Plan<-ifelse(StatsQ4$Voice.Mail.Plan=="yes",1,0)
StatsQ4$Churn.<-ifelse(StatsQ4$Churn.=="Yes",1,0)

# Check the distribution using histogram
hist(StatsQ4$Account.Length, main = "Approximately normally distributed, but it's a bit right skewed")
hist(StatsQ4$Voice.Mail.Messages, main = "Significantly right skewed, approximately exponential distribution") ##most short
hist(StatsQ4$Day.Minutes, main ="Approximately normally distributed") 
hist(StatsQ4$Day.Calls, main = "A bit left skewed") 
hist(StatsQ4$Day.Charge, main = "Approximately normally distributed") 
hist(StatsQ4$Evening.Minutes, main = "Approximately normally distributed") 
hist(StatsQ4$Evening.Calls, main = "A bit left skewed") 
hist(StatsQ4$Evening.Charge, main = "Approximately normally distributed") 
hist(StatsQ4$Night.Minutes, main = "Approximately normally distributed") 
hist(StatsQ4$Night.Calls, main = "Approximately normally distributed") 
hist(StatsQ4$Night.Charge, main = "Approximately normally distributed")
hist(StatsQ4$International.Minutes, main = "Approximately normally distributed") 
hist(StatsQ4$International.Calls, main = "Right skewed distribution") 
hist(StatsQ4$International.Charge, main = "Approximately normally distributed")

#' 
#' #### Question4(2) & 4(3)
#' 
#' We can observe some explicit correlation in the table. For example, voice mail plan has strong positive relation to voice mail messages, as well as day minutes, evening minutes, night minute, international minute have the same condition to day charge, evening charge, night charge, and international charge respectively. All other variables barely have any linear relationship to one another. If the company want to prevent customer to churn, the manager have to pay more attention on those variables that show linear relationship with the variable churn. As the graph shows, international plan, day minutes, day charge and customer service calls have weak positive relation to churn. This symbolizes some potential problems. First, customers may encounter trouble while reaching out to customer service. Second, the company's daily charge plan is probably not a good deal for a customer who use phone a lot every day. Third, some modification regarding international plan can also be considered. Although solving any of these problems may decrease customer churn rate to some degree, I will recommend the company to put emphasis on the second one, since the variables lead to this situation is relatively more than the other two.
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
cor(StatsQ4)

#' 
#' ### Question 5
#' 
#' #### Question5(a)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Create numeric vectors for the random variable and their probabilities
npv_semi <- c(40, 15, -20)
prob_semi <- c(0.5, 0.25, 0.25)

# Find the expected value
Expected_V_semi <- round(sum(npv_semi*prob_semi), digits = 3)

# Print out the result
sprintf("The expected net present value of expanding semiconductor business project is %.3f", Expected_V_semi)

#' 
#' #### Question5(b)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Find the variance
var_semi <- (npv_semi - Expected_V_semi)^2
Var_semi <- round(sum(var_semi * prob_semi), digits = 3)

# Determine the standard deviation
Std_Dev_semi <- round(sqrt(Var_semi), digits = 3)

# Print out the result
sprintf("Question5(b)")
sprintf("The variance of the net present value of expanding semiconductor business project is %.2f.", Var_semi)
sprintf("The std dev of the net present value of expanding semiconductor business project is %.2f.", Std_Dev_semi)

#' 
#' #### Question5(c)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Create numeric vectors for the random variable and their probabilities
npv_home <- c(140, 15, -35)
prob_home <- c(0.2, 0.5, 0.3)

# Find the expected value
Expected_V_home <- round(sum(npv_home*prob_home), digits = 3)

# Print out the result
sprintf("The expected net present value of entering home computer market is %.3f", Expected_V_home)

#' 
#' #### Question5(d)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Find the variance
var_home <- (npv_home - Expected_V_home)^2
Var_home <- round(sum(var_home * prob_home), digits = 3)

# Determine the standard deviation
Std_Dev_home <- round(sqrt(Var_home), digits = 3)

# Print out the result
sprintf("The variance of the net present value of entering home computer market is %.2f.", Var_home)
sprintf("The std dev of the net present value of entering home computer market is %.2f.", Std_Dev_home)

#' 
#' #### Question5(e)
#' 
#' Entering the home computer market have higher expected net present value.
#' 
#' #### Question5(f)
#' 
#' Although the expected net present value of the expansion project is lower, its std dev is also significantly lower than the other. Therefore, the expansion project carries the least risk.
#' 
#' #### Question5(g)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Calculate CV
CV_semi <- round((Std_Dev_semi/Expected_V_semi)*100, digits = 2)
CV_home <- round((Std_Dev_home/Expected_V_home)*100, digits = 2)

# Print out the result
sprintf("The CV of the expansion project is %.2f per cent. And the CV of entering home computer market is %.2f per cent. Since the lower the CV is, the less risky is the project, the result is consistent with part f, the expansion project carries the least risk.", CV_semi, CV_home)

#' 
#' ### Question6
#' 
#' #### Question6(a)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Create numeric vectors for the random variable and their probabilities
num_tries <- c(1, 2, 3, 4)
money <- c(500, 1000, 1500, 2000)
prob <- c(0.27, 0.1971, 0.1438, 0.105)

# Create a probability distribution
prob_distr <- data.frame(num_tries, round(prob, 4))
names(prob_distr) <- c("No. of Tries", "Probability")
prob_distr

#' 
#' #### Question6(b)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Find the expected value
Expected_Value <- round(sum(num_tries*prob), digits = 3)

# Interpret the result
sprintf("The expected number of service technicians that will be called in is %.3f", Expected_Value)

#' 
#' #### Question6(c)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
expected_money <- money*prob
sprintf("The expected amount spent on this machine is %.f", sum(expected_money) + (7500 * 0.73^4))

#' 
#' ### Question7
#' 
#' #### Question7(a)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
sprintf("The probability that exactly 10 of the attendees will purchase a club membership is %.3f.",dbinom(10, size = 20, prob = 0.4))

#' 
#' #### Question7(b)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
sprintf("The probability that no more than 10 of the attendees will purchase a club membership is %.3f.", pbinom(10, size = 20, prob = 0.4, lower.tail = TRUE))

#' 
#' #### Question7(c)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
sprintf("The probability that at least 15 of the attendees will purchase a club membership is %.3f.", pbinom(14, size = 20, prob = 0.4, lower.tail = FALSE))

#' 
#' ### Question8
#' 
#' #### Question8(a)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
avg_num_hour <- 400/16
avg_num_30min <- 400/16/2
avg_num_15min <- 400/16/2/2

# Print out the result
sprintf("The average number of calls in a one-hour interval is %.3f, in 30-minute interval is %.3f, in 15-minute interval is %.3f", avg_num_hour, avg_num_30min, avg_num_15min)

#' 
#' #### Question8(b)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
sprintf("The probability of exactly 6 calls in a 15-minute interval is %.3f.", dpois(6, 6.25))

#' 
#' #### Question8(c)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
sprintf("The probability of no calls in a 15-minute interval is %.3f.", dpois(0, 6.25))

#' 
#' #### Question8(d)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
sprintf("The probability of at least two calls in a 15-minute interval is %.3f.", ppois(1, 6.25, lower.tail = FALSE))

#' 
#' ### Question9
#' 
#' When the defective rate increases, the acceptance prob drop accordingly. If we loosen the acceptance criterion from 4 or fewer to 5 or fewer, there will be visible increase in acceptance prob when the defective rate>0.06.
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Create vectors for probability
p_fraction <- seq(from = 20, to = 180, by = 20)

# Hypergeometric Distribution and create a barplot
barplot(phyper(4, p_fraction, 1000-p_fraction, 50)~seq(from = 0.02, to = 0.18, by = 0.02), xlab = "defective rate", ylab = "acceptance prob", main = "Acceptance prob when acceptance rate is 4 or fewer")

# Hypergeometric Distribution when acceptance rate change and create a barplot
barplot(phyper(5, p_fraction, 1000-p_fraction, 50)~seq(from = 0.02, to = 0.18, by = 0.02), xlab = "defective rate", ylab = "acceptance prob", main = "Acceptance prob when acceptance rate is 5 or fewer")

#' 
#' ### Question10
#' 
#' #### Question10(a)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Create vectors
num_air_left <- c(0, 1, 2, 3, 4, 5)
num_special <- c(0, 1, 2, 3, 4)
prob_air_left <- c(0.16, 0.3, 0.16, 0.08, 0.05, 0.05)
prob_special <- c(0.05+0.05+0.08+0.16+0.3+0.16, 0.1, 0.05, 0.05, 0)

# Create Probability Distribution for X and Y
# For X
prob_distr_air <- data.frame(num_air_left, prob_air_left)
names(prob_distr_air) <- c("No. of Air Conditioner Left", "Probability")
prob_distr_air

# For Y
prob_distr_special <- data.frame(num_special, prob_special)
names(prob_distr_special) <- c("No. of special order", "Probability")
prob_distr_special

#' 
#' #### Question10(b)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# For X
Expected_V_X <- round(sum(num_air_left*prob_air_left), digits = 3)
sprintf("The expected value of X is %.3f.", Expected_V_X)

# For Y
Expected_V_Y <- round(sum(num_special*prob_special), digits = 3)
sprintf("The expected value of Y is %.3f.", Expected_V_Y)

#' 
#' #### Question10(c)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
# Create vectors
profit <- c(0, 60, 120, 180, 240, 300, 340, 380, 420)
prob_profit <- c(0.05, 0.05, 0.08, 0.16, 0.3, 0.16, 0.1, 0.05, 0.05)

# Create Probability Distribution
prob_distr_profit <- data.frame(profit, prob_profit)
names(prob_distr_profit) <- c("No. of Profit", "Probability")
prob_distr_profit

#' 
#' #### Question10(d)
#' 
## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
Expected_V_Z <- round(sum(profit*prob_profit), digits = 3)
sprintf("The expected value of Z is %.3f.", Expected_V_Z)

