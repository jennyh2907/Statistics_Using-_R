#' ---
#' title: "HW3_FOA"
#' output: html_document
#' date: "2022-08-28"
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(pwr)
library(EnvStats)

#' 
#' ### Question1
#' 
#' 47 is the recommended sample size for hypothesis test.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
power.t.test(delta = 400-385, sd = 30, power = 0.90, sig.level = 0.02, alternative = "one.sided", type = "one.sample")

#' 
#' ### Question2
#' 
#' The confidence interval suggest a significant difference among waiting time. Noted the mean waiting time of Bank A and Bank B are the same. Since the single line system that Bank A adopts lead to lower variation in waiting time, it seems better than the other one.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
Wait_Time <- read_csv("WaitingTimes-2.csv")

# Var for bank A
mean_A <- mean(Wait_Time$BankA)
n_wait <- nrow(Wait_Time)
var_A <- var(Wait_Time$BankA)

# 95% CI for bank A
chi025 <- qchisq(.025, df=n_wait-1)
chi975 <- qchisq(.975, df=n_wait-1)
lb_wait_A <- sqrt(((n_wait-1)*var_A)/chi975)
ub_wait_A <- sqrt(((n_wait-1)*var_A)/chi025)

lb_wait_A_rounded <- round(lb_wait_A, digits = 3)
ub_wait_A_rounded <- round(ub_wait_A, digits = 3)

# Var for bank B
mean_B <- mean(Wait_Time$BankB)
var_B <- var(Wait_Time$BankB)

# 95% CI for bank A
lb_wait_B <- sqrt(((n_wait-1)*var_B)/chi975)
ub_wait_B <- sqrt(((n_wait-1)*var_B)/chi025)

lb_wait_B_rounded <- round(lb_wait_B, digits = 3)
ub_wait_B_rounded <- round(ub_wait_B, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval for Bank A: We are 95 per cent confident that the interval (%s, %s) actually contain the true value of population std of Bank A", lb_wait_A_rounded, ub_wait_A_rounded)
sprintf("95 percent Confidence Interval for Bank B: We are 95 per cent confident that the interval (%s, %s) actually contain the true value of population std of Bank B", lb_wait_B_rounded, ub_wait_B_rounded)

#' 
#' ### Question3
#' 
#' #### Question3(a)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
p_hat <- 0.5
q_hat <- 1-0.5
z95 <- qnorm(0.025, mean = 0, sd = 1, lower.tail = FALSE)
Margin_of_error <- 0.04

# 95% CI
n <- p_hat * q_hat * (z95/Margin_of_error)^2

# Print out the result
sprintf("The minimum sample size needed is %s", ceiling(n))

#' 
#' #### Question3(b)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
p_hat_CBS <- 0.35
q_hat_CBS <- 1-0.35

# 95% CI
n_CBS <- p_hat_CBS * q_hat_CBS * Margin_of_error^2

# Print out the result
sprintf("The minimum sample size needed is %s", ceiling(n_CBS))

#' 
#' #### Question3(c)
#' The minimum sample size is larger when there are no preliminary estimate available. Once there is a rough estimation, the number of minimum sample size decreases
#' 
#' ### Question4
#' 
#' #### Question4(a)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
Ad <- read_csv("Admissions.csv")

# Calculate the proportion of all acceptable students
offer <- sum(Ad$`Accepted our offer`=="Yes")
n_offer <- nrow(Ad)
p_hat_offer <- offer/n_offer
q_hat_offer <- 1-p_hat_offer

# 95% CI for all acceptable students
l95_offer <- p_hat_offer - (z95 * sqrt(p_hat_offer * q_hat_offer/n_offer))
r95_offer <- p_hat_offer + (z95 * sqrt(p_hat_offer * q_hat_offer/n_offer))

l95_rounded_offer <- round(l95_offer, digits = 3)
r95_rounded_offer <- round(r95_offer, digits = 3)

# Calculate the proportion of all students <330
offer_330 <- sum(Ad$`Combined Score`<330)
offer_ac_330 <- Ad%>%filter(`Combined Score`<330 & `Accepted our offer`=="Yes")
n_offer_ac_330 <- nrow(offer_ac_330)
p_hat_offer_330 <- n_offer_ac_330/offer_330
q_hat_offer_330 <- 1-p_hat_offer_330

# 95% CI for all students <330
l95_offer_330 <- p_hat_offer_330 - (z95 * sqrt(p_hat_offer_330 * q_hat_offer_330/offer_330))
r95_offer_330 <- p_hat_offer_330 + (z95 * sqrt(p_hat_offer_330 * q_hat_offer_330/offer_330))

l95_rounded_offer_330 <- round(l95_offer_330, digits = 3)
r95_rounded_offer_330 <- round(r95_offer_330, digits = 3)

# Calculate the proportion of all students between 330 and 375
offer_330_375 <- sum(Ad$`Combined Score` <= 375 & 330 <= Ad$`Combined Score`)
offer_ac_330_375 <- Ad%>%filter(`Combined Score` <= 375 & 330 <= `Combined Score` & `Accepted our offer`=="Yes")
n_offer_ac_330_375 <- nrow(offer_ac_330_375)
p_hat_offer_330_375 <- n_offer_ac_330_375/offer_330_375
q_hat_offer_330_375 <- 1-p_hat_offer_330_375

# 95% CI for all students between 330 and 375
l95_offer_330_375 <- p_hat_offer_330_375 - (z95 * sqrt(p_hat_offer_330_375 * q_hat_offer_330_375/offer_330_375))
r95_offer_330_375 <- p_hat_offer_330_375 + (z95 * sqrt(p_hat_offer_330_375 * q_hat_offer_330_375/offer_330_375))

l95_rounded_offer_330_375 <- round(l95_offer_330_375, digits = 3)
r95_rounded_offer_330_375 <- round(r95_offer_330_375, digits = 3)

# Calculate the proportion of all students greater than 375
offer_375 <- sum(375 < Ad$`Combined Score`)
offer_ac_375 <- Ad%>%filter(`Combined Score` > 375 & `Accepted our offer`=="Yes")
n_offer_ac_375 <- nrow(offer_ac_375)
p_hat_offer_375 <- n_offer_ac_375/offer_375
q_hat_offer_375 <- 1-p_hat_offer_375

# 95% CI for all students greater than 375
l95_offer_375 <- p_hat_offer_375 - (z95 * sqrt(p_hat_offer_375 * q_hat_offer_375/offer_375))
r95_offer_375 <- p_hat_offer_375 + (z95 * sqrt(p_hat_offer_375 * q_hat_offer_375/offer_375))

l95_rounded_offer_375 <- round(l95_offer_375, digits = 3)
r95_rounded_offer_375 <- round(r95_offer_375, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval for proportion of all acceptable applicants who accept Williamson’s invitation to enroll: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of all acceptable applicants who accept Williamson’s invitation to enroll", l95_rounded_offer, r95_rounded_offer)
sprintf("95 percent Confidence Interval for proportion of all acceptable applicants with a combined score less than 330 who accept Williamson’s invitation to enroll: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of all acceptable applicants with a combined score less than 330 who accept Williamson’s invitation to enroll", l95_rounded_offer_330, r95_rounded_offer_330)  
sprintf("95 percent Confidence Interval for proportion of all acceptable applicants with a combined score between 330 and 375 who accept Williamson’s invitation to enroll: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of all acceptable applicants with a combined score between 330 and 375 who accept Williamson’s invitation to enroll", l95_rounded_offer_330_375, r95_rounded_offer_330_375)
sprintf("95 percent Confidence Interval for proportion of all acceptable applicants with a combined score greater than 375 who accept Williamson’s invitation to enroll: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of all acceptable applicants with a combined score greater than 375 who accept Williamson’s invitation to enroll", l95_rounded_offer_375, r95_rounded_offer_375)

#' 
#' ####Question 4(b)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Find the median combined score
median <- median(Ad$`Combined Score`)

# Calculate the proportion of all rival students less than median
offer_less <- sum(Ad$`Combined Score` < median)+4
offer_less_rival <- Ad%>%filter(`Went to main rival` == "Yes" & `Combined Score` < median)
n_offer_less_rival <- nrow(offer_less_rival)+2
p_hat_offer_less <- n_offer_less_rival/offer_less
q_hat_offer_less <- 1-p_hat_offer_less

# 95% CI for all rival students less than median
l95_offer_less <- p_hat_offer_less - (z95 * sqrt(p_hat_offer_less * q_hat_offer_less/offer_less))
r95_offer_less <- p_hat_offer_less + (z95 * sqrt(p_hat_offer_less * q_hat_offer_less/offer_less))

l95_rounded_offer_less <- round(l95_offer_less, digits = 3)
r95_rounded_offer_less <- round(r95_offer_less, digits = 3)

# Calculate the proportion of all rival students greater or equal than median
offer_great <- sum(Ad$`Combined Score` >= median)
offer_great_rival <- Ad%>%filter(`Went to main rival` == "Yes" & `Combined Score` >= median)
n_offer_great_rival <- nrow(offer_great_rival)
p_hat_offer_great <- n_offer_great_rival/offer_great
q_hat_offer_great <- 1-p_hat_offer_great

# 95% CI for all students greater or equal to median
l95_offer_great <- p_hat_offer_great - (z95 * sqrt(p_hat_offer_great * q_hat_offer_great/offer_great))
r95_offer_great <- p_hat_offer_great + (z95 * sqrt(p_hat_offer_great * q_hat_offer_great/offer_great))

l95_rounded_offer_great <- round(l95_offer_great, digits = 3)
r95_rounded_offer_great <- round(r95_offer_great, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval that is less than median: (%s, %s), We are 95 per cent confident that the interval actually contain the true proportion of all acceptable students with a combined score less than the median who choose Williamson’s rival over Williamson", l95_rounded_offer_less, r95_rounded_offer_less)
sprintf("95 percent Confidence Interval that is greater or equal to median: (%s, %s), We are 95 per cent confident that the interval actually contain the true proportion of all acceptable students with a combined score greater or equal to the median who choose Williamson’s rival over Williamson.", l95_rounded_offer_great, r95_rounded_offer_great)

#' 
#' #### Question4(c)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Williamson for combined score
Accept <- Ad %>% filter(`Accepted our offer` == "Yes")
x_bar_score <- mean(Accept$`Combined Score`)
sigma_score <- sd(Accept$`Combined Score`)
n_accept <- nrow(Accept)

# 95% CI -> t-distribution
t95 <- qt(0.975, df = n_accept - 1, lower.tail = TRUE)

l95_score <- x_bar_score - (t95*sigma_score/sqrt(n_accept))
r95_score <-x_bar_score + (t95*sigma_score/sqrt(n_accept))

l95_rounded_score <- round(l95_score, digits = 3)
r95_rounded_score <- round(r95_score, digits = 3)

# Williamson for GPA
x_bar_gpa <- mean(Accept$`HS GPA`)
sigma_gpa <- sd(Accept$`HS GPA`)

# 95% CI
l95_gpa <- x_bar_gpa - (t95*sigma_gpa/sqrt(n_accept))
r95_gpa <- x_bar_gpa + (t95*sigma_gpa/sqrt(n_accept))

l95_rounded_gpa <- round(l95_gpa, digits = 3)
r95_rounded_gpa <- round(r95_gpa, digits = 3)

# Williamson for SAT
x_bar_sat <- mean(Accept$SAT)
sigma_sat <- sd(Accept$SAT)

# 95% CI
l95_sat <- x_bar_sat - (t95*sigma_sat/sqrt(n_accept))
r95_sat <- x_bar_sat + (t95*sigma_sat/sqrt(n_accept))

l95_rounded_sat <- round(l95_sat, digits = 3)
r95_rounded_sat <- round(r95_sat, digits = 3)

# No Williamson for combined score
Elsewhere <- Ad %>% filter(`Accepted our offer` == "No")
n_elsewhere <- nrow(Elsewhere)
x_bar_nwscore <- mean(Elsewhere$`Combined Score`)
sigma_nwscore <- sd(Elsewhere$`Combined Score`)

# 95% CI
t95_r <- qt(0.975, df = n_elsewhere - 1, lower.tail = TRUE)
l95_nwscore <- x_bar_nwscore - (t95_r*sigma_nwscore/sqrt(n_elsewhere))
r95_nwscore <- x_bar_nwscore + (t95_r*sigma_nwscore/sqrt(n_elsewhere))

l95_rounded_nwscore <- round(l95_nwscore, digits = 3)
r95_rounded_nwscore <- round(r95_nwscore, digits = 3)

# No Williamson for GPA
x_bar_nwgpa <- mean(Elsewhere$`HS GPA`)
sigma_nwgpa <- sd(Elsewhere$`HS GPA`)

# 95% CI
l95_nwgpa <- x_bar_nwgpa - (t95_r*sigma_nwgpa/sqrt(n_elsewhere))
r95_nwgpa <- x_bar_nwgpa + (t95_r*sigma_nwgpa/sqrt(n_elsewhere))

l95_rounded_nwgpa <- round(l95_nwgpa, digits = 3)
r95_rounded_nwgpa <- round(r95_nwgpa, digits = 3)

# No Williamson for SAT
x_bar_nwsat <- mean(Elsewhere$SAT)
sigma_nwsat <- sd(Elsewhere$SAT)

# 95% CI
l95_nwsat <- x_bar_nwsat - (t95_r*sigma_nwsat/sqrt(n_elsewhere))
r95_nwsat <- x_bar_nwsat + (t95_r*sigma_nwsat/sqrt(n_elsewhere))

l95_rounded_nwsat <- round(l95_nwsat, digits = 3)
r95_rounded_nwsat <- round(r95_nwsat, digits = 3)

# Difference
score_d <- t.test(Accept$`Combined Score`, Elsewhere$`Combined Score`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
GPA_d <- t.test(Accept$`HS GPA`, Elsewhere$`HS GPA`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
SAT_d <- t.test(Accept$`SAT`, Elsewhere$`SAT`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

# Print out the result
sprintf("95 percent Confidence Interval for the mean combined score of enrolled students: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean of combined score of all acceptable students who accept Williamson’s invitation to enroll.", l95_rounded_score, r95_rounded_score)
sprintf("95 percent Confidence Interval for the mean combined score of students enrolled elsewhere: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean of combined score of students enrolled elsewhere.", l95_rounded_nwscore, r95_rounded_nwscore)
sprintf("95 percent Confidence Interval for the mean GPA of enrolled students: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean of GPA of all acceptable students who accept Williamson’s invitation to enroll.", l95_rounded_gpa, r95_rounded_gpa)
sprintf("95 percent Confidence Interval for the mean GPA of students enrolled elsewhere: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean of GPA of students enrolled elsewhere.", l95_rounded_nwgpa, r95_rounded_nwgpa)
sprintf("95 percent Confidence Interval for the mean SAT of enrolled students: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean of SAT of all acceptable students who accept Williamson’s invitation to enroll.", l95_rounded_sat, r95_rounded_sat)
sprintf("95 percent Confidence Interval for the mean SAT of students enrolled elsewhere: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean of SAT of students enrolled elsewhere.", l95_rounded_nwsat, r95_rounded_nwsat)
sprintf("95 percent Confidence Interval for the difference between mean combined score where each difference is a mean for students enrolling at Williamson minus the similar mean for students enrolling elsewhere is: (%s, %s), We are 95 per cent confident that the interval actually contain the true difference between two means.", score_d$conf.int[1], score_d$conf.int[2])
sprintf("95 percent Confidence Interval for the difference between mean GPA where each difference is a mean for students enrolling at Williamson minus the similar mean for students enrolling elsewhere is: (%s, %s), We are 95 per cent confident that the interval actually contain the true difference between two means.", GPA_d$conf.int[1], GPA_d$conf.int[2])
sprintf("95 percent Confidence Interval for the difference between mean SAT where each difference is a mean for students enrolling at Williamson minus the similar mean for students enrolling elsewhere is: (%s, %s), We are 95 per cent confident that the interval actually contain the true difference between two means.", SAT_d$conf.int[1], SAT_d$conf.int[2])

#' 
#' ####Question4(d)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the proportion of student officer(club) 
club <- Ad%>% filter(`HS Clubs` > 1 & `Accepted our offer`=="Yes")
officer <- nrow(club)
p_hat_officer <- officer/offer
q_hat_officer <- 1-p_hat_officer

# 95% CI for officers(club)
l95_officer <- p_hat_officer - (z95 * sqrt(p_hat_officer * q_hat_officer/offer))
r95_officer <- p_hat_officer + (z95 * sqrt(p_hat_officer * q_hat_officer/offer))

l95_rounded_officer <- round(l95_officer, digits = 3)
r95_rounded_officer <- round(r95_officer, digits = 3)

# Calculate the proportion of student sport varsity
sport <- Ad%>% filter(`HS Sports` >= 4 & `Accepted our offer`=="Yes")
varsity <- nrow(sport)
p_hat_varsity <- varsity/offer
q_hat_varsity <- 1-p_hat_varsity

# 95% CI for sport varsity
l95_varsity <- p_hat_varsity - (z95 * sqrt(p_hat_varsity * q_hat_varsity/offer))
r95_varsity <- p_hat_varsity + (z95 * sqrt(p_hat_varsity * q_hat_varsity/offer))

l95_rounded_varsity <- round(l95_varsity, digits = 3)
r95_rounded_varsity <- round(r95_varsity, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval for proportion of enrolled students who have been officers of at least two clubs: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of enrolled students who have been officers of at least two clubs.", l95_rounded_officer, r95_rounded_officer)
sprintf("95 percent Confidence Interval for proportion of enrolled students who have earned at least four varsity letters in sports: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of enrolled students who have earned at least four varsity letters in sports.", l95_rounded_varsity, r95_rounded_varsity)

#' 
#' #### Question4(e)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Set the criteria, >mean will be taken as large and high-ranked
HS_standard <- mean(Ad$`HS Size`)
HS_percentile <- mean(Ad$`HS Percentile`)

# Calculate the proportion of high-score students from large high school
l_HS <- Ad %>% filter(`Accepted our offer` == "Yes" & `HS Size` > HS_standard & `HS Percentile` > HS_percentile)
L_HS <- nrow(l_HS)
p_hat_HS <- L_HS/n_offer
q_hat_HS <- 1-p_hat_HS

# 95% CI 
l95_HS <- p_hat_HS - (z95 * sqrt(p_hat_HS * q_hat_HS/n_offer))
r95_HS <- p_hat_HS + (z95 * sqrt(p_hat_HS * q_hat_HS/n_offer))

l95_rounded_HS <- round(l95_HS, digits = 3)
r95_rounded_HS <- round(r95_HS, digits = 3)

# Calculate the proportion of high-score students from small high school
s_HS <- Ad%>% filter(`Accepted our offer`=="Yes" & `HS Size` < HS_standard & `HS Percentile` > HS_percentile)
S_HS <- nrow(s_HS)
p_hat_SHS <- S_HS/n_offer
q_hat_SHS <- 1-p_hat_SHS

# 95% CI
l95_SHS <- p_hat_SHS - (z95 * sqrt(p_hat_SHS * q_hat_SHS/n_offer))
r95_SHS <- p_hat_SHS + (z95 * sqrt(p_hat_SHS * q_hat_SHS/n_offer))

l95_rounded_SHS <- round(l95_SHS, digits = 3)
r95_rounded_SHS <- round(r95_SHS, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval for proportion of highly-ranked students who come from a large high school: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of highly-ranked students who come from a large high school.", l95_rounded_HS, r95_rounded_HS)
sprintf("95 percent Confidence Interval for proportion of highly-ranked students who come from a small high school: (%s, %s). We are 95 per cent confident that the interval actually contains the true proportion of highly-ranked students who come from a small high school.", l95_rounded_SHS, r95_rounded_SHS)
sprintf("As two CI show, the estimated number of highly-ranked students from large high school is even lower than those from smaller ones. So, Williamson is not relatively successful in attracting students from large high schools than from small high schools.")

#' 
#' ### Question5
#' 
#' ####Question5(a)(i)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
DT <- read_csv("DeliveryTimes.csv")

# For Deliverer 1's delivery time
DT1 <- DT %>% filter(Deliverer == "1")
x_bar_DT1 <- mean(DT1$`Travel Time`)
sigma_DT1 <- sd(DT1$`Travel Time`)
n_DT1 <- nrow(DT1)

# 95% CI
t95_DT1 <- qt(0.975, df = n_DT1 - 1, lower.tail = TRUE)

l95_DT1 <- x_bar_DT1 - (t95_DT1*sigma_DT1/sqrt(n_DT1))
r95_DT1 <- x_bar_DT1 + (t95_DT1*sigma_DT1/sqrt(n_DT1))

l95_rounded_DT1 <- round(l95_DT1, digits = 3)
r95_rounded_DT1 <- round(r95_DT1, digits = 3)

# For Deliverer 2's delivery time
DT2 <- DT %>% filter(Deliverer == "2")
x_bar_DT2 <- mean(DT2$`Travel Time`)
sigma_DT2 <- sd(DT2$`Travel Time`)
n_DT2 <- nrow(DT2)

# 95% CI
t95_DT2 <- qt(0.975, df = n_DT2 - 1, lower.tail = TRUE)

l95_DT2 <- x_bar_DT2 - (t95_DT2*sigma_DT2/sqrt(n_DT2))
r95_DT2 <- x_bar_DT2 + (t95_DT2*sigma_DT2/sqrt(n_DT2))

l95_rounded_DT2 <- round(l95_DT2, digits = 3)
r95_rounded_DT2 <- round(r95_DT2, digits = 3)

# For Deliverer 3's delivery time
DT3 <- DT %>% filter(Deliverer == "3")
x_bar_DT3 <- mean(DT3$`Travel Time`)
sigma_DT3 <- sd(DT3$`Travel Time`)
n_DT3 <- nrow(DT3)

# 95% CI
t95_DT3 <- qt(0.975, df = n_DT3 - 1, lower.tail = TRUE)

l95_DT3 <- x_bar_DT3 - (t95_DT3*sigma_DT3/sqrt(n_DT3))
r95_DT3 <- x_bar_DT3 + (t95_DT3*sigma_DT3/sqrt(n_DT3))

l95_rounded_DT3 <- round(l95_DT3, digits = 3)
r95_rounded_DT3 <- round(r95_DT3, digits = 3)

# For Deliverer 4's delivery time
DT4 <- DT %>% filter(Deliverer == "4")
x_bar_DT4 <- mean(DT4$`Travel Time`)
sigma_DT4 <- sd(DT4$`Travel Time`)
n_DT4 <- nrow(DT4)

# 95% CI
t95_DT4 <- qt(0.975, df = n_DT4 - 1, lower.tail = TRUE)

l95_DT4 <- x_bar_DT4 - (t95_DT4*sigma_DT4/sqrt(n_DT4))
r95_DT4 <- x_bar_DT4 + (t95_DT4*sigma_DT4/sqrt(n_DT4))

l95_rounded_DT4 <- round(l95_DT4, digits = 3)
r95_rounded_DT4 <- round(r95_DT4, digits = 3)

# For Deliverer 1's total time
DT1_total <- DT %>% filter(Deliverer == "1")
DT1_total$`Total` <- DT1_total$`Travel Time`+DT1_total$`Prep Time`
x_bar_DT1_total <- mean(DT1_total$`Total`)
sigma_DT1_total <- sd(DT1_total$`Total`)
n_DT1_total <- nrow(DT1_total)

# 95% CI
t95_DT1_total <- qt(0.975, df = n_DT1_total - 1, lower.tail = TRUE)

l95_DT1_total <- x_bar_DT1_total - (t95_DT1_total*sigma_DT1_total/sqrt(n_DT1_total))
r95_DT1_total <- x_bar_DT1_total + (t95_DT1_total*sigma_DT1_total/sqrt(n_DT1_total))

l95_rounded_DT1_total <- round(l95_DT1_total, digits = 3)
r95_rounded_DT1_total <- round(r95_DT1_total, digits = 3)

# For Deliverer 2's total time
DT2_total <- DT %>% filter(Deliverer == "2")
DT2_total$`Total` <- DT2_total$`Travel Time`+DT2_total$`Prep Time`
x_bar_DT2_total <- mean(DT2_total$`Total`)
sigma_DT2_total <- sd(DT2_total$`Total`)
n_DT2_total <- nrow(DT2_total)

# 95% CI
t95_DT2_total <- qt(0.975, df = n_DT2_total - 1, lower.tail = TRUE)

l95_DT2_total <- x_bar_DT2_total - (t95_DT2_total*sigma_DT2_total/sqrt(n_DT2_total))
r95_DT2_total <- x_bar_DT2_total + (t95_DT2_total*sigma_DT2_total/sqrt(n_DT2_total))

l95_rounded_DT2_total <- round(l95_DT2_total, digits = 3)
r95_rounded_DT2_total <- round(r95_DT2_total, digits = 3)

# For Deliverer 3's total time
DT3_total <- DT %>% filter(Deliverer == "3")
DT3_total$`Total` <- DT3_total$`Travel Time`+DT3_total$`Prep Time`
x_bar_DT3_total <- mean(DT3_total$`Total`)
sigma_DT3_total <- sd(DT3_total$`Total`)
n_DT3_total <- nrow(DT3_total)

# 95% CI
t95_DT3_total <- qt(0.975, df = n_DT3_total - 1, lower.tail = TRUE)

l95_DT3_total <- x_bar_DT3_total - (t95_DT3_total*sigma_DT3_total/sqrt(n_DT3_total))
r95_DT3_total <- x_bar_DT3_total + (t95_DT3_total*sigma_DT3_total/sqrt(n_DT3_total))

l95_rounded_DT3_total <- round(l95_DT3_total, digits = 3)
r95_rounded_DT3_total <- round(r95_DT3_total, digits = 3)

# For Deliverer 4's total time
DT4_total <- DT %>% filter(Deliverer == "4")
DT4_total$`Total` <- DT4_total$`Travel Time`+DT4_total$`Prep Time`
x_bar_DT4_total <- mean(DT4_total$`Total`)
sigma_DT4_total <- sd(DT4_total$`Total`)
n_DT4_total <- nrow(DT4_total)

# 95% CI
t95_DT4_total <- qt(0.975, df = n_DT4_total - 1, lower.tail = TRUE)

l95_DT4_total <- x_bar_DT4_total - (t95_DT4_total*sigma_DT4_total/sqrt(n_DT4_total))
r95_DT4_total <- x_bar_DT4_total + (t95_DT4_total*sigma_DT4_total/sqrt(n_DT4_total))

l95_rounded_DT4_total <- round(l95_DT4_total, digits = 3)
r95_rounded_DT4_total <- round(r95_DT4_total, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval for mean delivery time of deliverer 1: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean delivery time of deliverer 1.", l95_rounded_DT1, r95_rounded_DT1)
sprintf("95 percent Confidence Interval for mean total time of deliverer 1: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean total time of deliverer 1.", l95_rounded_DT1_total, r95_rounded_DT1_total)
sprintf("95 percent Confidence Interval for mean delivery time of deliverer 2: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean delivery time of deliverer 2.", l95_rounded_DT2, r95_rounded_DT2)
sprintf("95 percent Confidence Interval for mean total time of deliverer 2: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean total time of deliverer 2.", l95_rounded_DT2_total, r95_rounded_DT2_total)
sprintf("95 percent Confidence Interval for mean delivery time of deliverer 3: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean delivery time of deliverer 3.", l95_rounded_DT3, r95_rounded_DT3)
sprintf("95 percent Confidence Interval for mean total time of deliverer 3: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean total time of deliverer 3.", l95_rounded_DT3_total, r95_rounded_DT3_total)
sprintf("95 percent Confidence Interval for mean delivery time of deliverer 4: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean delivery time of deliverer 4.", l95_rounded_DT4, r95_rounded_DT4)
sprintf("95 percent Confidence Interval for mean total time of deliverer 4: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean total time of deliverer 4.", l95_rounded_DT4_total, r95_rounded_DT4_total)
sprintf("The first reason is the deliverers can not control prep time, maybe it's the chef who makes the total delivery time longer. You can not blame deliverer for that. The second reason is that the distance they need to travel atr different. It's normal to spend longer to travel further.")

#' 
#' #### Question5(a)(ii)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# For Deliverer 1's speed
DT1_speed <- DT %>% filter(Deliverer == "1")
DT1_speed$`Speed` <- DT1_total$Distance/(DT1_speed$`Travel Time`/60)
x_bar_DT1_speed <- mean(DT1_speed$`Speed`)
sigma_DT1_speed <- sd(DT1_speed$`Speed`)
n_DT1_speed <- nrow(DT1_speed)

# 95% CI
t95_DT1_speed <- qt(0.975, df = n_DT1_speed - 1, lower.tail = TRUE)

l95_DT1_speed <- x_bar_DT1_speed - (t95_DT1_speed*sigma_DT1_speed/sqrt(n_DT1_speed))
r95_DT1_speed <- x_bar_DT1_speed + (t95_DT1_speed*sigma_DT1_speed/sqrt(n_DT1_speed))

l95_rounded_DT1_speed <- round(l95_DT1_speed, digits = 3)
r95_rounded_DT1_speed <- round(r95_DT1_speed, digits = 3)

# For Deliverer 2's speed
DT2_speed <- DT %>% filter(Deliverer == "2")
DT2_speed$`Speed` <- DT2_total$Distance/(DT2_speed$`Travel Time`/60)
x_bar_DT2_speed <- mean(DT2_speed$`Speed`)
sigma_DT2_speed <- sd(DT2_speed$`Speed`)
n_DT2_speed <- nrow(DT2_speed)

# 95% CI
t95_DT2_speed <- qt(0.975, df = n_DT2_speed - 1, lower.tail = TRUE)

l95_DT2_speed <- x_bar_DT2_speed - (t95_DT2_speed*sigma_DT2_speed/sqrt(n_DT2_speed))
r95_DT2_speed <- x_bar_DT2_speed + (t95_DT2_speed*sigma_DT2_speed/sqrt(n_DT2_speed))

l95_rounded_DT2_speed <- round(l95_DT2_speed, digits = 3)
r95_rounded_DT2_speed <- round(r95_DT2_speed, digits = 3)

# For Deliverer 3's speed
DT3_speed <- DT %>% filter(Deliverer == "3")
DT3_speed$`Speed` <- DT3_total$Distance/(DT3_speed$`Travel Time`/60)
x_bar_DT3_speed <- mean(DT3_speed$`Speed`)
sigma_DT3_speed <- sd(DT3_speed$`Speed`)
n_DT3_speed <- nrow(DT3_speed)

# 95% CI
t95_DT3_speed <- qt(0.975, df = n_DT3_speed - 1, lower.tail = TRUE)

l95_DT3_speed <- x_bar_DT3_speed - (t95_DT3_speed*sigma_DT3_speed/sqrt(n_DT3_speed))
r95_DT3_speed <- x_bar_DT3_speed + (t95_DT3_speed*sigma_DT3_speed/sqrt(n_DT3_speed))

l95_rounded_DT3_speed <- round(l95_DT3_speed, digits = 3)
r95_rounded_DT3_speed <- round(r95_DT3_speed, digits = 3)

# For Deliverer 4's speed
DT4_speed <- DT %>% filter(Deliverer == "4")
DT4_speed$`Speed` <- DT4_total$Distance/(DT4_speed$`Travel Time`/60)
x_bar_DT4_speed <- mean(DT4_speed$`Speed`)
sigma_DT4_speed <- sd(DT4_speed$`Speed`)
n_DT4_speed <- nrow(DT4_speed)

# 95% CI
t95_DT4_speed <- qt(0.975, df = n_DT4_speed - 1, lower.tail = TRUE)

l95_DT4_speed <- x_bar_DT4_speed - (t95_DT4_speed*sigma_DT4_speed/sqrt(n_DT4_speed))
r95_DT4_speed <- x_bar_DT4_speed + (t95_DT4_speed*sigma_DT4_speed/sqrt(n_DT4_speed))

l95_rounded_DT4_speed <- round(l95_DT4_speed, digits = 3)
r95_rounded_DT4_speed <- round(r95_DT4_speed, digits = 3)

# Difference
speed12_d <- t.test(DT1_speed$`Speed`, DT2_speed$`Speed`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
speed13_d <- t.test(DT1_speed$`Speed`, DT3_speed$`Speed`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
speed14_d <- t.test(DT1_speed$`Speed`, DT4_speed$`Speed`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
speed23_d <- t.test(DT2_speed$`Speed`, DT3_speed$`Speed`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
speed24_d <- t.test(DT2_speed$`Speed`, DT4_speed$`Speed`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
speed34_d <- t.test(DT3_speed$`Speed`, DT4_speed$`Speed`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

# Print out the result
sprintf("95 percent Confidence Interval for mean speed of deliverer 1: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean speed of deliverer 1.", l95_rounded_DT1_speed, r95_rounded_DT1_speed)
sprintf("95 percent Confidence Interval for mean speed of deliverer 2: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean speed of deliverer 2.", l95_rounded_DT2_speed, r95_rounded_DT2_speed)
sprintf("95 percent Confidence Interval for mean speed of deliverer 3: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean speed of deliverer 3.", l95_rounded_DT3_speed, r95_rounded_DT3_speed)
sprintf("95 percent Confidence Interval for mean speed of deliverer 4: (%s, %s). We are 95 per cent confident that the interval actually contains the true mean speed of deliverer 4.", l95_rounded_DT4_speed, r95_rounded_DT4_speed)
sprintf("95 percent Confidence Interval for the mean difference in speed between deliverer 1 and 2 is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in speed between deliverer 1 and 2.", speed12_d$conf.int[1], speed12_d$conf.int[2])
sprintf("95 percent Confidence Interval for the mean difference in speed between deliverer 1 and 3 is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in speed between deliverer 1 and 3.", speed13_d$conf.int[1], speed13_d$conf.int[2])
sprintf("95 percent Confidence Interval for the mean difference in speed between deliverer 1 and 4 is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in speed between deliverer 1 and 4.", speed14_d$conf.int[1], speed14_d$conf.int[2])
sprintf("95 percent Confidence Interval for the mean difference in speed between deliverer 2 and 3 is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in speed between deliverer 2 and 3.", speed23_d$conf.int[1], speed23_d$conf.int[2])
sprintf("95 percent Confidence Interval for the mean difference in speed between deliverer 2 and 4 is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in speed between deliverer 2 and 4.", speed24_d$conf.int[1], speed24_d$conf.int[2])
sprintf("95 percent Confidence Interval for the mean difference in speed between deliverer 3 and 4 is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in speed between deliverer 3 and 4.", speed34_d$conf.int[1], speed34_d$conf.int[2])

#' 
#' #### Question5(b)(i)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate the proportion of deliveries that will be on time if M=25
DT_10 <- DT %>% filter(DT$Distance < 10)
DT_10$total <- DT_10$`Prep Time`+ DT_10$`Travel Time`
M_25 <- sum(DT_10$total <= 25)
n_DT <- nrow(DT_10)
p_hat_M_25 <- M_25/n_DT
q_hat_M_25 <- 1-p_hat_M_25

# 95% CI for all deliveries that will be on time if M=25
l95_M_25 <- p_hat_M_25 - (z95 * sqrt(p_hat_M_25 * q_hat_M_25/n_DT))
r95_M_25 <- p_hat_M_25 + (z95 * sqrt(p_hat_M_25 * q_hat_M_25/n_DT))

l95_rounded_M_25 <- round(l95_M_25, digits = 3)
r95_rounded_M_25 <- round(r95_M_25, digits = 3)

# Calculate the proportion of deliveries that will be on time if M=30
M_30 <- sum(DT_10$total <= 30)
p_hat_M_30 <- M_30/n_DT
q_hat_M_30 <- 1-p_hat_M_30

# 95% CI for all deliveries that will be on time if M=30
l95_M_30 <- p_hat_M_30 - (z95 * sqrt(p_hat_M_30 * q_hat_M_30/n_DT))
r95_M_30 <- p_hat_M_30 + (z95 * sqrt(p_hat_M_30 * q_hat_M_30/n_DT))

l95_rounded_M_30 <- round(l95_M_30, digits = 3)
r95_rounded_M_30 <- round(r95_M_30, digits = 3)

# Calculate the proportion of deliveries that will be on time if M=35
M_35 <- sum(DT_10$total <= 35)
p_hat_M_35 <- M_35/n_DT
q_hat_M_35 <- 1-p_hat_M_35

#95% CI for all deliveries that will be on time if M=35
l95_M_35 <- p_hat_M_35 - (z95 * sqrt(p_hat_M_35 * q_hat_M_35/n_DT))
r95_M_35 <- p_hat_M_35 + (z95 * sqrt(p_hat_M_35 * q_hat_M_35/n_DT))

l95_rounded_M_35 <- round(l95_M_35, digits = 3)
r95_rounded_M_35 <- round(r95_M_35, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval for the proportion of deliveries that will be on time if M = 25 minutes: (%s, %s). We are 95 per cent confident that the interval actually contains the proportion of deliveries that will be on time if M = 25 minutes.", l95_rounded_M_25, r95_rounded_M_25)
sprintf("95 percent Confidence Interval for the proportion of deliveries that will be on time if M = 30 minutes: (%s, %s). We are 95 per cent confident that the interval actually contains the proportion of deliveries that will be on time if M = 30 minutes.", l95_rounded_M_30, r95_rounded_M_30)
sprintf("95 percent Confidence Interval for the proportion of deliveries that will be on time if M = 35 minutes: (%s, %s). We are 95 per cent confident that the interval actually contains the proportion of deliveries that will be on time if M = 35 minutes.", l95_rounded_M_35, r95_rounded_M_35)

#' 
#' #### Question5(b)(ii)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
sprintf("The range is (%s, %s) when M=25", 10*1000*(1 - r95_rounded_M_25), 10*1000*(1 - l95_rounded_M_25))
sprintf("The range is (%s, %s) when M=30", 10*1000*(1 - r95_rounded_M_30), 10*1000*(1 - l95_rounded_M_30))
sprintf("The range is (%s, %s) when M=35", 10*1000*(1 - r95_rounded_M_35), 10*1000*(1 - l95_rounded_M_35))

#' 
#' #### *Question5(c)(i)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
DT$`Predicted` <- DT$Distance*2.06 + 14.8 + 5
DT$`Total` <- DT$`Prep Time`+DT$`Travel Time`
DT$`on time` <- DT$`Predicted` - DT$`Total`
on_time <- sum(DT$`on time` >= 0)
p_hat_on_time <- on_time/nrow(DT)
q_hat_on_time <- 1-p_hat_on_time

#95% CI for all deliveries that will be on time
l95_on_time <- p_hat_on_time - (z95 * sqrt(p_hat_on_time * q_hat_on_time/nrow(DT)))
r95_on_time <- p_hat_on_time + (z95 * sqrt(p_hat_on_time * q_hat_on_time/nrow(DT)))

l95_rounded_on_time <- round(l95_on_time, digits = 3)
r95_rounded_on_time <- round(r95_on_time, digits = 3)

#Print out the result
sprintf("95 percent Confidence Interval for the proportion of all deliveries that will be within the guaranteed total delivery time: (%s, %s). We are 95 per cent confident that the interval actually contains the proportion of all deliveries that will be within the guaranteed total delivery time.", l95_rounded_on_time, r95_rounded_on_time)

#' 
#' #### Question5(c)(ii)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
sprintf("The range for the dollar amount of certificates is (%s, %s) . We are 95 per cent confident that this interval contains the true range for the dollar amount of certificate.", 10*1000*(1 - r95_rounded_on_time), 10*1000*(1 - l95_rounded_on_time))

#' 
#' ### Question6
#' 
#' $H_{0}: \mu=22$, $H_{1}: \mu < 22$
#' Since the main goal of the company is to generate profit, type 2 error is more acceptable(do not reject a bad advice) than type 1. Therefore, we should decrease the probability of type 1 error occurance and set significance level as 0.01. As the result rejects null hypothesis, the plan will be profitable.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
#Read in the file
SSA <- read_csv("SSA.csv")

#T test
t.test(SSA, conf.level = 0.99, mu=22)


#' 
#' ### Question7
#' 
#' $H_{0}:proportion of Republican < 0.5$, $H_{1}: proportion of Republican \ge 0.5$
#' As the result shows, it rejects null hypothesis at a 10 per cent significance level. There is sufficient evidence to support the claim that Republican will win the election.(proportion of votes > 0.5) Therefore, the networks can conclude from these data that the Republican candidate will win the state. They should announce the result as well.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
election <- read_csv("Elections.csv")
vote <- nrow(election)
vote_rep <- sum(election$Votes == "2")
vote_demo <- vote - vote_rep

prop.test(vote_rep, vote, 
          alternative = "greater", conf.level = 0.9, p = 0.5)

#' 
#' ### Question8
#' 
#' #### Question8(a)
#' 
#' $H_{0}$: \mu = 150, $H_{1}: \mu\neq150$
#' Yes, the sample evidence is statistically significant. At 1, 5 or 10 per cent significance level, we can reject null hypothesis.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
bill <- read_csv("GroceryBills.csv")
x_bar_bill <- mean(bill$Weekly_food_expense)

t.test(bill$Weekly_food_expense, alternative = "two.sided", conf.level = 0.99, mu=150)
t.test(bill$Weekly_food_expense, alternative = "two.sided", conf.level = 0.95, mu=150)
t.test(bill$Weekly_food_expense, alternative = "two.sided", conf.level = 0.9, mu=150)

#' 
#' #### Question8(b)
#' 
#' At the 1% confidence level, when sample mean < 153.0458 or mean > 165.619, it will reject the null hypothesis. At the 10% confidence level, when sample mean < 155.3581 or mean > 163.3067, it will reject the null hypothesis
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
t.test(bill$Weekly_food_expense, alternative = "two.sided", mu=150, conf.level = 0.99)
t.test(bill$Weekly_food_expense, alternative = "two.sided", mu=150, conf.level = 0.9)

#' 
#' ### Question9
#' 
#' $H_{0}:\sigma^2 = 250$, $H_{1}:\sigma^2 \neq 250$
#' At the 5 per cent significance, we can not reject null hypothesis. We don't have sufficient evidence to infer the operations research analyst’s assumption about the variance is wrong.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
demand <- read_csv("Demand.csv")

varTest(demand$Demand, alternative = "two.sided", 
        sigma.squared = 250)

#' 
#' ### Question10
#' Type 1 error: Reject sound advice. That is, reject $H_{0}$ (fuel can only last 45 mins) when it should not be rejected. That is, fuel can in fact last 45mins. In this situation, type 1 error can endanger other flights.Type 2 error: Do not reject bad advice. That is, do not reject $H_{0}$ when it should be rejected. That is, fuel can not last 45mins. In this situation, type 2 error can lead to a plane crash due to insufficient fuel.
#' 
#' ### Question11
#' 
#' ####Question11(a)(i)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
bay <- read_csv("BAY.csv", col_types = cols(`On Road Pct` = col_number()))

# For those over 3y
bay_3y <- bay%>%filter(`Stayed 3 Years` == "Yes")
x_bar_salary <- mean(bay_3y$`Starting Salary`)
sigma_salary <- sd(bay_3y$`Starting Salary`)
n_salary <- nrow(bay_3y)
  
# 95% CI
t95_salary <- qt(0.975, df = n_salary - 1, lower.tail = TRUE)
l95_salary <- x_bar_salary - (t95_salary*sigma_salary/sqrt(n_salary))
r95_salary <-x_bar_salary + (t95_salary*sigma_salary/sqrt(n_salary))

l95_rounded_salary <- round(l95_salary, digits = 3)
r95_rounded_salary <- round(r95_salary, digits = 3)

# For those leave before 3y
bay_n3y <- bay%>%filter(`Stayed 3 Years` == "No")
x_bar_nsalary <- mean(bay_n3y$`Starting Salary`)
sigma_nsalary <- sd(bay_n3y$`Starting Salary`)
n_nsalary <- nrow(bay_n3y)

# 95% CI
t95_nsalary <- qt(0.975, df = n_nsalary - 1, lower.tail = TRUE)
l95_nsalary <- x_bar_nsalary - (t95_nsalary*sigma_nsalary/sqrt(n_nsalary))
r95_nsalary <-x_bar_nsalary + (t95_nsalary*sigma_nsalary/sqrt(n_nsalary))

l95_rounded_nsalary <- round(l95_nsalary, digits = 3)
r95_rounded_nsalary <- round(r95_nsalary, digits = 3)

# Difference
salary_d <- t.test(bay_3y$`Starting Salary`, bay_n3y$`Starting Salary`, paired = FALSE, var.equal = TRUE , conf.level = 0.95)

# Print out the result
sprintf("95 percent Confidence Interval for the mean starting salary of all employees who stay at least three years with BAY: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean starting salary of all employees who stay at least three years with BAY.", l95_rounded_salary, r95_rounded_salary)
sprintf("95 percent Confidence Interval for the mean starting salary of all employees who leave before three years: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean starting salary of all employees who leave before three years.", l95_rounded_nsalary, r95_rounded_nsalary)
sprintf("95 percent Confidence Interval for the mean difference in starting salary between employees who stayed more than 3 years and who leave before 3 years is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in starting salary between employees who stayed more than 3 years and who leave before 3 years.", salary_d$conf.int[1], salary_d$conf.int[2])

#' 
#' #### Question11(a)(ii)
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# For those who stay and earn less
bay_less <- bay%>%filter(`Stayed 3 Years` == "Yes" & `Starting Salary` <= 37750)
bay_small_med <- bay%>%filter(`Starting Salary` <= 37750)
n_bay_less <- nrow(bay_less)
n_bay_small_med <- nrow(bay_small_med)
p_hat_bay_less <- n_bay_less/n_bay_small_med
q_hat_bay_less <- 1-p_hat_bay_less

# 95% CI
l95_bay_less <- p_hat_bay_less - (z95 * sqrt(p_hat_bay_less * q_hat_bay_less/n_bay_small_med))
r95_bay_less <- p_hat_bay_less + (z95 * sqrt(p_hat_bay_less * q_hat_bay_less/n_bay_small_med))

l95_rounded_bay_less <- round(l95_bay_less, digits = 3)
r95_rounded_bay_less <- round(r95_bay_less, digits = 3)

# For those who stay and earn more
bay_more <- bay%>%filter(`Stayed 3 Years` == "Yes" & `Starting Salary` > 37750)
bay_big_med <- bay%>%filter(`Starting Salary` > 37750)
n_bay_big_med <- nrow(bay_big_med)
n_bay_more <- nrow(bay_more)
p_hat_bay_more <- n_bay_more/n_bay_big_med
q_hat_bay_more <- 1-p_hat_bay_more

# 95% CI
l95_bay_more <- p_hat_bay_more - (z95 * sqrt(p_hat_bay_more * q_hat_bay_more/n_bay_big_med))
r95_bay_more <- p_hat_bay_more + (z95 * sqrt(p_hat_bay_less * q_hat_bay_more/n_bay_big_med))

l95_rounded_bay_more <- round(l95_bay_more, digits = 3)
r95_rounded_bay_more <- round(r95_bay_more, digits = 3)

# Difference
median_d <- prop.test(c(n_bay_less, n_bay_more), c(n_bay_small_med, n_bay_big_med))

# Print out the result
sprintf("95 percent Confidence Interval for the proportion who stay with BAY for at least three years whose starting salary is less than or equal to the median 37750: (%s, %s). We are 95 per cent confident that the interval actually contain the true proportion who stay with BAY for at least three years whose starting salary is less than or equal to the median 37750.", l95_rounded_bay_less, r95_rounded_bay_less)
sprintf("95 percent Confidence Interval for the proportion who stay with BAY for at least three years whose starting salary is above the median 37750: (%s, %s). We are 95 per cent confident that the interval actually contain the true proportion who stay with BAY for at least three years whose starting salary is above the median 37750.", l95_rounded_bay_more, r95_rounded_bay_more)
sprintf("95 per cent Confidence Interval for the difference between proportion of employees who stayed at least 3 years with starting salary less than or equal to median and those who is higher is: (%s, %s). We are 95 per cent confident that the interval actually contain the true difference in the proportion of these two clusters of employees.", median_d$conf.int[1], median_d$conf.int[2])

#' 
#' #### Question11(b)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# For those who stay at least 3y
x_bar_road <- mean(bay_3y$`On Road Pct`)
sigma_road <- sd(bay_3y$`On Road Pct`)
n_road <- nrow(bay_3y)
  
# 95% CI
t95_road <- qt(0.975, df = n_road - 1, lower.tail = TRUE)
l95_road <- x_bar_road - (t95_road*sigma_road/sqrt(n_road))
r95_road <- x_bar_road + (t95_road*sigma_road/sqrt(n_road))

l95_rounded_road <- round(l95_road, digits = 3)
r95_rounded_road <- round(r95_road, digits = 3)

# For those leave before 3y
bay_n3y <- bay%>%filter(`Stayed 3 Years` == "No")
x_bar_nroad <- mean(bay_n3y$`On Road Pct`)
sigma_nroad <- sd(bay_n3y$`On Road Pct`)
n_nroad <- nrow(bay_n3y)

# 95% CI
t95_nroad <- qt(0.975, df = n_nroad - 1, lower.tail = TRUE)
l95_nroad <- x_bar_nroad - (t95_nroad*sigma_nroad/sqrt(n_nroad))
r95_nroad <-x_bar_nroad + (t95_nroad*sigma_nroad/sqrt(n_nroad))

l95_rounded_nroad <- round(l95_nroad, digits = 3)
r95_rounded_nroad <- round(r95_nroad, digits = 3)

# Difference
on_road_d <- t.test(bay_3y$`On Road Pct`, bay_n3y$`On Road Pct`, paired = FALSE, var.equal = TRUE, conf.level = 0.95)

# For those who stay and earn less
med <- median(bay$`On Road Pct`)
bay_less_road <- bay%>%filter(`Stayed 3 Years` == "Yes" & `On Road Pct` <= med)
bay_less_med <- bay%>%filter(`On Road Pct` <= med)
n_bay_less_med <- nrow(bay_less_med)
n_bay_less_road <- nrow(bay_less_road)
p_hat_bay_less_road <- n_bay_less_road/n_bay_less_med
q_hat_bay_less_road <- 1-p_hat_bay_less_road

# 95% CI
l95_bay_less_road <- p_hat_bay_less_road - (z95 * sqrt(p_hat_bay_less_road * q_hat_bay_less_road/n_bay_less_med))
r95_bay_less_road <- p_hat_bay_less_road + (z95 * sqrt(p_hat_bay_less_road * q_hat_bay_less_road/n_bay_less_med))

l95_rounded_bay_less_road <- round(l95_bay_less_road, digits = 3)
r95_rounded_bay_less_road <- round(r95_bay_less_road, digits = 3)

# For those who stay and drive more
bay_more_road <- bay%>%filter(`Stayed 3 Years` == "Yes" & `On Road Pct` > med)
bay_big_med <- bay%>%filter(`On Road Pct` > med)
n_bay_big_med <- nrow(bay_big_med)
n_bay_more_road <- nrow(bay_more_road)
p_hat_bay_more_road <- n_bay_more_road/n_bay_big_med
q_hat_bay_more_road <- 1-p_hat_bay_more_road

# 95% CI
l95_bay_more_road <- p_hat_bay_more_road - (z95 * sqrt(p_hat_bay_more_road * q_hat_bay_more_road/n_bay_big_med))
r95_bay_more_road <- p_hat_bay_more_road + (z95 * sqrt(p_hat_bay_less_road * q_hat_bay_more_road/n_bay_big_med))

l95_rounded_bay_more_road <- round(l95_bay_more_road, digits = 3)
r95_rounded_bay_more_road <- round(r95_bay_more_road, digits = 3)

# Difference
median_road_d <- prop.test(c(n_bay_less_road, n_bay_more_road), c(n_bay_less_med, n_bay_big_med))

# Print out the result
sprintf("95 percent Confidence Interval for the mean percentage of time on the road  of all employees who stay at least three years with BAY: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean percentage of time on the road of all employees who stay at least three years with BAY.", l95_rounded_road, r95_rounded_road)
sprintf("95 percent Confidence Interval for the mean percentage of time on the road of all employees who leave before three years: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean percentage of time on the road of all employees who leave before three years.", l95_rounded_nroad, r95_rounded_nroad)
sprintf("95 percent Confidence Interval for the mean difference in time on the road between employees who stayed more than 3 years and who leave before 3 years is: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean difference in time on the road between employees who stayed more than 3 years and who leave before 3 years.", on_road_d$conf.int[1], on_road_d$conf.int[2])
sprintf("95 percent Confidence Interval for the proportion who stay with BAY for at least three years whose percentage of time on the road is less than or equal to the median: (%s, %s), We are 95 per cent confident that the interval actually contain the true proportion who stay with BAY for at least three years whose percentage of time on the road is less than or equal to the median.", l95_rounded_bay_less_road, r95_rounded_bay_less_road)
sprintf("95 percent Confidence Interval for the proportion who stay with BAY for at least three years whose percentage of time on the road is above the median: (%s, %s), We are 95 per cent confident that the interval actually contain the true proportion who stay with BAY for at least three years whose percentage of time on the road is above the median.", l95_rounded_bay_more_road, r95_rounded_bay_more_road)
sprintf("95 per cent Confidence Interval for the difference between proportion of employees who stayed at least 3 years with time on road less than or equal to median and those who is higher is: (%s, %s). We are 95 per cent confident that the interval actually contain the true difference in the proportion of these two clusters of employees.", median_road_d$conf.int[1], median_road_d$conf.int[2])

#' 
#' ####Question11(c)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
tenure <- na.omit(bay)
x_bar_tenure <- mean(tenure$Tenure)
sigma_tenure <- sd(tenure$Tenure)
n_tenure <- nrow(tenure)
  
# 95% CI
t95_tenure <- qt(0.975, df = n_tenure - 1, lower.tail = TRUE)
l95_tenure <- x_bar_tenure - (t95_tenure*sigma_tenure/sqrt(n_tenure))
r95_tenure <- x_bar_tenure + (t95_tenure*sigma_tenure/sqrt(n_tenure))

l95_rounded_tenure <- round(l95_tenure, digits = 3)
r95_rounded_tenure <- round(r95_tenure, digits = 3)

# Print out the result
sprintf("95 percent Confidence Interval for the mean tenure (in months) of all employees who leave BAY within three years of being hired: (%s, %s), We are 95 per cent confident that the interval actually contain the true mean tenure (in months) of all employees who leave BAY within three years of being hired.", l95_rounded_tenure, r95_rounded_tenure)
sprintf("Because current data do not contain such record. Once an employee stayed over three years in BAY, her/his Tenure column will show NA(no record). Therefore, we can not calculate a confidence interval for the mean tenure at BAY among all systems analysts who leave BAY after three years")

#' 
#' #### Question11(d)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Went to State U
bay_state <- bay%>%filter(`State Univ`=="Yes")
bay_3y_state <- sum(bay_state$`Stayed 3 Years`=="Yes")
n_bay_state <- nrow(bay_state)
p_hat_bay_3y_state <- bay_3y_state/n_bay_state
q_hat_bay_3y_state <- 1-p_hat_bay_3y_state

# 95% CI
l95_bay_3y_state <- p_hat_bay_3y_state - (z95 * sqrt(p_hat_bay_3y_state * q_hat_bay_3y_state/n_bay_state))
r95_bay_3y_state <- p_hat_bay_3y_state + (z95 * sqrt(p_hat_bay_3y_state * q_hat_bay_3y_state/n_bay_state))

l95_rounded_bay_3y_state <- round(l95_bay_3y_state, digits = 3)
r95_rounded_bay_3y_state <- round(r95_bay_3y_state, digits = 3)

# No State U
bay_nstate <- bay%>%filter(`State Univ`=="No")
bay_3y_nstate <- sum(bay_nstate$`Stayed 3 Years`=="No")
n_bay_nstate <- nrow(bay_nstate)
p_hat_bay_3y_nstate <- bay_3y_nstate/n_bay_nstate
q_hat_bay_3y_nstate <- 1-p_hat_bay_3y_nstate

# 95% CI
l95_bay_3y_nstate <- p_hat_bay_3y_nstate - (z95 * sqrt(p_hat_bay_3y_nstate * q_hat_bay_3y_nstate/n_bay_nstate))
r95_bay_3y_nstate <- p_hat_bay_3y_nstate + (z95 * sqrt(p_hat_bay_3y_nstate * q_hat_bay_3y_nstate/n_bay_nstate))

l95_rounded_bay_3y_nstate <- round(l95_bay_3y_nstate, digits = 3)
r95_rounded_bay_3y_nstate <- round(r95_bay_3y_nstate, digits = 3)

# With CIS
bay_CIS <- bay%>%filter(`CIS Degree`=="Yes")
bay_3y_CIS <- sum(bay_CIS$`Stayed 3 Years`=="Yes")
n_bay_CIS <- nrow(bay_CIS)
p_hat_bay_3y_CIS <- bay_3y_CIS/n_bay_CIS
q_hat_bay_3y_CIS <- 1-p_hat_bay_3y_CIS

# 95% CI
l95_bay_3y_CIS <- p_hat_bay_3y_CIS - (z95 * sqrt(p_hat_bay_3y_CIS * q_hat_bay_3y_CIS/n_bay_CIS))
r95_bay_3y_CIS <- p_hat_bay_3y_CIS + (z95 * sqrt(p_hat_bay_3y_CIS * q_hat_bay_3y_CIS/n_bay_CIS))

l95_rounded_bay_3y_CIS <- round(l95_bay_3y_CIS, digits = 3)
r95_rounded_bay_3y_CIS <- round(r95_bay_3y_CIS, digits = 3)

# No CIS
bay_nCIS <- bay%>%filter(`CIS Degree` == "No")
bay_3y_nCIS <- sum(bay_nCIS$`Stayed 3 Years`=="Yes")
n_bay_nCIS <- nrow(bay_nCIS)
nCIS <- prop.test(bay_3y_nCIS, n_bay_nCIS, alternative = "two.sided")

# Print out the result
sprintf("95 percent Confidence Interval for proportion of employee who studied in State University and stayed 3 years: (%s, %s), We are 95 per cent confident that the interval actually contain the true proportion of employee who studied in State University and stayed 3 years.", l95_rounded_bay_3y_state, r95_rounded_bay_3y_state)
sprintf("95 percent Confidence Interval for proportion of employee who never studied in State University and stayed 3 years: (%s, %s), We are 95 per cent confident that the interval actually contain the true for proportion of employee who never studied in State University and stayed 3 years.", l95_rounded_bay_3y_nstate, r95_rounded_bay_3y_nstate)
sprintf("95 percent Confidence Interval for proportion of employee who got a CIS degree and stayed 3 years: (%s, %s), We are 95 per cent confident that the interval actually contain the true proportion of employee who got a CIS degree and stayed 3 years.", l95_rounded_bay_3y_CIS, r95_rounded_bay_3y_CIS)
sprintf("95 percent Confidence Interval for proportion of employee who never got a CIS degree and stayed 3 years: (%s, %s), We are 95 per cent confident that the interval actually contain the true proportion of employee who stayed 3 years and never got a CIS degree.", nCIS$conf.int[1], nCIS$conf.int[2])
sprintf("As the confidence interval shows, among those who studied in State University, the proportion of employees who stayed over three years is similar to the proportion of employees who stayed over three years but have never been to a State University. So, employees from State University are not harder to retain. However, employees with a CIS degree are indeed harder to retain, since the proportion of employees with a CIS degree who stayed over 3 years is lower than those without a CIS degree.")

#' 
#' ### Question12
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
pwr.p.test(h = ES.h(p1 = 0.6, p2 = 0.5),
           sig.level = 0.1, alternative = "greater",
           n = 50)
pwr.r.test(r=0.2, sig.level = 0.1, alternative = "greater", power=0.8)

#Print out the result
sprintf("The power of the test is 55.6 per cent. The sample size should be at least 112 to attain an 80 per cent power.")

