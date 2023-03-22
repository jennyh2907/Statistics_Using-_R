#' ---
#' title: "HW4_FOA_Ching-Wen Huang"
#' output: html_document
#' date: "2022-09-10"
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

#' 
#' ### Question 1
#' 
#' #### Question1(a)
#' 
#' According to the plot, the normality condition is satisfied. Also the p-value from Shapiro-Wilk test is high, so we fails to reject the null hypothesis. The data are coming from normal population.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
#Read in the file
time <- read_csv("IRS 1.csv")

# Test the normality: residuals and fitted value
time_stacked <- stack(time)
names(time_stacked) <- c("Time", "Form")

Time <- time_stacked$Time
Form <- time_stacked$Form

model_time <- lm(Time ~ Form)
resids_time <- residuals(model_time)
preds_time <- predict(model_time)

# Test the normality formally: Shapiro-Wilk Test
shapiro.test(resids_time)

# Test the normality informally: Draw QQ plot
qqnorm(resids_time)
qqline(resids_time)

#' 
#' #### Question1(b)
#' 
#' According to the plot, and the high p-value from Bartlett test, we fail to reject null hypothesis. The variances are equal.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Equal variance check
bartlett.test(Time ~ Form)

# Residuals versus Fits plot
plot(fitted(model_time), residuals(model_time))

#' 
#' #### Question1(c)
#' 
#' The test statistic is 2.936 and the p-value is 0.0363. There is enough evidence to infer that there are differences among the forms.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# One way ANOVA
ANOVA_time <- aov(Time ~ Form)
summary(ANOVA_time)

#' 
#' #### Question1(d)
#' 
#' According to the p-value from Tukey's test, there are differences between form4-form1.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
#Tukey's test
TukeyHSD(ANOVA_time)

#' 
#' ### Question 2
#' 
#' ####Question2(a)
#' 
#' According to the plot, the normality condition is not satisfied. Also the p-value from Shapiro-Wilk test is low, so we reject the null hypothesis. The data are not coming from normal population.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
time2 <- read_csv("IRS 2.csv")

time2_stacked <- gather(time2, key="Form Type", value="Time", 2:5)

# Test the normality: residuals and fitted value
Time2 <- time2_stacked$Time
form_type <- factor(time2_stacked$`Form Type`)
taxpayer <- factor(time2_stacked$`Taxpayer`)

# Calculate the residuals and the fitted values
model_time2 <- lm(Time2 ~ form_type + taxpayer)
resids_time2 <- residuals(model_time2)

# Test the normality formally: Shapiro Wilk Test
shapiro.test(resids_time2)

# Test the normality informally: QQ plot
qqnorm(resids_time2)
qqline(resids_time2)

#' 
#' #### Question2(b)
#' 
#' According to the plot, and the high p-value from Levene's test, we fail to reject null hypothesis. The variances are equal.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Run Levene's test(not normal) for equal variances
car::leveneTest(Time2 ~ taxpayer)
car::leveneTest(Time2 ~ form_type)

# Residuals versus Fits plot
plot(fitted(model_time2), residuals(model_time2))

#' 
#' #### Question2(c)(d)
#' 
#' The test statistic is 21.16 and the p-value is really small. There is enough evidence to infer that there are differences among the forms. And the blocking is effective as p-value for form_type and taxpayer both drop significantly.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# One way ANOVA
ANOVA_time2 <- aov(Time2 ~ form_type + taxpayer)
summary(ANOVA_time2)

#' 
#' #### Question2(e)(f)
#' 
#' According to the test result, except form2-form1, the p-values for all groups are high, so the difference lies in form3-form1, form4-form1, form3-form2, form4-form2, form4-form3. Since we do blocking in Question2, we can make a more accurate inference to the data, and therefore detect delicate differences that lie in different group.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Tukey's test
TukeyHSD(ANOVA_time2, which = 'form_type', ordered = TRUE)

#' 
#' #### Question2(g)
#' 
#' In Question1, the residual standard error is 31.00054. In Question2, the residual standard error is 8.139346. Since the experiment adopted a RBD design, SSE is partitioned by SSB and SSE. SSB, which is explainable, account for partial SSE. Therefore, SSE is lower in Question 2.
#' 
#' ### Question 3
#' 
#' #### Question3(a)(b)(c)
#' 
#' There are two factors, form type and income level respectively. For form type, there are 4 levels(Form 1, 2, 3, 4). For income level, there are 3 levels(1 = Low Income, 2 = Next Income Bracket, 3 = Highest Bracket). And there are 12 treatments.(3x4)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Read in the file
time3 <- read_csv("IRS 3.csv")

#' 
#' #### Question3(d)
#' 
#' First, the p-value for income group:form type is 0.403>any reasonable significance level. So, we can conclude that income group and form type do not interact to affect the time to finish the form. However, in interaction plot, we can still observe some interaction(lines are crossing each other) that contradicts ANOVA result. Noted that it may cause by chance, and the interaction does not necessarily exists.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Stack the data
time3_stacked <- gather(time3, key="Form Type", value="Time", 2:5)

# Test the normality: residuals and fitted value
Time3 <- time3_stacked$Time
form_type3 <- factor(time3_stacked$`Form Type`)
income_group <- factor(time3_stacked$Group)

# Calculate the residuals and the fitted values
model_time3 <- lm(Time3 ~ form_type3 + income_group)
resids_time3 <- residuals(model_time3)

# Test the normality formally: Shapiro Wilk Test
shapiro.test(resids_time3)

# Test the normality informally: QQ plot
qqnorm(resids_time3)
qqline(resids_time3)

# Run Levene's test for equal variances
car::leveneTest(Time3 ~ income_group)
car::leveneTest(Time3 ~ form_type3)

# Constant Variance Test: Residuals versus Fits
plot(fitted(model_time3), residuals(model_time3))

# Run two way ANOVA
ANOVA_time3 <- aov(Time3 ~ income_group + form_type3 + income_group*form_type3)
summary(ANOVA_time3)

# Draw Interaction plot
interaction.plot(income_group, form_type3, Time3) 
interaction.plot(form_type3, income_group, Time3) 

#' 
#' #### Question3(e)
#' 
#' The p-value for form type is 0.0586>0.05 significance level. So, we can conclude that form type has no effect on the time to finish the form. There are no difference exist among the 4 forms at 5% significance level.
#' 
#' #### Question3(f)
#' 
#' The p-value for income group is 0.019<0.05 significance level. So, we can conclude that income group has an effect on the time to finish the form. Taxpayers in different brackets require different amounts of time to complete their tax forms.
#' 
#' ### Question 4
#' 
#' #### Question4(a)(b)(c)
#' 
#' There are two factors, detergent brand and water temperature respectively. For detergent brand, there are 5 levels(detergent 1, 2, 3, 4, 5). For water temperature, there are 3 levels(cold, warm, hot). And there are 15 treatments.(3x5)
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
detergent <- read_csv("Detergents.csv")

#' 
#' #### Question4(d)
#' 
#' The p-value for water_temp:detergent_brand is 2.881e-06<any reasonable significance level. So, we can conclude that water temperature and detergent brand do interact to affect the clean score. For all detergent brands except detergent 2, higher water temperature led to better clean score. However, for detergent 2, as it performed well in hot water, its clean score in cold water was actually higher than in warm water. Also, we can also observe a significant drop of clean score for detergent 4 in cold water compared to its score in hot and warm water.
#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Stack the data
clean_stacked <- gather(detergent, key="Detergent Brand", value="Clean Score", 2:6)
clean_stacked$`Detergent Brand` <- replace(clean_stacked$`Detergent Brand`, clean_stacked$`Detergent Brand`=="Detergent1", "1")
clean_stacked$`Detergent Brand` <- replace(clean_stacked$`Detergent Brand`, clean_stacked$`Detergent Brand`=="Detergent2", "2")
clean_stacked$`Detergent Brand` <- replace(clean_stacked$`Detergent Brand`, clean_stacked$`Detergent Brand`=="Detergent3", "3")
clean_stacked$`Detergent Brand` <- replace(clean_stacked$`Detergent Brand`, clean_stacked$`Detergent Brand`=="Detergent4", "4")
clean_stacked$`Detergent Brand` <- replace(clean_stacked$`Detergent Brand`, clean_stacked$`Detergent Brand`=="Detergent5", "5")

# Test the normality: residuals and fitted value
Clean_score <- clean_stacked$`Clean Score`
Water_Temp <- factor(clean_stacked$Temperature)
Detergent_brand <- factor(clean_stacked$`Detergent Brand`)

# Calculate the residuals and the fitted values
model_clean <- lm(Clean_score ~ Water_Temp + Detergent_brand)
resids_clean <- residuals(model_clean)

# Test the normality formally: Shapiro Wilk Test
shapiro.test(resids_clean)

# Test the normality informally: QQ plot
qqnorm(resids_clean)
qqline(resids_clean)

# Run Bartlett's test for equal variances
bartlett.test(Clean_score ~ Water_Temp)
bartlett.test(Clean_score ~ Detergent_brand)

# Constant Variance Test: Residuals versus Fits
plot(fitted(model_clean), residuals(model_clean))

# Run Welch's ANOVA
oneway.test(Clean_score ~ Water_Temp, var.equal = FALSE)
oneway.test(Clean_score ~ Detergent_brand, var.equal = FALSE)
oneway.test(Clean_score ~ Water_Temp*Detergent_brand, var.equal = FALSE)

# Draw Interaction plot
interaction.plot(Water_Temp, Detergent_brand, Clean_score) 
interaction.plot(Detergent_brand, Water_Temp, Clean_score) 

#' 
#' #### Question4(e)
#' 
#' No, there's no reason to conduct the tests of the main effect, since we've already had enough evidence to infer that there is an interaction.
#' 
#' #### Question4(f)
#' 
#' No reason to conduct the tests of the main effect. No answer.
