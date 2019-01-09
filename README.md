# Statistics-R-Project: BRFFS Database

Project Description:
Identify 3 research questions similar to questions introduced at Duke Univerisyt Statistics R course: the Behavioral Risk Factor Surveillance System (BRFSS) dataset (provided below).

Database:
Behavioral Risk Factor Surveillance System (BRFSS) dataset from Centers for Disease Control and Prevention (CDC)


# Introduction & Research Questions:
My research questions are divided into 2 sections. 
First, i would like to examine whether having enough sleeping time or not will affect people's physical health and the ability to do usual daily tasks. According to National Sleep Foundation, adults normally needs at least 7hrs of sleep to maintain a health status. My hypothesis is that poeple who have sleep at least 7hrs will have better physical health condition, and will have better ability to do usual activities.
Second, Medicare in U.S is designed to help people over 65+ or people have disabilities. I would like to find out whether income level, medical cost, and disabilities correlates with whether have Medicare or not.

Research Question 1: 
Do people sleep at least 7hrs have better physical health condition?

Research Question 2: 
Do people sleep at least 7hrs have low difficulty doing usual activities?

Research Question 3:
How income level correlates with medical cost & is Medicare helping people with disabilities?

# Exploratory data analysis
Research Question 1
Sleeptime <- brfss2013%>%
  mutate(sleeptime = ifelse(sleptim1 >=7, "over 7hr","below 7hr"))%>%
  select(physhlth, sleeptime)
Sleeptime <- na.omit(Sleeptime)




