library(dplyr)
library(tidyverse)
library(ggplot2)
library(DataExplorer)

df <- OfficialDataMiningSheet

#Exclude players with under 300 minutes in the season - these players are often misrepresented by advanced or per game stats
filt <- df %>% filter(df$MP > 300)

#Allows us to view only players making over $10 million in '18-'19
overTenMil <- df %>% filter(df$Salary > 10000000)

#Maps a player's True Shooting % (overall indicator of scoring efficiency) by '18-'19 salary
filt %>% ggplot() + geom_point(aes(x=TS, y=Salary))


#Produces a series of charts based off of total population
filt %>% plot_histogram()
filt %>% plot_qq()
filt %>% plot_qq(by = "SalaryRange")
filt %>% plot_correlation()

#Produces a series of charts based off of population making over $10 million
overTenMil %>% plot_histogram()
overTenMil %>% plot_qq()
overTenMil %>% plot_qq(by = "SalaryRange")
overTenMil %>% plot_correlation()


#Produces summary statistics based on different variables
filt %>% group_by(Position) %>% summarize(avgPER = mean(PER), avgSalary = mean(Salary), count = n())
filt %>% group_by(AgeRange) %>% summarize(avgPER = mean(PER), avgSalary = mean(Salary), count = n())
filt %>% group_by(SalaryRange) %>% summarize(avgPER = mean(PER), avgAge = mean(Age))


#Produces summary statistics based on different variables, limited to players making over $10 million
overTenMil %>% group_by(Position) %>% summarize(avgPER = mean(PER), avgSalary = mean(Salary), count = n())
overTenMil %>% group_by(AgeRange) %>% summarize(avgPER = mean(PER), avgSalary = mean(Salary), count = n())
overTenMil %>% group_by(SalaryRange) %>% summarize(avgPER = mean(PER), avgAge = mean(Age))

#Breaks down players by age ranges and position & produces summary statistics on each group
disp<-filt %>% group_by(AgeRange, Position) %>% summarize(avgSalary = mean(Salary), count = n(), avgMP = mean(MP))
View(disp)


#Analyze salary statistics for players we consider "average" as defined by PER
avgPER <- filt %>% filter(14 < PER, 16 > PER)
View(avgPER)
avgPER %>% group_by(Position) %>% summarize(avgSalary = mean(Salary), medSalary = median(Salary), count = n())
avgPER %>% ggplot() + geom_point(aes(x=Salary, y=PER, color=Position))

#Analyze salary statistics for players we consider "elite" as defined by PER
elitePER <- filt %>% filter(PER > 22)
view(elitePER)
elitePER %>% group_by(Position) %>% summarize(avgSalary = mean(Salary), medSalary = median(Salary), count = n())

#Analyze salary statistics for players we consider "average" as defined by Usage %, or how often they were involved in the final shot/turnover
avgUSG <- filt %>% filter(19 < `USG%`, 21 > `USG%`)
View(avgUSG)
avgUSG %>% group_by(Position) %>% summarize(avgSalary = mean(Salary), medSalary = median(Salary), count = n())
avgUSG %>% ggplot() + geom_point(aes(x=Salary, y=PER, color=Position))

#Analyze salary statistics for players we consider "elite" as defined by Usage %, or how often they were involved in the final shot/turnover
eliteUSG <- filt %>% filter(`USG%` > 28)
view(eliteUSG)
eliteUSG %>% group_by(Position) %>% summarize(avgSalary = mean(Salary), medSalary = median(Salary), count = n())



