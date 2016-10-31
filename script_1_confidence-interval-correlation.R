library(learnSampling)
library(psych)
library(ggplot2)
library(pwr)
library(predictionInterval)

psych::r.con(r=.21,n=100)
#0.01416621 0.39031834
##means that population correlation can be as low as ... and high as ... 

set.seed(8)
lower.pop <- get_cor_data(.01,100000)
upper.pop <- get_cor_data(.39,100000)
head(lower.pop)
head(upper.pop)
#EACH ROW OF DATA FRAME CORRESPONDS TO AN INDIVIDUAL IN THE POPULATION

rho.lower <- cor(lower.pop$x,lower.pop$y)
rho.upper <- cor(upper.pop$x,upper.pop$y)
print(rho.lower)
print(rho.upper)

#LOWER BOUND DISTRIBUTION

ci.lower.bound.correlations <- get_cor_samples(pop.data=lower.pop,n=100,number.of.samples=10000,number.of.decimals=2)
#Shows which sample correlations are possible when the population correlation is .01 

ci.lower.bound.correlations.sorted <- sort_samples_by_r(ci.lower.bound.correlations)
#Shows which sample correlations are likely when population correlation is .01 

lower.cilowerbound <- ci.lower.bound.correlations.sorted$r[251]
upper.cilowerbound <- ci.lower.bound.correlations.sorted$r[9750]
print(lower.cilowerbound)
print(upper.cilowerbound)
#means that any sample correlation in range of -.19, and .21 is likely due to the effects of random sampling when the population correlation is .01 

lower.bound.hist <- qplot(r, data=ci.lower.bound.correlations,binwidth=.05)
print(lower.bound.hist)
#see that .21 well within possible range 

#UPPER BOUND DISTRIBUTION
ci.upper.bound.correlations <- get_cor_samples(pop.data=upper.pop, n=100, number.of.samples=10000,number.of.decimals=2)
ci.upper.bound.correlations.sorted <- sort_samples_by_r(ci.upper.bound.correlations)
lower.ciuppderbound <- ci.upper.bound.correlations.sorted$r[251]
upper.ciupperbound <- ci.upper.bound.correlations.sorted$r[9750]
print(upper.ciupperbound)
print(lower.ciuppderbound)
#.21 to .54
upper.bound.hist <- qplot(r, data=ci.upper.bound.correlations,binwidth=.05)
print(upper.bound.hist)

#GRAPH BOTH DISTRIBUTIONS 
both.hist <- ggplot (ci.upper.bound.correlations,aes(r))
both.hist <- both.hist + geom_histogram(aes(x= ci.upper.bound.correlations$r, y=..count..),fill="red", binwidth=.05)
both.hist <- both.hist + geom_histogram(aes(x=ci.lower.bound.correlations$r, y=..count..), fill="blue", binwidth = .05)
print(both.hist)      

#12. Applying cOnfidence intervals
psych::r.con(r=.30,n=50)
psych::r.con(r=.30,n=500)
psych::r.con(r=.30,n=1000)
#0.02362508 0.53377519
#0.2180456 0.3777530
#0.2425172 0.3553837

pwr.r.test(r=.35,power=.80,alternative="two.sided")
#N=60.93514
##take with a grain of salt - traditional power analysis assumes population correlation is known
##thus if population correlation is lower but caused this sample r, this power analysis would 
##vastly underestimate the number of participants needed to obtain a power of .8


#solution = SAFEGUARD POWER ANALYSIS 
psych::r.con(r=.35,n=100)
#0.1649195 0.5112702
#Then use the lower limit (lowest probable parameter) and calculate power analysis using this 
pwr.r.test(r=.01649, power=.8, alternative="two.sided")
#N = 28861.45

#DOING IT YOURSELF
##Assume you're looking into a new field of research with ONE study. N=70, r=.41
psych::r.con(r=.41,n=70)
pwr.r.test(r=.1937, power=.8, alternative = "two.sided")

pi.r(r=.41,n=70,rep.n=214)
#95% CI[.19,.59], 95% PI [.17,.62]
#Indicating that the population correlation responsible for the r=.41 correlation could be as low 
##as p=.19 or as high as .59. But due to sampling error, you get a replication correlation out 
##of this range as indicated by the prediction interval. Thus, replication correlation can be 
##as low as r=.17 or as high as r=.62. 

#The intervals are wide because original study used a small sample size. 
