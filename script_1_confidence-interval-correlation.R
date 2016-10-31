library(learnSampling)
library(psych)
library(ggplot2)
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
