dat = read.csv('dagdata-master/inst/extdata/femaleMiceWeights.csv')
mean(dat[13:24,2]) - mean(dat[1:12,2])
s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)
abline(h = sapply(s, mean), col=1:2)
#1.1
length(which(s$hf < mean(s$chow)))

#1.2
length(which(s$chow > mean(s$hf)))

#1.3
highfat =  s[["hf"]]
sample(highfat, 6) #The sample function goes to the vector and choses 6 values at random
#default is like chosing from a vector something and never putting it back if replace is true only then we put it back
#what it means that multiple draws of sample thing try 
sample(highfat, 6, replace=TRUE)

as.numeric(highfat > 30)
mean(highfat > 30)

mean(dat[13:24,2]) - mean(dat[1:12,2])
#So the typical a statistician tries to answer is that the diff is due to chance or something else
#We define a null hypothesis
population <- read.csv('dagdata-master/inst/extdata/femaleControlsPopulation.csv')
control <- sample(population[,1], 12)
mean(control) # I repeat the above again and it changes again and agian and this mean is my rand variable

n <- 1000
null <- vector("numeric", n)
for(i in 1:n)
{
  control <- sample(population[,1], 12)
  treatment <- sample(population[,1], 12)
  null[i] <- mean(treatment) - mean(control) # null distribution of differences s.t no true effect of a high fat diet
}
diff <- mean(dat[13:24,2]) - mean(dat[1:12,2])
mean(null > diff) #Prob of null hypothesis being true and p-value

#2.1
mean(population[,1])
#An improved version of what we did earlier using replicate
sampleMean = replicate(10000, mean(sample(population[,1], 12)))
plot(sampleMean)
null = replicate(10000, mean(sample(population[,1], 12)) - mean(sample(population[,1], 12)))
plot(null)
hist(null)
#quiet often while stacking the values which are closer gives us an idea
#If we look at the number of null distributions values to the right of line we calculate the prob of observing 
#larger diff from null distribution also called one-tailed probabilty
#By looking at tails at both sides we calculated the probaility of observing as extreme difference from null distribution
#Also called two tail probaility and referred to as p-value
#3.1
null = replicate(10000, mean(sample(population[,1], 12)) - mean(sample(population[,1], 12)))
mean(null > diff)

#3.2
null = replicate(10000, abs(mean(sample(population[,1], 12)) - mean(sample(population[,1], 12))))
mean(null > diff)

#Prob Dist2
library(devtools)
install_github("jennybc/gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

#CDF F(a) tells you the proportion of the values which are less than or equal to a
?ecdf
gapminder[which(gapminder$year == 1952)]

#1.1
x = gapminder$lifeExp[gapminder$year == 1952]
mean(x <= 40)
#1.2
mean(x <= 60) - mean(x <= 40)
prop = function( q){
  mean(x <= q)
}
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)
plot(ecdf(x))

#Normal Distribution
#If  x follows normal distribution then we need only sigma and mu to know everything needed to know
#Standarised Units- We subtract from each point the mean and divide by standard deviation
#We define each point and say how far in terms of sd is that point from my mean
#2.1,2.2
pop = gapminder$pop[which(gapminder$year == 1952)]
hist(pop)
hist(log10(pop))
sd(log10(pop))
x = log10(pop)
qqnorm(x)
z = (x-mean(x))/sd(x)
max(z)

#2.3
#pnorm(q) #takes a value q and returns the proportion of normal distribution which is less than q
F = function(q)
{
  pnorm(q, mean = mean(x), sd = sd(x))
}
(F(7)-F(6))*length(x)
sum(x > 6 & x <= 7)
sd(z)
head(pnorm(z))
n = length(x)
ps = ((1:n) - 0.5)/n
sort(x)

#2.4
qnorm(ps)[1]


