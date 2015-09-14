#Statistical Inference
#are 2 things different
#smoker's babies weigh less than non-smoker's; risk of cancer in smoker vs non-smoker etc
#statistical inference tries to quantify above

#Imagine you are given a task you are given a population you have never seen before and have to decide whether men are
#taller than women, you can measure but not see and each measurement costs $1000 
#How many people you measure to answer the question
#Taking measurement of all not feasible
#Statistical Inference is the tool
#You take a random sample of 10 men and 10 women and diff is 3.3 of that random sample
#If we take another sample will the answer be same or diff or neg
#Statistical Inference helps us out without having to take measurements
#We take sample average and we want to find out how close it is to the population average
#This is the question that central limit theorem helps us to answer

#Central Limit Theorem
#Look Notes
#We still don't know population sds
#Examples
dat = read.csv('dagdata-master/inst/extdata/mice_pheno.csv')
hfPopulation <- dat[dat$Sex=="F" & dat$Diet=="hf",3]
controlPopulation <- dat[dat$Sex=="F" & dat$Diet=="chow",3]
#t-test
dat = read.csv('dagdata-master/inst/extdata/femaleMiceWeights.csv')
control <- dat[1:12, 2]
treatment <- dat[13:24, 2]
diff <- mean(treatment) - mean(control)
t.test(treatment, control)

#CLT
sd(control)
sd(control)/sqrt(length(control))

se <- sqrt(var(treatment)/length(treatment) + var(control)/length(control))

tstat <- diff/se
pval <- 1 - pnorm(tstat) + pnorm(-tstat) #Note that the above is not same as given by t.test as it does not follow CLT(small sample size)
#We can use t-distribution if population is normally distributed
#We assume that sample comes from a nature that is normally distributed
qqnorm(control)
qqline(control)
#t-distribution takes into account that you are estimating the s.e of r.v

#1
babies = read.table("dagdata-master/inst/extdata/babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

dat.s = bwt.smoke[1:30]
dat.ns = bwt.nonsmoke[1:30]
X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/30+sd.s^2/30)
tval = (X.ns - X.s)/sd.diff

#No matter how we compute it, p-val is the prob that null hypothesis could have generated a t-val more extreme than
#the t-val we observed. If the p-value is very small, this means that observing a a value more extreme than tval would be very rare if the null hypothesis were true, 
#and would give strong evidence that we should reject the null hypothesis. We determine how small the p-value needs 
#to be to reject the null by deciding how often we would be willing to mistakenly reject the null hypothesis.

#The standard decision rule is the following: Choose some small value alpha 
#(in most disciplines the conventional choice is to choose alpha = 0.05), and reject the null hypothesis if 
#the p-value is less than alpha. We call alpha the significance level of the test.

#It turns out if we follow this decision rule, the probability that we will reject the null hypothesis
#by mistake is equal to alpha.We call the event of rejecting the null hypothesis when it is in fact true a
#Type I error, we call the probability of making a Type I error the Type I error rate, and we say that 
#rejecting the null hypothesis when the p-value is less than alpha controls the Type I error rate so that it is
#equal to alpha. 
