# Functions for PSTAT 120B Easy Calculator


#----------------------------Sample Avg and Std--------------------------------#

numbers <- c(23, 31, 13, 19, 23, 17, 28, 26, 25, 28)
# Average
nums.mean <- mean(numbers)
nums.mean
# Standard Deviation
nums.sd <- sd(numbers)
nums.sd * nums.sd

#--------------------------Z-Scores, t-Scores, etc.----------------------------#

# z-score value
qnorm(0.005, lower.tail = FALSE)    # Right-Tailed Test
qnorm(0.05, lower.tail = TRUE)      # Left-Tailed Test
2 * qnorm(0.01, lower.tail = FALSE)     # Two-Tailed Test

## X-axis value *below* which the probability is 5%
qnorm(0.05,lower.tail = TRUE)   # P(X<c)
# [1] -1.644854

## X-axis value *above* which the probability (AUC) is 5%
qnorm(p = 0.05, lower.tail = FALSE)   # P(X>c)
# [1] 1.644854


# t-distribution critical value
qt(0.05/2, df=18, lower.tail = FALSE)
qt(0.025, df=9, lower.tail= TRUE)


# x^2-distribution critical value
qchisq(1-(0.1/2), df=5, lower.tail = FALSE)
qchisq(0.95, df=5, lower.tail = FALSE)

#------------------------Pooled Estimator for Sigma^2--------------------------#

# Sp^2 Calculator
n1<-11
n2<-14
s1<-sqrt(52)
s2<-sqrt(71)
pooled <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n1-2))
pooled*pooled # this equals Sp not Sp^2

#-------------------------Confidence Intervals---------------------------------#

# Confidence Interval Calculator for mew
avg=98.25
std<-0.73
z_score<-qnorm(0.005, lower.tail = FALSE)
size<-130
std_size<-((std)/(sqrt(size)))

lower_CI<-(avg-(z_score)*(std_size))
lower_CI  
upper_CI<-(avg+(z_score)*(std_size))
upper_CI  


# Confidence Interval Calculator for p hat
p_hat<-0.45
z_score<-qnorm(0.01, lower.tail = FALSE)
size<-800
  
lower_CI<-(p_hat-(z_score)*(sqrt((p_hat*(1-p_hat))/size)))
lower_CI  
upper_CI<-(p_hat+(z_score)*(sqrt((p_hat*(1-p_hat))/size)))
upper_CI  



# Confidence Intervals for difference in means

y_bar1<-1.65
y_bar2<-1.43
n1<-30
n2<-35
s1<-0.26
s2<-0.22
z_score<-qnorm(0.005, lower.tail = FALSE)

lower_CI<-(y_bar1-y_bar2)-z_score*(sqrt((s1^2/n1)+(s2^2/n2)))
lower_CI  
upper_CI<-(y_bar1-y_bar2)+z_score*(sqrt((s1^2/n1)+(s2^2/n2)))
upper_CI  



# Confidence Intervals for mew, SMALL SAMPLE

n<-10
mew_not<-4.85
sample_mew<-3.781
sample_std<-0.18095

t_observed<- ((sample_mew-mew_not)/(sample_std/sqrt(n)))
t_observed

t_critical_value<-qt(0.1, df=18, lower.tail = FALSE)

lower_CI<-(sample_mew)-t_critical_value*(sample_std/sqrt(n))
lower_CI  
upper_CI<-(sample_mew)+t_critical_value*(sample_std/sqrt(n))
upper_CI  
#------------------------------------------------------------------------------#
# 2 Sample Tests where Sigma^2 is unknown
# X is N(mew_x , sig_x)
# Y is N(mew_y , sig_y)

standard_d <- 1/(sample_size-1)

# test statistic = t
t <- (sqrt(sample_size)*d_bar)/(standard_d)

# p-value for t distribution
pt(t , df=sample_size-1)

#------------------------------------------------------------------------------#


## 2-tailed p-value for t distribution
pt(1.74, df = 17)

pt(-3.24, df=17)

## upper 1% point for an F(2, 7) distribution
qf(0.01, 2, 7, lower.tail = FALSE)

#------------------------------------------------------------------------------#

# Standard Normal Distribution Plot
x<-seq(-4, 4, length=100)
y<-dnorm(x)
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

pnormGC(62, region="below", mean=64, sd=8,graph=TRUE)

#------------------------------------------------------------------------------#

?mtcars
# Loading
data(mtcars)
# Print the first 6 rows
head(mtcars, 6)

?USArrests
data("USArrests")
head(USArrests)

boxplot(USArrests$Assault~USArrests$UrbanPop)
# Boxplot Comparison clearly shows that there isn't a correlation between larger populations and higher assault.

coplot(USArrests$UrbanPop)
summary(USArrests)

hist(USArrests$Murder)
boxplot(USArrests$Murder~USArrests$UrbanPop)

pairs(USArrests, main= "US Arrests Data")

head(quakes)
coplot(Murder ~ Assault | UrbanPop, data = USArrests)
given.depth <- co.intervals(USArrests$UrbanPop, number = 4, overlap = .1)
coplot(Murder ~ Assault | UrbanPop, data = USArrests, given.values = given.UrbanPop, rows = 1)

help.start()

require(graphics)
pairs(mtcars, main = "mtcars data", gap = 1/4)
coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)
## possibly more meaningful, e.g., for summary() or bivariate plots:
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
summary(mtcars2)


A<-c(15, 20, 11, 23, 16, 21, 18, 16, 27, 24)
B<-c(23, 31, 13, 19, 23, 17, 28, 26, 25,28)
num_b<-mean(B)
num_a<-mean(A)

boxplot(A,B)

summary(A)
t.test(A, B, var.equal=TRUE)
plot(ecdf(A), do.points=FALSE, verticals=TRUE, xlim=range(A, B))

#------------------------------------------------------------------------------#

test_dataframe<- data.frame(mtcars)
hist(mtcars$mpg)
hist(mtcars$gear)
stem(mtcars$mpg)
mean(mtcars$mpg)

# 20.09062 Mean for mtcars$mpg values

