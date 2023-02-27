##Inference and Modeling

#Section 1 ##############################

# Infering outcomes of a large group that is impossible to sample fully by sampling
# a random, small group
library(dslabs)
library(tidyverse)
ds_theme_set()
take_poll(25) #randomly samples red and blue beads in urn
# p = proportion of blue beads
# 1-p = proportion of red beads
# spread = p - (1-p) or 2p -1
# beads in the urn are the population

# X = 1 for blue, X = 0 for red
# sampling of N beads results in average proportion of blue beads
# we do not know the proportion of blue beads (unlike in a roulette or card game where we 
# have a known probability of certain outcomes)
# Xbar = average of X
# E(Xbar) = p
# SE(sum) = sqrt(N) * sd(values)
# (a - b) * sqrt(p * (1-p)) or (1-0) * sqrt(p * (1-p)) for our blue/red 1/0 example
# as an example, assuming p = 0.51 : 
N <- seq(10, 10^5, by = 10)
SEM <- sqrt(0.51 * (1 - 0.51)) / sqrt(N) # stderror of average
data.frame(N, SEM) %>% ggplot(aes(N, SEM)) + geom_line() + #graph show number of people sample
  scale_x_log10(labels = function(x) format(x, scientific = F)) +  # vs standard error mean
  geom_hline(yintercept = 0.01, color = "red3")

# Section 2 ###########################

# p = E(Xbar)
# Pr(Z <= E/SE)
# What is the probability we are within one percentage point of X?
# Pr(Z <= 0.01 / sqrt(p * (1-p)/N)) - Pr(Z <= -0.01 / sqrt(p * (1-p)/ N))
# plug in estimate to use Xbar in place of p
# SE(Xbar) = sqrt(Xbar * (1 - Xbar)/ N)
# for an example in which p = 0.48 (12 blue, 13 red)
X_bar <- 0.48
se <- sqrt(X_bar * (1 - X_bar) / 25)
se #about 0.1
pnorm(0.01 / se) - pnorm(-0.01 / se) #probability of being 1 percentage point away
# This means that we only have an 8% chance of actually have the right p based on our sample size
# margin of error is 2 * se
2 * se
# Probability of being within 2 standard errors of p
pnorm(2) - pnorm(-2) # ~ 95% of being within 2 se of p

#Monte Carlo simulations
B <- 10000
N <- 1000
p <- 0.45 #pick a value for p
X_bar <- replicate(B, {
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p)) #we don't know p
  mean(X)
})

X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p)) #simulate 1 poll
X_bar <- mean(X)
X_bar <- replicate(B, {
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p)) #we don't know p
  mean(X)
})
mean(X_bar)
sd(X_bar) 
sqrt(p * (1-p)/ N) #central limit theorem actual standard error
library(gridExtra)
p1 <- data.frame(X_bar) %>% ggplot(aes(X_bar)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(X_bar) %>% ggplot(aes(sample = X_bar)) + 
      stat_qq(dparams = list(mean = mean(X_bar), sd = sd(X_bar))) +
        geom_abline() +
        labs(y = "X_bar", x = "Theoretical normal")
grid.arrange(p1, p2, nrow = 1)     
# we can't do this in real life because we don't know p, but we can see that 
# central limit theorem holds up for many values p and N
# spread = 2p - 1
# estimated spread = 2 * Xbar - 1
# for the original p = 0.48 from 25 draws:
# estimated spread = 2 * 0.48 -1  or 4 %
# estimated standard error or spread is = 2 * sqrt(0.48 * (1 - 0.48)/ 25)
# margin of error for spread is = 2 * 2 * sqrt(0.48 * (1 - 0.48) / 25) or about 40%


p <- seq(0.35, 0.65, by = 0.01)
SE <- 2 * sqrt(p * (1 - p )/ 10^5) #error for sampling 10,000 people over a range of p
plot(p, SE)

# although we can theoretically think of polling people as picking beads from an urn
# it is not as simple; people may lie to you. You also miss people without phones, 
# most importantly, you don't know who is in the population and who is not; not all ppl called
# may vote. 
# polls are typically biased by 1-2%
# standard deviation of errors = sqrt(mean(errors^2))

# Section 3 #######################
#confidence intervals
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>% 
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() + # gives confidence intervals in shaded area
  ggtitle("Average yearly temperatures in New Haven")

p <- 0.45
N <- 1000
X <- sample(c(0, 1), N, replace = TRUE, prob = c(1-p, p))
X_bar <- mean(X)
SE_bar <- sqrt(X_bar * (1 - X_bar) / N)
c(X_bar - 2 * SE_bar, X_bar + 2 * SE_bar) #range of values for standard error of spread
# Pr( X_bar - 2 * SE_bar <= p <= X_bar + 2 * SE_bar)
# Pr (-2 <= X_bar - p / SE(X_bar) <= 2)
# Pr(-2 <= Z <= 2)
# Pr(-z <= Z <= z) = 0.99 would give 99% confidence interval rather than 95%
# we use 0.995 instead of 0.99 because it is 1 - (1-q) / 2 or
# 1 - (1-0.99)/2 
# because confidence interval is centered on 0, 95% confidence means 0 to 0.975 and 
# 0 to -0.975
pnorm(qnorm(0.995))
pnorm(qnorm(1-0.995))
z <- qnorm(0.995)
pnorm(z) - pnorm(-z)
qnorm(0.975) # approximately 1.96 
pnorm(qnorm(0.975)) - pnorm(-qnorm(0.975)) #gives exactly 95% confidence


inside <- replicate(10000, {
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
  X_bar <- mean(X)
  SE_bar <- sqrt(X_bar * (1 - X_bar) / N)
  between(p, X_bar - 2*SE_bar, X_bar + 2*SE_bar)
  
})
mean(inside) # this shows that 95% of values are within 2 standard errors of the mean

#this is a graphical representation of above, roughly 5 points will not overlap p
data.frame(X_bar = replicate(100, {
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
  X_bar <- mean(X)
})) %>% mutate(SE_bar = sqrt(X_bar * (1 - X_bar) / N)) %>% 
  mutate(inside = ifelse(p <= X_bar - 2 *SE_bar | p >= X_bar + 2 * SE_bar, "No", "Yes")) %>% 
ggplot(aes(y = 1:100, x = X_bar, color = inside)) +
  geom_point() + 
  geom_errorbar(aes(xmin = X_bar - 2*SE_bar, xmax = X_bar + 2*SE_bar)) +
  geom_vline(xintercept = p, lty = 2)

#95% confidence refers to random intervals that fall on p. p is not random and doesn't change
# saying p has a 95% chance of being between lower and upper limit is technically incorrect

#confidence interval for spread
N <- 25
X_hat <- 0.48
(2 * X_hat - 1) + c(-2, 2) * 2 * sqrt(X_hat * (1 - X_hat) / N)
#this range includes zero, so a definitive answer cannot be reached, even though X_hat = 0.48
#power can be thought of as the probability of detecting a spread different from 0

# p-values
# Rather than trying to figure out proportion of blue vs red, you can ask: 
# are there more blue than red; is 2p - 1 > 0
# null hypothesis = spread is 0
# for 52 blue from 100 draws; spread = (2 * 0.52) - 1 = 0.04
# p-value = how likely are we to see a value this large when the null hypothesis is true
# null hypothesis = 50 blue, 50 red; 0 spread.
# p-value given the above spread: Pr(|(2 * X_bar) - 1| > 0.04) = Pr(|X_bar - 0.5| > 0.02)
# Null hypothesis = X_bar - 0.5 / sqrt(0.5 * (1 - 0.5)/ N) # random variable 
# subtracting its expected value and dividing by standard error
# Pr(X_bar - 0.5 / sqrt(0.5 * (1 - 0.5)/ N)  > 0.02 / sqrt(0.5 * (1 - 0.5)/ N))
# simplifying = Pr(sqrt(N) * |X_bar - 0.5| / 0.5 > Z)

N <- 100
z <- sqrt(N) * 0.02/0.5
1 - (pnorm(z) - pnorm(-z))
# If a 95% confidence interval of the spread does not include 0, we can deduce that 
# p must be < 0.05* 

# To find p-values for a given z-score z in a normal distribution with mean mu,
# and standard deviation sigma, use 2*(1-pnorm(z, mu, sigma)) 

#Find a 95% confidence interval :
# X_hat - qnorm(0.975) * se_hat (lower), 
# X_hat + qnorm(0.975) * se_hat (upper)

# Section 4 #############################

# poll aggregators
# Monte carlo simulation of 12 polls
d <- 0.039 # actual point spread between obama and romney
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# all 12 polls report confidence intervals that include the actual spread
# but pretty much all include 0, which means they cannot confidently predict a winner
polls %>% ggplot(aes(x = estimate, y = poll)) +
  geom_point() +
  geom_errorbar(aes( xmin = low, xmax = high),
                color = "skyblue", linewidth = 1) +
  geom_vline(xintercept = c(0, d), lty = c(1, 2), linewidth = c(1, 0.5))

# aggregate polls together
d_hat <- polls %>%  # estimated spread
  summarise(avg = sum(estimate*sample_size) / sum(sample_size)) %>% 
  .$avg

p_hat <- (1 + d_hat) / 2
moe <- 2 * qnorm(0.975) * sqrt(p_hat * (1 - p_hat) / sum(polls$sample_size))
moe #margin of error
print(paste0("Spread = ", round(d_hat * 100, 1),"%", " \u00B1 ",
             round(moe * 100, 1), "%"))
# this shows that aggregated spread includes actual d (3.9%) and not zero

# 2016 presidential election data
data("polls_us_election_2016")
names(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
polls <- polls %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#p = proportion voting for Clinton,
# 1-p = proportion voting for Trump
# spread (d) = 2p - 1
# standarderro = 2 * sqrt(p * (1 - p) / N)

d_hat <- polls %>%  # estimated spread
  summarise(d_hat = sum(spread * samplesize) / sum(samplesize)) %>% 
  .$d_hat
p_hat <- (1 + d_hat) / 2
moe <- qnorm(0.975) * 2 * sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
moe
print(paste0("Spread = ", round(d_hat * 100, 1),"%", " \u00B1 ",
             round(moe * 100, 1), "%"))

# histogram of spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
# this shows differences BETWEEN pollsters, not polls within pollsters
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# Data-driven models
# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

# Section 5 ##############################
# cystic fibrosis testing example
# 1 = disease, 0 = no disease
# accuracy of testing is 99%
# what is chance that someone testing positive has the disease?
# Rate of disease is 1 in 3500 
D <- 0.00025 # approximately 1 / 3900

#probability of A happening given B Pr(A|B) is Pr(A and B) / Pr(B) =
# Pr(B|A) * Pr(A) / Pr(B)
# Pr(D = 1 | +) = Pr(+|D=1) * Pr(D=1) / Pr(+)
# Pr(+) = Pr(+|D=1) * Pr(D=1) + Pr(+|D=0) * Pr(D=0)
0.99 * 0.00025 / (0.99 * 0.00025 + 0.01 * 0.9975)
#Despite 99% accuracy test, only 2% of those + tests will be true positive

# Monte Carlo visualization of Bayes' theorem
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)
# True positive (disease +) / total positive (disease/healthy) 
# will give ~ 2% as calculated above for Bayes' theorem

# What Bayesian statistics lets us do is compute the probability distribution
# of p given that we have observed data.
# This is called a posterior distribution.

# Expected value of p given Y (an observed mean from a population) =
# mu + (1 - B) * (Y - mu)
# where B = sigma^2 / (sigma^2 + tau^2)
# sigma = singular sd, tau = total sd
# B will be large(more weight), when sd of observed data are high
# Expected value   of posterior distribution = 
# B * mu + (1 - B) * Y
# Stamdard error of posterior distrubiton = 
# sqrt(1 / (1 / sigma^2 + 1 / tau^2))
# confidence interval of posterior distrubiton = 
# B*mu + (1-B)*Y + c(-1,1)*qnorm(0.975)*sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Section 6 ############################################
# election forecasting

polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se) 
# this is basically 1, which means we hypothetically perfectly predict the election outcome
# however, this can't be for a variety of reasons including failing to account for biases (see below)

# Mathematical Representation of Models # # # ## # 
J <- 6 # six random data points
N <- 2000 # 2000 data points per poll
d <- .021 # spread
p <- (d+1)/2 # probability
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N)) #X given a rate of sampling error
# error can come from sampling error and pollster error

I <- 5 # five pollsters sampled
J <- 6 #six random data points
N <- 2000 #2000 data points per poll
d <- .021 #spread
p <- (d+1)/2 #probability
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
# This model ^^ doesn't account for pollster to pollster variability

I <- 5 #five pollsters sampled
J <- 6 #six random data points per poll
N <- 2000 #2000 data points per poll
d <- .021 # spread
p <- (d+1)/2 #probability
h <- rnorm(I, 0, 0.025) # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
X %>% as.data.frame() %>% reshape2::melt() %>% ggplot(aes(variable, value)) + geom_point() +
  labs(x = "Pollster", y = "spread")
# Compare this graph^^ with line 259. Looks pretty similar.
# This accounts for pollster to pollster variability, but not general variability between elections

# general bias term added
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2) # adds extra bias term (~ 0.025) here
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)
# No matter how many polls you take, the b term (0.025 added to sigma above) does not get reduced

# Predicting electoral college # # # #
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg)) #avg is averaging the spread

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

# Bayesian calculation for electoral college predicction
mu <- 0
tau <- 0.02 # estimated standard dev for each state year to year
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean)) %>% view()

# Monte Carlo simulation with NO GENERAL BIAS
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election
# Clinton has 99% chance of winning. However, this ignores general bias

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Same as above, but taking General Bias into account
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election
# Clinton now has 85% chance of winning
data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Forecasting  # # # ## # ## # #
# variability across time
# select all national polls by one pollster to remove pollster-to-pollster variability
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

#This shows variability in spread over time
one_pollster %>% ggplot(aes(enddate, spread)) + geom_point() + geom_smooth(span = 0.1)

#Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

# plotting raw percentages over time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

# t-distrubtions # # # ## # ### # #
z <- qt(0.975, nrow(one_poll_per_pollster) - 1) #using qt rather than qnorm
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)
data.frame(normal = rnorm(2000), t_20 = rt(2000, 20), t_14 = rt(2000, 14)) %>% 
  reshape2::melt() %>% ggplot(aes(value, color = variable)) + geom_density(size = 1)

# Section 7 ##################################################

data("research_funding_rates")
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

#Fisher's exact test
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

#If the sum of the rows and the sum of the columns are fixed, you can use Fisher's tesdt
# if not, then Chi-squared works well too

# Using the funding rate above to make 2-by-2 table
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate
 
# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# you don't need the null hypothesis table to run chisquared though
two_by_two %>% 
  select(-awarded) %>% 
  chisq.test()
# p = 0.051

# summary statistic. Odds ratio for 2-by-2 table
# X = 1 for male, X = 0 for female
# Y = 1 for funded, Y = 0 for no funding
# Pr(Y = 1 | X = 1) / Pr(Y = 0 | X = 1)

# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women
