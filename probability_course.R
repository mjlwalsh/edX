#random number generator
beads <- rep(c("red","blue"), times = c(2,3)) #make a str of red or blue "beads"
sample(beads, 1) #picks one value from beads object

#Monte Carlo simulation continues the above random selection iteratively many times to get 
#a probability of a certain outcome
events <- replicate(10000, sample(beads, 1)) #replicate repeats the sample(beads,1) call 10,000 times 
table(events) # a table of outcome counts
prop.table(table(events)) #table of proportions from above

#sample() defaults to reseting variables. However, you can "replace" samples back 
events <- sample(beads, 10000, replace = TRUE) #this avoids needing to replicate 10,000 because we are doing 10,000 picks with replacement
prop.table(table(events)) 

#use set.seed() function to always generate the same random numbers as someone else
#can use mean() function to a logical vector returns proportion of elements that are TRUE
mean(beads == "blue")

#probability can be independent or conditional
#Independent probability is like rolling dice: the outcome of the next roll doesn't depend on the previous
#Conditional probability is like choosing a card from a deck: if you choose a King and don't replace it
  #you will have a lower likelihood of choosing a King since there are now 3 (rather than 4) Kings. 
  #conditional probabilities change when you see the other outcomes

# If two events A and B are independent, Pr(A|B)=Pr(A).
# The multiplication rule for independent events is:
  #Pr(A and B) = Pr(A)*Pr(B|A)
  #  Pr(A and B and C) = Pr(A)*Pr(B)*Pr(C) for independent events
# The multiplication rule for dependent events considers the conditional probability of both events occurring:
  #   Pr(A and B)=Pr(A)|Pr(B|A)
# We can expand the multiplication rule for dependent events to more than 2 events:
  #   Pr(A and B and C)=Pr(A)|Pr(B|A)|Pr(C|A and B)

#BLACKJACK STATISTICS
#probability of getting an ace (1/13) times probability of getting facecard or 10 next (16/51)
(1 / 13) * (16 / 51)


# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

##create deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Two", "Three", "Four", "Five",
             "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits) ##Shows all the combinations of deck of cards
deck <- paste(deck$number, deck$suit) #convert this to character list
kings <- paste("King", suits) #all king cards
mean(deck %in% kings) #shows that 1/13 chance to get kings in a deck

#permutations() : For any list of size "n", all the different ways we can select "r" items
#ORDER matters for permutations
library(gtools)
permutations(5,2) #list of 5 numbers, choosing two numbers
permutations(10,7,v = 0:9, repeats.allowed = TRUE) #this shows all possible permutations of 7 digit phone numbers
phone_numbers <- permutations(10,7,repeats.allowed = TRUE,v = 0:9)
n <- nrow(phone_numbers) #length of phone#s
index <- sample(n, 5) #random sampling of 5 phone #s
phone_numbers[index,] #shows the random phone #s

sapply(1:length(index), function(x) { #convert phone number grid to actual numbers
  e <- paste0(phone_numbers[index,][x,], collapse = "") #collapse to digit string
gsub("^(.{3})(.*)$", "\\1-\\2", e) #add a "-" after the third character
  })

hands <- permutations(52, 2, v=deck) #gives you all the permutations of hand of two cards
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings) #number of hands where first card is king
sum(first_card %in% kings & second_card%in%kings) / sum(first_card%in%kings) #chance of getting a second king if you already have one
#answer above is 3/51 chance
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings) #equivalent chance as above

#combinations() : is similar to permutations, but order does NOT matter
combinations(5,2) #notice how this is different from permutations(); if 2 3 appears, 3 2 will not appear in combinations(
aces <- paste("Ace",suits)
facecard <- c("King","Queen","Jack","Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v = deck) ##now it doesn't matter the order;e.g. if you have Ten-Ace or Ace-Ten

mean(hands[,1] %in% aces & hands[,2]%in%facecard) #chance of getting a clean BlackJack hand
#aces comes first because we knew it did in the combinations vector, but 
# this code ensures you have covered both possibilities:
mean((hands[,1] %in% aces & hands[,2]%in%facecard) | 
   (hands[,2] %in% aces & hands[,1] %in% facecard))

library(tidyverse)
#monte Carlo simulation of blackjack
hand <- sample(deck,2)
hand
set.seed(2)
replicate(10000,{
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% facecard)|
    (hand[2] %in% aces & hand[1] %in% facecard)}) %>% mean()
#this is close to the actual probability of getting a clean BlackJack hand

#THE Birthday problem
#what are the chances that of a group of 50 random people, 2 people have the same bday?
bdays <- sample(1:365, 50, replace=TRUE) #randomly picking a day 50 times, and replacing the values
duplicated(bdays) %>% any() #duplicated() checks for duplicates and any() checks for any values that are TRUE

#Monte carlo simulation of the above BDAY problem
set.seed(5)
replicate(10000,{
  bdays <- sample(1:365,50,replace=TRUE)
  any(duplicated(bdays))}) %>% mean()
 ##mean is ~97%

#compute the probability of shared bdays across a certain number of people
compute_prob <- function(n, B = 10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
compute_prob(50) #this computes the probability for 50 people (as above)
n <- seq(1, 60) #a range of values for n
compute_prob(n) ##THIS CODE DOESN'T DO WHAT WE WANT. 
#we need to apply the compute_prob() to all "n" values
prob <- sapply(n, compute_prob) #this does what we want and applies compute_prob() to all "n" variables
plot(n, prob)

#Above was just using Monte Carlo simulations
#Using the precise mathematical calculations, this becomes faster
#Chances of having all UNIQUE birthdays is this calculation:
# 1 * (364/365) * (363/365) * (362/365) * .... (365-n+1/365)
exact_prob <- function(n){
  prob_unique <- seq(365,365 - n + 1) / 365
  1 - prod(prob_unique) ##prod computes product of all values
}

exact <- sapply(n,exact_prob)
plot(n, exact)
plot(n, prob) #compare to Monte Carlo-predicated probability
lines(n, exact, col="red") #Red line of real 

#How many Monte Carlo simulations are enough?
B <- 10^seq(1, 5, len = 100) #define B to be a range of numbers
compute_prob <- function(B, n = 22){ #using 22 people for the birthday experiment
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
prob <- sapply(B, compute_prob) #computes range of Monte Carlo estimates based on B
plot(log10(B),prob,type="l") #Estimate is not stable with low B values

#Addition rule
# Pr(A or B) = Pr(A) + Pr(B) - Pr(A and B)
#probability of ace followed by facecard = 1/13 * 16/51 
#probability of a facecard followed by an ace = 4/13 * 3/51
1/13 * 16/51 + 4/13 * 4/51

#Monty Hall problem
#Make a Monte Carlo simulation to decide if sticking to same door or changing 
# is a better choice
stick <- replicate(10000, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
})
mean(stick) # 33% chance of getting prize if you stick with the door

switch <- replicate(10000, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})
mean(switch) #67% chance of getting prize if you switch doors

##
#Continuous probability
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height #male heights
F <- function(a) mean(x <= a) #define a function to see how probability of heights under 
                          #a value "a"
1 - F(70) #shows the proportion/probability of someone being over 70 inches
# probability of a student being between 70 and 80 inches is:
# the difference in probability of being over 70 inches and over 80 inches
(1 - F(70)) - (1 -F(80)) #F(B) - #F(A)
F(80) - F(70) #also gives the same answer

#normal distribution
pnorm() # gives normal distribution 
# F(a) = pnorm(a, avg, stdev)
# given a normal distrubtion, you just need avg and stdev to get a probability for a given value
1 - F(70.5)
1 - pnorm(70.5, mean(x), sd(x)) #similar, but not quite the same answer as the function
#even though each person may have reported height slightly differently, it is helpful
# to think of height as a continuous value with intervals, such as between 69.5 and 70.5 inches
F(70.5) - F(69.5)
F(69.5) - F(68.5)
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
# normal distribution is useful around intervals close to mean

# normal distribution is  NOT helpful for intervals not containing an integer
F(70.9) - F(70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
#farther from avg also skews normal vs real distribution
F(mean(x) - 2 * sd(x)) - F(mean(x) - 2 * sd(x) - 1)
pnorm((mean(x) - 2 * sd(x)), mean(x), sd(x)) - pnorm((mean(x) - 2 * sd(x) - 1), mean(x), sd(x))

#probability density
dnorm() # gives probability density function
dnorm(z) #gives probability density of a certain z-score
# 99.7% of values should fall between sd-3 and sd+3
data.frame(x, d = dnorm(x, mean(x), sd(x))) %>%  #normal distribution of heights
  ggplot(aes(x, d)) +
  geom_line() +
  geom_density(aes(y = ..density..), col = "red") #actual density in red

y <- seq(-4, 4, length = 100)
data.frame(y, f = dnorm(y)) %>%  #gives a normal distribution centered around 0, going to -4, 4
  ggplot(aes(y, f)) +
  geom_line()

#monte carlo simulations of normally distributed variables
rnorm() #generates normally distributed outcomes
rnorm(2) #generates random numbers with a normal distribution, centered around 0
x <- heights %>% filter(sex == "Male") %>% .$height #male heights
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)
ds_theme_set()
data.frame(simulated_heights) %>% 
  ggplot(aes(simulated_heights)) +
  geom_histogram(color = "black", binwidth = 2)
#simulate a distribution. How tall is the tallest person from 800 people
tallest <- replicate(10000, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})
mean(tallest > 7*12) #what proportion of the tallest people are over 7 ft?
data.frame(tallest) %>%  #looking at the histogram for this 
  ggplot(aes(tallest)) + 
  geom_histogram(color = "black") + 
  geom_vline(xintercept = 7*12, lty = 7)

# d for density, q for quantiles, p for probability density function, r for random
# these can be used for non-normal distributions, such as t-distribution, chi-squared, etc
# e.g. dt(), pt(), rt() or dchisq(), pchisq(), rchisq()

#random variables
beads <- rep(c("red", "blue"), times = c(2, 3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0) #generates a random 1 or 0
#Sampling models
# using an example of a Casino roulette wheel
# roulette wheel has 18 black, 18 red, 2 green pockets
wheel <- rep(c("black", "red", "green"), times = c(18, 18, 2))
X <- replicate(1000, {
  ifelse(sample(wheel, 1, replace = TRUE) == "red", -1, 1)
})
sum(X) #gives money output for casino from 1000 bets on red
#this below code also works as we know the probability involved
X <- sample(c(-1,1), 1000, replace = TRUE, prob = c(18/38, 20/38))
sum(X)
# F(a) = Pr(S <= a) probability of S (sum of X) being less than a given variable, a
S <- replicate(10000, {
  X <- sample(c(-1, 1), 1000, replace = TRUE, prob = c(18/38, 20/38))
  sum(X)
})
mean(S < 0) #what is the probability of the Casino losing money
ggplot() +
  geom_histogram(aes(S), color = "black") #distribution of casino winnings
s <- seq(min(S), max(S), len = 100)
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S)))
data.frame(S = S) %>% 
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, aes(s, f), color = "blue", size = 1.5)

#Not part of course, just interesting
coin_flip <- replicate(10000, {
  x <- sample(c("heads", "tails"), 1000, replace = TRUE)
  mean(x == "heads")
})
mean(coin_flip == 0.5) # 1000 coin flips only gives a ~2.5% chance of getting exact 500 heads, 500 tails

replicate(5, {coin_flip <- replicate(10000, {
  x <- sample(c("heads", "tails"), 1000, replace = TRUE)
  mean(x == "heads")
})
mean(coin_flip == 0.5)
}) 

#CENTRAL LIMIT THEOREM
# In statistics, Capital letters are used to denote random variables, 
# lowercase letters are used to denote observed values
# E[X] = mu (expected value of random variable, X, is mu/mean )
# Going back to roulette analogy:
# If you have an urn with 20 +1s and 18 -1s, your expected value is (20 + -18) / 38
# Therefore, on average, the casino wins ~ $0.05 per game ($2/38)
X <- sample(c(-1, 1), 1e6, replace = TRUE, prob = c(18/38, 20/38))
mean(X) # this confirms that average win per game is 0.05
# if urn has a and b possible outcomes with p and 1-p probability:
# average = ap + b(1-p)
1 * (20/38) + -1 * (1 - (20/38)) #roulette example expected theoretical value
# expected value of sum of draws = avg * # draws : 1000 people playing roulette = $0.05 * 1000 = $50
# SE[X] = standard error of X
# sqrt(# draws ) * stdev(numbers in the urn)
sd(X) 
mean(X) * 1000 
sd(X) / sqrt(1000) * 1000 #standard error of $1000 is ~$32
# sd = | b - a | * sqrt(p(1 - p)) 
abs(1 - (-1)) * sqrt(20/38 * (1- 20/38)) #roulette example sd
# se = sqrt(n) * |b -a | * sqrt(p(1-p))
sqrt(1000) * abs(1 - -1) * sqrt(20/38 * (1-20/38)) #roulette example std error
#Using Central Limit Theorem, we can skip Monte Carlo Distribution
mu <- 1000 * (1 * (20/38) + -1 * (1 - (20/38))) ##average winnings for 1000 plays
se <- sqrt(1000) * 2 * sqrt(20/38 * (1-20/38)) #std error for 1000 plays
pnorm(0, mu, se)
mean(S < 0) #above Monte Carlo simulation
# variance[X] = SE[X]^2 
# standard error grows smaller as # draws grows bigger

# when probability of success is very low, such as with the lottery, central limit theorem
# doesn't hold true (e.g. # of winners of the lottery doesn't approximate the odds)
# Poisson distribution is better suited at approximating low probability of success odds

##The Big Short
# Calculate bank interest rates based on loses
n <- 1000 #1000 loans 
loss_per_foreclosure <- -200000
p <- 0.02 #chance of someone defaulting on their loan and incurring foreclosure
defaults <- sample(c(0, 1), n, prob = c(1-p, p), replace= TRUE)
sum(defaults * loss_per_foreclosure) # how much your bank would would lose in a year
# Monte Carlo simulation of above
B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0, 1), n, prob = c(1-p, p), replace= TRUE)
  sum(defaults * loss_per_foreclosure)
})
data.frame(losses_in_millions = losses/10^6) %>% 
  ggplot(aes(losses_in_millions)) +
  geom_histogram(color = "black", binwidth = 0.6)
# We don't need Monte Carlo simulation, however. We can use Central limit theorem
n * (p * loss_per_foreclosure + (1-p)*0) # expected avg of losses
sqrt(n) * (abs(loss_per_foreclosure - 0) * sqrt(p*(1-p))) #se of avg
# we need to add an interest rate such at the expected average of losses = 0
# loss*p + x(1-p) = 0 where x = extra interest money to charge per loan
loss_per_foreclosure * p / -(1-p) # extra money to charge per loan
# ~ $4801 which means ~2% of 180,000 loan
# this still leaves 50% chance that your bank loses money
# if we want 1% chance of losing money 
# Pr(S < 0) = 0.01 where S is our expected money gain/loss
# we can subtract Expected[S] and divide by SE[S] on both sides of the equation
# Pr((S - E[S]) / SE[S] < -E[S]/SE[S])  = 0.01
# S - E[S]/SE[S] is the definition of a Z score
# Pr( Z < -E[S]/SE[S]) = 0.01
# Pr( Z < -  -(l*p + x(1-p)*n) / abs(x-l)*sqrt(n * p * (1-p)) ) = 0.01 
 #x = interest, p = probability of default, l = loss per default, Z is random variable 
# Z = qnorm(0.01) given that Z is a random variable with avg 0 and sd 1
qnorm(0.01) # = z
# Pr(Z <= z) = 0.01
# z = -E[S]/SE[S]
# -(l*p + x(1-p))*n) / abs(x-l)*sqrt(n * p * (1-p)) = qnorm(0.01) = z
# x = -l * (n*p - z*sqrt(n * p * (1-p))) / (n * (1-p) + z * sqrt(n * p * (1-p)))
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l * (n*p - z*sqrt(n * p * (1-p))) / (n * (1-p) + z * sqrt(n * p * (1-p))) 
x
mu <- l*p + 0*(1-p)
sd <- abs(-l)*sqrt(p*(1-p))
# average = ap + b(1-p)
# sd = | b - a | * sqrt(p(1 - p)) 


#^^ interest amt that will give us a 1% chance of losing money
loss_per_foreclosure * p + x * (1-p) #expected profit per loan

r <- 0.05 #interest rate
x <- r*180000 #interest/profit on 180,000 loan
p <- 0.04 #new default rate
loss_per_foreclosure*p + x*(1-p)
# E[S] = n * mu
# SE[S] = sqrt(n) * sigma
# z = -E[S]/SE[S] = - n * mu / sqrt(n) * sigma = -sqrt(n) * mu / sigma
# solving for n = z^2 * sigma ^2 / mu^2. If n >= this, then guarenteed a probability of 0.01 losing money
# remember z = qnorm(0.01)
z <- qnorm(0.01)
n <- ceiling(z^2 * (x-l)^2 * p*(1-p) / (l * p + x*(1-p))^2) #ceiling fxn gives next highest integer
n #number of loans we need so that probability of losing is <0.01%
n * (loss_per_foreclosure*p + x*(1-p)) #total profits

#however, treating every person as independent doesn't necessarily model this correctly
#global changes to markets could affect many people at once, making draws connected

profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-.01, .01, length = 100), 1) #changing p randomly for everyone
  draws <- sample(c(x, loss_per_foreclosure), n, prob = c((1-new_p), new_p), replace = TRUE)
  sum(draws)
})
mean(profit) #expected profit is still high
mean(profit < 0) #however, there is a much higher chance of losing money now
mean(profit < -10*10^6) #probability of losing $10 million
data.frame(profit) %>% 
  ggplot() + 
  geom_histogram(aes(profit), color = "black") #very broad distribution of profits


### not part of course
cousin <- replicate(10000, {
  x <- sample(c(1, 0), 14, replace = TRUE, prob = c(1/10, 9/10))
  sum(x)
})
mean(cousin >= 1)
sum(cousin) / (10000 * 14)
data.frame(cousin) %>% ggplot(aes(cousin)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth = 1, color = "black") +
  labs(x = "", y = "Probability") + 
  theme(panel.background = element_rect(fill = NA),
        axis.line.y = element_line(color = "black", 
                                   arrow = arrow(length = unit(10, "points")),
                                   size = rel(1.2)),
        axis.line.x = element_line(color = "black",
                                   size = rel(1.2)), 
        axis.text.y = element_text(color = "black", 
                                   size = rel(1.2)),
        axis.title.y = element_text(color = "black",
                                    size = rel(1.15)))
