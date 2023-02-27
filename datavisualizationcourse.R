#Data visualization course
#section 1.1 Intro
library(dslabs)
data(murders)
data(heights)
#names function lets you view the variables in a dataset
#head function gives you the first few entries in a dataset
#length function gives the number of a certain entry (e.g. how many entries fullfil a specific role)
#for example, the following command gives how many entries there are in the height category
length(heights$height)

#distribution of sex in dataset:
prop.table(table(heights$sex))

#histogram of heights, with 35 bars, labeled 
hist(heights$height,breaks=35,main="Distribution of Heights",xlab="Heights(in)")

#an example of CDF (cumulative distribution function) from class notes:
a <- seq(min(heights$height), max(heights$height), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(heights$height <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values,xlab="Height(in)",ylab="F(a)")

#Smooth density plots. Better way to visualize data than histogram
#area under curve of smooth density plot is 1.

index<-heights$sex=="Male"
x<-heights$height[index]
average<-mean(x)
SD<-sd(x)
c(average=average,SD=SD)

z<-scale(x)#scale gives you z-score

mean(abs(z)<2) #95% of normal-distribution data should be between -2 to 2=z

#pnorm gives the percentile for a given value, if supplied mean and stdev

1-pnorm(70.5,mean(x),sd(x)) #probability of a student being taller than 70.5

plot(prop.table(table(x)),xlab="a=Height in inches", ylab="Pr(X=a)") #distribution of reported heights

mean(x<=68.5)-mean(x<=67.5)
mean(x<=69.5)-mean(x<=68.5)
mean(x<=70.5)-mean(x<=69.5)

pnorm(68.5,mean(x),sd(x))-pnorm(67.5,mean(x),sd(x)) #pnorm can approximate well if the interval contains an integer
pnorm(69.5,mean(x),sd(x))-pnorm(68.5,mean(x),sd(x)) #pnorm can approximate well if the interval contains an integer
pnorm(70.5,mean(x),sd(x))-pnorm(69.5,mean(x),sd(x)) #pnorm can approximate well if the interval contains an integer

#quantiles
summary(heights$height)
p<-seq(0.01,0.99,0.01) #gives percentiles
percentiles<-quantile(heights$height,p)
percentiles[names(percentiles)=="25%"] #confirming that 25%percentile is 1st quantile
percentiles[names(percentiles)=="75%"] #confirming that 75%percentile is 1st quantile
quantile(heights$height,.25) #alternatively, you can find percentile using quantile function

#qnorm vs pnorm
#qnorm(p,mean,stdev) where p is percentile
#pnorm gives the percentile, which qnorm gives the value. 
#pnorm and qnorm are inverse functions
pnorm(-1.96,0,1) #gives the percentile that -1.96 would fall
qnorm(0.025) #gives the 2.5% percentile value (which is -1.96)

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles[25] #will give theoretical 25% for above distribution

mean(x<=69.5) #percent of x (male heights) below 69.5inches
#if p=0.5, then q=69.5

p<-seq(0.05,0.95,0.05)
observed_quantiles<-quantile(x,p) #gives range of actual observed quantiles every 0.05
theoretical_quantiles<-qnorm(p,mean=mean(x),sd=sd(x))
plot(observed_quantiles,theoretical_quantiles)
abline(0,1)


observed_quantiles<-quantile(z,p)
theoretical_quantiles<-qnorm(p)
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)
mean(x)
mean(heights$height[heights$sex=="Male"])
sd(heights$height[heights$sex=="Male"])
mean(heights$height)

#percentiles are quantiles you obtain when you define p as 0.01, 0.02 up to 0.99
#50th percentile is median


rate<-murders$total/murders$population*1e5
library(dplyr)
head(murders)
murders<-mutate(murders,rate)
head(murders)

#Q-Q plot of murder rates shows this is not a normal distribution
hist(rate,data=murders)
hist(rate)
zm<-scale(murders$rate)
observed_quantiles<-quantile(zm,p)
theoretical_quantiles<-qnorm(p)
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)

#ggplot2 cheat sheet 
"https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf"
library(tidyverse) #dplyr and #ggplot2

library(dslabs)
ggplot(data=murders)
murders %>% ggplot() #these two functions are equivalent...loads murder data into ggplot

#first added layer usually defines geometry
?geom_point
murders %>% ggplot() + 
  geom_point(aes(x=population/10^6, y=total)) #don't need x or y

p<-ggplot(data=murders) #can assign ggplot to p and add layers to p
p + geom_point(aes(population/10^6,total))
?geom_label
p + geom_text(aes(population/10^6,total,label=abb))
ggplot(data=murders) + geom_label(aes(population, total, label=abb))

#another plot (made up from another course), with colors and different sizes
ggplot(midwest,aes(area,poptotal))+geom_point(aes(col=state,size=popdensity))+geom_text(aes(label=county,size=4000))+labs(y="population",x="area",title="population density",caption="source:midwest")

#size added to geom_point argument
p+geom_point(aes(population/10^6,total),size=3)+geom_text(aes(population/10^6,total,label=abb))

#nudge_x added to geom_text argument to move the labels
p+geom_point(aes(population/10^6,total),size=3)+geom_text(aes(population/10^6,total,label=abb),nudge_x = 3)

#global aesthetic mappings 
p <- ggplot(data=murders,aes(population/10^6, total, label = abb))

# pipe function %>% does not work without dplyr loaded
p<- murders %>% ggplot(aes(population/10^6,total,label=abb))

#local mappings overwrite global aesthetic mappings

p + geom_point(size = 3) + 
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))+geom_abline(slope=r)

coef(lm(total~population,data=murders))


#scales the axes to log10 scale
p+geom_point(size=3)+
  geom_text(nudge_x = 0.075)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")

#or this function does the same thing
p+geom_point(size=3)+
  geom_text(nudge_x = 0.075)+
  scale_x_log10()+
  scale_y_log10()

#adding labels
p+geom_point(size=3)+
  geom_text(nudge_x = 0.075)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Population in millions(log scale)")+
  ylab("Total number of murders(log scale)")+
  ggtitle("US Gun Murders in US 2010")

#redefining p to be everything except the geom_point argument
p<-p+geom_text(nudge_x = 0.075)+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Population in millions(log scale)")+
  ylab("Total number of murders(log scale)")+
  ggtitle("US Gun Murders in US 2010")

#map the region onto a color aesthetic of geom_point
p+geom_point(aes(col=region),size=3)

#define average murder rate for entire country
r<-sum(murders$total)/sum(murders$population)*10^6
#or this does the same thing
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

#y=r*x where r is average murder rate and x is population

p+geom_point(aes(col=region),size=3)+
  geom_abline(slope=log(log(r)),intercept=log(log(r))) #geom_abline intercept a, slope b

#change abline to be greyed out and below points by putting it before in gg function
p + geom_abline(slope=log(log(r)),intercept=log(log(r)),lty=2, color="darkgrey")+
  geom_point(aes(col=region),size=3)

#changing legend to be capitalized
p<-p + geom_abline(slope=log(log(r)),intercept=log(log(r)),lty=2, color="darkgrey")+
  geom_point(aes(col=region),size=3)+scale_color_discrete(name="Region")

###Add-on packages
library(ggthemes)
p+theme_economist() #various themes
p+theme_fivethirtyeight() #various themes packages theme_""

library(ggrepel)#package that will allow points on top of one another to separate

#all steps:
r<- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate #define r as average murder rate and
murders %>% ggplot(aes(population/10^6,total,label=abb))+
  geom_abline(intercept=log(log(r)),slope=log(log(r)),lty=2,color="darkgrey")+
  geom_point(aes(col=region),size=3)+
  geom_text_repel()+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Population in millions (log scale)")+
  ylab("Total number of murders (log scale")+
  ggtitle("US Gun Murders in 2010")+
  scale_color_discrete(name="Region")+
  theme_economist()

#other plots
heights %>% filter(sex=="Male") #filter heights to only males
h<-heights %>% filter(sex=="Male") %>% 
  ggplot(aes(x=height))
h+geom_histogram() 

#change binwidth
h+geom_histogram(binwidth = 1)
h+geom_histogram(binwidth = 1, fill="blue",col="black")+ #adding color to inside and outside of bars
  xlab("Male heights in inches")+
  ggtitle("Histogram of Male heights")

#smooth density plot
h+geom_density(fill="blue")
 
#Q-Q plots need a sample definition
h<-heights %>% filter(sex=="Male") %>% 
  ggplot(aes(sample=height))
h+geom_qq()
##Q-Q plot default is compared to normalized distribution with average zero and std 1
#change parameter of Q-Q plot
params<-heights %>% filter(sex=="Male") %>% 
  summarize(mean=mean(height),sd=sd(height))
h+geom_qq(dparams = params) #assigning sd and average from data set parameters
h+geom_qq(dparams = params)+geom_abline() #see how closely they fit theoretical normal distribution

#alternative to above, you can scale the data rather than scaling the parameters
heights %>% filter(sex=="Male") %>% 
  ggplot(aes(sample=scale(height)))+
  geom_qq()+
  geom_abline()

library(gridExtra)
h<-heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
#defining a bunch of objects to arrange in one graph: 
h1<-h+geom_histogram(binwidth = 1,fill="blue",col="black")
h2<-h+geom_histogram(binwidth = 2,fill="blue",col="black")
h3<-h+geom_histogram(bins=10,fill="blue",col="black")
grid.arrange(h1,h2,h3,ncol=3)

##SECTION 3: DPLYR Functions
library(tidyverse)
s<-heights %>% filter(sex=="Male") %>% 
  summarize(average=mean(height),standard_deviation=sd(height))
#generates mean of heights and stdev of MALE heights as a table data frame
s$average
s$standard_deviation

#can use summarize for any operation of a vector (sum, mean, median, etc). 
#can only use functions that return a single value with summarize
s<-heights %>% filter(sex=="Male") %>% 
  summarize(median=median(height),minimum=min(height),maximum=max(height))

rate<-murders$total/murders$population*10^6
murders<-mutate(murders,rate)
summarize(murders,mean(rate)) #this isn't actually the average murder rate
#because it takes small states as the same value as large states
us_murder_rate<-murders %>% summarize(rate=sum(total)/sum(population)*10^6)
us_murder_rate

us_murder_rate %>% .$rate #gives the numeric value (rather than data frame) of the pipe

#grouping data
heights %>% group_by(sex) %>% 
  summarize(average=mean(height),standard_deviation=sd(height))

murders %>% arrange(population) %>% head() #arranging the data frame by population
murders %>% arrange(rate) %>% head() #arranging the data frame by murder rate
murders %>% arrange(desc(rate)) %>% head() #descending order of above
murders %>% arrange(region,desc(rate)) %>% head() #order by region, then by murder rate, descending

#top_n function allows you to perform "head" function and tell how many rows to show
murders %>% top_n(10,rate) #shows 10 states with highest murder rates 
murders %>% arrange(desc(rate)) %>% top_n(10) #same as above, but ordered by rate
#na.rm can remove NAs in a function e.g.: mean(x,na.rm=TRUE) 

#Gapminder dataset
library(tidyverse)
data("gapminder")
head(gapminder)
#find infant mortality in 2015 for two different countries
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)
names(gapminder)
ds_theme_set()
#viewing fertility vs life expectancy colored by continent
gapminder %>% filter(year==1962) %>% ggplot(aes(fertility, life_expectancy,color=continent))+geom_point(size=3)

#faceting up to two variables. Columns represent one variable, rows represent another
#rows and columns separated by tilde
filter(gapminder, year%in%c(1962,2012)) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent))+
  geom_point()+
  facet_grid(continent~year) #continent is the rows, year is the column

filter(gapminder, year%in%c(1962,2012)) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent))+
  geom_point()+
  facet_grid(.~year) #same code as before, but just interested in one variable
#so you add a dot to before year

#adding more years, filtering by just europe and asia
filter(gapminder, year%in%c(1962,1980,1990,2000,2012),
       continent %in%c("Europe","Asia")) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent))+
  geom_point()+
  facet_wrap(~year) #wraps the data to look more pleasing visually
#when using facet, the range of axes is based on all plots and kept fixed

##Time series plots
filter(gapminder, country=="United States") %>% ggplot(aes(year,fertility))+
  geom_line()

filter(gapminder, country%in%c("South Korea","Germany")) %>%
  ggplot(aes(year,fertility, group=country))+ ##MUST include grouping/coloring or gg plot will connect points from two different countries
  geom_line() 

filter(gapminder, country%in%c("South Korea","Germany")) %>%
  ggplot(aes(year,fertility, color=country))+ ##MUST include grouping or gg plot will connect points from two different countries
  geom_line()

#labeling is usually preferred over legends
countries<-c("South Korea","Germany")
labels<-data.frame(country=countries,x=c(1975,1965),y=c(60,72)) #location of labels
gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(year,life_expectancy,col=country))+
  geom_line()+
  geom_text(data=labels,aes(x,y,label=country),size=5)+
  theme(legend.position="none")

#GDP/population/365=dollars per day per person
gapminder<-mutate(gapminder,dollar_per_day=gdp/population/365)

gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(dollar_per_day))+
  geom_histogram(binwidth = 1,color="black")

#logtransform the data above
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(log2(dollar_per_day)))+
  geom_histogram(binwidth = 1,color="black")
##this shows there are local modes in the data

#transforming axes, not data
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% ggplot(aes(dollar_per_day))+
  geom_histogram(binwidth = 1,color="black")+scale_x_continuous(trans="log2")

p<-gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(region,dollar_per_day))
p+geom_boxplot() #can't read label names

#changing label orientation
p+geom_boxplot()+theme(axis.text.x=element_text(angle=90,hjust=1)) #hjust justifies text

#REORDERING
fac<-factor(c("Asia","Asia","West","West","West"))
levels(fac)
value<-c(10,11,12,6,4)
fac<-reorder(fac,value,FUN=mean) #reordered factors by mean value
levels(fac)

#reordering the region based on dollars per day
p<-gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  mutate(region=reorder(region,dollar_per_day,FUN=median)) %>% #reordering region based on dollars per day
  ggplot(aes(region,dollar_per_day,fill=continent))+ #if you want descending order, simply put minus sign in front of "dollar_per_day"
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+xlab("")
p

#change to log2 scale
p+scale_y_continuous(trans="log2")

#can also add points
p+scale_y_continuous(trans="log2")+geom_point(show.legend = FALSE)

west<-c("Western Europe","Northern Europe","Southern Europe",
        "Northern America","Australia amd New Zealand")

#histogram of "west" vs the rest income distribution 1970 vs 2010
gapminder %>% filter(year%in%c(1970,2010) & !is.na(gdp)) %>% 
  mutate(group=ifelse(region%in%west,"West","Developing")) %>% #West vs developing based on above defined vector
  ggplot(aes(dollar_per_day))+ 
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)

#remake plots so they only contain countries shared between 1970 and 2010
#since new countries were established after 1970
country_list1<-gapminder %>% 
  filter(year==1970 & !is.na(dollar_per_day)) %>% .$country #get character vector
country_list2<-gapminder %>% 
  filter(year==2010 & !is.na(dollar_per_day)) %>% .$country
country_list<-intersect(country_list1,country_list2) #shared countries
##subset of countries that existed in both 1970 and 2010
gapminder %>% filter(year%in%c(1970,2010) & country %in%country_list) %>% 
  mutate(group=ifelse(region%in%west,"West","Developing")) %>% #West vs developing based on above defined vector
  ggplot(aes(dollar_per_day))+ 
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)

p<-gapminder %>% filter(year%in%c(1970,2010) & country %in%country_list) %>% #adding 1970/2010 shared country list 
  mutate(region=reorder(region,dollar_per_day,FUN=median)) %>% #reordering region based on dollars per day
  ggplot()+ #if you want descending order, simply put minus sign in front of "dollar_per_day"
  theme(axis.text.x=element_text(angle=90,hjust=1))+xlab("")+
  scale_y_continuous(trans="log2")
p+  geom_boxplot(aes(region,dollar_per_day,fill=continent))+
  facet_grid(year~.)


#hard to compare boxplots when they are on top of each other
#we want to rearrange them to be side-by-side:
p+geom_boxplot(aes(region,dollar_per_day,fill=factor(year)))

#density plots
gapminder %>% filter(year%in%c(1970,2010) & country %in%country_list) %>% 
  ggplot()+geom_density(aes(dollar_per_day),fill="gray")+facet_grid(year~.)+
  scale_x_continuous(trans="log2")

#shows that there are more countries in "developing" vs "west"
gapminder %>% filter(year%in%c(1970) & country %in%country_list) %>% 
  mutate(group=ifelse(region %in%west,"West","Developing")) %>% 
  group_by(group) %>% summarize(n=n()) %>% knitr::kable()

#this plot is skewed, even though there are more countries in developing
#it looks like there are fewer
gapminder %>% filter(year%in%c(1970,2010) & country %in%country_list) %>% 
  mutate(group=ifelse(region %in%west,"West","Developing")) %>% 
  ggplot(aes(dollar_per_day,fill=group))+
  scale_x_continuous(trans="log2")+geom_density(alpha=0.3)+facet_grid(year~.)

## .. accesses variable from ggplot. 
aes(x=dollar_per_day,y=..count..)
p<-gapminder %>% filter(year%in%c(1970,2010) & country %in%country_list) %>% 
  mutate(group=ifelse(region %in%west,"West","Developing")) %>% 
  ggplot(aes(dollar_per_day,y=..count..,fill=group))+
  scale_x_continuous(trans="log2")
#this allows us to scale down based on the number of countries
p+geom_density(alpha=0.2,bw=0.75)+facet_grid(year~.)

#can look more specifically at other regions. Use case_when and dot placeholder
gm<-gapminder %>% mutate(group = case_when(
  .$region %in% west ~ "West",
  .$region %in% c("Eastern Asia", "South-Eastern Asia")~"East Asia",
  .$region %in% c("Caribbean","Central America","South America")~ "Latin America",
  .$continent=="Africa" & .$region != "Northern Africa"~ "Sub-Saharan Africa",
  TRUE~"Others"))

#make these regions factors (this puts the legend in specific order)
gm<-gm %>% mutate(group=factor(group,levels=c("Others","Latin America",
                                              "East Asia","Sub-Saharan Africa",
                                              "West")))

# gm<-gm %>% mutate(group=factor(group)) #this function does the same as above
#but without a specific order (so it's just alphabetically ordered)

p<-gm %>% filter(year%in%c(1970,2010) & country %in% country_list) %>% 
  ggplot(aes(dollar_per_day,y=..count..,fill=group))+
  scale_x_continuous(trans="log2")
p+geom_density(alpha=0.2,bw=0.75,position="stack")+facet_grid(year~.) 
#stacks densities on top of one another

#weight population densities
gm %>% filter(year %in% c(1970,2010) & country %in% country_list) %>% 
  group_by(year) %>% 
  ungroup() %>%  #unclear why this function is needed...the graph is the same without
  mutate(weight=population/sum(population)*2) %>% 
  ggplot(aes(dollar_per_day, fill=group,weight=weight))+
  scale_x_continuous(trans="log2")+
  geom_density(alpha=0.2,bw=0.75,position="stack")+facet_grid(year~.)


#defining more regions
gm<- gapminder %>% mutate(group=case_when(
  .$region %in% west~"The West",
  .$region %in% "Northern Africa"~"Northern Africa",
  .$region %in% c("Eastern Asia","South-Eastern Asia")~"East Asia",
  .$region =="Southern Asia" ~"Southern Asia",
  .$region %in% c("Central America", "South America", "Caribbean")~
    "Latin America",
  .$continent == "Africa"& .$region != "Northern Africa"~
    "Sub-Saharan Africa",
  .$region %in% c("Melanesia","Micronesia","Polynesia")~
    "Pacific Islands"))

#discovering income and infant mortality across the newly defined regions (group)
surv_income<-gm %>% filter(year %in% c(2010) & !is.na(gdp) &
                             !is.na(infant_mortality) & !is.na(group)) %>% 
  group_by(group) %>% summarize(income=sum(gdp)/sum(population)/365,
                                infant_survival_rate=(1-sum(infant_mortality/
                                1000*population)/sum(population)))
surv_income %>% arrange(income)

#plot this using limits of the axis
surv_income %>% ggplot(aes(income,infant_survival_rate, label=group,color=group))+
  scale_x_continuous(trans="log2", limit=c(0.25,150))+ #limits the axis
  scale_y_continuous(trans="logit",limit=c(.875, .9981), #logit seems to need to be between 0 and 1 scale
                     breaks=c(.85,.90,.95,.99,.995,.998))+ #sets the location of axis labels
  geom_label(size=3,show.legend=FALSE)
#logit scale is helpful to highlight differences near zero or near 1


#generating income and infant mortality by country
surv_income_country<-gapminder %>% filter(!is.na(gdp) &
                                            !is.na(infant_mortality)) %>% 
  group_by(country) %>% summarize(income=sum(gdp)/sum(population)/365,
                                  infant_survival_rate=(1-sum(infant_mortality/
                                                                1000*population)/sum(population)))

gm<-left_join(gm,surv_income_country,by="country")
gm %>% filter(year %in% 2010 & !is.na(income) & !is.na(infant_survival_rate) & !is.na(group)) %>% 
  ggplot(aes(income,infant_survival_rate))+geom_point(aes(color=group),size=3) +
  scale_x_continuous(trans="log2", limit=c(0.25,150))+ 
  scale_y_continuous(trans="logit",limit=c(.875, .9981), 
                     breaks=c(.85,.90,.95,.99,.995,.998))+geom_text(aes(label=country),size=2)


#life expectancy
gapminder %>% filter(year%in% c(1960:2016) & country=="Cambodia" &!is.na(life_expectancy)) %>%
  ggplot(aes(year,life_expectancy))+geom_line(size=2)+xlab("Year")+ylab("Life Expectancy")+
  ggtitle("Life Expectancy in Cambodia")+scale_x_continuous(breaks=seq(1960,2010,10))+
  geom_vline(xintercept=c(1975,1979),lty=2,color="red")+
  geom_text(aes(x=1970, y= 65, label="Pol Pot Rule"),color="red",size=5)+
  annotate("rect", xmin=1975, xmax=1979, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") 
#annotate to add a separate rectangle to plot +theme_fivethirtyeight() 



heights %>% ggplot(aes(sex, height)) + geom_point()
heights %>% ggplot(aes(sex, height)) + geom_jitter(width=0.1,alpha=0.2) #width squeezes the values together
#alpha blending adds transparency to points; darker points fall on top of one another

heights %>% ggplot()+geom_histogram(aes(height,..density..),binwidth = 1)+facet_grid(sex~.)+theme_bw()
#..density.. allows unequal numbers in distributions to be normalized to their relative frequency

#Slope chart
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

#Bland-Altman plot (MA plot)
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

#Vaccine dataset
data("us_contagious_diseases")
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 / (weeks_reporting/52)) %>% ###adjusts for fewer than 52 weeks of reporting
  mutate(state = reorder(state, rate)) %>% 
  mutate(rate=ifelse(is.nan(rate),0,rate))

library(RColorBrewer)
display.brewer.all(type="seq")
#Tile plot of measles cases by state and year 
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") + ##Sqrt transformation scales data to prevent outliers
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) + ###ALL STATES ADDED AS GRAY LINES
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") + ##USE AVERGAE MAPPED
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")

#set global significant digits option
options(digits=3)

#TITANTIC DATASET quiz
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
titanic_test %>% view()
titanic_train %>% view()
titanic %>% ggplot()+geom_density(aes(Age,..count..,fill=Sex),position="stack",alpha=0.3)
titanic %>% ggplot()+geom_density(aes(Age,..count..,fill=Sex),position="identity",alpha=0.3)+
  scale_x_continuous(breaks=c(seq(0,80,10)))
titanic %>% ggplot()+geom_density(aes(Age,fill=Sex),alpha=0.3)+facet_grid(~Sex)+geom_vline(xintercept=c(18,34))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>%  ggplot()+geom_qq(aes(sample=Age),dparams = params)+geom_abline(slope=1) #QQ Age plot

titanic %>% ggplot()+geom_bar(aes(Sex,fill=Survived),position="dodge")
titanic %>% ggplot()+geom_bar(aes(Survived,fill=Sex))
titanic %>% ggplot()+geom_bar(aes(Survived,fill=Sex),position="dodge") ##Separates bars so they're not stacked

titanic %>% ggplot()+geom_density(aes(Age,..count..,fill=Survived),alpha=0.2)
titanic %>% filter(!Fare==0) %>% ggplot()+geom_boxplot(aes(Survived,log2(Fare),fill=Survived))+
  geom_jitter(aes(Survived,log2(Fare)),alpha=0.2,width=0.1)

titanic %>% filter(!Fare==0) %>% ggplot()+geom_boxplot(aes(Survived,Fare,fill=Survived))+
  geom_jitter(aes(Survived,Fare),alpha=0.2,width=0.1)+scale_y_continuous(trans="log2")

titanic %>% ggplot()+geom_bar(aes(Pclass,fill=Survived))
titanic %>% ggplot()+geom_bar(aes(Pclass,fill=Survived),position = "fill") #sets each group to be proportions out of 1
titanic %>% ggplot()+geom_bar(aes(Survived,fill=Pclass),position = "fill") #sets each group to be proportions out of 1


titanic %>% ggplot()+geom_density(aes(Age,..count..,fill=Survived),alpha=0.3)+facet_grid(Sex~Pclass)
titanic %>% ggplot()+geom_density(aes(Age,fill=Survived),alpha=0.3)+facet_grid(Sex~Pclass)

data(stars)


#
########################################interactive graphs
plo<-filter(gapminder, year%in%c(1962,1970,1980,1990,2000)) %>% 
  ggplot(aes(infant_mortality,life_expectancy,col=continent,label=country))+ 
  geom_point()+ #added country label
  facet_grid(.~year)
library(plotly)
ggplotly(plo) #put this into ggplotly for interactive data points

###########################################################
#########INFECTIOUS DISEASE graphs I MADE USING EXISTING US_CONTAGIOUS_DISEASE data frame
library(dslabs)
library(tidyverse)
us_contagious_diseases<-mutate(us_contagious_diseases,rate=count/population*1e6)
us_contagious_diseases %>% filter(state%in%c("Massachusetts","Alabama","California"),disease=="Measles") %>%
  ggplot(aes(x=year,y=rate,color=state))+geom_line()

disease_rate<-us_contagious_diseases %>% group_by(year,disease) %>% summarize(cases=sum(count))
library(scales)
disease_rate %>% ggplot(aes(x=year,y=cases,color=disease))+geom_line(size=1)+xlab("Year")+ylab("Cases")+
  scale_color_discrete(name="Disease")+ #renames legend
  ggtitle("Cases of Contagious Disease in US")+
  scale_y_continuous(labels=comma)+ #adds comma-separated numbers
  theme(plot.title=element_text(hjust=0.5)) #centers title of graph

#adding regions to states
st.reg<-data.frame(state=state.name,region=state.region)
st.reg<-rbind(st.reg,data.frame(state="District Of Columbia",region="South"))
us_contagious_diseases<-us_contagious_diseases %>% left_join(st.reg,by="state") #adding in regions of states
us_contagious_diseases %>% filter(disease=="Measles"& year%in%c(1970,1975,1980,1990)) %>% ggplot(aes(region,rate,fill=factor(year)))+
  geom_boxplot()

#creating labels rather than a legend
labels<-data.frame(disease=levels(disease_rate$disease), #creating data frame of labels
                   x=c(1985,1950,1975,1935,1940,1990,1930), #these are coordinated for label
                   y=c(100000,800000,200000,250000,50000,50000,75000))
disease_rate %>% ggplot(aes(x=year,y=cases,color=disease))+geom_line(size=1)+xlab("Year")+ylab("Cases")+
  scale_color_discrete(name="Disease")+ #renames legend
  geom_text(data=labels,aes(x,y,label=disease),size=4)+ #location of labels based on above setpoints
  ggtitle("Cases of Contagious Disease in US")+
  scale_y_continuous(labels=comma)+ #adds comma-separated numbers
  theme(plot.title=element_text(hjust=0.5),legend.position = "none")
#centers title of graph and removes legend

labels

#ordered by year, then by disease, then by descending counts of the disease
data(us_contagious_diseases)
da<-us_contagious_diseases %>% arrange(year,disease,desc(count)) 

measles<-us_contagious_diseases %>% filter(disease=="Measles") %>%  group_by(year) %>% summarize(measlescases=sum(count))
HepA<-us_contagious_diseases %>% filter(disease=="Hepatitis A") %>%  group_by(year) %>% summarize(hepAcases=sum(count))
mumps<-us_contagious_diseases %>% filter(disease=="Mumps") %>%  group_by(year) %>% summarize(mumpscases=sum(count))
pertusis<-us_contagious_diseases %>% filter(disease=="Pertussis") %>%  group_by(year) %>% summarize(pertussiscases=sum(count))
polio<-us_contagious_diseases %>% filter(disease=="Polio") %>%  group_by(year) %>% summarize(poliocases=sum(count))
rubella<-us_contagious_diseases %>% filter(disease=="Rubella") %>%  group_by(year) %>% summarize(rubellacases=sum(count))
smallpox<-us_contagious_diseases %>% filter(disease=="Smallpox") %>%  group_by(year) %>% summarize(smallpoxcases=sum(count))

library(gridExtra)

m<-ggplot(data=measles,(aes(year,measlescases)))+geom_line()+xlim(1928,2012)+ylab("Cases")+ggtitle("Measles")+scale_y_continuous(labels=comma)
h<-ggplot(data=HepA,(aes(year,hepAcases)))+geom_line()+xlim(1928,2012)+ylab("Cases")+ggtitle("Hepatitis A")+scale_y_continuous(labels=comma)
mu<-ggplot(data=mumps,(aes(year,mumpscases)))+geom_line()+xlim(1928,2012)+ylab("Cases")+ggtitle("Mumps")+scale_y_continuous(labels=comma)
pe<-ggplot(data=pertusis,(aes(year,pertussiscases)))+geom_line()+xlim(1928,2012)+ylab("Cases")+ggtitle("Pertussis")+scale_y_continuous(labels=comma)
po<-ggplot(data=polio,(aes(year,poliocases)))+geom_line()+xlim(1928,2012)+ylab("Cases")+ggtitle("Polio")+scale_y_continuous(labels=comma)
r<-ggplot(data=rubella,(aes(year,rubellacases)))+geom_line()+xlim(1928,2012)+ylab("Cases")+ggtitle("Rubella")+scale_y_continuous(labels=comma)
s<-ggplot(data=smallpox,(aes(year,smallpoxcases)))+geom_line()+xlim(1928,2012)+ylab("Cases")+ggtitle("Smallpox")+scale_y_continuous(labels=comma)
grid.arrange(m,h,mu,pe,po,r,ncol=3,s)

###SIMPLER CODE FOR ABOVE
us_contagious_diseases %>% group_by(year,disease) %>% summarise(cases=sum(count)) %>% 
  ggplot()+geom_line(aes(year,cases,col=disease),show.legend = FALSE,size=1.5)+
  facet_wrap(~disease,scales="free")+
  scale_y_continuous(labels=scales::comma)+labs(y="Cases",x="",title="Infectious disease cases over time")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5,size=rel(1.25)),
        axis.title.y = element_text(size = rel(1.15)))
