#-----------------------------------------------------------
# MX500 Assignment 1 Problem solving task 1 
#-----------------------------------------------------------

# Completed using R version 4.4.1 (2024-06-14 ucrt)

# Student details:
# Last name: Hand
# First name: Melanie
# Student number: n11484594

# Load required libraries
library(tidyverse)
library(ggplot2)
library(forcats)

#-----------------------------------------------------------
# Loading data
#-----------------------------------------------------------

# Read in PST1 Data
all_airline_data <- read_csv("PST1Data.csv")

# Read in airport code data
airport_codes <- read_csv("PST1AirportCodes.csv")

# Read in airline code data
airline_codes <- read_csv("PST1AirlineCodes.csv")

#-----------------------------------------------------------
# Wrangling data
#-----------------------------------------------------------

# Filter the data to only retain origin 'SFO' (my allocated origin for this assignment
SFO_df <- filter(all_airline_data, Origin == "SFO") 

# Remove Origin and OriginStateName columns
SFO_df <- subset(SFO_df, select = -c(Origin,OriginStateName))

# Add the destination airport name as a column to the SFO dataframe
# Join using Dest column from SFO_df and Airport Code column from airport_codes
SFO_df <- left_join(SFO_df, airport_codes, by = c("Dest"="Airport Code"))

# Add the airline name as a column to the SFO dataframe
# Join using Reporting_Airline column from SFO_df and Airline Code column from airport_codes
SFO_df <- left_join(SFO_df, airline_codes, by = c("Reporting_Airline"="Airline code"))

# Check the datatype assigned to each variable in the dataframe
str(SFO_df)
summary(SFO_df)

# Use the outputs from above to create a data dictionary (see associated PDF)

# Rename columns to remove spaces (which may causes issues later)
colnames(SFO_df) <- c("FlightDate","Reporting_Airline","Dest","DestStateName","DepDelay","ArrDelay","AirportName","AirlineName")

# Change the FlightDate column class from character to date type
SFO_df$FlightDate <- as.Date(SFO_df$FlightDate, "%d/%m/%y")

# Change the character columns to factor data type
SFO_df$Reporting_Airline <- as.factor(SFO_df$Reporting_Airline)
SFO_df$Dest <- as.factor(SFO_df$Dest)
SFO_df$DestStateName <- as.factor(SFO_df$DestStateName)
SFO_df$AirlineName <- as.factor(SFO_df$AirlineName)
SFO_df$AirportName <- as.factor(SFO_df$AirportName)

# Re-check the data types
str(SFO_df)
summary(SFO_df)

#-----------------------------------------------------------
# Summary statistics
#-----------------------------------------------------------

# Determine summary statistics for the departure delay variable
summarise(SFO_df,
          Mean =mean(DepDelay, na.rm=T),
          median =median(DepDelay, na.rm=T),
          sd =sd(DepDelay, na.rm=T),
          n = sum(!is.na(DepDelay))) # only counting non-NA values

# Determine summary statistics by airline for the 5 most popular airlines
# Group by airline
airline_grouped <- group_by(SFO_df, AirlineName)

# Calculate mean, median, sd and number of observations
airline_grouped_summaries <-
  summarise(airline_grouped,
            Mean =mean(DepDelay, na.rm=T),
            Median =median(DepDelay, na.rm=T),
            sd =sd(DepDelay, na.rm=T),
            n =sum(!is.na(DepDelay)))

# Sort by n (will assume more observations means more popular)
arrange(airline_grouped_summaries, desc(n))

#-----------------------------------------------------------
# Graphical summaries
#-----------------------------------------------------------

# Create a graphically excellent plot that shows the relationship between departure delay and arrival delay.
# Will use a scatterplot
ggplot(data=SFO_df, aes(x=DepDelay, y=ArrDelay)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x="Departure Delay (minutes)", y="Arrival Delay (minutes)", 
       title="Relationship between departure delay and arrival delay for all airlines") +
  theme(plot.title=element_text(hjust=0.5)) # Centre the title

# Create a graphically excellent plot that shows the distribution of departure delays.
# Will use a histogram
# First try with a continuous x scale
ggplot(data=SFO_df, aes(x=DepDelayPlus100)) +
  geom_histogram(bins=80,colour = "darkgrey", fill = "paleturquoise") +
  theme_bw() +
  labs(x="Departure Delay (minutes)",
       title="Distribution of Departure Delay for all airlines") +
  theme(plot.title=element_text(hjust=0.5))

# The values are very concentrated around 0 so this histogram doesn't provide a good visual representation of the distribution
# Can use a log scale, but this will not accept negative values
# So will first transform data just for the purpose of this histogram
# Add a column that shifts departure delay by 50 minutes to make all values positive
SFO_df <- mutate(SFO_df, DepDelayPlus100 = DepDelay+100)
# Repeat for arrival delay
SFO_df <- mutate(SFO_df, ArrDelayPlus100 = ArrDelay+100)

# Now that all values are positive, create a histogram with log x scale to show distribution
ggplot(data=SFO_df, aes(x=DepDelayPlus100)) +
  geom_histogram(bins=80,colour = "darkgrey", fill = "paleturquoise") +
  scale_x_log10(breaks=c(100,300,1000), labels=c(0,200,900)) + # change the labels to reflect the actual (not transformed) data
  theme_bw() +
  labs(x="Departure Delay (minutes)",
       title="Distribution of Departure Delay for all airlines") +
  theme(plot.title=element_text(hjust=0.5))

# Repeat the same process for the arrival delay distribution
ggplot(data=SFO_df, aes(x=ArrDelayPlus100)) +
  geom_histogram(bins=80,colour = "darkgrey", fill = "mediumaquamarine") +
  scale_x_log10(breaks=c(100,300,1000), labels=c(0,200,900)) +
  theme_bw() +
  labs(x="Arrival Delay (minutes)",
       title="Distribution of Arrival Delay for all airlines") +
  theme(plot.title=element_text(hjust=0.5))

# Create a plot that shows how departure delay varies by airline.
# First create a new dataframe where only the 3 most popular airlines are named
# and the others are combined and labelled 'Other'
SFO_df_grouped_airline <- SFO_df %>%
  group_by(AirlineName = fct_lump(AirlineName, 3))

# Check it has worked by viewing the values and counts of the AirlineName column
count(SFO_df_grouped_airline, AirlineName)

# Create a boxplot to compare departure delay by airline
ggplot(data=SFO_df_grouped_airline,
       aes(x=AirlineName, y=DepDelay)) +
  geom_boxplot(color="black", fill="thistle", outlier.size = 1) +
  theme_bw() +
  labs(x="Airline",y="Departure delay (minutes)", 
       title="Departure delay by Airline") +
  ylim(c(-20,20)) + # limit x-axis to -20 to 20 as per task instructions
  theme(plot.title=element_text(hjust=0.5))

# Create a plot that shows how arrival delay varies by destination
# First create a new dataframe where only the 3 most popular destinations are named
# and the others are combined and labelled 'Other'
SFO_df_grouped_destination <- SFO_df %>%
  group_by(Dest = fct_lump(Dest, 3))

# Check it has worked by viewing the values and counts of the AirlineName column
count(SFO_df_grouped_destination, Dest)

# Create a boxplot to compare arrival delay by destination
ggplot(data=SFO_df_grouped_destination,
       aes(x=Dest, y=ArrDelay)) +
  geom_boxplot(color="black", fill="peachpuff", outlier.size = 1) +
  theme_bw() +
  labs(x="Destination Airport Code",y="Arrival delay (minutes)", 
       title="Arrival delay by Destination") +
  ylim(c(-40,40)) + # limit x-axis to -40 to 40 as per task instructions
  theme(plot.title=element_text(hjust=0.5))

#-----------------------------------------------------------
# Hypothesis Testing
#-----------------------------------------------------------

# In this next section, only the 2 most popular airlines will be considered
# In this dataset, the two most popular airlines are United Airlines and SkyWest Airlines
# Filter out all other airlines and drop missing data
SFO_df_top2 <- filter(SFO_df, AirlineName == "United Airlines" | 
                        AirlineName == "SkyWest Airlines") %>% drop_na(DepDelay)

# Drop levels from the AirlineName column. Converting to factors caused some confusion
SFO_df_top2$AirlineName <- droplevels(SFO_df_top2$AirlineName)

# Create a new column to capture whether the departure was delayed in binary terms
# DepDelay > 0 is Late, DepDelay <= 0 is NotLate
SFO_df_top2 <- mutate(SFO_df_top2, DepStatus = case_when(
  DepDelay>0 ~ "Late",
  DepDelay<=0 ~ "NotLate"))

# Will perform a Chi squared independence test

# H0 - there is no association between SES and destination
# H1 - there is some association between SES and destination

# Summarise and determine the observed counts of late and not late for each airline
observed <- table(SFO_df_top2$AirlineName, SFO_df_top2$DepStatus)

# Summarise and determine the expected counts of late and not late for each airline
total_depstatus <- summarise(group_by(SFO_df_top2, DepStatus), count=n())
total_airline <- summarise(group_by(SFO_df_top2, AirlineName), count=n())

total_depstatus <- matrix(total_depstatus$count)
total_airline <- matrix(total_airline$count)

expected <- data.frame(total_depstatus%*%t(total_airline)/sum(total_depstatus))
expected

# Perform chi squared test
Xsq <- chisq.test(observed)
Xsq

#-----------------------------------------------------------
# Linear Model
#-----------------------------------------------------------

# Fit a linear model to arrival delay and departure delay using the full dataset
# Arrival delay is the dependent variable (Y), departure delay is the independent variable (X)
delay_lm <- lm(data=SFO_df, ArrDelay ~ DepDelay)
summary(delay_lm)

# Obtain parameter estimates, confidence intervals and p values for each of the parameters in the model. 
tidy(delay_lm, conf.int = T, conf.level = 0.95) %>%
  select(term, estimate, conf.low, conf.high, p.value)

glance(delay_lm)

# Create a plot that shows how the residuals vary with the values fitted through your regression model.
# First use the fortify function to get this fitted and residual values
delay_lm_fort <- fortify(delay_lm)

# Now create a scatter plot of fitted values vs residuals
ggplot(data=delay_lm_fort, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")), 
       y=expression(paste("Residual (",epsilon[i],")")),
       title=expression(paste("Residual homogeneity check: ",hat(y[i]), " vs ",epsilon[i],""))) +
  theme(plot.title=element_text(hjust=0.5))

# Create a QQ plot that compares the standardised residuals to a standard normal distribution.
ggplot(data=delay_lm_fort, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline(ntercept=0, slope=1) +
  coord_equal()+
  xlab("Theoretical (Z ~ N(0,1))") +
  ylab("Sample") + coord_equal() + theme_bw() +
  labs(title = "Quantile-Quantile plot comparing standardised residuals to a standard normal distribution") +
  theme(plot.title=element_text(hjust=0.5))

# Plot the standard normal cumulative density function (CDF) 
# and the empirical CDF of the standardised residuals.

# First create the empirical CDF of the standardised residuals
delay_ecdf <- ecdf(delay_lm_fort$.stdresid)

# Plot the standard normal CDF and the empirical CDF
ggplot(data = delay_lm_fort, aes( x= .stdresid))+
  stat_ecdf(args = list(mean = 0, sd = 1), linewidth=1, aes(colour="Normal CDF"))+
  stat_function(fun = "delay_ecdf", linewidth = 1, aes(colour="Empirical CDF"))+
  labs(y="Probability Density", x="Standard Residual",
       title = "Cumulative Distribution Function",
       color="Legend") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))

# Plot the standard normal CDF and the empirical CDF
ggplot(data = delay_lm_fort, aes( x= .stdresid))+
  geom_function(fun = "pnorm", args = list(mean = 0, sd = 1), linewidth=1, aes(colour="Normal CDF"))+
  geom_function(fun = "delay_ecdf", linewidth = 1, aes(colour="Empirical CDF"))+
  labs(y="Probability Density", x="Standard Residual",
       title = "Cumulative Distribution Function",
       color="Legend") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))


# Plot the standard normal CDF and the empirical CDF
ggplot(data = delay_lm_fort, aes( x= .stdresid))+
  stat_function(fun = "pnorm", args = list(mean = 0, sd = 1), linewidth=1, aes(colour="Normal CDF"))+
  stat_function(fun = "delay_ecdf", linewidth = 1, aes(colour="Empirical CDF"))+
  labs(y="Probability Density", x="Standard Residual",
       title = "Cumulative Distribution Function",
       color="Legend") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))

# Use the ks.test function in R to perform a KS goodness of fit test. 
ks.test(x = delay_lm_fort$.stdresid, y = "pnorm")

# The p-value is 2.2e-16. There is sufficient evidence to reject the hypothesis 
# of the data being normally distributed
