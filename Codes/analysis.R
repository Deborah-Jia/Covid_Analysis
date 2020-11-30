#######################
## Analysis of       ##
##  Life expectancy  ##
##    and            ##
##  Total GDP        ##
##      OR           ##
##  GPD/capita       ##
##                   ##
##      NO. 3        ##
##                   ##
## Analysis of       ##
#       the data     ##
##                   ##
#######################



# Preparation work --------------------------------------------------------


# Clear memory
rm(list=ls())

# Packeges to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
library(lspline)
# Estimate robust SE
library(estimatr)
# Compare models with robust SE
library(texreg)
# For different themes
library(ggthemes)

# Call the data from github
my_url <- "/Users/wodediannao/Desktop/ECBS-5208-Coding-1-Business-Analytics/assignment_covid/Data/clean/covid_pop_23_10_2020_clean.csv"
df <- read_csv( my_url )


# Check basic plots -------------------------------------------------------

# Quick check on all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key,scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

summary( df )


# Check basic scatter-plots
#   Four models: 
#   1) death = alpha + beta * confirmed
#   2) ln_death = alpha + beta * confirmed
#   3) death = alpha + beta * ln_confirmed
#   4) ln_death = alpha + beta * ln_confirmed
#   
#
#
# 1) death - confirmed: level-level model
ggplot( df , aes(y = death, x = confirmed)) +
  geom_point() +
  geom_smooth(method="loess")+
  scale_x_continuous(
    breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(y = "Death Case", 
       x = "Confirmed Case ") 

# 2) ln_death - confirmed: log-level model
ggplot( df , aes(y = death, x = confirmed ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(y = "Death Case(ln scale)",x = "Confirmed Case(2020-10-23)") +
  scale_y_continuous( trans = log_trans(), breaks = pretty_breaks())

# 3) death - confirmed: level-log model
ggplot( df , aes(y = death, x = confirmed)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(y = "Death Case",x = "Confirmed Case(2020-10-23, ln scale )") +
  scale_x_continuous( trans = log_trans(), waiver(), 
                      breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) 

# 4) ln_death - ln_confirmed: log-log model
ggplot( df , aes(y = death, x = confirmed ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(y = "Death Case(ln scale)",x = "Confirmed Case(2020-10-23, ln scale)") +
  scale_x_continuous( trans = log_trans(),waiver(), breaks= pretty_breaks())+
  scale_y_continuous( trans = log_trans(),waiver(), breaks = pretty_breaks())



# Make and Compare Models -------------------------------------------------


# Take Log of gdp/capita and log GDP total
df <- df %>% mutate( ln_death = log(death),
                     ln_confirmed = log( confirmed ),
                     ln_confirmed_sq = ln_confirmed^2 )

# Remove negative infinite ln_death observations
df <- df %>% filter( ln_death >= 0)
######
# Make some models:
#     reg1: ln_death = alpha + beta * ln_confirmed
#     reg2: ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2
#     reg3: ln_death = alpha + beta_1 * ln_confirmed * 1(ln_confirmed <= 8) + beta_2 * ln_confirmed * 1(8 < ln_confirmed <= 12) + beta_3 * ln_confirmed * 1(12 < ln_confirmed <= 16) 
#     reg4: ln_death = alpha + beta * ln_confirmed, weights: population

# 0) Built in regression in R

reg_b <- lm(ln_death ~ ln_confirmed, data = df ) 
reg_b
# Summary statistics
summary( reg_b )


# 1) simple linear regression (for robust SE):
reg1 <- lm_robust(ln_death ~ ln_confirmed, data = df , se_type = "HC2" )
reg1
# Summary statistics
summary( reg1 )
# Visual inspection:
ggplot( data = df, aes( x = ln_confirmed, y = ln_death) ) + 
  geom_point( color="#C4961A") +
  geom_smooth( method = lm , color = "steelblue") +
  labs(y = "Death Cases(in scale)", 
       x = "Confirmed Cases(in scale, 2020-10-23)") 


# 2) Quadratic regression
reg2 <- lm_robust( ln_death ~ ln_confirmed + ln_confirmed_sq , data = df )
# Summary statistics
summary( reg2 )
# Visual inspection:
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color="#C4961A") +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = "steelblue") +
  labs(y = "Registered death(in scale)", 
       x = "Registered case(in scale, 2020-10-23)") 


# 3) Regression with piecewise linear spline:

cutoff <- c(8,12,14) # define the cutoff for ln(confirmed case)

# Use simple regression with the lspline function
reg3 <- lm_robust(ln_death ~ lspline( ln_confirmed , cutoff), data = df )
# Summary statistics
summary(reg3)
# Visual inspection:
ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point(color = "#C4961A") +
  geom_smooth( formula = y ~ lspline(x, cutoff) , method = lm , color = "steelblue" ) +
  labs(y = "Death Cases(in scale)", 
       x = "Confirmed Cases(in scale, 2020-10-23)") 


# 4) Weighted-OLS:weight with population
reg4 <- lm_robust(ln_death ~ ln_confirmed, data = df , weights = population)
# Summary statistics
summary( reg4 )
# Visual inspection:
ggplot(data = df, aes(x = ln_confirmed, y = ln_death)) +
  geom_point(data = df, aes(size=population),  color = "#C4961A", shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color= "steelblue")+
  scale_size(range = c(1, 15)) +
  coord_cartesian(ylim = c(0, 12.5)) +
  labs(y = "Death Cases(in scale)", x = "Confirmed Cases(in scale, 2020-10-23)")  +
  annotate("text", x = 16, y = 11, label = "India", size=5)+
  annotate("text", x = 11, y = 8, label = "China", size=5)+
  annotate("text", x = 16,  y = 12.75, label = "USA", size=5) +
  annotate("text", x = 15.05,  y = 12, label = "Brazil", size=5) 

#####
# Creating model summary with texreg
data_out <- "~/Desktop/ECBS-5208-Coding-1-Business-Analytics/assignment_covid/Out/"

htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("Linear","Quadratic","P.L.S","WOLS"),
         caption = "Modelling death cases and confirmed cases of countries(2020-10-23)",
         file = paste0( data_out,'model_comparison.html'), include.ci = FALSE) 



######
# Residual analysis.

# Get the predicted y values from the model
df$ln_death_pred <- reg1$fitted.values

# Calculate the errors of the model
df$reg1_res <- df$ln_death- df$ln_death_pred

# Find countries with largest negative errors
df %>% top_n( -5 , reg1_res ) %>% 
  select( country , ln_death, ln_death_pred, reg1_res)


# Find countries with largest positive errors
df %>% top_n( 5 , reg1_res ) %>% 
  select( country , ln_death,ln_death_pred,reg1_res)
