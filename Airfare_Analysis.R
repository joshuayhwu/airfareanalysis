install.packages("gap")
install.packages("orcutt")
library(gap)
library(orcutt)
# QUESTION 1 - AIRLINE DATA

## LOADING THE AIRLINE DATA
airline_data <- read.csv("C:\\Users\\Joshua Wu\\Documents\\R\\R Work\\ECON140_PS6_Airline.csv")
names(airline_data)
## A) UNRESRICTED MODEL OF PASSENGERS ON TIME AND SEASONAL DUMMIES
airline_unre <- lm(pass ~ time + jan + feb + mar + apr + may + jun + jul + aug
                   + sep + oct + nov, data=airline_data)
summary(airline_unre)

# SUM OF SQAURE DATA ON UNRESTRICTED MODEL
anova(airline_unre)

## B) F-TEST (CHOW TEST) ON SEASONAL DUMMIES

# RESTRICTED MODEL - OMIT SEASONAL DUMMIES
airline_re <- lm(pass ~ time, data=airline_data)
summary(airline_re)

# SUM OF SQUARE DATA ON RESTRICTED MODEL
anova(airline_re)

# TEST STATISTIC FOR CHOW TEST

airline_chowts <- ((301219-90820)/11) / (90820 / 144-12-1 )
airline_chowts
pf(airline_chowts,df1=11, df2=131, lower.tail=FALSE)


## C) PLACE RESTRICTED ESTIMATED COEFICIENTS SE, T-STAT, RMSE
summary(airline_re)



## D) ONLY INCLUDES VARIABLES SIGNIFICANT AT .05 LEVEL
airline_adj <- lm(pass~ time + mar + apr + may + jun + jul 
                  + aug + sep + nov, data = airline_data)
summary(airline_adj)
anova(airline_adj)
## E) PLACE COEFFICIENTS IN REGRESSION TABLE WITH SE, T-STAT, RMSE

## F) COMPARE UNADJUSTED VS. ADJUSTED R-SQUARE



# QUESTION 2 - AIRFARE DATA


airfare_data <- read.csv("C:\\Users\\Joshua Wu\\Documents\\R\\R Work\\ECON140_PS6_Airfare.csv")
names(airfare_data)

## A) UNRESTRICTED MODEL OF PASSEN ON FARES, LOG(DISTANCE), ANNUAL DUMMY VARIABLES
airfare_unre <- lm(passen ~ fare + ldist + y98 + y99 + y00, data = airfare_data)
summary(airfare_unre)
## B) CHOW TEST ON SEASONAL DUMMY VARIABLES

# SUM OF SQUARE DATA ON UNRESTRICTED MODEL
anova(airfare_unre)

# RESTRICTED MODEL FOR SEASONAL DUMMIES
airfare_re <- lm(passen~ fare + ldist, data=airfare_data)
summary(airfare_re)

# SUM OF SQUARE DATA ON RESTRICTED MODEL
anova(airfare_re)

# CALCULATING CHOW STATISTIC
airfare_chowts <- ((2964069108-2958830185)/4) / (2958830185) / (4596-5-1) 
airfare_chowts
pf(airfare_chowts,df1=4, df2=4590, lower.tail=FALSE)



## D) F-TEST ON FARES AND LOG(DISTANCE) 
# RESTRICTED MODEL FOR AIRFARES
airfare_re2 <- lm(passen ~ y98 + y99 + y00, data = airfare_data)
summary(airfare_re2)

# SUM OF SQUARES DATA FOR RESTICTED AIRFARES
anova(airfare_re2)

# CHOW STATISTIC FOR AIRFARES & LOG(DISTANCE)
airfare_chowts2 <- ((3025943511 -2958830185)/1) / (2958830185) / (4596-5-1) 
airfare_chowts2

pf(airfare_chowts2, df1=1, df2=4590)

## F) NARROW DESIGN MATRIX TO ONLY INCLUDE SIGNIFICANT VARIABLES AT a = 0.05 
airfare_adj <- lm(passen ~ fare + ldist + y00, data=airfare_data)
summary(airfare_adj)

anova(airfare_adj)

## G) RESTRICTED ESTIMATE MODEL WITH SE, T-STAT, RMSE


## H) COMPARE ADJUSTED R-SQUARE BETWEEN UNRESTRICTED AND RESTRICTED FOR
## FARES AND LOG(DISTANCE), AND RESTRICTED MODEL FOR FARES AND ANNUAL DUMMY


# QUESTION 3 - AIRLINE DATA

## A) ESTIMATE MODEL FOR PASSENGER ON TIME, SEASONAL DUMMY WITH OLS
summary(airline_unre)



## B) ESTIMATE MODEL FOR PASSENGER ON TIME, SEASONAL DUMMY WITH COCHRANE-ORCUTT
cochrane.orcutt(airline_unre, convergence = 8, max.iter=100)


## C) ESTIMATE MODEL FOR PASSENGER FOR TIME, SEASONAL DUMMY WITH PRAIS-WINSTEN
airline_prais2 <- prais_winsten(pass ~ time + jan + feb + mar + apr + may + jun + jul + aug
                                   + sep + oct + nov, data=airline_data)
summary(airline_prais2)

## D) ESTIMATE MODEL FOR PASEENGER FOR TIME, SEASONAL DUMMY WITH NEWEY-WEST
coeftest(airline_unre, vcov = NeweyWest(airline_unre, lag=3))


## E) PLACE COEFFIEINTS FOR ABOVE EQUATIONS, SE, T-STAT, AND RMSE IN TABLE