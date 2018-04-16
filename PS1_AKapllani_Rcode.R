rm(list = ls())

# Armand Kapllani
# Problem Set I 
# ECO7707
#----------------------------------------------------------------------------------------

# Required packages 
#----------------------------------------------------------------------------------------
library(data.table)
library(ggplot2)
library(stargazer)
library(rjson)
library(devtools)
library(easyGgplot2)
library(knitr) 
library(png)

# Function that downloads and formats the data.
#----------------------------------------------------------------------------------------

get.Comtrade <- function(
  # construct the url for downloading
  url="http://comtrade.un.org/api/get?"
  ,maxrec=10000
  ,type="C"
  ,freq="A"
  ,px="HS"
  ,ps="now"
  ,r
  ,p
  ,rg="all"
  ,cc="TOTAL"
  ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" # maximum no. of records returned
                 ,"type=",type,"&"  # type of trade (c=commodities)
                 ,"freq=",freq,"&"  # frequency
                 ,"px=",px,"&"      # classification
                 ,"ps=",ps,"&"      # time period
                 ,"r=",r,"&"        # reporting area
                 ,"p=",p,"&"        # partner country
                 ,"rg=",rg,"&"      # trade flow
                 ,"cc=",cc,"&"      # classification code
                 ,"fmt=",fmt        # Format
                 ,sep = ""
  )
  raw.data<- fromJSON(file=string)
  data<- raw.data$dataset
  print(data)
  validation<- unlist(raw.data$validation, recursive=TRUE)
  var.names<- names(data[[1]])
  data<- as.data.frame(t( sapply(data,rbind)))
  ndata<- NULL
  for(i in 1:ncol(data)){
    data[sapply(data[,i],is.null),i]<- NA
    ndata<- cbind(ndata, unlist(data[,i]))
  }
  ndata<- as.data.frame(ndata)
  colnames(ndata)<- var.names
  return(ndata)
}

# Import the datasets on countries GDP for two years 2014 and 1995. 
#-----------------------------------------------------------------------------------------

gdp_1995 <- read.csv("/Users/armandkapllani/Desktop/UF Econ/International Economic Relations/PS1/gdp_1995.csv", header = T)
gdp_2014 <- read.csv("/Users/armandkapllani/Desktop/UF Econ/International Economic Relations/PS1/gdp_2014.csv", header = T)
distance <- read.csv("/Users/armandkapllani/Desktop/UF Econ/International Economic Relations/PS1/distance.csv", header = T)

gdp_1995 <- data.table(gdp_1995)
gdp_2014 <- data.table(gdp_2014)


# 1.1. Use the function get.Comtrade() which we discussed in class to download all 
#      trade flows between the 20 largest countries for the year 2014. Collect only 
#      information on the total trade (i.e. use c = "TOTAL") but get information on 
#      both imports and exports.
#-----------------------------------------------------------------------------------------

  ## Exports (2014) 

I_C <- as.character(c(gdp_2014$cty_code))
d1 <- NULL
for (i_c in I_C[1:5]) {     # We keep changing I_C[1:5] for the next indices. 
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "2014")
      d1 <- rbind(d1, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_2014$cty_code))
d2 <- NULL
for (i_c in I_C[6:10]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "2014")
      d2 <- rbind(d2, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_2014$cty_code))
d3 <- NULL
for (i_c in I_C[11:15]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "2014")
      d3 <- rbind(d3, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_2014$cty_code))
d4 <- NULL
for (i_c in I_C[16:20]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "2014")
      d4<- rbind(d4, data_i_c)
    }
  }
  Sys.sleep(10)
}

dta <- rbind(d1, d2, d3, d4)
dta <- data.table(data)

  ## Imports (2014)

I_C <- as.character(c(gdp_2014$cty_code))
d5 <- NULL
for (i_c in I_C[1:5]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "2014")
      d5 <- rbind(d5, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_2014$cty_code))
d6 <- NULL
for (i_c in I_C[6:10]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "2014")
      d6 <- rbind(d6, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_2014$cty_code))
d7 <- NULL
for (i_c in I_C[11:15]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "2014")
      d7 <- rbind(d7, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_2014$cty_code))
d8 <- NULL
for (i_c in I_C[16:20]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "2014")
      d8 <- rbind(d8, data_i_c)
    }
  }
  Sys.sleep(10)
}

data_countries_2014 <- rbind(d1, d2, d3, d4, d5, d6, d7, d8)  
data_countries_2014 <- data.table(data_countries_2014) 


# 1.2. Match each country pair to its bilateral distance using the file ”distance.RData”. 
#      In this file ”o” and ”d” denote the origin and destination country, respectively. 
#      Distance is reported in km between a set of the biggest cities of the two 
#      countries.
#-----------------------------------------------------------------------------------------

keycols <- c("cty_code_o","cty_code_d")
setnames(data_countries_2014, c("rtCode","ptCode"), keycols)
setnames(gdp_2014, "cty_code", "cty_code_o")

  ## Convert column classes to numeric. 
data_countries_2014$cty_code_o <- as.numeric(as.character(data_countries_2014$cty_code_o))
data_countries_2014$cty_code_d <- as.numeric(as.character(data_countries_2014$cty_code_d))

  ## Match by distance. 
data_countries_2014 <- merge(data_countries_2014, distance, by = keycols)
data_countries_2014$dist <- as.numeric(as.character(data_countries_2014$dist))


# 1.3. Match each importing and exporting country to its respective country GDP in 2014 
#      using the file "gdp 2014.RData".
#-----------------------------------------------------------------------------------------

gdp_2014 <- data.table(gdp_2014)
data_countries_2014 <- data.table(data_countries_2014)

  ## Match for gdp_2014_i

data_countries_2014$gdp_2014_i <- NA
I <- as.numeric(c(gdp_2014$cty_code_o))
K <- as.numeric(c(data_countries_2014$cty_code_o))

for (i in seq_along(I)) {
  for (j in seq_along(K)) {
    if (data_countries_2014$cty_code_o[j] == gdp_2014$cty_code_o[i]) {
      data_countries_2014$gdp_2014_i[j] <- gdp_2014$gdp_2014[i]
      }
    else if (data_countries_2014$cty_code_o[j] != gdp_2014$cty_code_o[i]) {
      next
      }
  }
}

  ## Match for gdp_2014_j

data_countries_2014$gdp_2014_j <- NA
I <- as.numeric(c(gdp_2014$cty_code_o))
K <- as.numeric(c(data_countries_2014$cty_code_o))

for (i in seq_along(I)) {
  for (j in seq_along(K)) {
    if (data_countries_2014$cty_code_d[j] == gdp_2014$cty_code_o[i]) {
      data_countries_2014$gdp_2014_j[j] <- gdp_2014$gdp_2014[i]
    }
    else if (data_countries_2014$cty_code_d[j] != gdp_2014$cty_code_o[i]) {
      next
    }
  }
}

data_countries_2014 <- data_countries_2014[ , .(rgDesc, rtTitle, cty_code_o, ptTitle, 
                                                cty_code_d, dist, gdp_2014_i, gdp_2014_j, 
                                                TradeValue)]

data_countries_2014 <- data.frame(data_countries_2014)
data_countries_2014$TradeValue <- as.numeric(as.character(data_countries_2014$TradeValue))
data_countries_2014 <- data.table(data_countries_2014)

setnames(data_countries_2014, c("rgDesc", "dist"), c("TradeFlow", "Distance"))

# Note: 
        # TradeFlow:  Export or Import.
        # Distance:   Distance measured in km between country i and country j. 
        # gdp_2014_i: GDP of country i. 
        # gdp_2014_j: GDP of country j. 
        # TradeValue: The total value of trade from country i to country j. 

# 1.4. Using ggplot2, plot the natural log of trade flows on the y-axis and ln(distance) 
#      on the x-axis. Is the plot consistent with the gravity equation? Repeat the same 
#      exercise but use the natural log of the exporting country’s GDP on the x-axis 
#      instead.  
#-----------------------------------------------------------------------------------------

  ## Plot the natural log of trade flows on the y-axis and ln(distance) on the x-axis

plot1 <- ggplot(data_countries_2014, 
                aes(log(Distance), log(TradeValue))) + geom_point() + 
  xlab("ln(Distance)") + ylab("ln(TradeValue)")

  ## Plot the natural log of trade flows on the y-axis and natural log of the exporting 
  ## country’s GDP on the x-axis. 

plot2 <- with(data_countries_2014[data_countries_2014$TradeFlow == "Exports",], 
     ggplot(data_countries_2014, 
            aes(log(gdp_2014_i), log(TradeValue))) + geom_point()) + 
  xlab("ln(exporting countries GDP)") + ylab("ln(TradeValue)")


  ## The following plots TradeValue vs ln(Distance) but grouped by TradeFlow

plot3 <- ggplot(data_countries_2014, 
                aes(log(Distance), log(TradeValue), colour = TradeFlow)) + 
  geom_point() + xlab("ln(distance)") + ylab("ln(Trade Value)") + 
  geom_smooth(method='lm', formula = y ~ x)

ggplot2.multiplot(plot1, plot2, plot3, cols = 2)

# 1.5. Run the following gravity equation using the lm() function in R:
#-----------------------------------------------------------------------------------------

grav_eq <- lm(log(TradeValue) ~ log(gdp_2014_i) + log(gdp_2014_j) + log(Distance), 
              data = data_countries_2014)
summary(grav_eq)

stargazer(grav_eq, type = "latex", 
                             title            = "Estimation of Gravity Equation",
                             covariate.labels = c("ln(y_i)", "ln(y_j)", "ln(Distance)"),
                             dep.var.caption  = "Trade Value",
                             dep.var.labels   = "$$")

# 1.6. What do you find? Are your results comparable to the ones by Frankel et al (1995) 
#      that we covered in class?
#-----------------------------------------------------------------------------------------

# We find that the estimates on $\ln(y_{i})$ (country i's GDP) and on $\ln(y_{j})$ 
# (country j's GDP) are both positive and less than one. While the estimate on $\ln(dist)$
# is negative as expected. Hence the Gravity equation holds as expected. 



# 1.7. Create a dummy variable if both countries are in the Eurozone. 
#-----------------------------------------------------------------------------------------

  ## Eurozone countries: Austria, Belgium, Cyprus, Estonia, Finland, France (251), 
  ## Germany (276), Greece, Ireland, Italy (381), Latvia, Lithuania, Luxembourg, Malta, 
  ## the Netherlands (528), Portugal, Slovakia, Slovenia, and Spain (724). 

  ## In our dataset the countries in Eurozone are: France (251), Germany (276), Italy (381),
  ##  the Netherlands (528), Spain (724). 


  ## Let's create all the possible combinations using the Eurozone country codes. 

euro <- c(251, 276, 381, 528, 724)
comb <- data.table(expand.grid(euro, euro))
setnames(comb, c("Var1", "Var2"), c("cty_code_o", "cty_code_d"))
comb <- data.frame(comb)


  ## Create a dummy variable using the cty_code_O and cty_code_d as pairs. 

M <- as.numeric(c(comb$cty_code_o))
L <- as.numeric(c(data_countries_2014$cty_code_o))

EuroDummy <- matrix(NA, nrow(data_countries_2014))

for (j in seq_along(M)) {
  for (i in seq_along(L)) {
    if ( (comb$cty_code_o[j] == data_countries_2014$cty_code_o[i])  && (comb$cty_code_d[j] == data_countries_2014$cty_code_d[i]) )
      EuroDummy[i] <- i
  else 
      next
  }
}

data_countries_2014$EuroDummy <- ifelse(is.na(EuroDummy), 0, 1)


# 1.8. Reestimation of the gravity equation including the Eurozone dummy. 
#      What do you find regarding the coefficient on this dummy variable? 
#      How does the R2 compare to the one for regression (1). 
#-----------------------------------------------------------------------------------------


grav_eq_euro <- lm(log(TradeValue) ~ log(gdp_2014_i) + log(gdp_2014_j) + log(Distance) + EuroDummy, 
              data = data_countries_2014)

summary(grav_eq_euro)

stargazer(grav_eq_euro, type = "latex", 
          title            = "Estimation of Gravity Equation controlling for Eurozone pairs",
          covariate.labels = c("ln(y_i)", "ln(y_j)", "ln(Distance)", "EuroDummy"),
          dep.var.caption  = "Trade Value",
          dep.var.labels   = "$$")

  ## From the estimation of the gravity equation controlling for all pair of countries 
  ## who are in the Eurozone we see that the estimated coefficient on the EuroDummy is 
  ## statistically significant at 5 % significance level and has also a positive sign. 
  ## This shows that a pair countries who is part of Eurozone has a trade flow of 0.374 % 
  ## higher than a pair of countries who is not part of the Eurozone. 
  
  ## We find that R2 in this model is almost the same with R2 with the previous model. 
  ## This shows that the EuroDummy that we controlled for, does not explain much of the
  ## variation in the trade flow. 


# 1.9. Do you think your estimate of the coefficient β4 reflects the causal impact of 
#      the Euro on trade? Why or why not? Can you think of reasons why the error term 
#      uij might be correlated with the Euro dummy?
#-----------------------------------------------------------------------------------------

  ## I do not think that the estimated coeffcient on beta_4 reflects the impact of the Euro on trade. 
  ## The reason is due to simultaneity in the equation. We can raise the question: do these countries
  ## trade more because of the common currency or they joined the common currency because they trade 
  ## more with each other.  
  
  ## The gravity equation can only capture correlations and not causality. Hence the estimated 
  ## coefficient must be biased and inconsistent. 


# 1.10. Collect the same data on trade flows as before but now for the year 1996.  
#-----------------------------------------------------------------------------------------


## Exports (1996)

I_C <- as.character(c(gdp_1995$cty_code))
d1_96 <- NULL
for (i_c in I_C[1:5]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d1_96 <- rbind(d1_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d2_96 <- NULL
for (i_c in I_C[6:10]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d2_96 <- rbind(d2_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d3_96 <- NULL
for (i_c in I_C[11:15]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d3_96 <- rbind(d3_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d4_96 <- NULL
for (i_c in I_C[16:20]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "2", c = "TOTAL", ps = "1996")
      d4_96<- rbind(d4_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

## Imports (1996)

I_C <- as.character(c(gdp_1995$cty_code))
d5_96 <- NULL
for (i_c in I_C[1:5]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d5_96 <- rbind(d5_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d6_96 <- NULL
for (i_c in I_C[6:10]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d6_96 <- rbind(d6_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d7_96 <- NULL
for (i_c in I_C[11:15]) {     
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d7_96 <- rbind(d7_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

Sys.sleep(3660)

I_C <- as.character(c(gdp_1995$cty_code))
d8_96 <- NULL
for (i_c in I_C[16:20]) {
  for (i_k in I_C) {
    if (i_c == i_k) {
      next
    }
    else if (i_c != i_k) {
      data_i_c <- get.Comtrade(r = i_c, p = i_k, rg = "1", c = "TOTAL", ps = "1996")
      d8_96 <- rbind(d8_96, data_i_c)
    }
  }
  Sys.sleep(10)
}

  ## Entire dataset for year 1996. 

data_countries_1996 <- rbind(d1_96, d2_96, d3_96, d4_96, d5_96, d6_96, d7_96, d8_96)  
data_countries_1996 <- data.table(data_countries_1996)


keycols <- c("cty_code_o","cty_code_d")
setnames(data_countries_1996, c("rtCode","ptCode"), keycols)
setnames(gdp_1995, "cty_code", "cty_code_o")

  ## Convert column classes to numeric. 

data_countries_1996$cty_code_o <- as.numeric(as.character(data_countries_1996$cty_code_o))
data_countries_1996$cty_code_d <- as.numeric(as.character(data_countries_1996$cty_code_d))

gdp_1995 <- data.table(gdp_1995)
data_countries_1996 <- data.table(data_countries_1996)

  ## Please note that all the matchings was done using the GDP 1995
  ## using the data on year 1996 for TradeFlow. 
  ## Match for gdp_1995_j
  
  ## Match for gdp_1995_i. 

data_countries_1996$gdp_1995_i <- NA
I <- as.numeric(c(gdp_1995$cty_code_o))
K <- as.numeric(c(data_countries_1996$cty_code_o))

for (i in seq_along(I)) {
  for (j in seq_along(K)) {
    if (gdp_1995$cty_code_o[i] == data_countries_1996$cty_code_o[j]) {
      data_countries_1996$gdp_1995_i[j] <- gdp_1995$gdp_1995[i]
    }
    else if (gdp_1995$cty_code_o[i] != data_countries_1996$cty_code_o[j]) {
      next
    }
  }
}

data_countries_1996$gdp_1995_j <- NA
I <- as.numeric(c(gdp_1995$cty_code_o))
K <- as.numeric(c(data_countries_1996$cty_code_o))

for (i in seq_along(I)) {
  for (j in seq_along(K)) {
    if (data_countries_1996$cty_code_d[j] == gdp_1995$cty_code_o[i]) {
      data_countries_1996$gdp_1995_j[j] <- gdp_1995$gdp_1995[i]
    }
    else if (data_countries_1996$cty_code_d[j] != gdp_1995$cty_code_o[i]) {
      next
    }
  }
}

setnames(data_countries_1996, "TradeValue", "TradeValue_1996")
data_countries_1996 <- data_countries_1996[ , .(rgDesc, cty_code_o, cty_code_d, gdp_1995_i, 
                                                gdp_1995_j, TradeValue_1996)]

data_countries_1996$TradeValue_1996 <- as.numeric(as.character(data_countries_1996$TradeValue_1996))


# 1.11. Add the 1995 data to the previous one for 2014.
#-----------------------------------------------------------------------------------------

  ## The following loop will merge based on: TradeFlow, cty_code_o, and cty_code_d. 

M <- as.numeric(c(data_countries_2014$cty_code_o))
N <- as.numeric(c(data_countries_1996$cty_code_o))


for (i in seq_along(M)) {
  for (j in seq_along(N)) {
    if (data_countries_2014$cty_code_o[i] == data_countries_1996$cty_code_o[j] && 
        data_countries_2014$cty_code_d[i] == data_countries_1996$cty_code_d[j] &&
        data_countries_2014$TradeFlow[i] == data_countries_1996$TradeFlow_1996[j]) {
      
      data_countries_2014$TradeValue_1996[i] <- data_countries_1996$TradeValue_1996[j]
      data_countries_2014$gdp_1995_i[i] <- data_countries_1996$gdp_1995_i[j]
      data_countries_2014$gdp_1995_j[i] <- data_countries_1996$gdp_1995_j[j]
      
    }
  }
}

  ## Create a new dataset to use it for the DID modeling. 
  ## Note that the first difference between the indicator function for euro in 2014
  ## minus the indicator function for euro 1995 is equal to the indicator function
  ## for euro in 2014. 
  
  ## We have the compute the following first differences. 

diff_log_trade_value <- log(data_countries_2014$TradeValue) - log(data_countries_2014$TradeValue_1996)
diff_log_gdp_i <- log(data_countries_2014$gdp_2014_i) - log(data_countries_2014$gdp_1995_i)
diff_log_gdp_j <- log(data_countries_2014$gdp_2014_j) - log(data_countries_2014$gdp_1995_j)
diff_euro <- data_countries_2014$EuroDummy  

  ## The following dataset includes all the first differences. 


diff_data <-cbind(diff_log_trade_value, diff_log_gdp_i, diff_log_gdp_j, diff_euro )
diff_data <- data.table(diff_data)


# 1.12. In order to derive the diff-in-diff specification, first write down equation (2) 
#       for both years, explicitly keeping track of the respective period. Estimate this 
#       equation again with the lm() function. What do you find? How do your results 
#       differ from those found in (8)?
#-----------------------------------------------------------------------------------------

  ## First difference model. 

first_diff_model <- lm(diff_log_trade_value ~ 0 + diff_log_gdp_i + diff_log_gdp_j + diff_euro, diff_data)
summary(first_diff_model)

stargazer(first_diff_model, type = "latex", 
          title            = "Estimation of Gravity Equation with first differencing",
          covariate.labels = c("d(ln(y_i))", "d(ln(y_j))", "EuroDummy)"),
          dep.var.caption  = "Trade Value",
          dep.var.labels   = "$$", header=FALSE)


# 1.13 Suppose the error term $u_{ij}$ depends partially on factors that are time-invariant $\nu_{ij}$, e.g.
# the language spoken in a country, and partially on factors that might vary over time
# $\nu(t)$, e.g. tariffs and shipping costs:
#-----------------------------------------------------------------------------------------


# After first differencing I am willing to interpret the estimate for $\beta_4$ as 
# association but no causality. Even if we applied first differencing to remove fixed time 
# effects there are still other time invariant factors that should be considered. As I show 
# above $\Delta u_{ij}$ might still be correlated with $\Delta I\{Euro\}$, and this will 
# happen due to the time invariant effects which change across pair of countries but do not 
# change with time. A case I would think could be historical ties that countries may have had 
# prior of joining the Eurozone. The European countries in the dataset which are part of the 
# Eurozone long before the Euro was introduced were part of the European Economic Area which 
# is a free movement of persons, goods, services and capital within the European Single Market. 
# So I believe that $\Delta u_{ij}$ might still be correlated with $\Delta I\{Euro\}$. 






