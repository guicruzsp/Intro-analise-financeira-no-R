#################
#
# This code will introduce you to 2 packages: quantmod and rcbc
# - quantmod will enable you to import financial data from the internet
# - rcbc will enable you to import data from the Brazilian Central Bank
#
########################


#### Preparation ####


# If this is your first time using these packages, you will need to install them first
install.packages("devtools")
install.packages("quantmod")
devtools::install_github("wilsonfreitas/rbcb")

# Load packages into our R environment
library(quantmod)
library(rbcb)


#### Quantmod ####


# We will start with quantmod
# If you want more detail about the package you can type ?quantmod into the console
# This will bring up the documentation from the package

# Now, we will Obtain stocks data
# The quantmod package enables us to import data from yahoo finance as standard

# We can build a list of the assets we want to get 
stock_list <- c("PETR4.SA","VALE3.SA","ITUB3.SA")
# Then we pass this list to the getsymbols command, which will download
# The data and create a variable for each asset in our list
# We can also select the periodicity of the time-series and the period
getSymbols(stock_list,
           periodicity='monthly',
           from="2000-01-01",
           to ="2020-12-31")

# Print the first observations from the dataset
head(PETR4.SA)
# The command downloads the following information
colnames(PETR4.SA)

# Plot adjusted prices
plot(PETR4.SA$PETR4.SA.Adjusted)

# Calculate returns
stock_returns <- diff(PETR4.SA$PETR4.SA.Adjusted)/lag(PETR4.SA$PETR4.SA.Adjusted)
# Eliminate first observation
# We do this since after applying diff, our first observations becomes NA
stock_returns <- stock_returns[-1]

# Plot returns and a histogram
plot(stock_returns)
hist(stock_returns, breaks = 50)


#### rbcb ####


# Next we import data from BACEN using the package rbcb
# It imports data from https://www3.bcb.gov.br/sgspub
# If you want to get different variables, you should check 
# The site and search for the specific code

# Import monthly IPCA data from BCB using rbcb package
IPCA <- get_series(c(IPCA = 433),
                   start_date = "2000-01-01",
                   end_date = "2020-12-31",
                   as = "xts")
# Plot monthly IPCA
plot(IPCA)

# Import annualized Selic data from BCB using rbcb package
SELIC <- get_series(c(SELIC = 1178),
                          start_date = "2000-01-01",
                          end_date = "2020-12-31",
                          as = "xts")
# Plot annualized Selic
plot(SELIC)


#### Analysis ####


# Lets analyze the data we just downloaded

# Arithmetic average of returns
AAR <- sum(stock_returns)/nrow(stock_returns)
# Geometric average of returns
GAR <- prod(1+stock_returns)^(1/nrow(stock_returns))-1

# Lets compare them
print(paste("Arithmetic average of returns: ",round(AAR*100,2),"%", sep = ""))
print(paste("Geometric average of returns: ",round(GAR*100,2),"%", sep = ""))

# Now, lets obtain the excess returns for our assets 
# Using the risk-free rate we just obtained

# Since our data was downloaded at a daily frequency and
# An annualized rate
# Lets convert it to monthly
RF <- SELIC
RF <- (1+RF/100)^(1/252)
RF <- apply.monthly(RF,prod)
RF <- round((RF-1),4)

index(RF) <- index(RF)+5
index(RF) <- as.Date(format(index(RF),"%Y-%m-01"))

# Calculate excess returns
stock_excess_returns <- stock_returns - RF

# Plot excess returns
plot(stock_excess_returns)
hist(stock_excess_returns, breaks = 50)

# Next we will look at the sharpe ratio for our asset
# on the analyzed period
Sharpe_ratio <- mean(stock_excess_returns)/sd(stock_excess_returns)

# Lets build a new dataset where we have the two series together
# and then plot them

# This command merge our two datasets, which are in the XTS format
mydata <- merge.xts(stock_returns,RF)
plot(mydata*100, type = "l")
addLegend("topleft", 
          legend.names=c("Stock Returns", "Selic"),
          col=1:2,
          fill=1:2,
          bg="white")


# At last, we will look at the real interest rates in Brazil for 
# The period we are analyzing
inflation_rate <- IPCA/100
nominal_interest_rate <- RF

real_interest_rate <- (1+nominal_interest_rate)/(1+inflation_rate) - 1

plot(real_interest_rate*100)

annualized_real_interest_rate <- (1+real_interest_rate)^12-1

plot(annualized_real_interest_rate*100)

