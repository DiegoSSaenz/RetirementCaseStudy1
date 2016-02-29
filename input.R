#### Pre-Retirement Variables #####
# All variables in 2016 Dollars
# I made a change!!!

#Traditional TSP Contributions
tsp_trad <- .05   # 5% self contributions
tsp_match <- .05  # 5% Employer TSP Matching
tsp_lim <- 18000  # TSP Contribution Limit

# Savings Rate: Savings = After-tax Income - Spending
sav_rate <- 0.15

#Social Security
ss_bp1 <- 856
ss_bp2 <- 5157
aime <- 0

# FERS Contribution Percent FERS=0.8%, FERS-RAE=3.1% (1/1/2013), FERS-FRAE=4.4% (1/1/2014)
fers <- 0.008

#inflation adjusted returns
ret <- .05
#inflation rate
inflation <- .025
state <- "Maryland"
locality <- "Montgomery County"
status <- "joint"

####### Post-Retirement Variables ########
r_state <- "Florida"
r_locality <- "Orlando"
r_status <- "joint"
#inflation adjusted returns
ret <- .05
#inflation rate
inflation <- .025
# Make retirement age a variable
retireAge <- 37
age_leave <- 37  # Age stop working
pens_age <- 57   # Age FERS Pension begins
eo <- FALSE      # Whether offered Early Out
# Percentage for Roth ladder
ladder <- .04
# Percentage for TSP withdrawal rate between 62 and 70
tspRate <- .04
# Percentage for Roth IRA withdrawal rate post-retirement
rothRate <- .07
# Percentage for taxable investment withdrawal rate before age 63 (at which point it goes to 0%)
taxAcctRate <- .33
# Make age at death a variable (not working properly)
die <- 100