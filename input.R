#### Pre-Retirement Variables

#Traditional TSP Contributions
tsp_trad <- .05
tsp_lim <- 18000

#Social Security
ss_bp1 <- 856
ss_bp2 <- 5157
aime <- 0

#inflation adjusted returns
ret <- .05
#inflation rate
inflation <- .025
state <- "Maryland"
locality <- "Montgomery County"
status <- "joint"

##### Post-Retirement Variables
state <- "Florida"
locality <- "Orlando"
status <- "joint"
#inflation adjusted returns
ret <- .05
#inflation rate
inflation <- .025
# Make retirement age a variable
retireAge <- 37
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