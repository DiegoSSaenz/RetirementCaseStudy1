# Example 
library(dplyr)
library(gridExtra)
source('TaxFunctions.R')

# Traditional TSP Contributions and contribution limit
tsp_trad <- .05   # 5% self contributions
tsp_match <- .05  # 5% Employer TSP Matching
tsp_lim <- 18000  # TSP Contribution Limit

# Social Security Bend Points
ss_bp1 <- 856
ss_bp2 <- 5157
# Initial Social Security AIME based on past contributions
aime <- 0
# FERS Contribution Percent FERS=0.8%, FERS-RAE=3.1% (1/1/2013), FERS-FRAE=4.4% (1/1/2014)
fers <- 0.008

# Inflation adjusted returns
ret <- .05
# Inflation rate
inflation <- .025
# Savings Rate: Savings = After-tax Income - Spending
sav_rate <- 0.15


# Residence while working
state <- "Maryland"
locality <- "Montgomery County"
status <- "single"

#Calls functions to create appropriate tax brackets
fTax <- fed_br(status)
sTax <- st_br(state,status)
lTax <- loc_br(state,locality)
has_loc <- hasLocality(state)

# This is a starter "input file"
dat <- tbl_df(read.csv("pay.csv",
                       stringsAsFactors=FALSE))

# Calculate tax-deferred contributions
dat <- dat %>%
    mutate(tsp = pmin(gross * tsp_trad,tsp_lim))
dat <- dat %>%
    mutate(tsp_bal = tsp)

# Calculate net income
net <- mapply(net, as.list(dat$gross),
              replicate(nrow(dat), list(fTax)),
              replicate(nrow(dat), list(sTax)),
              replicate(nrow(dat), list(lTax)),
              replicate(nrow(dat), list(has_loc)),
              as.list(dat$tsp)
)

dat <- bind_cols(dat, as.data.frame(net))

# FERS Reductions of post-tax income
dat$net <- dat$net *(1-fers)

# Calculate Savings and Spending based on savings rate (sav_rate)
dat <- dat %>% mutate(spend = net * (1-sav_rate))
dat <- dat %>% mutate(savings = (net-spend))

# Create TSP contribution data based on minimum contributions
dat <-
    dat %>%
    # Create additional data fields for sensitivity study
    mutate(ss      = gross)

for(i in 1:nrow(dat)){
    #Post-tax and post-TSP contribution income
    # dat[i,6] <- net(dat$gross[i],fTax,sTax,lTax,has_loc,dat$tsp[i])
    #TSP Balance assumed inflation adjusted rate of return equal to ret
    if(i>1){
        dat$tsp_bal[i] <- dat$tsp_bal[i] + dat$tsp_bal[i-1]*(1+ret)+
            dat$gross[i]*tsp_match
    }else{
        dat$tsp_bal[i] <- dat$tsp_bal[i] + dat$gross[i]*tsp_match 
    }
    #### Social Security at 62 if retire at this point ####
    if(dat$yos[i]>=10){
        aime <- dat %>% filter(yos>=(i-35) & yos<=i-1) %>% select(gross) %>% 
            as.matrix %>% pmin(.,118500) %>% sum / 420
        if(aime<=ss_bp1){
            dat$ss[i] <- (aime * 0.9)*12
        }else if(aime<=ss_bp2){
            dat$ss[i] <- (ss_bp1*0.9 + (aime-ss_bp1)*0.32)*12
        }else{
            dat$ss[i] <- (ss_bp1*0.9 + (ss_bp2-ss_bp1)*0.32 + (aime-ss_bp2)*0.15)*12
        }
    }else{
        dat$ss[i] <- NA
    }
}

pdf("working.pdf",height=22,width=17)
grid.table(round(dat,2))
dev.off()

##### Post-Retirement Calculation #####
state <- "Florida"
locality <- "Orlando"
status <- "single"
#inflation adjusted returns
ret <- .05
# Age when last worked
age_leave <- 57  # Age when last worked
ret_age <- 57    # Age when pension collection starts
eo <- FALSE      # Whether offered Early Out

#inflation rate
inflation <- .025

#Calls functions to create appropriate tax brackets
fTax <- fed_br(status)
sTax <- st_br(state,status)
lTax <- loc_br(state,locality)
has_loc <- hasLocality(state)
rmd <- c(27.4,26.5,25.6,24.7,23.8,22.9,22.0,21.2,20.3,19.5,18.7,17.9,
            17.1,16.3,15.5,14.8,14.1,13.4,12.7,12.0,11.4,10.8,10.2,9.6,
            9.1,8.6,8.1,7.6,7.1,6.7,6.3,5.9,5.5,5.2,4.9,4.5,
            4.2,3.9,3.7,3.4,3.1,2.9,2.6,2.4,2.1,1.9)

dat_last3 <- dat[dat$age %in% (age_leave-2):age_leave,]
gross_3 <- x$gross[1];gross_2 <- x$gross[2];gross_1 <- x$gross[3]

r_dat <- dat %>% filter(age==age_leave) %>% select(-pens_eo,-pens_60,-pens_62)
# Set spending in retirement to be the same as last year of work
exp <- r_dat$spend
#years to RMDs
rmd_y <- 70-r_dat$age
for(i in 1:(101-r_dat$age)){
    if(i==1){
        age <- r_dat$age
        pens <- pension(gross_1,gross_2,gross_3,age_leave,r_dat$yos,age_leave,eo,inflation)
        ss <- 0
        tsp_bal <- r_dat$tsp_bal
        gross <- exp+20000
        tsp_bal <- tsp_bal*(1+ret) - exp-20000+pens[i]
        net <- net_r(gross,fTax,sTax,lTax,has_loc)
        taxable <- 0
    }else if(i<(63-r_dat$age)){
        age <- append(age,age[i-1]+1)
        pens <- append(pens,pens[i-1]/(1+inflation))
        ss <- append(ss, 0)
        gross <- append(gross, exp+20000)
        tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - 
                              exp-20000 + pens[i])
        net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
        taxable <- append(taxable, 0)
    }else if(i>70-r_dat$age){
        age <- append(age,age[i-1]+1)
        pens <- append(pens,pens[i-1])
        ss <- append(ss, r_dat$ss*(.7+.325))
        gross <- append(gross, pens[i] + r_dat$ss*(.7+.325) +
                            tsp_bal[i-1]/rmd[i-rmd_y])
        tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - 
                              tsp_bal[i-1]/rmd[i-rmd_y])
        net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
        taxable <- append(taxable, taxable[i-1]*(1+ret)+ net[i] - exp)
    }else{
        age <- append(age,age[i-1]+1)
        pens <- append(pens,pens[i-1])
        ss <- append(ss, r_dat$ss*(.7+.325))
        gross <- append(gross, exp+20000)
        tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - exp-20000 +
                              pens[i] + r_dat$ss*(.7+.325))
        net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
        taxable <- append(taxable, 0)
    }
}
dat_r <- data.frame(age,gross,net,pens,ss,tsp_bal)
pdf("retirement.pdf",height=15,width=8.5)
grid.table(round(dat_r,2))
dev.off()

