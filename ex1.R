# Example 
library(dplyr)
library(gridExtra)
library(reshape2)
library(ggplot2)

source('TaxFunctions.R')
source('input.R')

#Calls functions to create appropriate tax brackets
fTax <- fed_br(status)
sTax <- st_br(state,status)
lTax <- loc_br(state,locality)
has_loc <- hasLocality(state)

# This is a starter "input file"
dat <- tbl_df(read.csv(file, stringsAsFactors=FALSE))

########################################
##### Pre-Retirement Calculation ######
########################################

# Calculate tax-deferred contributions
# dat <- dat %>%
#     mutate(tsp = pmin(gross * tsp_trad,tsp_lim))

# dat <- dat %>%
#     mutate(tsp_bal = tsp)

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
dat$net <- dat$net - fers*dat$gross

# Calculate Savings and Spending based on savings rate (sav_rate)
# dat <- dat %>% mutate(spend = net * (1-sav_rate))
# dat <- dat %>% mutate(savings = (net-spend))

# Create TSP contribution data based on minimum contributions
dat <-
    dat %>%
    # Create additional data fields for sensitivity study
    mutate(spend = gross,
           tsp_bal  = gross,
           roth_bal = gross,
           trad_bal = gross,
           hsa_bal  = gross,
           tax_bal  = gross,
           ss      = gross)

for(i in 1:nrow(dat)){
    #Post-tax and post-TSP contribution income
    # dat[i,6] <- net(dat$gross[i],fTax,sTax,lTax,has_loc,dat$tsp[i])
    #TSP Balance assumed inflation adjusted rate of return equal to ret
    if(i>1){
        dat$tsp_bal[i] <- dat$tsp[i-1] + dat$tsp_bal[i-1]*(1+ret)+
            dat$gross[i-1]*tsp_match
        dat$trad_bal[i] <- dat$trad[i-1]     + dat$trad_bal[i-1] * (1+ret)
        dat$hsa_bal[i]  <- dat$hsa[i-1]      + dat$hsa_bal[i-1]  * (1+ret)
        dat$roth_bal[i] <- dat$roth[i-1]     + dat$roth_bal[i-1] * (1+ret)
        dat$tax_bal[i]  <- dat$taxable[i-1]  + dat$tax_bal[i-1]  * (1+ret)
    }else{
        dat$tsp_bal[i] <- 0
        dat$trad_bal[i] <- 0
        dat$hsa_bal[i]  <- 0
        dat$roth_bal[i] <- 0
        dat$tax_bal[i]  <- 0        
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

# Calculate spending based on money not saved
dat$spend <- dat$net - dat$roth - dat$hsa - dat$taxable


# pdf("working.pdf",height=22,width=17)
# grid.table(round(dat,2))
# dev.off()

########################################
##### Post-Retirement Calculation ######
########################################

#Calls functions to create appropriate retirement tax brackets
fTax <- fed_br(r_status)
sTax <- st_br(r_state,r_status)
lTax <- loc_br(r_state,r_locality)
has_loc <- hasLocality(r_state)

r_dat <- dat %>% filter(age==retireAge) %>% select(-gross,-tsp,-roth,-trad,-hsa)

# Set spending to spend_ret percent of last year of work
# spend <- r_dat$spend*spend_ret
spend <- spend_const

# Pension at year of first collection
high_3 <- dat[dat$age %in% (age_leave-2):age_leave,]$gross
pens_0 <- pension(high_3[3],high_3[2],high_3[1],age_leave,
                r_dat$yos,pens_age,eo,inflation)
pens_0 <- pens_0/(1+inflation)^(pens_age-age_leave)

# Inflation Adjusted Roth IRA balance available
inf_adj <- 1/(1+inflation)^(retireAge-filter(dat,age<=retireAge)$age)
rothAvail_0 <- sum(filter(dat,age<=retireAge)$roth*inf_adj)

#years to RMDs
rmd_y <- 70-r_dat$age

age <- numeric(die+1-r_dat$age)
pens <- numeric(die+1-r_dat$age)
ss <- numeric(die+1-r_dat$age)
tspT_bal <- numeric(die+1-r_dat$age)
tspR_bal <- numeric(die+1-r_dat$age)
tradIRA_bal <- numeric(die+1-r_dat$age)
rothIRA_bal <- numeric(die+1-r_dat$age)
rothAvail <- numeric(die+1-r_dat$age)
hsa_bal <- numeric(die+1-r_dat$age)
tax_bal <- numeric(die+1-r_dat$age)
gross <- numeric(die+1-r_dat$age)
net <- numeric(die+1-r_dat$age)
avail <- numeric(die+1-r_dat$age)
efc <- numeric(die+1-r_dat$age)
# kic <- numeric(die+1-r_dat$age)

for(i in 1:(die+1-r_dat$age)){
    if(i==1){
        age[i] <- r_dat$age
        if(age[i]>=pens_age) {
            pens[i] <- pens_0
        }
        if(age[i]>=63) {
            ss[i] <- r_dat$ss*(.7)
        }
        tspT_bal[i] <- r_dat$tsp_bal
        tspR_bal[i] <- 0
        tradIRA_bal[i] <- 0
        rothIRA_bal[i] <- r_dat$roth_bal
        rothAvail[i] <- rothAvail_0
        tax_bal[i] <- r_dat$tax_bal
        hsa_bal[i] <- r_dat$hsa_bal
        update <- acct_opt(age[i], age_leave,spend,ss[i],pens[i],tspT_bal[i],
                 tspR_bal[i],tradIRA_bal[i],rothIRA_bal[i],hsa_bal[i],
                 tax_bal[i],rothAvail[i])
    }else{
        age[i] <- age[i-1]+1 
        if(age[i]==pens_age) {
            pens[i] <- pens_0  
        }else if(age[i]>=63){
            pens[i] <- pens[i-1]
        }else if(age[i]>pens_age){
            pens[i] <- pens[i-1]/(1+inflation)
        }
        if(age[i]>=63) {
            ss[i] <- r_dat$ss*(.7)
        }
        update <- acct_opt(age[i], age_leave,spend,ss[i],pens[i],
                           tspT_bal[i-1],tspR_bal[i-1],
                           tradIRA_bal[i-1],rothIRA_bal[i-1],hsa_bal[i-1],
                           tax_bal[i-1],rothAvail[i-1])
    }
    tspT_bal[i] <- update[[1]]*(1+ret)
    tspR_bal[i] <- update[[2]]*(1+ret)
    tradIRA_bal[i] <- update[[3]]*(1+ret)
    rothIRA_bal[i] <- update[[4]]*(1+ret)
    hsa_bal[i] <- update[[5]]*(1+ret)
    tax_bal[i] <- update[[6]]*(1+ret)
    rothAvail[i] <- update[[7]]
    taxed <- update[[8]] # all of taxed income including conversions
    tax_free <- update[[9]] # tax free money to spend
    taxed_spend <- update[[10]] # taxed money to spend
    gross[i] <- taxed_spend+tax_free #taxed+untaxed income/withdrawals
    net[i] <- net_r(taxed_spend,fTax,sTax,lTax,has_loc)+tax_free
    avail[i] <- available(age[i], age_leave,tspT_bal[i],
                          tspR_bal[i],tradIRA_bal[i],rothIRA_bal[i],hsa_bal[i],
                          tax_bal[i],rothAvail[i])
    efc[i] <- fafsa(agi=taxed,p1_ei=0,p2_ei=0,tax_free,afi=0,
                    fed_tax=fed_tax(taxed,fTax),assets=tax_bal[i],
                    bus_farm=0,hh=3,kic=1,age[i],st=r_state)
}

# rothBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(roth_bal)
# tradBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(tradIRA_bal)
# hsaBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(hsa_bal)
# taxBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(tax_bal)
mort <- dat %>% filter(age>=retireAge, age<=die) %>% select(mortgage)

dat_r <- data.frame(age,gross,net,pens,ss,tspT_bal,tspR_bal,tradIRA_bal,
                    rothIRA_bal,hsa_bal,tax_bal,avail,efc,mort)

# Add margin column
dat_r <- tbl_df(dat_r) # %>% mutate(avail=ss+tspT_bal+rothIRA_bal+tax_bal)

write.table(dat_r, "retirement.out")
# pdf("retirement.pdf",height=(die+1-r_dat$age)*15/44,width=12)
# grid.table(round(dat_r,2))
# dev.off()

plotDat <- tbl_df(melt(dat_r, id.vars=c("age")))

ggplot(plotDat %>% filter(variable=="gross" | variable=="avail" | 
                              variable=="tspT_bal" | variable=="rothIRA_bal" | 
                              variable=="hsa_bal" | variable=="tax_bal" |
                              variable=="efc"), 
       aes(age, value)) +
    geom_line() +
    facet_grid(variable~., scales="free") +
    labs(title=paste("Retire at", retireAge, "Case Study"))

ggsave("net.png")