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
dat <- tbl_df(read.csv("pay.csv", stringsAsFactors=FALSE))

# Calculate net income
net <- mapply(net, as.list(dat$gross),
              replicate(nrow(dat), list(fTax)),
              replicate(nrow(dat), list(sTax)),
              replicate(nrow(dat), list(lTax)),
              replicate(nrow(dat), list(has_loc)),
              as.list(dat$tsp),
              as.list(dat$trad),
              as.list(dat$hsa)
)

dat <- bind_cols(dat, as.data.frame(net))

# Create TSP contribution data based on minimum contributions
dat <-
    dat %>%
    # Create additional data fields for sensitivity study
    mutate(tsp_bal  = gross,
           roth_bal = gross,
           trad_bal = gross,
           hsa_bal  = gross,
           tax_bal  = gross,
           pens_eo  = gross,
           pens_57  = gross,
           pens_60  = gross,
           pens_62  = gross,
           ss       = gross)

for(i in 1:nrow(dat)){
    #Post-tax and post-TSP contribution income
    # dat[i,6] <- net(dat$gross[i],fTax,sTax,lTax,has_loc,dat$tsp[i])
    #TSP Balance assumed inflation adjusted rate of return equal to ret
    if(i>1){
        dat$tsp_bal[i] <- dat$tsp[i-1] + dat$tsp_bal[i-1]*(1+ret)+
            dat$gross[i-1]*.05
        
        dat$trad_bal[i] <- dat$trad[i-1]     + dat$trad_bal[i-1] * (1+ret)
        dat$hsa_bal[i]  <- dat$hsa[i-1]      + dat$hsa_bal[i-1]  * (1+ret)
        dat$roth_bal[i] <- dat$roth[i-1]     + dat$roth_bal[i-1] * (1+ret)
        dat$tax_bal[i]  <- dat$taxable[i-1]  + dat$tax_bal[i-1]  * (1+ret)
        
    }else{
        dat$tsp_bal[i]  <- 0 
        dat$trad_bal[i] <- 0
        dat$hsa_bal[i]  <- 0
        dat$roth_bal[i] <- 0
        dat$tax_bal[i]  <- 0
    }
    ####  Early Out if retire at this point  ####
    if(((dat$age[i]>=50) & (dat$yos[i]>=20)) | (dat$yos[i]>=25)){
        dat$pens_eo[i] <- dat$yos[i] * 0.01 *
            (dat$gross[i] + dat$gross[i-1]/(1+inflation) + dat$gross[i-2]/(1+inflation)^2)/3
        #            max(dat$gross[1:i]) * (1 + (1+inflation)^-1 + (1+inflation)^-2)/3
        
    }else{
        dat$pens_eo[i] <- NA
    }
    #### Pension at 57 if retire at this point ####
    if(dat$yos[i]>=30){
        dat$pens_57[i] <- dat$yos[i] * 0.01 *
            (dat$gross[i] + dat$gross[i-1]/(1+inflation) + dat$gross[i-2]/(1+inflation)^2)/3
    }else if(dat$yos[i]>=10){
        dat$pens_57[i] <- dat$yos[i] * 0.01 * 0.75 *
            (dat$gross[i] + dat$gross[i-1]/(1+inflation) + dat$gross[i-2]/(1+inflation)^2)/3
    }else{
        dat$pens_57[i] <- NA
    }
    #### Pension at 60 if retire at this point ####
    if(dat$yos[i]>=20){
        dat$pens_60[i] <- dat$yos[i] * 0.01 * 
            (dat$gross[i] + dat$gross[i-1]/(1+inflation) + dat$gross[i-2]/(1+inflation)^2)/3
    }else if(dat$yos[i]>=10){
        dat$pens_60[i] <- dat$yos[i] * 0.01 * 0.90 *
            (dat$gross[i] + dat$gross[i-1]/(1+inflation) + dat$gross[i-2]/(1+inflation)^2)/3
    }else{
        dat$pens_60[i] <- NA
    }
    #### Pension at 62 if retire at this point ####
    if((dat$yos[i]>=20) & (dat$age[i]>=62)){
        dat$pens_62[i] <- dat$yos[i] * 0.011 * 
            (dat$gross[i] + dat$gross[i-1]/(1+inflation) + dat$gross[i-2]/(1+inflation)^2)/3
    }else if(dat$yos[i]>=5){
        dat$pens_62[i] <- dat$yos[i] * 0.01 * 
            (dat$gross[i] + dat$gross[i-1]/(1+inflation) + dat$gross[i-2]/(1+inflation)^2)/3
    }else{
        dat$pens_62[i] <- NA
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

# pdf("working.pdf",height=22,width=20)
# grid.table(round(dat,2))
# dev.off()

##### Post-Retirement Calculation #####

#Calls functions to create appropriate tax brackets
fTax <- fed_br(status)
sTax <- st_br(state,status)
lTax <- loc_br(state,locality)
has_loc <- hasLocality(state)
rmd <- c(27.4,26.5,25.6,24.7,23.8,22.9,22.0,21.2,20.3,19.5,18.7,17.9,
         17.1,16.3,15.5,14.8,14.1,13.4,12.7,12.0,11.4,10.8,10.2,9.6,
         9.1,8.6,8.1,7.6,7.1,6.7,6.3,5.9,5.5,5.2,4.9,4.5,
         4.2,3.9,3.7,3.4,3.1,2.9,2.6,2.4,2.1,1.9)

r_dat <- dat %>% filter(age==retireAge) %>% select(-pens_eo,-pens_60,-pens_62,-gross,-tsp,-roth,-trad,-hsa)

# Add capability to specify desired spending level? Otherwise, should depend on if mortgage exists, taxable bal, and roth bal
# spend <- r_dat %>% mutate(exp=ifelse(r_dat$age < 57,
#                           ifelse(mortgage > 0, (roth_bal+tax_bal-mort*12)/(58-r_dat$age), roth_bal+taxable),
#                           tsp_bal/(151-r_dat$age))) %>%
#         select(exp)

# Check balances
# tail(dat, 50) %>% select(-pens_eo,-pens_60,-pens_62,-gross,-tsp,-roth,-trad,-hsa,-cashflow) 

# exp <- r_dat$net
# exp <- spend$exp

# Roth balance available
rothAvail <- dat %>% summarise(sum(roth))

#years to RMDs
rmd_y <- 70-r_dat$age

# Loop modified w/ following assumptions:
# 
# - All vectors are pre-allocated
# - Turned off PDF printing (significantly increases runtime)
# - Retirement age is now a variable
# - Add logic accounting for "early" retirement which makes withdrawals @?% from TSP based on Roth ladder concept; the % is a variable
# - Begin drawing pension at 57; none before
# - SS + pension is still re-invested into TSP
# - Added age-dependent pension and SS vector initializations
# - Assumes variable TSP withdrawal rate after 63
# - Taxable investment account is factored in
# - HSA balance is not factored in
# - Roth IRA balance is factored in
# - Traditional IRA is not factored in
# - Mortgage balance not factored in (optimize taxable balance to payoff mortgage once retire?)
# - Kid(s) going to college not factored in
# - Spending vs. time not factored in
# - Available account balance column added
# - Married filing jointly (assumes two income streams; one teacher + one GG14 NRC emp)
# - Maxing TSP and 403(b) beginning in 8th year of career (~5% TSP matching before)
# - Maxing HSA in 8th year of career
# - Maxing Roth IRA in 8th year of career
# - Deposit of $25k in taxable investment account in 8th year of career
# - Could pay mortgage off at retirement by using ~$50k in taxable investment account
# - Roth IRA to help pay for college if necessary (~$187k available -- i.e. contributions only)

age <- numeric(die+1-r_dat$age)
pens <- numeric(die+1-r_dat$age)
ss <- numeric(die+1-r_dat$age)
tsp_bal <- numeric(die+1-r_dat$age)
roth_bal <- numeric(die+1-r_dat$age)
tax_bal <- numeric(die+1-r_dat$age)
gross <- numeric(die+1-r_dat$age)
net <- numeric(die+1-r_dat$age)

for(i in 1:(die+1-r_dat$age)){
    if(i==1){
        age <- r_dat$age
        pens <- 
            if(age>=57) {
                r_dat$pens_57  
            } else{
                0
            }
        ss <-
            if(age>=63) {
                r_dat$ss*(.7)  
            } else{
                0
            }
        
        tsp_bal <- r_dat$tsp_bal
        roth_bal <- as.numeric(rothAvail)
        tax_bal <- r_dat$tax_bal
        
        tsp_bal <- tsp_bal*(1+ret) - tsp_bal*ladder
        roth_bal <- roth_bal - roth_bal*rothRate
        tax_bal <- tax_bal*(1+ret) - tax_bal*taxAcctRate
        
        gross <- tsp_bal*ladder + roth_bal*rothRate + tax_bal*taxAcctRate
        
        net <- net_r(gross,fTax,sTax,lTax,has_loc)
        # taxable <- 0
    }else if(i<(63-r_dat$age)){
        age <- append(age,age[i-1]+1)
        if (i<=58-r_dat$age) {
            pens <- append(pens,pens[i-1])
            tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret)-tsp_bal[i-1]*ladder) 
            roth_bal <- append(roth_bal, roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate)
            tax_bal <- append(tax_bal, tax_bal[i-1]*(1+ret)-tax_bal[i-1]*taxAcctRate)
            ss <- append(ss, 0)            
            gross <- append(gross, tsp_bal[i]*ladder + roth_bal[i]*rothRate + tax_bal[i]*taxAcctRate)
            net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
            # taxable <- append(taxable, 0)
            if (i==58-r_dat$age) pens[i] <- r_dat$pens_57
            # exp <- tsp_bal[i-1]*.04
        } else if (i<=63-r_dat$age & i>58-r_dat$age) {
            pens <- append(pens,pens[i-1])
            tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret)-tsp_bal[i-1]*ladder)
            roth_bal <- append(roth_bal, roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate)   
            tax_bal <- append(tax_bal, tax_bal[i-1]*(1+ret)-tax_bal[i-1]*taxAcctRate)
            ss <- append(ss, 0)            
            gross <- append(gross, tsp_bal[i]*ladder + pens[i] + roth_bal[i]*rothRate + tax_bal[i]*taxAcctRate)
            net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
            # taxable <- append(taxable, 0)
        }    
    }else if(i>70-r_dat$age){
        age <- append(age,age[i-1]+1)
        pens <- append(pens,pens[i-1])
        tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - 
                              tsp_bal[i-1]/rmd[i-rmd_y])        
        roth_bal <- append(roth_bal, roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate)
        tax_bal <- append(tax_bal, tax_bal[i-1]*(1+ret)-tax_bal[i-1]*0)
        ss <- append(ss, r_dat$ss*(.7))
        gross <- append(gross, pens[i] +
                               r_dat$ss*(.7) +
                               tsp_bal[i]/rmd[i-rmd_y] +
                               roth_bal[i]*rothRate +
                               tax_bal[i]*0
                        )

        net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
        # taxable <- append(taxable, taxable[i-1]*(1+ret)+ net[i] - exp)
    }else{
        age <- append(age,age[i-1]+1)
        pens <- append(pens,pens[i-1])
        tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - tsp_bal[i-1]*tspRate)         
        roth_bal <- append(roth_bal, roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate)
        tax_bal <- append(tax_bal, tax_bal[i-1]*(1+ret)-tax_bal[i-1]*0)
        ss <- append(ss, r_dat$ss*(.7))
        gross <- append(gross, pens[i] + 
                               r_dat$ss*(.7) +
                               tsp_bal[i]*tspRate +
                               roth_bal[i]*rothRate +
                               tax_bal[i]*0
                        )
        net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
        # taxable <- append(taxable, 0)
    }
}

# rothBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(roth_bal)
tradBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(trad_bal)
hsaBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(hsa_bal)
# taxBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(tax_bal)
mort <- dat %>% filter(age>=retireAge, age<=die) %>% select(mortgage)

dat_r <- data.frame(age,gross,net,pens,ss,tsp_bal,roth_bal,hsaBal,tax_bal,mort)

# Add margin column
dat_r <- tbl_df(dat_r) %>% mutate(avail=ss+tsp_bal+roth_bal+tax_bal)

write.table(dat_r, "retirement.out")
# pdf("retirement.pdf",height=(die+1-r_dat$age)*15/44,width=11)
# grid.table(round(dat_r,2))
# dev.off()

plotDat <- tbl_df(melt(dat_r, id.vars=c("age")))

ggplot(plotDat %>% filter(variable == "net" | variable == "avail"), aes(age, value)) +
  geom_line() +
  facet_grid(variable~., scales="free") +
  labs(title=paste("Retire at", retireAge, "Case Study"))

ggsave("net.png")