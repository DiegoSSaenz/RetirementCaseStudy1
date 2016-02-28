library(dplyr)
# library(gridExtra)
library(reshape2)
library(ggplot2)

source('TaxFunctions.R')
source('input.R')

dat <- tbl_df(read.csv("pay.csv", stringsAsFactors=FALSE))

ex1 <-function(inDat=dat, retireAge=46, ladder=.04, tspRate=.04, rothRate=.07, taxAcctRate=.33, ret=.05) {
#Calls functions to create appropriate tax brackets
fTax <- fed_br(status)
sTax <- st_br(state,status)
lTax <- loc_br(state,locality)
has_loc <- hasLocality(state)

# This is a starter "input file"
# dat <- tbl_df(read.csv("pay.csv", stringsAsFactors=FALSE))
dat <- inDat

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
dat <- dat %>% mutate(spend = net * (1-sav_rate))
dat <- dat %>% mutate(savings = (net-spend))

# Create TSP contribution data based on minimum contributions
dat <-
    dat %>%
    # Create additional data fields for sensitivity study
    mutate(tsp_bal  = gross,
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

# pdf("working.pdf",height=22,width=17)
# grid.table(round(dat,2))
# dev.off()

########################################
##### Post-Retirement Calculation ######
########################################

# r_dat <- dat %>% filter(age==age_leave)
# # Set spending in retirement to be the same as last year of work
# exp <- r_dat$spend
# #years to RMDs
# rmd_y <- 70-r_dat$age
# for(i in 1:(101-r_dat$age)){
#     if(i==1){
#         age <- r_dat$age
#         pens <- pension(gross_1,gross_2,gross_3,age_leave,r_dat$yos,age_leave,eo,inflation)
#         ss <- 0
#         tsp_bal <- r_dat$tsp_bal
#         gross <- exp+20000
#         tsp_bal <- tsp_bal*(1+ret) - exp-20000+pens
#         net <- net_r(gross,fTax,sTax,lTax,has_loc)
#         taxable <- 0
#     }else if(i<(63-r_dat$age)){
#         age <- append(age,age[i-1]+1)
#         pens <- append(pens,pens[i-1]/(1+inflation))
#         ss <- append(ss, 0)
#         gross <- append(gross, exp+20000)
#         tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - 
#                               exp-20000 + pens[i])
#         net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
#         taxable <- append(taxable, 0)
#     }else if(i>70-r_dat$age){
#         age <- append(age,age[i-1]+1)
#         pens <- append(pens,pens[i-1])
#         ss <- append(ss, r_dat$ss*(.7+.325))
#         gross <- append(gross, pens[i] + r_dat$ss*(.7+.325) +
#                             tsp_bal[i-1]/rmd[i-rmd_y])
#         tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - 
#                               tsp_bal[i-1]/rmd[i-rmd_y])
#         net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
#         taxable <- append(taxable, taxable[i-1]*(1+ret)+ net[i] - exp)
#     }else{
#         age <- append(age,age[i-1]+1)
#         pens <- append(pens,pens[i-1])
#         ss <- append(ss, r_dat$ss*(.7+.325))
#         gross <- append(gross, exp+20000)
#         tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - exp-20000 +
#                               pens[i] + r_dat$ss*(.7+.325))
#         net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
#         taxable <- append(taxable, 0)
#     }
# }
# dat_r <- data.frame(age,gross,net,pens,ss,tsp_bal)
# pdf("retirement.pdf",height=15,width=8.5)
# grid.table(round(dat_r,2))
# dev.off()

#Calls functions to create appropriate retirement tax brackets
fTax <- fed_br(r_status)
sTax <- st_br(state,r_status)
lTax <- loc_br(state,r_locality)
has_loc <- hasLocality(r_state)

h3 <- dat[dat$age %in% (age_leave-2):age_leave,]
gross_3 <- h3$gross[1];gross_2 <- h3$gross[2];gross_1 <- h3$gross[3]

r_dat <- dat %>% filter(age==retireAge) %>% select(-gross,-tsp,-roth,-trad,-hsa)

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
rothAvail <- dat %>% filter(age<=retireAge) %>% summarise(sum(roth))

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
        age[i] <- r_dat$age
        pens[i] <- 
            if(age[i]>=57) {
                pension(gross_1,gross_2,gross_3,age_leave,r_dat$yos,pens_age,eo,inflation)  
            } else{
                0
            }
        ss[i] <-
            if(age[i]>=63) {
                r_dat$ss*(.7)  
            } else{
                0
            }
        tsp_bal[i] <- r_dat$tsp_bal
        roth_bal[i] <- as.numeric(rothAvail)
        tax_bal[i] <- r_dat$tax_bal
        tsp_bal[i] <- tsp_bal[i]*(1+ret) - tsp_bal[i]*ladder
        roth_bal[i] <- roth_bal[i] - roth_bal[i]*rothRate
        tax_bal[i] <- tax_bal[i]*(1+ret) - tax_bal[i]*taxAcctRate
        gross[i] <- tsp_bal[i]*ladder + 
            roth_bal[i]*rothRate + 
            tax_bal[i]*taxAcctRate
        net[i] <- net_r(gross[i],fTax,sTax,lTax,has_loc)
        # taxable[i] <- 0
    }else if(i<(63-r_dat$age)){
        age[i] <- age[i-1]+1 
        if (i<=58-r_dat$age) {
            pens[i] <- pens[i-1]
            tsp_bal[i] <- tsp_bal[i-1]*(1+ret)-tsp_bal[i-1]*ladder 
            roth_bal[i] <- roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate
            tax_bal[i] <- tax_bal[i-1]*(1+ret)-tax_bal[i-1]*taxAcctRate
            ss[i] <- 0            
            gross[i] <- tsp_bal[i]*ladder + roth_bal[i]*rothRate + tax_bal[i]*taxAcctRate
            net[i] <- net_r(gross[i],fTax,sTax,lTax,has_loc)
            # taxable[i] <- 0
            if (i==58-r_dat$age) pens[i] <- pension(gross_1,gross_2,gross_3,age_leave,r_dat$yos,pens_age,eo,inflation)
            # exp[i] <- tsp_bal[i-1]*.04
        } else if (i<=63-r_dat$age & i>58-r_dat$age) {
            pens[i] <- pens[i-1]
            tsp_bal[i] <- tsp_bal[i-1]*(1+ret)-tsp_bal[i-1]*ladder
            roth_bal[i] <- roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate   
            tax_bal[i] <- tax_bal[i-1]*(1+ret)-tax_bal[i-1]*taxAcctRate
            ss[i] <- 0            
            gross[i] <- tsp_bal[i]*ladder + pens[i] + roth_bal[i]*rothRate + tax_bal[i]*taxAcctRate
            net[i] <- net_r(gross[i],fTax,sTax,lTax,has_loc)
            # taxable[i] <- 0
        }    
    }else if(i>70-r_dat$age){
        age[i] <- age[i-1]+1
        pens[i] <- pens[i-1]
        tsp_bal[i] <- tsp_bal[i-1]*(1+ret) - 
                              tsp_bal[i-1]/rmd[i-rmd_y]        
        roth_bal[i] <- roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate
        tax_bal[i] <- tax_bal[i-1]*(1+ret)-tax_bal[i-1]*0
        ss[i] <- r_dat$ss*(.7)
        gross[i] <- pens[i] +
                            r_dat$ss*(.7) +
                            tsp_bal[i]/rmd[i-rmd_y] +
                            roth_bal[i]*rothRate +
                            tax_bal[i]*0
        net[i] <- net_r(gross[i],fTax,sTax,lTax,has_loc)
        # taxable[i] <- taxable[i-1]*(1+ret)+ net[i] - exp
    }else{
        age[i] <- age[i-1]+1
        pens[i] <- pens[i-1]
        tsp_bal[i] <- tsp_bal[i-1]*(1+ret) - tsp_bal[i-1]*tspRate         
        roth_bal[i] <- roth_bal[i-1]*(1+ret)-roth_bal[i-1]*rothRate
        tax_bal[i] <- tax_bal[i-1]*(1+ret)-tax_bal[i-1]*0
        ss[i] <- r_dat$ss*(.7)
        gross[i] <- pens[i] + 
                            r_dat$ss*(.7) +
                            tsp_bal[i]*tspRate +
                            roth_bal[i]*rothRate +
                            tax_bal[i]*0
        net[i] <- net_r(gross[i],fTax,sTax,lTax,has_loc)
        # taxable[i] <- 0
    }
}

# rothBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(roth_bal)
tradBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(trad_bal)
# hsaBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(hsa_bal)
# taxBal <- dat %>% filter(age>=retireAge, age<=die) %>% select(tax_bal)
mort <- dat %>% filter(age>=retireAge, age<=die) %>% select(mortgage)

dat_r <- data.frame(age=age,
                    gross=gross,
                    net=net,
                    pens=pens,
                    ss=ss,
                    tsp_bal=tsp_bal,
                    roth_bal=roth_bal,
                    # hsa_bal=hsaBal$hsa_bal
                    tax_bal=tax_bal
                    # mort=mort$mortgage
                    ) 

# Add margin column
dat_r <- tbl_df(dat_r) %>% mutate(avail=ss+tsp_bal+roth_bal+tax_bal)

write.table(dat_r, "retirement.out")
# pdf("retirement.pdf",height=(die+1-r_dat$age)*15/44,width=11)
# grid.table(round(dat_r,2))
# dev.off()

plotDat <- tbl_df(melt(dat_r, id.vars=c("age")))

# ggplot(plotDat %>% filter(variable == "net"), aes(age, value)) +
#     geom_line() +
#     facet_grid(variable~., scales="free") +
#     labs(title=paste("Retire at", retireAge, "Case Study"))
# 
# ggsave("net.png")
# plotDat
}