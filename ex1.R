#Example 
library(dplyr)
library(gridExtra)
source('TaxFunctions.R')

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
status <- "single"

#Calls functions to create appropriate tax brackets
fTax <- fed_br(status)
sTax <- st_br(state,status)
lTax <- loc_br(state,locality)
has_loc <- hasLocality(state)
#state_tax(dat[1,4],sTax,lTax,has_loc)

# tax <- dat %>% mutate(net(gross,fTax,sTax,lTax,has_loc))
# tax <- sapply(dat[,4],net)
# tax <- sapply(dat[,4],function(x,a,b,c,d) net(x,a,b,c,d), a=fTax,b=sTax,c=lTax,d=has_loc)
df <- read.csv("pay.csv")
df2 <- df
df2[,4] <- as.numeric(df[,4])
dat <- tbl_df(read.csv("pay.csv",
                       stringsAsFactors=FALSE))

dat <- dat %>% mutate(tsp = pmin(gross * tsp_trad,tsp_lim))
dummy <- as.data.frame(dat[,4])
dat <- bind_cols(dat,dummy,dummy,dummy,dummy,dummy,dummy,dummy)
names(dat)[6:12] <- c("net","tsp_bal","pens_eo","pens_57","pens_60","pens_62","ss")
for(i in 1:dim(dat)[1]){
    #Post-tax and post-TSP contribution income
    dat[i,6] <- net(dat$gross[i],fTax,sTax,lTax,has_loc,dat$tsp[i])
    #TSP Balance assumed inflation adjusted rate of return equal to ret
    if(i>1){
        dat$tsp_bal[i] <- dat$tsp[i-1] + dat$tsp_bal[i-1]*(1+ret)+
            dat$gross[i-1]*.05
    }else{
        dat$tsp_bal[i] <- 0 
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

pdf("working.pdf",height=22,width=17)
grid.table(round(dat,2))
dev.off()

##### Post-Retirement Calculation #####
state <- "Florida"
locality <- "Orlando"
status <- "single"
#inflation adjusted returns
ret <- .05
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

r_dat <- dat %>% filter(age==57) %>% select(-pens_eo,-pens_60,-pens_62)
exp <- r_dat$net
#years to RMDs
rmd_y <- 70-r_dat$age
for(i in 1:(101-r_dat$age)){
    if(i==1){
        age <- r_dat$age
        pens <- r_dat$pens_57
        ss <- 0
        tsp_bal <- r_dat$tsp_bal
        gross <- exp+10000
        tsp_bal <- tsp_bal*(1+ret) - exp-10000+pens[i]
        net <- net_r(gross,fTax,sTax,lTax,has_loc)
        taxable <- 0
    }else if(i<(63-r_dat$age)){
        age <- append(age,age[i-1]+1)
        pens <- append(pens,pens[i-1]/(1+inflation))
        ss <- append(ss, 0)
        gross <- append(gross, exp+10000)
        tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - 
                              exp-10000 + pens[i])
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
        gross <- append(gross, exp+10000)
        tsp_bal <- append(tsp_bal, tsp_bal[i-1]*(1+ret) - exp-10000 +
                              pens[i] + r_dat$ss*(.7+.325))
        net <- append(net, net_r(gross[i],fTax,sTax,lTax,has_loc))
        taxable <- append(taxable, 0)
    }
}
dat_r <- data.frame(age,gross,net,pens,ss,tsp_bal)
pdf("retired.pdf",height=15,width=8.5)
grid.table(round(dat_r,2))
dev.off()

