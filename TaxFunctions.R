##########################################################
##############  After Tax Calculations  ##################
##########################################################
#net <- function(income,status="single",state="Maryland",locality="Montgomery County"){
#    return(fed_tax(income) - fica(income) -
#               state_tax(income,status,state,locality))
#}
# net <- function(income,status="single",state="Maryland",locality="Montgomery County"){
#     return(income - fed_tax(income) - fica(income))
# }
net <- function(income,fTax,sTax,lTax,loc,tsp_trad){
    return(as.numeric(income-tsp_trad) - 
               fed_tax(as.numeric(income-tsp_trad),fTax) - 
               state_tax(as.numeric(income-tsp_trad),sTax,lTax,loc) - 
               fica(as.numeric(income))           
           )
}
net_r <- function(income,fTax,sTax,lTax,loc){
    return(as.numeric(income) - 
               fed_tax(as.numeric(income),fTax) - 
               state_tax(as.numeric(income),sTax,lTax,loc))
}


##########################################################
#############  Federal Tax Calculations  #################
##########################################################

fed_br <- function(status){
    if(status=="single"){
        fTax <- matrix(c(0,9275,37650,91150,190150,413350,415050,
                              0,927.5,5183.75,18558.75,46278.75,119934.75,120529.75,
                              .10,.15,.25,.28,.33,.35,.396)
                            , nrow=7, ncol=3)
        }else{
            fTax <- matrix(c(0,18150,73800,148850,226850,405100,0,1815,10162.5,
                          28925,50765,109587.50,.10,.15,.25,.28,.33,.35)
                        , nrow=6, ncol=3)
        }
}

fed_tax <- function(income,fTax){
    agi <- income - (4050 + 6300) 
    i <- dim(fTax)[1]
    while(i>0){
        if(agi >= fTax[i,1]){
            return(fTax[i,2] + (agi - fTax[i,1]) * fTax[i,3])
        }
        i <- i - 1
    }
    return(0)
}
##########################################################
##############  State Tax Calculations  ##################
##########################################################

#get the relevant state tax bracket
st_br <- function(name, status) {
    stateTax <- tbl_df(read.csv("stateTax3.csv", stringsAsFactors=FALSE))
    stateTax$state <- gsub(" \\(.+\\)", "", stateTax$state, perl=TRUE)
    stateTax$state <- gsub("\\(.+\\)", "", stateTax$state, perl=TRUE)
    
    sTax <- stateTax %>% filter(state==name, type==status)
}

#get the relevant locality tax bracket
loc_br <- function(name, loc){
    localTax <- tbl_df(read.csv("localities.csv", stringsAsFactors=FALSE))
    localTax$state <- gsub(" \\(.+\\)", "", localTax$state, perl=TRUE)
    localTax$state <- gsub("\\(.+\\)", "", localTax$state, perl=TRUE)
    
    lTax <- localTax %>% filter(state==name, locality==loc)
}

state_tax <- function(income, sTax,lTax,has_loc){
    if(has_loc == TRUE){
        state_tax_1(as.numeric(income),sTax)+
            local_tax(as.numeric(income),sTax,lTax)
    } else {
        state_tax_1(as.numeric(income), sTax)
    }
}

#sub routine of state_tax
state_tax_1 <- function (income,sTax) {   
    if(length(sTax$bracket)==1){
        sTax$rate * income
    } else {
        sum(sTax$rate * pmin(sTax$bracket,pmax(income - 
                                                   lag(sTax$cumBracket),0),na.rm=T))
    }
    
}

# returns TRUE if the state has locality taxes and FALSE if the state
# does not
hasLocality <- function(state){
    localTax <- tbl_df(read.csv("localTax.csv", stringsAsFactors=FALSE))
    localTax$state <- gsub(" \\(.+\\)", "", localTax$state, perl=TRUE)
    localTax$state <- gsub("\\(.+\\)", "", localTax$state, perl=TRUE)
    
    localTax_1 <- localTax[localTax$state == state,]
    
    localTax_1$rate != 0
}

local_tax <- function (income, sTax, lTax) {
    return(income * lTax$rate + lTax$flatFee +
               state_tax_1(income,sTax) * lTax$stateTax) 
}

localityList <- function(name){
    
    library(dplyr)
    
    localTax <- tbl_df(read.csv("localities.csv", stringsAsFactors=FALSE))
    localTax$state <- gsub(" \\(.+\\)", "", localTax$state, perl=TRUE)
    localTax$state <- gsub("\\(.+\\)", "", localTax$state, perl=TRUE)
    
    lTax <- localTax %>% filter(state==name)
    lTax$locality
}
##########################################################
###############  FICA Tax Calculations  ##################
##########################################################
fica <- function(income,ss_max=118500){
    fica_tax <- income * .0145
    if(income>ss_max){
        return(fica_tax + ss_max * .062)
    }else{
        return(max(fica_tax + income * .062),0)
    }
}
##########################################################
###############  FEHB Contribution  ######################
##########################################################
# fehb <- function(income){
#     return(income - .008)
# }
##########################################################
###############  Pension Calculation  ####################
##########################################################
# Input gross_1=income last year of work, gross_2=income for the year before last year,
# gross_2=income for two years before last year, 
# age_leave=age at last year of work (should be equal to of less than pens_age),
# yos=years of service, pens_age=age_leave to begin collecting pension,
# eo=T/F whether or not early out is offered, inflation=inflation rate
pension <- function(gross_1,gross_2,gross_3,age_leave,yos,pens_age,eo,inflation){
    if(age_leave>=62 & yos>=20){
        # Immediate Pension at 62 or later, with 20 years get 1.1% multiplier
        return(yos * 0.011 * 
                   (gross_1 + gross_2/(1+inflation) + gross_3/(1+inflation)^2)/3)
    }else if(eo==TRUE & ((age_leave>=50 & yos>=20) | (yos>=25))){
        # Early Out Compuation
        return(yos * 0.01*
                   (gross_1 + gross_2/(1+inflation) + gross_3/(1+inflation)^2)/3)
    }else if(pens_age>=60 & yos>=20){
        # Take Pension at 60 with 20 yos, no reduction in Computation
        return(yos * 0.01 * 
                   (gross_1 + gross_2/(1+inflation) + gross_3/(1+inflation)^2)/3)
    }else if(pens_age>=57 & yos>=30){
        # Take Pension at MRA(57) with 30 yos, no reduction in Computation
        return(yos * 0.01 * 
                   (gross_1 + gross_2/(1+inflation) + gross_3/(1+inflation)^2)/3)
    }else if(pens_age>=62 & yos>=5){
        return(yos * 0.01 * 
                   (gross_1 + gross_2/(1+inflation) + gross_3/(1+inflation)^2)/3)        
    }else if(pens_age>=57 & yos>=10){
        return(yos * 0.01 * (1-(62-pens_age)*.05) *
               (gross_1 + gross_2/(1+inflation) + gross_3/(1+inflation)^2)/3)
    }else{
        return(NA)
    }
}
##########################################################
#############  Retirement Account Rules  #################
##########################################################
rmd <- c(27.4,26.5,25.6,24.7,23.8,22.9,22.0,21.2,20.3,19.5,18.7,17.9,
         17.1,16.3,15.5,14.8,14.1,13.4,12.7,12.0,11.4,10.8,10.2,9.6,
         9.1,8.6,8.1,7.6,7.1,6.7,6.3,5.9,5.5,5.2,4.9,4.5,
         4.2,3.9,3.7,3.4,3.1,2.9,2.6,2.4,2.1,1.9)
##########################################################
######### FAFSA Calculation (College Tax)  ###############
##########################################################
# based on http://ifap.ed.gov/efcformulaguide/attachments/100615EFCFormulaGuide1617Attach.pdf
# agi=Parent's Adjusted Gross Income (if negative enter 0)
# p1_ei=Parent 1 earned income from work
# p2_ei=Parent 2 earned income from work (if no second parent enter NA)
# ti=Parents taxable income
# ui=Total untaxed income and benefits (includes Roth withdrawals)
# afi=Total additional financial information (optional)
# fed_tax=Federal income tax for previous year
# assets=Total cash, savings & checkings, positive net worth of investments,
# bus_farm=Net worth of business/or investment farm
# hh=Number is Parent's Household including student
# kic=Kid's in college, Number of children in college(don't include parents)
# s_agi=Student's Adjusted Gross Income
# s_ei=student's earned income
# s_tax=Student's Federal income tax paid for previous year
# st=state of residence

########### Exployment Expense Allowance ##########
# 35% of lesser of earned incomes (or of single parent's income)
# or $4,000 which ever is less
eea_rate <- .35  
eea_amt <- 4000
########### Contributions from Assets #############
# Parental Asset Conversion rate
asset_conv_rate <- 0.12
### Contributions from Adjusted Available Income (AAI) ###########
# AAI minimum, below this value contribution from AAI is aai_low
aai_min <- 3409
aai_low <- -750
################## Table A1 #######################
tA1 <- tbl_df(read.csv("tableA1.csv", stringsAsFactors=FALSE))
tA1$below <- as.numeric(sub("%","",tA1$below))/100
tA1$above <- as.numeric(sub("%","",tA1$above))/100
tA1_cutoff <- 15000
################## Table A3 #######################
tA3 <- tbl_df(read.csv("tableA3.csv", stringsAsFactors=FALSE))
tA3_addhh <- 4270
tA3_addstd <- 3040
################## Table A4 #######################
tA4 <- tbl_df(read.csv("tableA4.csv", stringsAsFactors=FALSE))
tA4$rate <- as.numeric(sub("%","",tA4$rate))/100
################## Table A5 #######################
tA5 <- tbl_df(read.csv("tableA5.csv", stringsAsFactors=FALSE))
################## Table A6 #######################
tA6 <- tbl_df(read.csv("tableA6.csv", stringsAsFactors=FALSE))
tA6$rate <- as.numeric(sub("%","",tA6$rate))/100
tA4_low <- -3409
tA4_cont <- -750
################## Table A7 #######################
tA7 <- tbl_df(read.csv("tableA7.csv", stringsAsFactors=FALSE))
tA7$rate <- as.numeric(sub("%","",tA7$rate))/100

fafsa <- function(agi,p1_ei,p2_ei,ui,afi,fed_tax,assets,bus_farm,hh,kic,age,
                  s_agi,s_ei,s_tax,s_assets,st){
    # Total Income (Line 7) assuming that taxes are filed
    total_income <- max(agi,0) + ui - afi # Line 7
    # Total Allowances (Line 14) 
    total_allowances <- max(fed_tax,0) + 
        max(min(total_income, tA1_cutoff-1)*filter(tA1,state==st)$below +
        max(total_income-tA1_cutoff, 0)*filter(tA1,state==st)$above,0) +
        fica(p1_ei) + fica(p2_ei) +
        filter(tA3,hsize==hh)[,kic+1] +
        min(min(p1_ei,p2_ei)*eea_rate,eaa_amt)
    # Available Income (Line 15) Note: Can be negative
    avail_income <- total_income - total_allowances
    # Contribution from Assets (Line 24)
    cont_assets <- max((assets + 
        max(bus_farm*tA4$rate[2],0) +
        max((bus_farm-tA4$netWorth[2])*(tA4$rate[3]-tA4$rate[2]),0) +
        max((bus_farm-tA4$netWorth[3])*(tA4$rate[4]-tA4$rate[3]),0) +
        max((bus_farm-tA4$netWorth[4])*(tA4$rate[5]-tA4$rate[4]),0) -
        filter(tA5,olderAge==age)$twoPar)*asset_conv_rate,0)
    # Adjusted Available Income (Line 25) Note: Can be negative
    aai <- avail_income + cont_assets
    # Total Parent's Contribution from AAI (Line 26)
    aai_cont <- max(aai*tA6$rate[1],-750) +
        max((aai-tA6$AAI[1])*(tA6$rate[2]-tA6$rate[1]),0) +
        max((aai-tA6$AAI[2])*(tA6$rate[3]-tA6$rate[2]),0) +
        max((aai-tA6$AAI[3])*(tA6$rate[4]-tA6$rate[3]),0) +
        max((aai-tA6$AAI[4])*(tA6$rate[5]-tA6$rate[4]),0) +
        max((aai-tA6$AAI[5])*(tA6$rate[6]-tA6$rate[5]),0)
    # Parent's Contribution (Line 28)
    return(parent_cont <- max(aai_cont/kic,0))
}