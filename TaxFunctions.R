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
               fica(as.numeric(income)))
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
###############  FICA Tax Calculations  ##################
##########################################################
fica <- function(income,max=118500){
    fica_tax <- income * .0145
    if(income>max){
        return(fica_tax + max * .062)
    }else{
        return(fica_tax + income * .062)
    }
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