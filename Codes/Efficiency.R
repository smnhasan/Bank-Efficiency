###################Efficiency measure with Translog and DEA analysis################
#                            Mohammad Nayeem Hasan                                 #
####################################################################################


#library
library(frontier)
library(plm)
library(Benchmarking)
library(lmtest)
library(AER)

#Profit_Public
sfa_dat_P_Pub <- read.table("E:\\ResearchProject\\Shakera Apu Mphill\\Cobb&Translog\\Profit_Public.csv",sep=',',header=T)
str(sfa_dat_P_Pub)


#Managing Data 
sfa_dat_P_Pub$Profitlog_P_Pub <- log(sfa_dat_P_Pub$Profit)
sfa_dat_P_Pub$Loanlog_P_Pub <- log(sfa_dat_P_Pub$LOAN)
sfa_dat_P_Pub$OffBalanceSheetItemlog_P_Pub <- log(sfa_dat_P_Pub$OffBalanceSheetItem)
sfa_dat_P_Pub$OtherEarningAssetslog_P_Pub <- log(sfa_dat_P_Pub$OtherEarningAssets)
sfa_dat_P_Pub$POFlog_P_Pub <- log(sfa_dat_P_Pub$POF)
sfa_dat_P_Pub$POFAlog_P_Pub <- log(sfa_dat_P_Pub$POFA)
sfa_dat_P_Pub$POLlog_P_Pub <- log(sfa_dat_P_Pub$POL)
sfa_dat_P_Pub$NonInterestIncomelog_P_Pub <- log(sfa_dat_P_Pub$NonInterestIncome)
sfa_dat_P_Pub$NonPerformingLoanlog_P_Pub <- log(sfa_dat_P_Pub$NonPerformingLoan)
sfa_dat_P_Pub$ReturnOnAssetslog_P_Pub <- log(sfa_dat_P_Pub$ReturnOnAssets)
sfa_dat_P_Pub$ReturnOnEquitylog_P_Pub <- log(sfa_dat_P_Pub$ReturnOnEquity)
sfa_dat_P_Pub$CapitalAdequacyRatiolog_P_Pub <- log(sfa_dat_P_Pub$CapitalAdequacyRatio)
sfa_dat_P_Pub$OperatingCostlog_P_Pub <- log(sfa_dat_P_Pub$OperatingCost)

# Cobb-Douglas production frontier
sfa_Panel_P_Pub <- pdata.frame(sfa_dat_P_Pub, c( "ï..bank", "time" ))
cobbDouglas_P_Pub <- frontier::sfa(Profitlog_P_Pub ~ Loanlog_P_Pub 
                                   + OffBalanceSheetItemlog_P_Pub 
                                   +  POFlog_P_Pub 
                                   + POFAlog_P_Pub 
                                   + POLlog_P_Pub
                                   ,
                                   data = sfa_Panel_P_Pub,
                                   timeEffect = T)

summary(cobbDouglas_P_Pub)
lrtest(cobbDouglas_P_Pub)
mod <- summary( cobbDouglas_P_Pub, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)

mod

# estimate the translog function (prob)
translog_P_Pub <- frontier::frontierQuad( yName = "Profitlog_P_Pub",
            xNames = c( "Loanlog_P_Pub", 
                       "OffBalanceSheetItemlog_P_Pub",
                       "POFlog_P_Pub", 
                       "POFAlog_P_Pub", 
                       "POLlog_P_Pub"),
                       shifterNames = c("NonInterestIncomelog_P_Pub",
                                        "NonPerformingLoanlog_P_Pub",
                                        "ReturnOnAssetslog_P_Pub",
                                        "ReturnOnEquitylog_P_Pub",
                                        "CapitalAdequacyRatiolog_P_Pub"),
            data = sfa_Panel_P_Pub)

summary(translog_P_Pub)
lrtest(translog_P_Pub)
summary( translog_P_Pub, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)

df <- length(coef(cobbDouglas_P_Pub)) - length(coef(translog_P_Pub)) 
df

teststat<--2*(- 34.03578 -(-7.940137))
teststat

pchisq(teststat,df=15)


#Profit_Private
sfa_dat_P_Pri <- read.table("F:\\ResearchProject\\Shakera Apu\\Cobb&Translog\\Profit_Private.csv",sep=',',header=T)
str(sfa_dat_P_Pri)
attach(sfa_dat_P_Pri)
sfa_dat_P_Pri$Profit <- as.numeric(Profit)
sfa_dat_P_Pri$LOAN <- as.numeric(LOAN)
sfa_dat_P_Pri$NonInterestIncome<- as.numeric(NonInterestIncome)
sfa_dat_P_Pri$NonPerformingLoan<- as.numeric(NonPerformingLoan)
#Managing Data 
sfa_dat_P_Pri$Profitlog_P_Pri <- log(sfa_dat_P_Pri$Profit)
sfa_dat_P_Pri$Loanlog_P_Pri <- log(sfa_dat_P_Pri$LOAN)
sfa_dat_P_Pri$OffBalanceSheetItemlog_P_Pri <- log(OffBalanceSheetItem)
sfa_dat_P_Pri$OtherEarningAssetslog_P_Pri <- log(OtherEarningAssets)
sfa_dat_P_Pri$POFlog_P_Pri <- log(POF)
sfa_dat_P_Pri$POFAlog_P_Pri <- log(POFA)
sfa_dat_P_Pri$POLlog_P_Pri <- log(POL)
sfa_dat_P_Pri$NonInterestIncomelog_P_Pri <- log(sfa_dat_P_Pri$NonInterestIncome)
sfa_dat_P_Pri$NonPerformingLoanlog_P_Pri <- log(sfa_dat_P_Pri$NonPerformingLoan)
sfa_dat_P_Pri$ReturnOnAssetslog_P_Pri <- log(ReturnOnAssets)
sfa_dat_P_Pri$ReturnOnEquitylog_P_Pri <- log(ReturnOnEquity)
sfa_dat_P_Pri$CapitalAdequacyRatiolog_P_Pri <- log(CapitalAdequacyRatio)


# Cobb-Douglas production frontier
str(sfa_dat_P_Pri)
sfa_Panel_P_Pri <- pdata.frame(sfa_dat_P_Pri)
cobbDouglas_P_Pri <- frontier::sfa(Profitlog_P_Pri ~ Loanlog_P_Pri 
                                   + OffBalanceSheetItemlog_P_Pri 
                                   + POFlog_P_Pri 
                                   + POFAlog_P_Pri 
                                   + POLlog_P_Pri
                                   | NonInterestIncomelog_P_Pri 
                                   + NonPerformingLoanlog_P_Pri
                                   + ReturnOnAssetslog_P_Pri
                                   + ReturnOnEquitylog_P_Pri
                                   + CapitalAdequacyRatiolog_P_Pri -1,
                                   timeEffect = T, 
                                   data= sfa_Panel_P_Pri )

summary(cobbDouglas_P_Pri)
lrtest(cobbDouglas_P_Pri)
summary( cobbDouglas_P_Pri, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)


# estimate the translog function

translog_P_Pri <- frontier::frontierQuad( yName = "Profitlog_P_Pri",
                                xNames = c( "Loanlog_P_Pri", 
                                            "OffBalanceSheetItemlog_P_Pri",
                                            "POFlog_P_Pri", 
                                            "POFAlog_P_Pri", 
                                            "POLlog_P_Pri"),
                                shifterNames = c("NonInterestIncomelog_P_Pri",
                                                 "NonPerformingLoanlog_P_Pri",
                                                 "ReturnOnAssetslog_P_Pri",
                                                 "ReturnOnEquitylog_P_Pri",
                                                 "CapitalAdequacyRatiolog_P_Pri"),
                                data = sfa_Panel_P_Pri)


translog_P_Pri
summary(translog_P_Pri)
lrtest(translog_P_Pri)
summary( translog_P_Pri, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)


df <- length(coef(cobbDouglas_P_Pri)) - length(coef(translog_P_Pri)) 
df

teststat<--2*(-165.3241 - (-165.9347)  )
teststat

pchisq(teststat,df=15)

#Cost_Public
sfa_dat_C_Pub <- read.table("F:\\ResearchProject\\Shakera Apu\\Cobb&Translog\\Cost_Public.csv",sep=',',header=T)
str(sfa_dat_C_Pub)

#Managing Data 
sfa_dat_C_Pub$OperatingCostlog_C_Pub <- log(sfa_dat_C_Pub$OperatingCost)
sfa_dat_C_Pub$Loanlog_C_Pub <- log(sfa_dat_C_Pub$LOAN)
sfa_dat_C_Pub$OffBalanceSheetItemlog_C_Pub <- log(sfa_dat_C_Pub$OffBalanceSheetItem)
sfa_dat_C_Pub$OtherEarningAssetslog_C_Pub <- log(sfa_dat_C_Pub$OtherEarningAssets)
sfa_dat_C_Pub$POFlog_C_Pub <- log(sfa_dat_C_Pub$POF)
sfa_dat_C_Pub$POFAlog_C_Pub <- log(sfa_dat_C_Pub$POFA)
sfa_dat_C_Pub$POLlog_C_Pub <- log(sfa_dat_C_Pub$POL)
sfa_dat_C_Pub$NonInterestIncomelog_C_Pub <- log(sfa_dat_C_Pub$NonInterestIncome)
sfa_dat_C_Pub$NonPerformingLoanlog_C_Pub <- log(sfa_dat_P_Pub$NonPerformingLoan)
sfa_dat_C_Pub$ReturnOnAssetslog_C_Pub <- log(sfa_dat_C_Pub$ReturnOnAssets)
sfa_dat_C_Pub$ReturnOnEquitylog_C_Pub <- log(sfa_dat_C_Pub$ReturnOnEquity)
sfa_dat_C_Pub$CapitalAdequacyRatiolog_C_Pub <- log(sfa_dat_C_Pub$CapitalAdequacyRatio)


# Cobb-Douglas production frontier
sfa_Panel_C_Pub <- pdata.frame(sfa_dat_C_Pub)
cobbDouglas_C_Pub <- frontier::sfa(OperatingCostlog_C_Pub ~ Loanlog_C_Pub 
                                   + OffBalanceSheetItemlog_C_Pub
                                   + POFlog_C_Pub 
                                   + POFAlog_C_Pub 
                                   + POLlog_C_Pub
                                   | NonInterestIncomelog_C_Pub 
                                   + NonPerformingLoanlog_C_Pub
                                   + ReturnOnAssetslog_C_Pub
                                   + ReturnOnEquitylog_C_Pub
                                   + CapitalAdequacyRatiolog_C_Pub -1,
                                   timeEffect = T,
                                   data =sfa_Panel_C_Pub)

summary(cobbDouglas_C_Pub)
lrtest(cobbDouglas_C_Pub)
summary( cobbDouglas_C_Pub, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)

# estimate the translog function
translog_C_Pub <- frontier::frontierQuad( yName = "OperatingCostlog_C_Pub",
                                xNames = c( "Loanlog_C_Pub",
                                            "OffBalanceSheetItemlog_C_Pub",
                                            "POFlog_C_Pub", 
                                            "POFAlog_C_Pub",
                                            "POLlog_C_Pub"),
                                shifterNames = c("NonInterestIncomelog_C_Pub",
                                                 "NonPerformingLoanlog_C_Pub",
                                                 "ReturnOnAssetslog_C_Pub",
                                                 "ReturnOnEquitylog_C_Pub",
                                                 "CapitalAdequacyRatiolog_C_Pub"),
                                data = sfa_Panel_C_Pub)

translog_C_Pub

lrtest(translog_C_Pub)
summary( translog_C_Pub, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)


df <- length(coef(cobbDouglas_C_Pub)) - length(coef(translog_C_Pub)) 
df

teststat<--2*((32.30196 ) -7.368741 )
teststat

pchisq(teststat,df=15)

#Cost_Private
sfa_dat_C_Pri  <- read.table("F:\\ResearchProject\\Shakera Apu\\Cobb&Translog\\Cost_Private.csv",sep=',',header=T)
str(sfa_dat_C_Pri)
attach(sfa_dat_C_Pri)
sfa_dat_C_Pri$OperatingCost <- as.numeric(OperatingCost)
sfa_dat_C_Pri$LOAN <- as.numeric(LOAN)
sfa_dat_C_Pri$NonInterestIncome<- as.numeric(NonInterestIncome)

#Managing Data 
sfa_dat_C_Pri$OperatingCostlog_C_Pri  <- log(sfa_dat_C_Pri$OperatingCost)
sfa_dat_C_Pri$Loanlog_C_Pri  <- log(sfa_dat_C_Pri$LOAN)
sfa_dat_C_Pri$OffBalanceSheetItemlog_C_Pri  <- log(OffBalanceSheetItem)
sfa_dat_C_Pri$OtherEarningAssetslog_C_Pri  <- log(OtherEarningAssets)
sfa_dat_C_Pri$POFlog_C_Pri  <- log(POF)
sfa_dat_C_Pri$POFAlog_C_Pri  <- log(POFA)
sfa_dat_C_Pri$POLlog_C_Pri  <- log(POL)
sfa_dat_C_Pri$NonInterestIncomelog_C_Pri  <- log(sfa_dat_C_Pri$NonInterestIncome)
sfa_dat_C_Pri$NonPerformingLoanlog_C_Pri <- log(sfa_dat_P_Pri$NonPerformingLoan)
sfa_dat_C_Pri$ReturnOnAssetslog_C_Pri  <- log(ReturnOnAssets)
sfa_dat_C_Pri$ReturnOnEquitylog_C_Pri  <- log(ReturnOnEquity)
sfa_dat_C_Pri$CapitalAdequacyRatiolog_C_Pri  <- log(CapitalAdequacyRatio)


# Cobb-Douglas production frontier
str(sfa_dat_C_Pri )
sfa_Panel_C_Pri  <- pdata.frame(sfa_dat_C_Pri)
cobbDouglas_C_Pri  <- frontier::sfa(OperatingCostlog_C_Pri  ~ Loanlog_C_Pri  
                                    + OffBalanceSheetItemlog_C_Pri  
                                    + POFlog_C_Pri
                                    + POFAlog_C_Pri
                                    + POLlog_C_Pri
                                    | NonInterestIncomelog_C_Pri 
                                    + NonPerformingLoanlog_C_Pri
                                    + ReturnOnAssetslog_C_Pri
                                    + ReturnOnEquitylog_C_Pri
                                    + CapitalAdequacyRatiolog_C_Pri -1,
                                    data = sfa_Panel_C_Pri ,timeEffect = T)

summary(cobbDouglas_C_Pri )
lrtest(cobbDouglas_C_Pri)
summary( cobbDouglas_C_Pri, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)



# estimate the translog function
translog_C_Pri <- frontier::frontierQuad( yName = "OperatingCostlog_C_Pri",
                                xNames = c( "Loanlog_C_Pri", 
                                            "OffBalanceSheetItemlog_C_Pri",
                                            "POFlog_C_Pri",
                                            "POFAlog_C_Pri",
                                            "POLlog_C_Pri"),
                                shifterNames = c("NonInterestIncomelog_C_Pri",
                                                 "NonPerformingLoanlog_C_Pri",
                                                 "ReturnOnAssetslog_C_Pri",
                                                 "ReturnOnEquitylog_C_Pri",
                                                 "CapitalAdequacyRatiolog_C_Pri"),
                                data = sfa_dat_C_Pri)

translog_C_Pri
lrtest(translog_C_Pri)
summary( translog_C_Pri, extraPar = FALSE, effic = T,
         logDepVar = TRUE,  farrell = TRUE)



df <- length(coef(cobbDouglas_C_Pri)) - length(coef(translog_C_Pri)) 
df

teststat<--2*(-33.64061 - -64.81876  )
teststat

pchisq(teststat,df=15)






#Data Envelopment Analysis

#Profit Public
dea_dat_P_Pub <- read.table("F:\\ResearchProject\\Shakera Apu\\DEA\\Profit_DEA_Public.csv",sep=',',header=T)
dea_dat_P_Pub <- pdata.frame(dea_dat_P_Pub, c( "ï..bank", "time" ))



# technical efficiency
x <- with(dea_dat_P_Pub, cbind(TOTAL.deposit))
y <- with(dea_dat_P_Pub, cbind(LoanNadvance))
w <- with(dea_dat_P_Pub, cbind(POD ))
p <- with(dea_dat_P_Pub, cbind(POLA))

xopt <- profit.opt(x,y,w,p,RTS="vrs", 
                   param=NULL,
                   TRANSPOSE = FALSE, 
                   LP = FALSE, 
                   LPK = NULL)
pobs <- p * y  -  w * x

popt <-  p * xopt$y  - w * xopt$x

# profit efficiency
pe <- pobs/popt
pe

dea.plot(x,y,RTS="vrs")

#Profit Private
dea_dat_P_Pri <- read.table("F:\\ResearchProject\\Shakera Apu\\DEA\\Profit_DEA_Private.csv",sep=',',header=T)
str(dea_dat_P_Pri)


# technical efficiency
x <- with(dea_dat_P_Pri, cbind(TOTAL.deposit))
y <- with(dea_dat_P_Pri, cbind(LoanNadvance))
w <- with(dea_dat_P_Pri, cbind(mean(POD)))
p <- with(dea_dat_P_Pri, cbind(mean(POLA)))
pxopt <- profit.opt(x,y,w,p,RTS="vrs", 
                   param=NULL,
                   TRANSPOSE = FALSE, 
                   LP = FALSE, 
                   LPK = NULL)
pobs <- y %*% t(p)  -  x %*% t(w)

popt <-  pxopt$y %*% t(p)  - pxopt$x %*% t(w)
# profit efficiency
z <- data.frame(pobs)
pe <- z/26539.74
pe
dea.plot(x,y,RTS="vrs")

#Cost_Public
dea_dat_C_Pub <- read.table("F:\\ResearchProject\\Shakera Apu\\DEA\\Cost_DEA_Public.csv",sep=',',header=T)
str(dea_dat_C_Pub)
attach(dea_dat_C_Pub)
# technical efficiency
x <- with(dea_dat_C_Pub, cbind(TOTAL.deposit,
                               Fixed.assets,
                               Labor))
y <- with(dea_dat_C_Pub, cbind(LoanNadvance,
                               Off.balance.sheet.item))
w <- with(dea_dat_C_Pub, cbind(mean(POD),mean(POF),mean(POLA) ))
te <- dea(x,y,RTS="vrs")
min(te$eff)
max(te$eff)
xopt <- cost.opt(x,y,w,RTS=1)
cobs <- x %*% t(w)
copt <- xopt$x %*% t(w)
# cost efficiency
ce <- copt/cobs
ce
min(ce)
max(ce)
# allocaltive efficiency
ae <- ce/te$eff
min(ae)
max(ae)
data.frame("ce"=ce,"te"=te$eff,"ae"=ae)
print(cbind("ce"=c(ce),"te"=te$eff,"ae"=c(ae)),digits=3)



dea(TOTAL.deposit,LoanNadvance, RTS="vrs", ORIENTATION="in")
e <- dea(TOTAL.deposit,LoanNadvance, RTS="vrs", ORIENTATION="in")
e
dea.plot(x,y,RTS="vrs")

#Cost_Private
dea_dat_C_Pri <- read.table("F:\\ResearchProject\\Shakera Apu\\DEA\\Cost_DEA_Private.csv",sep=',',header=T)
str(dea_dat_C_Pri)


# technical efficiency
x <- with(dea_dat_C_Pri, cbind(TOTAL.deposit,
                               Fixed.assets,
                               Labor))
y <- with(dea_dat_C_Pri, cbind(LoanNadvance,
                               Off.balance.sheet.item))
w <- with(dea_dat_C_Pri, cbind(mean(POD),mean(POF),mean(POLA) ))
te <- dea(x,y,RTS="vrs")
min(te$eff)
max(te$eff)
xopt <- cost.opt(x,y,w,RTS=1)
cobs <- x %*% t(w)
copt <- xopt$x %*% t(w)
# cost efficiency
ce <- copt/cobs
ce
min(ce)
max(ce)


# allocaltive efficiency
ae <- ce/te$eff
min(ae)
max(ae)
data.frame("ce"=ce,"te"=te$eff,"ae"=ae)
print(cbind("ce"=c(ce),"te"=te$eff,"ae"=c(ae)),digits=3)
dea.plot(x,y,RTS="vrs")


#IT_PRIVATE_SFA_COST_OLS
IT_PRIVATE_SFA_COST_OLS <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\OLS&Tobit\\IT_PRIVATE_SFA_COST_OLS.csv",sep=',',header=T)
str(IT_PRIVATE_SFA_COST_OLS)

tobitmodel <- tobit(cost.efficiency ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_PRIVATE_SFA_COST_OLS)

summary(tobitmodel)


#IT_public_SFA_cost_OLS 
IT_public_SFA_cost_OLS <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\OLS&Tobit\\IT_public_SFA_cost_OLS.csv",sep=',',header=T)
str(IT_public_SFA_cost_OLS)

tobitmodel <- tobit(cost.efficiency ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_public_SFA_cost_OLS)
summary(tobitmodel)


#IT_PRIVATE_SFA_Profit_OLS  
IT_PRIVATE_SFA_Profit_OLS  <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_PRIVATE_SFA_Profit_OLS.csv",sep=',',header=T)
str(IT_PRIVATE_SFA_Profit_OLS)

tobitmodel <- tobit(Profit.Efficiency ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_PRIVATE_SFA_Profit_OLS )
summary(tobitmodel)


#IT_PUBLIC_SFA _PROFIT_OLS  
IT_PUBLIC_SFA_PROFIT_OLS<- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_PUBLIC_SFA _PROFIT_OLS.csv",sep=',',header=T)
str(IT_PUBLIC_SFA_PROFIT_OLS)

tobitmodel <- tobit(Profit.Efficiency ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_PUBLIC_SFA_PROFIT_OLS )
summary(tobitmodel)


#IT_public_pforit_DEA  
IT_public_pforit_DEA<- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_public_pforit_DEA.csv",sep=',',header=T)
str(IT_public_pforit_DEA)

tobitmodel <- lm(ce ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_public_pforit_DEA)
summary(tobitmodel)


#IT_Private_profit_DEA  
IT_Private_profit_DEA <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_Private_profit_DEA.csv",sep=',',header=T)
str(IT_Private_profit_DEA)

tobitmodel <- lm(pe ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_Private_profit_DEA)
summary(tobitmodel)


#IT_Public_Cost_DEA 
IT_Public_Cost_DEA <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_Public_Cost_DEA.csv",sep=',',header=T)
str(IT_Public_Cost_DEA)

tobitmodel <- tobit(ce ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_Public_Cost_DEA)
summary(tobitmodel)


#IT_Private_Cost_DEA 
IT_Private_Cost_DEA <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_Private_Cost_DEA.csv",sep=',',header=T)
str(IT_Private_Cost_DEA)

tobitmodel <- tobit(ce ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_Private_Cost_DEA)
summary(tobitmodel)


#IT_public_profit_cobb 
IT_public_profit_cobb <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_public_profit_cobb.csv",sep=',',header=T)
str(IT_public_profit_cobb)

tobitmodel <- tobit(ce ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_public_profit_cobb)
summary(tobitmodel)


#IT_private_profit_cobb 
IT_private_profit_cobb <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_private_profit_cobb.csv",sep=',',header=T)
str(IT_private_profit_cobb)

tobitmodel <- tobit(pe ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_private_profit_cobb)
summary(tobitmodel)


#IT_public_cost_cobb
IT_public_cost_cobb <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_public_cost_cobb.csv",sep=',',header=T)
str(IT_public_cost_cobb)

tobitmodel <- tobit(ce ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_public_cost_cobb)
summary(tobitmodel)


#IT_private_cost_cobb
IT_private_cost_cobb <- read.table("F:\\ResearchProject\\Shakera Apu\\OLS&Tobit\\IT_private_cost_cobb.csv",sep=',',header=T)
str(IT_private_cost_cobb)

tobitmodel <- tobit(ce ~ IT.Expenses + IT.Expenses + IT.Income +IT.Investment 
             + IT.labour + Price.of.IT + ATM.Transaction + ATM.expeses + Credit.Card.Transaction
             + Credit.card.expenses, data = IT_private_cost_cobb)
summary(tobitmodel)

