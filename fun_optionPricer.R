optionPricer = function(Sym, RF, PriceRange)
{
        #options
        library("quantmod")
        library("fOptions")
        library("fBasics")
        library("bizdays")
        library("timeDate")
        
        #create the trading calendar
        cal=create.calendar(("US/NSYE"), holidayNYSE(2018:2019), weekdays = c("saturday", "sunday"))
        #total trading days in this year
        Total_tradingDay = bizdays(as.Date("2018-01-01"), as.Date("2018-12-31"), cal)
        
        #Change this to grab the right symbol
        SymbolName = Sym
        # 3 month risk free rate
        RF30 = RF
        # Dividend Rate
        q = 0
        # Parameter for range of option price
        op_range = PriceRange
        #path
        
        path_call = paste ("/opt/shiny-server/samples/sample-apps/Dashboard/csvFiles/calls/", SymbolName, ".csv", sep = "")
        path_put = paste ("/opt/shiny-server/samples/sample-apps/Dashboard/csvFiles/puts/", SymbolName, ".csv", sep ="")
        
        #BlackScholes Function to calculate the greeks
        #S: underlying stock price
        #X: strike
        #HV: Sigma(Annual)
        #R:  continuously compounded risk-free interest rate annually
        #Q: continuously compounded dividend yeild annually 
        #T: Time to maturity % of a year 
        BS_greeks_calls= function(s, x, hv, r, q, t, ttd) {
                d1 = (log(s/x) + t*(r-q+(hv^2)/2))/(hv*t^.5)
                d2 = d1 - hv*(t^.5)
                premium = s*exp(-q*t)*pnorm(d1) -x*exp(-r*t)*pnorm(d2)
                delta = exp(-q*t)*pnorm(d1)
                gamma = exp(-q*t)/(s*hv*t^.5)*(1/((2*pi)^.5))*exp((-d1^2)/2)
                theta = 1/ttd*((-s*hv*exp(-q*t)*exp((d1^2)/-2)/(2*t^.5*(2*pi)^.5))-r*x*exp(-r*t)*pnorm(d2)+q*s*exp(-q*t)*pnorm(d1))
                vega = 1/100*s*exp(-q*t)*t^.5*1/(2*pi)^.5*exp(d1^2/-2)
                rho = 1/100*x*t*exp(-r*t)*pnorm(d2)  
                o=list("premium" = premium, "delta" =delta, "gamma" = gamma, "theta"= theta, "vega" =vega, "rho" = rho)
                return (o)  
        }
        
        BS_greeks_puts= function(s, x, hv, r, q, t, ttd) {
                d1 = (log(s/x) + t*(r-q+(hv^2)/2))/(hv*t^.5)
                d2 = d1 - hv*(t^.5)
                premium = x*exp(-r*t)*pnorm(-d2)-s*exp(-q*t)*pnorm(-d1) 
                delta = exp(-q*t)*(pnorm(d1)-1)
                gamma = exp(-q*t)/(s*hv*t^.5)*(1/((2*pi)^.5))*exp((-d1^2)/2)
                theta = 1/ttd*((-s*hv*exp(-q*t)*exp((d1^2)/-2)/(2*t^.5*(2*pi)^.5))+r*x*exp(-r*t)*pnorm(-d2)-q*s*exp(-q*t)*pnorm(-d1))
                vega = 1/100*s*exp(-q*t)*t^.5*1/(2*pi)^.5*exp(d1^2/-2)
                rho = -1/100*x*t*exp(-r*t)*pnorm(-d2)
                o=list("premium" = premium, "delta" =delta, "gamma" = gamma, "theta"= theta, "vega" =vega, "rho" = rho)
                return (o)
        }
        
        #pulling the otion chain and different time parameter for the stock return 
        Opt = getOptionChain(SymbolName, "2018/2019")
        Stock45day = getSymbols(SymbolName, from=Sys.Date()-45, to=Sys.Date(), auto.assign=FALSE)
        Stock1year= getSymbols(SymbolName, from=Sys.Date()-365, to=Sys.Date(), auto.assign = FALSE)
        
        #flatten call options dataframe 
        calls= list()
        puts=list()
        
        for (i in 1:length(Opt)) {
                calls=rbind(Opt[[i]]$calls, calls)
                puts = rbind(Opt[[i]]$puts, puts)
        }
        
        #Expiration Date  
        calls[8]=as.Date(substring(rownames(calls),nchar(SymbolName)+1,nchar(SymbolName)+6), "%y%m%d")
        names(calls)[8] = "ExpirationDate"
        puts[8]=as.Date(substring(rownames(puts),nchar(SymbolName)+1,nchar(SymbolName)+6), "%y%m%d")
        names(puts)[8] = "ExpirationDate"
        
        #Remaining Option Trading Date  
        calls[9]=bizdays(Sys.Date(), calls$ExpirationDate)
        names(calls)[9] = "O_RemainingDays" 
        puts[9]=bizdays(Sys.Date(), puts$ExpirationDate)
        names(puts)[9] = "O_RemainingDays" 
        
        
        #OptionStrike+LastTraded
        calls[10] = calls$Strike +calls$Last
        names(calls)[10] = "BreakEvenStockPrice"
        puts[10] = puts$Strike - puts$Last
        names(puts)[10] = "BreakEvenStockPrice"
        
        #Average of traded price of the underlying
        calls[11]=mean(Stock45day[,6])
        puts[11]=mean(Stock45day[,6])
        names(calls)[11] = "U_last30days_average"
        names(puts)[11] = "U_last30days_average"
        
        #LastTraded stock value
        U_LastTraded=as.numeric(last(Stock45day[,6]))
        calls[12]=U_LastTraded
        puts[12] =U_LastTraded
        names(calls)[12] = "U_LastTraded"
        names(puts)[12] = "U_LastTraded"
        
        calls[13]=(calls$BreakEvenStockPrice/calls$U_LastTraded)^(1/(calls$O_RemainingDays+1))
        puts[13]=(puts$BreakEvenStockPrice/puts$U_LastTraded)^(1/(puts$O_RemainingDays+1))
        names(calls)[13] = "ImpliedStockGrowth_daily"
        names(puts)[13] = "ImpliedStockGrowth_daily"
        
        #Calculating HV for the stock 
        Stock45day=cbind(Stock45day,diff(log(Stock45day[,6])))
        names(Stock45day)[7]="dailyReturn"
        Stock1year=cbind(Stock1year,diff(log(Stock1year[,6])))
        names(Stock1year)[7]="dailyReturn"
        
        AnnualVol_45=stdev(Stock45day$dailyReturn, na.rm = TRUE)*(252^.5)
        AnnualVol_1year=stdev(Stock1year$dailyReturn, na.rm = TRUE)*(252^.5)
        MeanDailyReturn_45=mean(Stock45day$dailyReturn, na.rm = TRUE)
        MeanDailyReturn_1year=mean(Stock1year$dailyReturn, na.rm=TRUE)
        
        calls[14] = MeanDailyReturn_45 + 1
        calls[15] = MeanDailyReturn_1year + 1
        calls[16] = AnnualVol_45
        calls[17] = AnnualVol_1year
        names(calls)[14] = "U_MeanDailyReturn_45"
        names(calls)[15] = "U_MeanDailyReturn_1year"
        names(calls)[16] = "U_Vol45days"
        names(calls)[17] = "U_Vol1year"
        
        puts[14] = MeanDailyReturn_45 + 1
        puts[15] = MeanDailyReturn_1year + 1
        puts[16] = AnnualVol_45
        puts[17] = AnnualVol_1year
        names(puts)[14] = "U_MeanDailyReturn_45"
        names(puts)[15] = "U_MeanDailyReturn_1year"
        names(puts)[16] = "U_Vol45days"
        names(puts)[17] = "U_Vol1year"
        
        
          for (i in 1:nrow(calls)) {
                #Calculating IV for the stocks   
                calls[i,18]=tryCatch(GBSVolatility(calls$Last[i], "c", S=U_LastTraded, X=calls$Strike[i], Time=(calls$O_RemainingDays[i])/Total_tradingDay, r=RF30, b=RF30,maxiter=500), error=function(e) {return(0)})
                #Calculating greeks 
        
                o = BS_greeks_calls(calls$U_LastTraded[i], calls$Strike[i], calls$U_Vol45days[i], RF30, q, calls$O_RemainingDays[i] / Total_tradingDay, Total_tradingDay)
                calls[i, 19] = o$premium
                calls[i, 20] = o$delta
                calls[i, 21] = o$gamma
                calls[i, 22] = o$theta
                calls[i, 23] = o$vega
                calls[i, 24] = o$rho
                calls[i, 25] = as.Date(Sys.Date(), "%y%m%d")
                calls[i, 26] = "c"
                calls[i, 27] = 1-pnorm(log(calls$BreakEvenStockPrice[i]/calls$U_LastTraded[i]),(calls$U_MeanDailyReturn_1year[i]-1)*calls$O_RemainingDays[i], (calls$U_Vol1year[i]/(252^.5))*(calls$O_RemainingDays[i]^.5))
                calls[i, 28] = 1-pnorm(log(calls$Strike[i]/calls$U_LastTraded[i]),(calls$U_MeanDailyReturn_1year[i]-1)*calls$O_RemainingDays[i], (calls$U_Vol1year[i]/(252^.5))*(calls$O_RemainingDays[i]^.5))
        }
        
        names(calls)[18] = "ImpliedVol"
        names(calls)[19] = "premium"
        names(calls)[20] = "delta"
        names(calls)[21] = "gamma"
        names(calls)[22] = "theta"
        names(calls)[23] = "Vega"
        names(calls)[24] = "rho"
        names(calls)[25] = "QuoteDate"
        names(calls)[26] = "OptionType"
        names(calls)[27]="BreakEvenProbability"
        names(calls)[28]="InTheMoneyProbability"
        
        ######
        for (i in 1:nrow(puts)) {
                #Calculating IV for the stocks   
                puts[i,18]=tryCatch(GBSVolatility(puts$Last[i], "p", S=U_LastTraded, X=puts$Strike[i], Time=(puts$O_RemainingDays[i])/Total_tradingDay, r=RF30, b=RF30,maxiter=500), error=function(e) {return(0)})
                #Calculating greeks 
                o =BS_greeks_puts(puts$U_LastTraded[i], puts$Strike[i], puts$U_Vol45days[i], RF30, q, puts$O_RemainingDays[i] / Total_tradingDay, Total_tradingDay)
                puts[i, 19] = o$premium
                puts[i, 20] = o$delta
                puts[i, 21] = o$gamma
                puts[i, 22] = o$theta
                puts[i, 23] = o$vega
                puts[i, 24] = o$rho
                puts[i, 25] = as.character(Sys.Date())
                puts[i, 26] = "p"
                puts[i, 27] = pnorm(log(puts$BreakEvenStockPrice[i]/puts$U_LastTraded[i]),(puts$U_MeanDailyReturn_1year[i]-1)*puts$O_RemainingDays[i], (puts$U_Vol1year[i]/(252^.5))*(puts$O_RemainingDays[i]^.5))
                puts[i, 28] = pnorm(log(puts$Strike[i]/puts$U_LastTraded[i]),(puts$U_MeanDailyReturn_1year[i]-1)*puts$O_RemainingDays[i], (puts$U_Vol1year[i]/(252^.5))*(puts$O_RemainingDays[i]^.5))
        }
        names(puts)[18] = "ImpliedVol"
        names(puts)[19] = "premium"
        names(puts)[20] = "delta"
        names(puts)[21] = "gamma"
        names(puts)[22] = "theta"
        names(puts)[23] = "Vega"
        names(puts)[24] = "rho"
        names(puts)[25] = "QuoteDate"
        names(puts)[26] = "OptionType"
        names(puts)[27]="BreakEvenProbability"
        names(puts)[28]="InTheMoneyProbability"
        
        calls_subset = subset(calls, (calls$Strike < calls$U_last30days_average *(op_range + 1)) & 
                                      (calls$Strike > calls$U_last30days_average * (1 - op_range)))
        puts_subset = subset(puts, (puts$Strike < puts$U_last30days_average *(op_range + 1)) &
                                     (puts$Strike > puts$U_last30days_average * (1 - op_range)))
        write.csv(calls_subset, path_call)
        write.csv(puts_subset, path_put)
        
}

        