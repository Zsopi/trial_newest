na most megy?

library(readr)
library(dplyr)
library(dummies)
library(ggplot2)
library(lmtest)  # for Breush-Godfrey
library(stargazer)

#creating all the helper functions
calculate_se <- function(lm_model, se = 'robust', max_lag) {
        if (!se %in% c('traditional', 'robust', 'newey-west')) stop("se should be one of traditional, robust or newey-west (default is robust).")
        if (!require(sandwich)) stop("Required sandwich package is missing.")
        
        if (se == 'robust') {
                sqrt(diag(vcovHC(lm_model, type="HC1")))
        } else if (se == 'newey-west') {
                sqrt(diag(NeweyWest(lm_model, lag = max_lag, prewhite = FALSE)))
        } else {
                sqrt(diag(vcov(lm_model)))
        }
}

summary_r <- function(model, se = 'robust', max_lag = 0, ...) {
        
        sumry <- summary(model)
        table <- coef(sumry)
        table[, 2] <- calculate_se(model, se, max_lag)
        table[, 3] <- table[,1]/table[, 2]
        table[, 4] <- 2*pt(abs(table[, 3]), df.residual(model), lower.tail=FALSE)
        
        sumry$coefficients <- table
        p <- nrow(table)
        if (p > 1) {
                if (se == 'robust') {
                        hyp <- cbind(0, diag(p - 1))    
                        sumry$fstatistic[1] <- linearHypothesis(model, hyp, white.adjust="hc1")[2, "F"]
                } else if (se == 'newey-west') {
                        sumry$fstatistic[1] <- NA
                }
        }
        
        print(sumry)
        cat("Number of observations:", length(residuals(model)), "\n\n")
        
        if (se == 'robust') {
                cat("Note: Heteroscedasticity-consistent standard errors (adjustment HC1)\n")
        } else if (se == 'newey-west') {
                cat("Note: Newey-West standard errors - maximum lag:", max_lag, "\n")
        }
        
        
}

stargazer_r <- function(list_of_models, type="text", align=TRUE, no.space=TRUE,
                        omit.stat=c("LL", "aic", "ser", "f", "adj.rsq", "sigma2"), 
                        se = 'robust', max_lag = 0, ...) {
        if (!require(stargazer)) stop("Required stargazer package is missing.")
        
        if (class(type) != "character") stop("Different models should be given in a list.")
        if (class(list_of_models) == "lm") list_of_models <- list(list_of_models)
        if (!length(se) %in% c(1, length(list_of_models))) stop("For parameter se you should give one string (if you want to apply it to all models) or a list of strings (if you want to apply different types of standard error for the different models). The string could take traditional, robust, and newey-west (default is robust).")
        
        if (length(se) == 1) {
                note <- paste(capwords(se[[1]]), "standard errors in parentheses")
                se <- as.list(rep(se[[1]], length(list_of_models)))
        } else {
                note <- "Standard errors in parentheses"
        }
        
        if (length(max_lag) == 1) {
                max_lag <- as.list(rep(max_lag[[1]], length(list_of_models)))
                if (all(se == 'newey-west')) {
                        note <- paste(note, "- max lag:", max_lag[[1]])
                }
        }
        
        if (any(se == 'newey-west')) omit.stat <- c(omit.stat, 'rsq')
        
        list_se_robust <- lapply(
                seq_along(list_of_models), 
                function(j) {
                        if (class(list_of_models[[j]]) == 'lm') {
                                calculate_se(list_of_models[[j]], se = se[[j]], max_lag = max_lag[[j]])
                        } else {
                                NULL
                        }
                }
        )
        
        args <- list(...)
        if (!is.null(args[['out']])) type="html"
        
        stargazer(
                list_of_models,
                se = list_se_robust,
                report ="vcs*",
                notes = note,
                type = type, align = align, omit.stat = omit.stat, no.space = no.space,
                ...
        )
}


pperron <- function(x, model = c('constant', 'trend'), type = "Z-tau") {
        if (!require(urca)) stop("Required urca package is missing.")
        
        results <- ur.pp(x, type = type, model = model)
        print(results)
        
        model <- match.arg(model)
        if (model == 'trend') trend = 'ct' else trend = 'c' 
        cat(
                "MacKinnon approximate p-value for Z-tau:", 
                punitroot(results@teststat, trend = trend), 
                "\n\n"
        )
}

capwords <- function(s, strict = FALSE) {
        cap <- function(s) paste(toupper(substring(s, 1, 1)),
                                 {s <- substring(s, 2); if(strict) tolower(s) else s},
                                 sep = "", collapse = "-" )
        sapply(strsplit(s, split = "-"), cap, USE.NAMES = !is.null(names(s)))
}

lags <- function(variable, lags) {
        var_string <- deparse(substitute(variable))
        paste(
                lapply(
                        lags, 
                        function(i) {
                                paste0("lag(", var_string, ",", i, ")")
                        }
                ),
                collapse = "+"
        )
}

d <- function(x) {
        c(NA, diff(x))
}

Arima <- function(..., transform.pars = FALSE) {
        model <- arima(...)
        
        # rename to be consistent with lm
        names(model$coef) <- gsub('intercept', '(Intercept)', names(model$coef))
        row.names(model$var.coef) <- gsub('intercept', '(Intercept)', row.names(model$var.coef))
        colnames(model$var.coef) <- gsub('intercept', '(Intercept)', colnames(model$var.coef))
        
        model
}


#Downloading data for small cap Comtech Telecomm. Corp.
CMTL<-read.csv("http://chart.finance.yahoo.com/table.csv?s=CMTL&a=7&b=8&c=1974&d=1&e=7&f=2016&g=d&ignore=.csv")
QCOM<-read.csv("http://chart.finance.yahoo.com/table.csv?s=QCOM&a=7&b=8&c=1974&d=1&e=7&f=2016&g=d&ignore=.csv")

#also downloading the Nasdaq composite
NSDQ<-read.csv("http://chart.finance.yahoo.com/table.csv?s=^IXIC&a=7&b=8&c=1974&d=1&e=7&f=2016&g=d&ignore=.csv")


# First data manipulation ------------------------------------------------------

NSDQ <- NSDQ %>%
        mutate(
                date=as.Date(Date),
                close_nsdq=Adj.Close,
                volume_nsdq=Volume)
NSDQ$Date<-NULL
NSDQ$Adj.Close<-NULL
NSDQ$Volume<-NULL
NSDQ$Open<-NULL
NSDQ$High<-NULL
NSDQ$Low<-NULL
NSDQ$Close<-NULL

CMTL <- CMTL %>%
        mutate(
                date=as.Date(Date),
                close=Adj.Close,
                volume=Volume)
CMTL$Date<-NULL
CMTL$Adj.Close<-NULL
CMTL$Volume<-NULL

CMTL <- CMTL %>%
        mutate(
                year = as.numeric(format(date, '%Y')),
                month = format(date, '%b'),
                day_of_week = weekdays(date)
        ) %>%
        arrange(date) %>%
        mutate(gap = factor(as.numeric(date - lag(date)) - 1))

# generate dummies for gap, month and day_of_week
install.packages("dummies")
library("dummies")
CMTL <- dummy.data.frame(as.data.frame(CMTL))
names(CMTL) <- gsub('month|day_of_week', '', names(CMTL))

CMTL %>% select(gap2, Monday) %>% table(.)

CMTL <- CMTL %>%
        rename(after_weekend = gap2, after_911 = gap6) %>%
        mutate(
                after_sandy = as.numeric(date == '2012-10-31'),
                after_holiday = as.numeric((gap3 > 0 | gap4 > 0) & after_sandy == 0),
                before_holiday = lead(after_holiday)
        )

# Exploratory part -------------------------------------------------------------

CMTL %>% ggplot(aes(x = date, y = close)) + geom_line(size = 1)
CMTL %>% ggplot(aes(x = date, y = volume)) + geom_line(size = 1)

hist(CMTL$close)
hist(CMTL$volume)


# take logs and plot
CMTL <- CMTL %>%
        mutate(
                ln_close = log(close),
                ln_volume = ifelse(volume == 0, NA, log(volume))  # to have NA instead of Inf
        )

hist(CMTL$ln_volume)
hist(CMTL$ln_close)

CMTL %>% ggplot(aes(x = date, y = ln_close)) + geom_line(size = 1)
CMTL %>% ggplot(aes(x = date, y = ln_volume)) + geom_line(size = 1)


CMTL %>% filter(year < 2004) %>% 
        ggplot(aes(x = date, y = ln_volume)) + geom_line(size = 1)
CMTL %>% filter(year >= 2004) %>% 
        ggplot(aes(x = date, y = ln_volume)) + geom_line(size = 1)


# unit root tests
pperron(CMTL$close)
pperron(CMTL$ln_close)

#close price is not stationary, we need to take log differences


pperron(CMTL$volume)
pperron(CMTL$ln_volume)


#pperron(filter(CMTL, year < 2001)$ln_volume)
#pperron(filter(CMTL, year < 2001)$ln_volume, 'trend')
#pperron(filter(CMTL, year >= 2001)$ln_volume)

# take log diffs
CMTL <- CMTL %>%
        mutate(
                return = ln_close - lag(ln_close),        
                dln_volume = ln_volume - lag(ln_volume)
        )

pperron(CMTL$return)
pperron(CMTL$dln_volume)


hist(CMTL$return)
hist(CMTL$dln_volume)

#looks OK now

CMTL %>% ggplot(aes(x = date, y = return)) + geom_line(size = 1)
CMTL %>% ggplot(aes(x = date, y = dln_volume)) + geom_line(size = 1)

# correlograms

acf(CMTL$return,na.action=na.pass)
pacf(CMTL$return,na.action=na.pass)

# make spline
CMTL <- mutate(CMTL, t = row_number())
knot <- which(CMTL$year == 2004)[1]  # first day in 2004
CMTL$t1 <- c(1:knot, rep(knot, nrow(CMTL) - knot))
CMTL$t2 <- c(rep(0, knot), 1:(nrow(CMTL) - knot))

# Regressions ------------------------------------------------------------------

#II. close price/ return analysis

days <- c('Monday', 'Tuesday', 'Wednesday', 'Friday')
x_vars <- c(
        'Jan', days, 
        'before_holiday', 'after_holiday', 'after_911', 'after_sandy', 
        't1', 't2'
)

#autoregression on return, with two lags
rg1<-lm(return~lag(return,1)+lag(return,2),data=CMTL)
summary(rg1)

stargazer_r(
        rg1, se = 'newey-west', max_lag = 17,
        dep.var.labels = 'return'
)

#negative and significant coefficient on the (once) lagged return variable
#seemingly violates the efficient market hypothesis
#on average, a 1% increase is followed by a 0.14% decline next day

#including Monday variable
rg2<-lm(return~lag(return,1)+lag(return,2)+Monday,data=CMTL)

stargazer_r(
        list(rg1,rg2), se = 'newey-west', max_lag = 17,
        dep.var.labels = 'return'
)

#No Monday effect for this stock

rg2_pre_2009<-lm(return~lag(return,1)+lag(return,2)+Monday,data=filter(CMTL, year <= 2008))
rg2_post_2009<-lm(return~lag(return,1)+lag(return,2)+Monday,data=filter(CMTL, year > 2008))

stargazer_r(
        list(rg1,rg2,rg2_pre_2009,rg2_post_2009), se = 'newey-west', max_lag = 17,
        dep.var.labels = 'return'
)
#predictability disappears after 2008, markets seemingly got more efficient


#III. volume analysis

hist(CMTL$volume)
hist(CMTL$ln_volume)

pperron(CMTL$volume)
pperron(CMTL$ln_volume)

#the distribution is skewed, so it makes sense to take logs,but it does not have unit root, no need to differentiate

acf(CMTL$ln_volume,na.action=na.pass)
pacf(CMTL$ln_volume,na.action=na.pass)

#looks like an ARIMA process

#estimating ARMA(2,2) for volume
arimareg1 <- Arima(CMTL$ln_volume, order = c(2, 0, 2))
arimareg2 <- Arima(CMTL$ln_volume, order = c(2, 0, 2),xreg=CMTL[c("t1","t2")])#now with time trends included
arimareg3 <- Arima(CMTL$ln_volume, order = c(5, 0, 5), xreg = CMTL[x_vars]) #everything thrown in

stargazer_r(
        list(arimareg1,arimareg2,arimareg3), se = 'newey-west', max_lag = 17,
        dep.var.labels = 'lnvolume'
)

#time trends are different in the two periods (coeffs are even different sign)

#IV. Regression

#merging with NASDAQ data
library(data.table)
setDT(CMTL)
setDT(NSDQ)
setkey(CMTL,date)
setkey(NSDQ,date)
NSDQ<-NSDQ[date>=min(CMTL$date),]

#merging datasets, creating NASDAQ daily return
CMTL<-CMTL[NSDQ]
CMTL <- CMTL %>%
        mutate(
                ln_close_nsdq = log(close_nsdq),
                return_nsdq = ln_close_nsdq - lag(ln_close_nsdq)
        )

#running OLS with two lags, return of stock on return of Nasdaq composite
rg3<-lm(as.formula(paste('return~',
                         lags(return,1:2),'+',lags(return_nsdq,0:2))),data = CMTL)

#collecting seasonal dummies + trend
x_seas <- c(
        'Jan','Feb','Mar','Apr','May','Jun','Jul','Sep','Oct','Nov','Dec', days, 
        'before_holiday', 'after_holiday', 'after_911', 'after_sandy', 
        't1', 't2'
)



#running OLS with two lags, return of stock on return of Nasdaq composite+seasonal dummies
rg4<-lm(as.formula(paste('return~',
                         lags(return,1:2),'+',
                         lags(return_nsdq,0:2),'+',
                         paste(x_seas, collapse = '+')
)),data = CMTL)

#running the same regression for after 2008
rg5<-lm(as.formula(paste('return~',
                         lags(return,1:2),'+',
                         lags(return_nsdq,0:2),'+',
                         paste(x_seas, collapse = '+')
)),data=filter(CMTL, year > 2008))


stargazer_r(
        list(rg3,rg4,rg5), se = 'newey-west', max_lag = 17,
        dep.var.labels = 'lnvolume'
)

#The two models are very similar. There is still a significant (at the 1% level) 
#coefficient on the lagged return of the stock
#NASDAQ returns (contemporenous and lagged) also are all  significant in the regression.
#Interpretation: the stock on average moves 60% of the NASDAQ move of the same day.
#If we add in the lagged coeffs on NASDAQ, the sum of the coeffs is close to one
#This does seem to violate the efficient market hypothesis
#for the post 2008 period, only the same day and two day lagged NASDAQ return remains significant,
#but this may be due to a smaller sample size


#re-running with ARIMA(2,2)
CMTL<-as.data.frame(CMTL)

x_reg<-c(x_seas,'return_nsdq')

arimareg4 <- Arima(CMTL$return, order = c(2, 0, 2), xreg = CMTL[x_reg])

stargazer_r(
        list(rg3,rg4,rg5,arimareg4), se = 'newey-west', max_lag = 17,
        dep.var.labels = 'lnvolume'
)


#here we get a similar result for the one lagged NASDAQ return, looks stable,
#I dont know how to include lagged NASDAQ return, would be interesting

#------------------------------------------END-----------------------------------