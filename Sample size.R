# Identifying paediatric pulmonary arterial hypertension from intra-operative monitoring: an observational study
# Tim Dawes
# August 2021

# Code to simulate sample sizes:

# Function to generate data for groups of size N
      anova_func <- function(N) {
        
        N1<- N2<- N3<- N
        CO21 <- rnorm(N1, 2.7, 1) # PAH
        CO22 <- rnorm(N2, 1.7, 1) # PVD
        CO23 <- rnorm(N3, 1.7, 1) # Normals
        
        d<- data.frame(CO2=c(CO21, CO22, CO23), diaggroup=c(rep(1,N1), rep(2,N2), rep(3,N3)))
        
        fit<- lm(CO2 ~ diaggroup, d)
        fit2<- anova(fit)
        p<- fit2$`Pr(>F)`[1]
        
        stat <- fit2$`F value`[1]
        
        return(c(t=stat, p=p, sig=(p < .05)))
        # return a named vector with the results we want to keep
      }


# Loops to check whether effect is detected in successive trials
      maxN<- 50
      maxTrials<- 500
      sig<- rep(0, maxN)
      for (i in 1:maxN)
      {
        cat(".")
        for (j in 1:maxTrials)
        {
        anova.results<- anova_func(i)
        sig[i]<- sig[i] + anova.results[3] 
        }
        df<- data.frame(X=1:maxN, Y=100 * sig / maxTrials)
        with(df, plot(X,Y, type='p', lwd=1, col="pink", pch=19, cex=0.8, xlab="N in each group", ylab="Power (%)"))
        lo<- loess(Y~X, span=0.5, data=df)
        with(df, points(X, predict(lo, X), type='l', lwd=4, col="red"))
        
      }

# Sample size needed in each group for 90% power
      with(df, which.min(abs(predict(lo,X)-90)))
      