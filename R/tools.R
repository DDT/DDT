require(changepoint)
require(portfolio)

#Normalize a vector to have unit length 1
normalize.vector = function(x) {
    x / norm(as.matrix(x), 'F')
}

#Get log returns of a time series
getLogReturns = function(x) {
    n = nrow(x)
    log(x[2:n,] / x[1:(n-1),])
}

#Use package changepoint to get change points of a multivariate time series
getcpt = function(mts) {
    cpt = apply(mts, 2, function(j) {
        price.smooth.ts = smooth.spline(j)
        price.ts.spline = splinefun(j)
        price.cpt = cpt.mean(price.smooth.ts$y, method="PELT", class=FALSE)
        price.cpt.x = price.cpt
        price.cpt.y = price.ts.spline(price.cpt) 
        return (list(spline.smooth = price.smooth.ts, spline.fun = price.ts.spline,
                cpt.x = price.cpt.x, cpt.y = price.cpt.y))
    })
}

plot.mts = function(x, cpt, ylab, filename, plot.all.cpt = FALSE) {
    if(!missing(filename)) {
        png(paste(filename, "%03d.png"), width=11, height=8.5, units="in", res = 96)
    }
    p = ncol(x)
    for (page in 1:ceiling(p/4)) {
        par(mfrow=c(2,2))
        startIndex = (page-1) * 4 + 1
        endIndex = min(page*4, p)
        for (j in startIndex:endIndex) {
            price.ts = x[,j]
            plot(price.ts, type='l', col='blue', xlab="Time", ylab=ylab,
                main=colnames(x)[j])
            if(!missing(cpt)) {
                lines(cpt[[j]]$spline.smooth, col='red')
                points(cpt[[j]]$cpt.x, cpt[[j]]$cpt.y, col='green', cex=5, lwd=2)
                if(plot.all.cpt) {
                    for (jj in 1:ncol(x)) {
                        if(jj != j) {
                            cpt.x = cpt[[jj]]$cpt.x
                            points(cpt.x, cpt[[j]]$spline.fun(cpt.x), col='purple',
                                   cex=5, lwd=2)
                        }
                    }
                }
                legend("topright", c(paste(ylab, "ts"), "smoothed ts", "change point"),
                       lty=c(1,1,1), lwd=c(2.5,2.5,2.5), 
                       col=c("blue","red", "green"))
            }
        }
    }
    dev.off()
}

sliding.sd = function(x, window.size = 15) {
    n = length(x)
    window.sd = rep(0, n-window.size)
    for (i in 1:(n-window.size)) {
        startIndex = i
        endIndex = i + window.size
        window.sd[i] = sd(x[startIndex:endIndex], na.rm = T)
    }
    return (window.sd)
}

ccf.matrix = function(mts) {
   numSeries = ncol(mts)
   x = matrix(0, numSeries, numSeries)
   diag(x) = 1
   for (i in 1:numSeries) {
       if(i == numSeries) break
       for (j in (i+1):numSeries) {
           mts.ccf = ccf(mts[,i], mts[,j], plot=F)$acf[,1,]
           x[j,i] = x[i,j] = mts.ccf[which.max(abs(mts.ccf))]
       }
   }
   return (x)
}

ddt.getAllData = function(symbols) {
    lapply(symbols, function(symbol) {
        url = paste('http://datadriventrading.com/mysql_api.php?symbol=', symbol, '&mode=Get+All+Data&sqlquery=&output=CSV', sep='')
        x = read.csv(url)
        x = x[order(x$unixTS),]
        return (x)
    })
}

ddt.getTodaysData = function(symbols) {
    lapply(symbols, function(symbol) {
        url = paste('http://datadriventrading.com/mysql_api.php?symbol=', symbol, '&mode=Get+Todays+Data&sqlquery=&output=CSV', sep='')
        x = read.csv(url)
        return (x)
    })
}
