source("baseball_functions.R") # Personally written baseball functions
library(devtools)
source_gist(8892981)  # reads in parse.retrosheet2.pbp
# parse.retrosheet2.pbp("2015") also read in for 2000-2014
# manually move .csv files from folder to working directory

# self-imposed limit on years
years <- c(2000:2015)
for (i in years){
    assign(paste0('dat', i),read.csv(paste0(paste0("all",i),".csv"), header=F))
}

# couldn't get this to work for whatever reason
fields <- read.csv("fields.csv")
names(dat2000) <- fields[,"Header"]
names(dat2001) <- fields[,"Header"]
names(dat2002) <- fields[,"Header"]
names(dat2003) <- fields[,"Header"]
names(dat2004) <- fields[,"Header"]
names(dat2005) <- fields[,"Header"]
names(dat2006) <- fields[,"Header"]
names(dat2007) <- fields[,"Header"]
names(dat2008) <- fields[,"Header"]
names(dat2009) <- fields[,"Header"]
names(dat2010) <- fields[,"Header"]
names(dat2011) <- fields[,"Header"]
names(dat2012) <- fields[,"Header"]
names(dat2013) <- fields[,"Header"]
names(dat2014) <- fields[,"Header"]
names(dat2015) <- fields[,"Header"]

# Takes data and divides it into n-sized rational groups of var
chart_div <- function(data, n, var){
    samp = c(0)
    check = floor(nrow(data)/n)
    remainder = nrow(data)-check*n
    col_num = which( colnames(data)==var)
    for (i in 1:(check+1)){
        if(i == (check + 1)){
            samp[i]= sum(((data[,col_num])[(1+n*(i-1)):(n*i-n+remainder)]))/remainder
        }
        else{
            samp[i]=sum(((data[,col_num])[(1+n*(i-1)):(n*i)]))/n
        }
    }
    # return the divisions
    samp 
}

# Quicker subset (option to limit to at-bats)
sub_set <- function(data, player_id, ab = FALSE){
    if (ab == TRUE){
        (subset(data, BAT_ID == player_id & AB_FL == TRUE))
    }
    else{
        (subset(data, BAT_ID==player_id))
    }
}

# DATE creation and sort
dates <- function(data){
    data$DATE <- substr(data$GAME_ID, 4, 12)
    data[order(data$DATE),]
}

# Binary K
alt_K <- function(data){
    ifelse(data$EVENT_CD == 3, 1,0)
}



# ichiro data
ichiro2010 <- sub_set(dat2010, "suzui001")
ichiro2010$K <- alt_K(ichiro2010)
ichiro2010 <- dates(ichiro2010)
samp2010 <- chart_div(ichiro2010, 30, "K")

ichiro2011 <- sub_set(dat2011, "suzui001")
ichiro2011$K <- alt_K(ichiro2011)
ichiro2011 <- dates(ichiro2011)
samp2011 <- chart_div(ichiro2011, 30, "K")

ichiro2001 <- sub_set(dat2001, "suzui001")
ichiro2001$K <- alt_K(ichiro2001)
ichiro2001 <- dates(ichiro2001)
samp2001 <- chart_div(ichiro2001, 30, "K")





# ichiro 2011 example
ichiro.AB <- sub_set(dat2011, "suzui001", ab=TRUE)
ichiro.AB$HIT <- ifelse(ichiro.AB$H_FL > 0, 1, 0)
ichiro.AB$DATE <- substr(ichiro.AB$GAME_ID, 4, 12)
ichiro.AB <- ichiro.AB[order(ichiro.AB$DATE),]

# using 2010 data as the basis of the assumption 
Dbar <- k0(sum(ichiro2010$K),length(ichiro2010$K))
SD   <- sd  (samp2010)
phat <- Dbar
CL  <-  Dbar
UCL <- CL + 3 * sqrt(phat * (1-phat) /(length(samp2010)))
LCL <- max(c(-1e-5,CL - 3 * sqrt(phat * (1-phat) /(length(samp2010)))))


plot(1:(length(samp2011)), samp2011, main="Figure 1: Ichiro - K% (2011) - 
     Based on 2010", ylab="K%", xlab="consecutive samples (size 30)",
     ylim=c(-0.02,0.35))
abline(h=Dbar, lty=1, col="red", lwd=2)
abline(h=UCL, lty=4, col="lightblue", lwd=2)
abline(h=LCL, lty=4, col="lightblue", lwd=2)
legend("topleft", c("upper control limit = 30.82%","K% 2010 season average = 11.61%"
                    ,"lower control limit = 0.00%"),lwd=c(2,2,2)
                    ,col=c("lightblue","red","lightblue"), lty=c(4,1,4))

# CUSUM looking for a shift of 1% K% (base 2010)
k = 1/2
h = 4
K = k*SD
H = h*SD
Cp = c(0)
Cm = c(0)
for(i in 1:(length(samp2011))){
    Cp[i] = max(0,samp2011[i]-(Dbar+K)+Cp[i-1])
    Cm[i] = min(0,(Dbar-K)-samp2011[i]+Cp[i-1])
}  

plot(1:(length(samp2011)), Cp, typ="l", col="red", ylim=c(-H-.05,H+.05), ylab="cumulative sum",lwd=2,
     xlab = "consecutive samples (size 30)", main="Figure 4: CUSUM Control Chart - Ichiro - K% (2011) - Based on 2010")
par(new=T)
plot(1:(length(samp2011)), Cm, typ="l", col="lightblue", axes=F, ylim=c(-H-.05,H+.05), ylab="", xlab="",lwd=2)
abline(h=0, lty=4, col= "blue", lwd=2)
abline(h=H, lty=4, col="blue",lwd=2)
abline(h=-H, lty=4, col="blue",lwd=2)
legend("topleft", c("upper limit","upper CUSUM","lower CUSUM","lower limit"),
       lwd=c(2,2,2,2)
       ,col=c("blue","red","lightblue","blue"), lty=c(4,1,1,4))
par(new=F)

# using 2001 data as the basis of the assumption (lazy coding by copying)
Dbar <- k0(sum(ichiro2001$K),length(ichiro2001$K))
SD   <- sd  (samp2001)
phat <- Dbar
CL  <-  Dbar
UCL <- CL + 3 * sqrt(phat * (1-phat) /(length(samp2001)))
LCL <- max(c(-1e-5,CL - 3 * sqrt(phat * (1-phat) /(length(samp2001)))))

plot(1:(length(samp2011)), samp2011, main="Figure 2: Ichiro - K% (2011) -
     Based on 2001", ylab="K%", xlab="consecutive samples (size 30)", 
     ylim=c(-0.02,0.35))
abline(h=Dbar, lty=1, col="red", lwd=2)
abline(h=UCL, lty=4, col="lightblue", lwd=2)
abline(h=LCL, lty=4, col="lightblue", lwd=2)
legend("topleft", c("upper control limit = 22.48%","K% 2001 season average = 7.09%"
                    ,"lower control limit = 0.00%"),lwd=c(2,2,2)
       ,col=c("lightblue","red","lightblue"), lty=c(4,1,4))

# CUSUM looking for a shift of 1% K% (base 2001)
k = 1/2
h = 4
K = k*SD
H = h*SD
Cp = c(0)
Cm = c(0)
for(i in 1:(length(samp2011))){
    Cp[i] = max(0,samp2011[i]-(Dbar+K)+Cp[i-1])
    Cm[i] = min(0,(Dbar-K)-samp2011[i]+Cp[i-1])
}  

plot(1:(length(samp2011)), Cp, typ="l", col="red", ylim=c(-H-.05,H+.05), ylab="cumulative sum",lwd=2,
     xlab = "consecutive samples (size 30)", main="Figure 5: CUSUM Control Chart - Ichiro - K% (2011) - Based on 2001")
par(new=T)
plot(1:(length(samp2011)), Cm, typ="l", col="lightblue", axes=F, ylim=c(-H-.05,H+.05), ylab="", xlab="",lwd=2)
abline(h=0, lty=4, col= "blue", lwd=2)
abline(h=H, lty=4, col="blue",lwd=2)
abline(h=-H, lty=4, col="blue",lwd=2)
legend("topleft", c("upper limit","upper CUSUM","lower CUSUM","lower limit"),
       lwd=c(2,2,2,2)
       ,col=c("blue","red","lightblue","blue"), lty=c(4,1,1,4))
par(new=F)

# Normality assumptions
qqnorm(samp2011, ylab="K%", main="Figure 3: Normal Q-Q Plot: Ichiro - K% (2011)")
qqline(samp2011)






