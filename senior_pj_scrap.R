source("baseball_functions.R") # Personally written baseball functions
library(devtools)
source_gist(8892981)  # reads in parse.retrosheet2.pbp

# parse.retrosheet2.pbp("2015") also read in for 2000-2014
data <- read.csv("all2000.csv", header=F)
roster <- read.csv("roster2000.csv")
fields <- read.csv("fields.csv")
names(data2010) <- fields[,"Header"]

# self-imposed limit on years
years <- c(2000:2015)
for (i in years){
    assign(paste0('dat', i),read.csv(paste0(paste0("all",i),".csv"), header=F))
}

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
    samp
}

# Quicker subset
sub_set <- function(data, player_id){
    (subset(data, BAT_ID=player_id))
}

# Date creation and sort
dates <- function(data){
    data$DATE <- substr(data$GAME_ID, 4, 12)
    data[order(data$DATE),]
}




# ichiro 2010 data
ichiro2010 <- sub_set(data2010, "suzui001")
ichiro2010$K <- ifelse(ichiro2010$EVENT_CD == 3, 1,0)
ichiro2010$DATE <- substr(ichiro2010$GAME_ID, 4, 12)
ichiro2010 <- ichiro2010[order(ichiro2010$DATE),]

# ichiro 2011 example
ichiro.AB <- subset(data, BAT_ID == "suzui001" & AB_FL == TRUE)
ichiro.AB$HIT <- ifelse(ichiro.AB$H_FL > 0, 1, 0)
ichiro.AB$DATE <- substr(ichiro.AB$GAME_ID, 4, 12)
ichiro.AB <- ichiro.AB[order(ichiro.AB$DATE),]

ichiro2011 <- sub_set(data2011, "suzui001")
ichiro$K <- ifelse(ichiro$EVENT_CD == 3, 1,0)
ichiro$DATE <- substr(ichiro$GAME_ID, 4, 12)
ichiro <- ichiro[order(ichiro$DATE),]


# using full data from 2010 
Dbar <- k0(sum(ichiro2010$K),length(ichiro2010$K))
SD   <- sd  (samp2010)
phat <- Dbar
CL  <-  Dbar
UCL <- CL + 3 * sqrt(phat * (1-phat) /(check+1))
LCL <- max(c(-1e-5,CL - 3 * sqrt(phat * (1-phat) /(check+1))))


plot(1:(check+1), samp, main="Figure 1: Ichiro - K% (2011)", ylab="K%",
     xlab="consecutive samples (size 30)", ylim=c(-0.02,0.35))
abline(h=Dbar, lty=1, col="red", lwd=2)
abline(h=UCL, lty=4, col="lightblue", lwd=2)
abline(h=LCL, lty=4, col="lightblue", lwd=2)
legend("topleft", c("upper control limit = 30.82%","K% 2010 season average = 11.61%"
                    ,"lower control limit = 0.00%"),lwd=c(2,2,2)
                    ,col=c("lightblue","red","lightblue"), lty=c(4,1,4))

qqnorm(samp, ylab="K%", main="Normal Q-Q Plot: Ichiro - K% (2011)")


# CUSUM looking for a shift of 1% K%
k = 1/2
h = 4
K = k*SD
H = h*SD
Cp = c(0)
Cm = c(0)
for(i in 1:(check+1)){
Cp[i] = max(0,samp[i]-(Dbar+K)+Cp[i-1])
Cm[i] = min(0,(Dbar-K)-samp[i]+Cp[i-1])
}  

plot(1:(check+1), Cp, typ="l", col="red", ylim=c(-H-.05,H+.05), ylab="cumulative sum",lwd=2,
     xlab = "consecutive samples (size 30)", main="Figure 2: CUSUM Control Chart - Ichiro - K% (2011)")
par(new=T)
plot(1:(check+1), Cm, typ="l", col="lightblue", axes=F, ylim=c(-H-.05,H+.05), ylab="", xlab="",lwd=2,)
abline(h=H, lty=4, col="blue",lwd=2,)
abline(h=-H, lty=4, col="blue",lwd=2,)
legend("topleft", c("upper limit","upper CUSUM","lower CUSUM","lower limit"),
       lwd=c(2,2,2,2)
       ,col=c("blue","red","lightblue","blue"), lty=c(4,1,1,4))
par(new=F)

