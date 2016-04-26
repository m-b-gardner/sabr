# Baseball functions

# Batting average
avg <- function(hits, ab){
        return(hits/ab)
}

# On-base percentage
obp <- function(hits, bb, hbp, sf, ab){
        if((ab+bb+hbp+sf) != 0){return((hits+bb+hbp)/(ab+bb+hbp+sf))}
        else{ return(.000)} # No plate appearances
}

# Slugging percentage
slg <- function(tb, ab){
        if(ab!=0){ return(tb/ab)}
        else{return(.000)} # No at-bats
}

# On base plus slugging
ops <- function(hits, bb, hbp, sf, ab, tb){
        return(obp(hits,bb,hbp,sf,ab)+slg(tb,ab))
}

# Isolated power
iso <- function(hits, ab, tb){
        return(slg(tb,ab)-avg(hits, ab))
}

# Batting average on balls in play
babip <- function(hits, hr, ab, k, sf){
        if((ab-k-hr+sf)!=0){ return((hits-hr)/(ab-k-hr+sf))}
        else{ return(.000)} # No balls in play
}

# Walk percentage
bb0 <- function (bb, pa){
        if(pa!=0){return(bb/pa)}
        else{return(.000)} # No plate appearances
}

# Strikeout percentage
k0 <- function (k, pa){
        if(pa!=0){return(k/pa)}
        else{return(.000)} # No plate appearances
}

# Weighted on-base average
wOBA <- function(bb,ibb, hbp,b1,b2,b3,hr,ab,sf){
        if((ab+bb-ibb+sf+hbp)!=0)
        {
        return((.69*(bb-ibb)+.722*hbp+.888*b1+1.271*b2+1.616*b3+2.101*hr)
                /(ab+bb-ibb+sf+hbp))
        }
        else{return(.000)} # No plate appearances
}

# Streak function from "Analyzing Baseball Data with R" pg 239
streaks <- function(y){
        n <- length(y)
        where <- c(0,y,0)==0
        location.zeroes <- (0:(n+1))[where]
        streak.lengths <- diff(location.zeroes) - 1
        streak.lengths[streak.lengths > 0]
}

