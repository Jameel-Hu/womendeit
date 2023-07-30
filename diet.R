time = sort(nym.2002$time)
round((max(nym.2002$time)/mean(nym.2002$time),3)
fast.over.median <- max(time)/median(time)
paste("Fastest over median time is ", format(fast.over.median, digits=3), sep="")
 library(dslabs)
 data("divorce_margarine")
 plot(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
 cor(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
 data(ChickWeight)
 head(ChickWeight)
 plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet) 
 chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                 direction="wide") 
 
 head(chick)
 chick = na.omit(chick)
 day04 <- subset(ChickWeight, ChickWeight$Time == 4)
 notOut04 <- day04$weight
 incOut04 <- c(day04$weight, 3000)
 cat("Day 4 Mean:", mean(notOut04))
 cat("\nDay 4 + Outlier Mean:", mean(incOut04))
 cat("\n(Day 4 + Outlier Mean)/(Day 4 Mean):", mean(incOut04)/mean(notOut04)) 
 1.4826 * median( c(chick$weight.4, 3000) - median(c(chick$weight.4, 30) )
 00)
 
 mean(newchick)/mean(chick$weight.4)
 
 newchick <- c(chick$weight.4, 3000)
 mad(c(chick$weight.4, 3000))/mad(chick$weight.4)
 median(c(chick$weight.4, 3000))/median(chick$weight.4)
 sd(c(chick$weight.4, 3000))/sd(chick$weight.4)
 mean(newchick)/mean(chick$weight.4)
 
 1.4826 * median( c(chick$weight.4, 3000) - median(c(chick$weight.4, 3000)) )
 
 plot(chick$weight.4, chick$weight.21)
 pearson1 <- cor(chick$weight.4, chick$weight.21)
 pearson2 <- cor(c(chick$weight.4, 3000), c(chick$weight.21, 3000)) 
 pearson2 / pearson1
 
 x <- chick$weight.4[1:16]
 x
 
 y <-chick$weight.4[37:45]
 y
 
 
 
 x <- subset(day04$weight, day04$Diet == 1)
 y <- subset(day04$weight, day04$Diet == 4)
 t.test(x, y)
 wilcox.test(c(x,200), y)$p.value
 
 t.test(c(x, 200), y)$p.value
 
 
 
 
 
 