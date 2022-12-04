## you need to obtain the data, since we are not authorize to share it
data <- read.csv("D_SN147.dat")

library(stagedtrees)

######### SPORT  from SPOCON

data$SPORT <- factor(data$SPOCON, levels = c(1,2), labels = c('no', 'yes'))

#########  friends you can count on (AMICI2)
data$FRIENDS <- factor(data$AMICI2, levels = c(1,2),
                       labels = c("no", "yes"))

######### trust in people

data$TRUST <- factor(data$FIDUCIA, levels = c(2,1),
                     labels = c("no", "yes")) 

data$D <- factor(data$PAESAGGIO, levels = c(1,2), labels = c("no", "yes"))

######### environment

data$ENV <- factor(data$AMBIENTE, levels = 4:1, labels = c("no", "no", "yes", "yes"))

######### television

data$TV <- factor(data$TELE, levels = c(1,2,3), labels = c('no', 'yes', 'yes'))


data.sel <- data[, c("SPORT", "FRIENDS", "TRUST", "ENV", "TV")]

data.sel <- na.omit(data.sel)

best <- search_best(data = data.sel)
plot(best)


layout(matrix(c(1,1,2,3), 2, 2))
plot(best)
barplot(best, "TV", main = "TV")
barplot(best, "TRUST", main = "TRUST")


dag1 <- bnlearn::tabu(data.sel)
-2*BIC(dag1, data.sel)
plot(bnlearn::cpdag(dag1))

dag2 <- bnlearn::pc.stable(data.sel)
plot(bnlearn::cpdag(dag2))
