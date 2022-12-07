library(stagedtrees)

data <- readRDS("data/simulated_covid_france.rds")


best <- search_best(data, alg = stages_bhc)
plot(best)

as_parentslist(best)


dag1 <- bnlearn::pc.stable(data)
plot(bnlearn::cpdag(dag1))

dag2 <- bnlearn::tabu(data)
plot(bnlearn::cpdag(dag2))
