load(url("http://www.openintro.org/stat/data/kobe.RData"))
head(kobe)

kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))
summary(kobe_streak)

outcomes <- c("heads", "tails")
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
sim_fair_coin
table(sim_fair_coin)
sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2, 0.8))
table(sim_unfair_coin)

outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))
sim_streak<-calc_streak(sim_basket)
barplot(table(sim_streak))
summary(sim_streak)
