source("http://www.openintro.org/stat/data/present.R")

# 1
dim(present)

# 3
plot(x = present$year, y = present$girls, type='l')

# 4
which(present$girls==max(present$girls))

# 5
plot(x = present$year, y = present$boys/(present$boys+present$girls), type='l')

#6
table(present$boys>present$girls)

# 7
plot(x = present$year, y = present$boys/present$girls, type='l')

# 8
which(abs(present$boys-present$girls)==max(abs(present$boys-present$girls)))

