load(url("http://www.openintro.org/stat/data/evals.RData"))

# 3
dim(subset(evals, score<3))
summary(evals)
hist(evals$score)

# 4
plot(evals$score ~ evals$bty_avg)
plot(evals$score ~ jitter(evals$bty_avg))
m_bty<-lm(evals$score ~ evals$bty_avg)
abline(m_bty)
summary(m_bty)

# 6
plot(evals$bty_avg ~ evals$bty_f1lower)
cor(evals$bty_avg, evals$bty_f1lower)
plot(evals[,13:19])

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)
multiLines(m_bty_gen)

# 8
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)

# 9
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m_full)

m_1 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
             + cls_students + cls_level + cls_credits + bty_avg, data = evals)
summary(m_1)

# 10
m1 <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval
         + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m1)$adj.r.squared

m2 = lm(score ~ rank + gender + language + age + cls_perc_eval +
          cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m2)$adj.r.squared

m3 <- lm(score ~ rank + ethnicity + language + age + cls_perc_eval
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m3)$adj.r.squared

m4 <- lm(score ~ rank + ethnicity + gender + age + cls_perc_eval
         + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m4)$adj.r.squared

m5 <- lm(score ~ rank + ethnicity + gender + language + cls_perc_eval
         + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m5)$adj.r.squared

m6 <- lm(score ~ rank + ethnicity + gender + language + age
         + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m6)$adj.r.squared

m7 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
         + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m7)$adj.r.squared

m8 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
         + cls_students + cls_profs + cls_credits + bty_avg, data = evals)
summary(m8)$adj.r.squared

m9 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
         + cls_students + cls_level + cls_credits + bty_avg, data = evals)
summary(m9)$adj.r.squared

m10 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
         + cls_students + cls_level + cls_profs + bty_avg, data = evals)
summary(m10)$adj.r.squared

m11 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
         + cls_students + cls_level + cls_profs + cls_credits , data = evals)
summary(m11)$adj.r.squared

# max
# m9 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval
#          + cls_students + cls_level + cls_credits + bty_avg, data = evals)
# summary(m9)$adj.r.squared
# [1] 0.1430683