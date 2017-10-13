library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OnRhcnJhbnRybCIsImlzcyI6ImFnZW50OnRhcnJhbnRybDo6MDE1OTQxYzQtNTUyZC00YjI3LWIxNGEtYzllN2ExMjYxN2FiIiwiaWF0IjoxNTA1MzEzMjAyLCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.vWrAbNkyU0mhgsdXXL-bxESWzppmpm8wguw9uI7pJ64ZsDtovi8kbWbPYS5pPcX8DDnVMuYxJJWHhqdxv--R_w"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/marcusgabe-ut/parkinsons-data"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons"),
  dataset = project
)
summary(df)
attach(df)
?sample_n

df1 <- data.world::query(
  data.world::qry_sql("SELECT mdvp_shimmer, mdvp_shimmer_db, shimmer_apq3, shimmer_apq5, mdvp_apq, shimmer_dda FROM parkinsons"),
  dataset = project
)

pairs(df1)

# fund_freq_df = dplyr::select(df, mdvp_fo_hz, mdvp_flo_hz, mdvp_fhi_hz)
# freq_var_df = dplyr::select(df, mdvp_jitter, mdvp_jitter_abs,mdvp_rap,mdvp_ppq,jitter_ddp)
# amp_var_df = dplyr::select(df, mdvp_shimmer, mdvp_shimmer_db, shimmer_apq3, shimmer_apq5, mdvp_apq, shimmer_dda)
# 
# other_preds_df = dplyr::select(df, nhr, hnr, rpde, d2, dfa, spread1, spread2, ppe)
# # ppe and spread1 correlated, spread1 and spread2 less correlated
# try_preds_df = dplyr::select(df, mdvp_fo_hz, mdvp_jitter, mdvp_shimmer, nhr, rpde, d2, dfa, spread1)
# #mdvp_jitter and mdvp_shimmer are correlated, jitter and nhr
# uncor_preds1_df = dplyr::select(df, mdvp_fo_hz, mdvp_jitter, rpde, d2, dfa, spread1)
# uncor_preds2_df = dplyr::select(df, mdvp_fo_hz, mdvp_shimmer, rpde, d2, dfa, spread1)
# 
# pairs(fund_freq_df) # correlated
# pairs(freq_var_df) # these are highly correlated
# pairs(amp_var_df) # also highly correlated
# pairs(other_preds_df) # mostly not correlated except for spread1 and ppe, spread2 and spread1
# pairs (try_preds_df) # jitter and shimmer correlated, jitter and nhr correlated
# pairs(uncor_preds1_df) # mostly uncorrelated
# pairs(uncor_preds2_df) # mostly uncorrelated


#df = df %>% dplyr::mutate()

#training = dplyr::sample_n(df,98)
#?dplyr::filter
training = sample(nrow(df), 97)
#train_df = df %>% dplyr::filter()
testing = df[-training,]
View(testing)

View(training)

##    ##
## LR ##
##    ##

#creating the training/testing set
df2 = df %>% dplyr::mutate(status2 = ifelse(status == "true", 1, 0))
#View(df2)
training2 = sample(nrow(df2), 97)
testing2 = df2[-training2,]

#
#glm1 - analyzing average hertz and jitter
#
glm1.fit=glm(status2~mdvp_fo_hz+mdvp_jitter,
            data=df2,family=binomial,
            subset=training2)
summary(glm1.fit)
glm1.probs=predict(glm.fit,newdata=testing2,type="response")
#glm1.probs[1:5]
glm1.pred=ifelse(glm1.probs>0.5,"1","0")
status2.testing = testing2$status2
table(glm1.pred,status2.testing)
mean(glm1.pred==status2.testing)

#
#glm2 - analyzing average hertz and shimmer
#
glm2.fit=glm(status2~mdvp_fo_hz+mdvp_shimmer,
            data=df2,family=binomial,
            subset=training2)
summary(glm2.fit)
glm2.probs=predict(glm.fit,newdata=testing2,type="response")
#glm2.probs[1:5]
glm2.pred=ifelse(glm.probs>0.5,"1","0")
status2.testing = testing2$status2
table(glm2.pred,status2.testing)
mean(glm2.pred==status2.testing)

#
#glm3 - analyzing five uncorrelated predictors with jitter
#
glm3.fit=glm(status2~mdvp_fo_hz + mdvp_jitter + rpde + d2 + dfa + spread1,
            data=df2,family=binomial,
            subset=training2)
summary(glm3.fit)
glm3.probs=predict(glm3.fit,newdata=testing2,type="response")
#glm.probs[1:5]
glm3.pred=ifelse(glm3.probs>0.5,"1","0")
status2.testing = testing2$status2
table(glm3.pred,status2.testing)
mean(glm3.pred==status2.testing)

#
#glm4 - analyzing five uncorrelated predictors with shimmer
#
glm4.fit=glm(status2~mdvp_fo_hz + mdvp_shimmer + rpde + d2 + dfa + spread1,
            data=df2,family=binomial,
            subset=training2)
summary(glm4.fit)
glm4.probs=predict(glm4.fit,newdata=testing2,type="response")
#glm4.probs[1:5]
glm4.pred=ifelse(glm.probs>0.5,"1","0")
status2.testing = testing2$status2
table(glm4.pred,status2.testing)
mean(glm4.pred==status2.testing)

##      ##
## LDA  ##
##      ##

#
#LDA1 - analyzing average hertz and jitter
#
lda1.fit=lda(status~mdvp_fo_hz+mdvp_jitter,data=df,subset=training)
lda1.fit
plot(lda1.fit)

lda1.pred=predict(lda1.fit, testing)
lda1_df = data.frame(lda1.pred)
#View(lda.pred)
ggplot(lda1_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda1_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda1.pred$class,testing$status)
table(lda1.pred$class==testing$status)
mean(lda1.pred$class==testing$status)
table(lda1.pred$class!=testing$status)

#
# LDA2 - analyzing average hertz and shimmer
#
lda2.fit=lda(status~mdvp_fo_hz+mdvp_shimmer,data=df,subset=training)
lda2.fit
plot(lda2.fit)

lda2.pred=predict(lda2.fit, testing)
lda2_df = data.frame(lda2.pred)
#View(lda.pred)
ggplot(lda2_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda2_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda2.pred$class,testing$status)
table(lda2.pred$class==testing$status) #no real falses because everyone with jitters has PD -> jitters is "perfect" predictor of PD
mean(lda2.pred$class==testing$status)
table(lda2.pred$class!=testing$status)

#
# LDA3 - analyzing five uncorrelated predictors with jitter
#
lda3.fit=lda(status~mdvp_fo_hz + mdvp_jitter + rpde + d2 + dfa + spread1,data=df,subset=training)
lda3.fit
plot(lda3.fit)

lda3.pred=predict(lda3.fit, testing)
lda3_df = data.frame(lda3.pred)
#View(lda.pred)
ggplot(lda3_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda3_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda3.pred$class,testing$status)
table(lda3.pred$class==testing$status)
mean(lda3.pred$class==testing$status)
table(lda3.pred$class!=testing$status)

#
# LDA4 - analyzing five uncorrelated predictors with shimmer
#
lda4.fit=lda(status~mdvp_fo_hz + mdvp_shimmer + rpde + d2 + dfa + spread1,data=df,subset=training)
lda4.fit
plot(lda4.fit)

lda4.pred=predict(lda4.fit, testing)
lda4_df = data.frame(lda4.pred)
#View(lda.pred)
ggplot(lda4_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda4_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda4.pred$class,testing$status)
table(lda4.pred$class==testing$status)
mean(lda4.pred$class==testing$status)
table(lda4.pred$class!=testing$status)


# df1 = dplyr::bind_cols(testing,lda_df)
# View(df1)
# df1 %>% dplyr::filter(status == class) %>% group_by(status) %>% summarise(n())
# df1 %>% dplyr::filter(status != class) %>% group_by(status) %>% summarise(n())
# df1 %>% dplyr::group_by(class) %>% dplyr::summarise(min(posterior.false), max(posterior.true), n())


##      ##
## QDA  ##
##      ##

#
# QDA1 - analyzing average hertz and jitter
#
qda.fit = qda(status~mdvp_fo_hz+mdvp_jitter,data=df,subset=training)
qda.fit
qda.pred = predict(qda.fit, testing)
table(qda.pred$class,testing$status)
table(qda.pred$class==testing$status)
table(qda.pred$class!=testing$status)
mean(qda.pred$class==testing$status)

#
# QDA2 - analyzing average hertz and shimmer
#
qda.fit2 = qda(status~mdvp_fo_hz+mdvp_shimmer,data=df,subset=training)
qda.fit2
qda.pred2 = predict(qda.fit2, testing)
table(qda.pred2$class,testing$status)
table(qda.pred2$class==testing$status)
table(qda.pred2$class!=testing$status)
mean(qda.pred2$class==testing$status)

#
# QDA3 - analyzing 5 uncorrelated predictors and jitter
#
qda.fit3 = qda(status~mdvp_fo_hz+mdvp_jitter+rpde+d2+dfa+spread1,data=df,subset=training)
qda.fit3
qda.pred3 = predict(qda.fit3, testing)
table(qda.pred3$class,testing$status)
table(qda.pred3$class==testing$status)
table(qda.pred3$class!=testing$status)
mean(qda.pred3$class==testing$status)

#
# QDA4 - analyzing 5 uncorrelated predictors and shimmer
#
qda.fit4 = qda(status~mdvp_fo_hz+mdvp_shimmer+rpde+d2+dfa+spread1,data=df,subset=training)
qda.fit4
qda.pred4 = predict(qda.fit4, testing)
table(qda.pred4$class,testing$status)
table(qda.pred4$class==testing$status)
table(qda.pred4$class!=testing$status)
mean(qda.pred4$class==testing$status)


##      ##
## KNN  ##
##      ##

#
#analyzing average hertz and jitter
#
predictors=cbind(mdvp_fo_hz,mdvp_jitter)
testing_knn = sample(nrow(testing), 97)
knn.pred=class::knn(predictors[training, ],predictors[testing_knn,],status[training],k=1) # dimensions differ
table(knn.pred,status[testing_knn])
mean(knn.pred==status[testing_knn])

#
#analyzing average hertz and shimmer
#
predictors=cbind(mdvp_fo_hz,mdvp_shimmer)
testing_knn = sample(nrow(testing), 97)
knn.pred=class::knn(predictors[training, ],predictors[testing_knn,],status[training],k=1) # dimensions differ
table(knn.pred,status[testing_knn])
mean(knn.pred==status[testing_knn])

#
#analyzing five uncorrelated predictors with jitter
#
predictors=cbind(mdvp_fo_hz,mdvp_jitter,rpde,d2,dfa,spread1)
testing_knn = sample(nrow(testing), 97)
knn.pred=class::knn(predictors[training, ],predictors[testing_knn,],status[training],k=1) # dimensions differ
table(knn.pred,status[testing_knn])
mean(knn.pred==status[testing_knn])

#
#analyzing five uncorrelated predictors with shimmer
#
predictors=cbind(mdvp_fo_hz,mdvp_shimmer,rpde,d2,dfa,spread1)
testing_knn = sample(nrow(testing), 97)
knn.pred=class::knn(predictors[training, ],predictors[testing_knn,],status[training],k=1) # dimensions differ
table(knn.pred,status[testing_knn])
mean(knn.pred==status[testing_knn])


# Comparison of the mean correct predictions
mean(glm.pred==status2.testing)
mean(lda.pred$class==testing$status)
mean(qda.pred$class==testing$status)
mean(knn.pred==status[testing_knn])
