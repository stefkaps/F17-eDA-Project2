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

#df = df %>% dplyr::mutate()

#training = dplyr::sample_n(df,98)
#?dplyr::filter
training = sample(nrow(df), 97)
#train_df = df %>% dplyr::filter()
testing = df[-training,]
View(testing)

View(training)

##
## LR
##

df2 = df %>% dplyr::mutate(status2 = ifelse(status == "true", 1, 0))
#View(df2)
training2 = sample(nrow(df2), 97)
testing2 = df2[-training2,]
View(testing2)
glm.fit=glm(status2~mdvp_fo_hz+mdvp_jitter,
            data=df2,family=binomial,
            subset=training2)
summary(glm.fit)
glm.probs=predict(glm.fit,newdata=testing2,type="response")
#glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"1","0")
status2.testing = testing2$status2
table(glm.pred,status2.testing)
mean(glm.pred==status2.testing)


##
## LDA
##

#LDA1 - hz predictor + 1 jitter predictor
lda.fit=lda(status~mdvp_fo_hz+mdvp_jitter,data=df,subset=training)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit, testing)
lda_df = data.frame(lda.pred)
#View(lda.pred)
ggplot(lda_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda.pred$class,testing$status)
table(lda.pred$class==testing$status)
mean(lda.pred$class==testing$status)
table(lda.pred$class!=testing$status)

# LDA2 - just all jitter predictors
lda2.fit=lda(status~mdvp_jitter+mdvp_rap+mdvp_ppq+jitter_ddp,data=df,subset=training)
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

##LDA3 - just all shimmer predictors
##  LDA3 vs LDA2 - note people without PD have shimmers (seen in LDA3) but not jitters (seen in LDA2)
lda3.fit=lda(status~mdvp_shimmer+mdvp_shimmer_db+shimmer_apq3+shimmer_apq5+mdvp_apq+shimmer_dda,data=df,subset=training)
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

## LDA4 - all shimmer predictors + all jitter predictors
lda4.fit=lda(status~mdvp_shimmer+mdvp_shimmer_db+shimmer_apq3+shimmer_apq5+mdvp_apq+shimmer_dda+mdvp_jitter+mdvp_rap+mdvp_ppq+jitter_ddp,data=df,subset=training)
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

## LDA5 - all shimmer predictors + hertz predictor
lda5.fit=lda(status~mdvp_fo_hz+mdvp_shimmer+mdvp_shimmer_db+shimmer_apq3+shimmer_apq5+mdvp_apq+shimmer_dda,data=df,subset=training)
lda5.fit
plot(lda5.fit) #compare LDA5 vs LDA3 (all shimmer) -> shimmer and hertz are not as good predictors of someone having PD as jitters

lda5.pred=predict(lda5.fit, testing)
lda5_df = data.frame(lda5.pred)
#View(lda.pred)
ggplot(lda5_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda5_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda5.pred$class,testing$status)
table(lda5.pred$class==testing$status) #hertz predictor is a better predictor of getting real false's
mean(lda5.pred$class==testing$status)
table(lda5.pred$class!=testing$status)


# df1 = dplyr::bind_cols(testing,lda_df)
# View(df1)
# df1 %>% dplyr::filter(status == class) %>% group_by(status) %>% summarise(n())
# df1 %>% dplyr::filter(status != class) %>% group_by(status) %>% summarise(n())
# df1 %>% dplyr::group_by(class) %>% dplyr::summarise(min(posterior.false), max(posterior.true), n())


##
## QDA
##

qda.fit = qda(status~mdvp_fo_hz+mdvp_jitter,data=df,subset=training)
qda.fit
testing <- dplyr::sample_n(df,98)
qda.pred = predict(qda.fit, testing)
table(qda.pred$class,testing$status)
table(qda.pred$class==testing$status)
table(qda.pred$class!=testing$status)
mean(qda.pred$class==testing$status)


##
## KNN
##

??knn
predictors=cbind(mdvp_fo_hz,mdvp_jitter)
testing_knn = sample(nrow(testing), 97)
knn.pred=class::knn(predictors[training, ],predictors[testing_knn,],status[training],k=1) # dimensions differ
table(knn.pred,status[testing_knn])
mean(knn.pred==status[testing_knn])



# Comparison of the mean correct predictions
mean(glm.pred==status2.testing)
mean(lda.pred$class==testing$status)
mean(qda.pred$class==testing$status)
mean(knn.pred==status[testing_knn])
