#QUESTION 1:
#plot:
plot(ted_main_correct$views,ted_main_correct$duration)
#model:
model1 <- lm(views ~ duration , data=ted_main_correct)
summary(model1)
#install.packages('stargazer')
install.packages("stargazer")
library('stargazer')
stargazer(model1, type="text",ci=TRUE)
scatter.smooth(x=ted_main_correct$views, y=ted_main_correct$duration, main="views~duration")  # scatterplot

install.packages("ggplot2")
library('ggplot2')
ggplot(ted_main_correct, aes(x = views, y = duration/60)) +
  geom_point() +
  ggtitle("Relation between Views and Duration") +
  xlab("Views") + ylab("Duration (mins)")


#Question 2
#controversial
ted_main_correct$controversial <- ifelse(grepl("'race'|'culture'|'religion'|'politics'|'feminism'|'women'|'crime'|'gender'|'equality'|'war'|'protests'", ted_main_correct$tags), "1","0")
#knowledge
ted_main_correct$knowledge <- ifelse(grepl("'education'|'technology'|'business'|'health'|'science'|'history'|'innovation'|'math
'", ted_main_correct$tags, ignore.case = T), "1","0")
#ART
ted_main_correct$art <- ifelse(grepl("'Dance'|'Music'|'Creativity'|'Comedy'|'Humor'|'design'|'writing'|'poetry'|'theater'|'piano'|'architecture'", ted_main_correct$tags, ignore.case = T), "1","0")

model2 <- lm(views ~ knowledge + art + controversial, data=ted_main_correct)
summary(model2)

sum(as.integer(ted_main_correct$art))/length(ted_main_correct$art)
sum(as.integer(ted_main_correct$controversial))/length(ted_main_correct$controversial)
sum(as.integer(ted_main_correct$knowledge))/length(ted_main_correct$knowledge)

#Question 3
install.packages("anytime")
library(anytime)
ted_main_correct$published_date2 <- anytime(ted_main_correct$published_date)
ted_main_correct$day_of_week <- weekdays(as.Date(ted_main_correct$published_date2))
ted_main_correct$day_binary <- ifelse(ted_main_correct$day_of_week %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday'), 1, 0)

ted_main_correct$vacation <- as.numeric(format(as.Date(anytime(ted_main_correct$published_date), '%Y/%m/%d', tz='UTC'), '%m'))

ted_main_correct$vacation <- as.numeric(ted_main_correct$vacation %in% c(5,6,7,8))


#QUESTION 4:
#plot:
plot(ted_main_correct$views,ted_main_correct$languages)
#model:
model1 <- lm(views ~ languages , data=ted_main_correct)
summary(model1)
install.packages('stargazer')
library('stargazer')
stargazer(model1, type="text",ci=TRUE)

install.packages("ggplot2")
library('ggplot2')
ggplot(ted_main_correct, aes(x = views, y = languages)) +
  geom_point() +
  stat_smooth(method = "auto", col = "blue") +
  ggtitle("Relation between Views and Duration") +
  xlab("Views") + ylab("languages")+xlim(0,.5e+07)

cor(ted_main_correct$views,ted_main_correct$languages)

#Question 3 (Revised)
library(anytime)

ted_main_correct$vacation <- as.numeric(format(as.Date(anytime(ted_main_correct$published_date), '%Y/%m/%d', tz='UTC'), '%m'))

ted_main_correct$vacation <- as.numeric(ted_main_correct$vacation %in% c(5,6,7,8))

install.packages("stringr")
library("stringr")

ted_main_correct$art_weight <- str_count(ted_main_correct$tags, "'Dance'|'Music'|'Creativity'|'Comedy'|'Humor'|'design'|'writing'|'poetry'|'theater'|'piano'|'architecture'")

ted_main_correct$controversial_weight <- str_count(ted_main_correct$tags, "'race'|'culture'|'religion'|'politics'|'feminism'|'women'|'crime'|'gender'|'equality'|'war'|'protests'")

ted_main_correct$knowledge_weight <- str_count(ted_main_correct$tags,"'space'|'statistics'|'economist'|'books'|'internet'|'brain'|education'|'technology'|'business'|'health'|'science'|'history'|'innovation'|'math
'")

#install.packages('aod')
library(aod)
model3 <- glm(vacation ~ knowledge_weight +art_weight +controversial_weight, family = binomial(link = "probit"), data=ted_main_correct)
summary(model3)

library(aod)
ted_main_correct$entertainment_weight <- str_count(ted_main_correct$tags, "'entertainment'|'music'|'film'|'online video'|'humor'|'performance'|'dance'|'creativity'|'comedy'|'singer'|'gaming'|'magic'|'poetry'|'origami'|'art'")


library(aod)
model3 <- glm(vacation ~ knowledge_weight + entertainment_weight + controversial_weight, family = binomial(link = "probit"), data=ted_main_correct)
summary(model3)

model3.1 <- glm(vacation ~ knowledge_weight + entertainment_weight + controversial_weight, family = binomial(link = "probit"), data=ted_main_correct)
model3.2<- glm(vacation ~ knowledge_weight + entertainment_weight, family = binomial(link = "probit"), data=ted_main_correct)
model3.3<- glm(vacation ~ knowledge_weight, family = binomial(link = "probit"), data=ted_main_correct)

stargazer(model3.1,model3.2,model3.3,type="text")


Pr(Vacation=1|knowledge, entertainment, controversial) = ???(??1+ ??2(knowledge)+ ??3(entertainment)+ ??4(controversial))






