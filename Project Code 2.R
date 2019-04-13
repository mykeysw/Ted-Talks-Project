#Question 1

ted_main_correct$duration2 <- ted_main_correct$duration^2
quadratic_model1 <- lm(views ~ duration2 , data=ted_main_correct)
summary(quadratic_model1)

quadratic_model2 <- lm(views ~ duration2 + comments + languages + published_date, data=ted_main_correct)
summary(quadratic_model2)


quadratic_model3 <- lm(views ~ duration2 + comments + published_date, data=ted_main_correct)
summary(quadratic_model3)

ted_main_correct$duration_range1 <- ifelse(ted_main_correct$duration < 10*60, 1, 0)
ted_main_correct$duration_range2 <- ifelse(ted_main_correct$duration > 10*60 & ted_main_correct$duration < 20*60, 1, 0)
ted_main_correct$duration_range3 <- ifelse(ted_main_correct$duration > 20*60, 1, 0)


sum(ted_main_correct$duration_range1)/length(ted_main_correct$duration_range1)
sum(ted_main_correct$duration_range2)/length(ted_main_correct$duration_range1)
sum(ted_main_correct$duration_range3)/length(ted_main_correct$duration_range1)

model_duration <- lm(views ~  duration_range1 + duration_range2 + duration_range3 + comments + published_date, data=ted_main_correct)
summary(model_duration)




