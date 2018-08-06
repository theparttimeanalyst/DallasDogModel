week18 <- read.csv("week18.csv")

dogss <- filter(week18, animal_type == "DOG")

x <- dogss$outcome_type

x1 <- str_replace(x, "EUTHANIZED", "DIED")
x2 <- str_replace(x1, "FOSTER", "UNADOPTED")
x3 <- str_replace(x2, "TRANSFER", "UNADOPTED")
x4 <- str_replace(x3, "OTHER", "UNADOPTED")




dogs <- dogss %>% mutate(outcome = x4) %>%
                select(animal_breed, kennel_status, intake_type, intake_condition, outcome, chip_status, animal_origin, council_district ) %>%
              filter(outcome != "RETURNED TO OWNER") %>%
              filter(outcome != "LOST REPORT") %>%
              filter(outcome != "MISSING") %>%
              filter(outcome != "DEAD ON ARRIVAL") %>%
              filter(outcome != "FOUND REPORT") %>%
              mutate(pedigree = if_else(animal_breed == "MIXED", "Cross" , "Pedigree")) %>%
              select(intake_type, intake_condition, outcome, chip_status, animal_origin, pedigree)
          

dogs$outcome <- as.factor(dogs$outcome)
dogs$pedigree <- as.factor(dogs$pedigree)

nrow(dogs) * 0.75

sampleR <- sample(20867,15650 )

dog_train <- dogs[sampleR,]



dog_test <- dogs[-sampleR,]

dog_model <- rpart(outcome ~., data = dog_train, method = "class", control = rpart.control(cp=0))


rpart.plot(dog_model, type = 3, fallen.leaves = TRUE, tweak = 2)

dog_test$pred <- predict(dog_model, dog_test, type = "class")

table(dog_test$pred, dog_test$outcome)

mean(dog_test$pred == dog_test$outcome)



dog2 <- separate(dogs, col = intake_condition, c("cond"), extra = "drop")

dog2$cond <- as.factor(dog2$cond)

sampleR <- sample(20867,15650 )

dog_train2 <- dog2[sampleR,]



dog_test2 <- dog2[-sampleR,]


dog_model2 <- rpart(outcome ~., data = dog_train2, method = "class", control = rpart.control(cp=0))

rpart.plot(dog_model2, type = 3, fallen.leaves = TRUE, tweak = 1.2)

dog_test2$pred <- predict(dog_model2, dog_test2, type = "class")

table(dog_test2$pred, dog_test2$outcome)

mean(dog_test2$pred == dog_test2$outcome)

invest <- week18 %>% filter(animal_type == "DOG") %>%
                     separate(col = intake_condition, c("cond"), extra = "drop") %>%
                     mutate(outcome = x4) %>%
                      select(cond, outcome_type, outcome) %>%
                     filter(cond == "TREATABLE") %>%
                      group_by(outcome_type, outcome) %>%
                        summarise(tot = n())
                      
ggplot(invest, aes(x = outcome_type, y = tot, fill = outcome)) + geom_col() + labs(x = "Outcome", y = "Total") + coord_flip()

invest2 <- week18 %>% filter(animal_type == "DOG") %>%
          separate(col = intake_condition, c("cond"), extra = "drop") %>%
          mutate(outcome = x4) %>%
  select(cond, outcome_type, kennel_status) %>%
  filter(outcome_type == "EUTHANIZED") %>%
  group_by(kennel_status) %>%
  summarise(tot = n())

ggplot(invest2, aes(x = kennel_status, y = tot)) + geom_col() + labs(x = "kennel status", y = "Total") + coord_flip()


###### add a died reason

dogs3 <- dogss %>% mutate(outcome = x4) %>%
        select(animal_breed, kennel_status, intake_type, intake_condition, outcome, chip_status, animal_origin, council_district ) %>%
  filter(outcome != "RETURNED TO OWNER") %>%
  filter(outcome != "LOST REPORT") %>%
  filter(outcome != "MISSING") %>%
  filter(outcome != "DEAD ON ARRIVAL") %>%
  filter(outcome != "FOUND REPORT") %>%
  mutate(pedigree = if_else(animal_breed == "MIXED", "Cross" , "Pedigree")) %>%
  select(intake_type, intake_condition, outcome, chip_status, animal_origin, pedigree, kennel_status)

sampleR <- sample(20867,15650 )

dog_train3 <- dogs3[sampleR,]



dog_test3 <- dogs3[-sampleR,]


dog_model3 <- rpart(outcome ~., data = dog_train3, method = "class", control = rpart.control(cp=0))

rpart.plot(dog_model3, type = 3, fallen.leaves = TRUE, tweak = 1.7)

dog_test3$pred <- predict(dog_model3, dog_test3, type = "class")

table(dog_test3$pred, dog_test3$outcome)

mean(dog_test3$pred == dog_test3$outcome)

plotcp(dog_model3)

dog_model_pruned <- prune(dog_model3, cp = 0.00075)

dog_test3$pred <- predict(dog_model_pruned, dog_test3, type = "class")

table(dog_test3$pred, dog_test3$outcome)

mean(dog_test3$pred == dog_test3$outcome)


dogs4 <- dogss %>% mutate(outcome = x4) %>%
  select(animal_breed, kennel_status, intake_type, intake_condition, outcome, chip_status, animal_origin, council_district ) %>%
  filter(outcome != "RETURNED TO OWNER") %>%
  filter(outcome != "LOST REPORT") %>%
  filter(outcome != "MISSING") %>%
  filter(outcome != "DEAD ON ARRIVAL") %>%
  filter(outcome != "FOUND REPORT") %>%
  mutate(pedigree = if_else(animal_breed == "MIXED", "Cross" , "Pedigree")) %>%
  select(intake_type, intake_condition, outcome, chip_status, animal_origin, pedigree, kennel_status)

sampleR <- sample(20867,15650 )

dog_train4 <- dogs4[sampleR,]



dog_test4 <- dogs4[-sampleR,]


dog_model4 <- rpart(outcome ~., data = dog_train4, method = "class", control = rpart.control(cp=0.00075, maxdepth = 5))

rpart.plot(dog_model4, type = 3, fallen.leaves = TRUE, tweak = 0.7)

dog_test4$pred <- predict(dog_model4, dog_test4, type = "class")

table(dog_test4$pred, dog_test4$outcome)

mean(dog_test4$pred == dog_test4$outcome)

