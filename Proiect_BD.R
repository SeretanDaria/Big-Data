#incarcarea setului de date
online_shoppers_intention <- read.csv("C:/Users/acer/Documents/R/win-library/4.1/PREDICTIE/online_shoppers_intention.csv")

###descarcarea librariilor necesare
library(tidyverse)
library(ggplot2)
library(GGally)
library(scatterplot3d)
library(modelr)
library(rpart)
library(magrittr)
library(purrr)
library(tidyr)
library(caret)
library(rsample)
library(ISLR)
library(dplyr)
library(corrplot)

###stergerea coloanelor irelevante
online_shoppers_intention[10:15]<- list(NULL)
online_shoppers_intention[11]<- list(NULL)

###redenumirea coloanelor
colnames(online_shoppers_intention)<-c("Nr_pag_admin","Timp_pag_admin","Nr_pag_info","Timp_pag_info","Nr_pag_prod","Timp_pag_prod","Rata_respingere", "Rata_iesire", "Medie_val_pag", "Tip_vizitator", "Intentia")

names(online_shoppers_intention)

###analiza setului de date
###histograma
ggplot(gather(online_shoppers_intention %>% select_if(is.numeric)), aes(value))  +
 geom_histogram(bins = 10) + 
 facet_wrap(~key, scales = 'free_x')

###grafice pentru redarea relatiilor dintre 2 variabile
ggplot(online_shoppers_intention) +
 geom_point(aes(x = Rata_iesire, y = Rata_respingere, color = Intentia, shape = Intentia)) +
 scale_shape_manual(values = c(1,4)) +
 theme(text = element_text(size=20)) 
ggplot(online_shoppers_intention) +
 geom_point(aes(x = Nr_pag_info, y = Timp_pag_info, color = Intentia, shape = Intentia)) +
 scale_shape_manual(values = c(1,4)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_point(aes(x = Nr_pag_admin, y = Timp_pag_admin, color = Intentia, shape = Intentia)) +
 scale_shape_manual(values = c(1,4)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_point(aes(x = Nr_pag_prod, y = Timp_pag_prod, color = Intentia, shape = Intentia)) +
 scale_shape_manual(values = c(1,4)) +
 theme(text = element_text(size=20))

###vizualizarea celor 4 quartile
ggplot(online_shoppers_intention) + 
 geom_boxplot(aes(x = Intentia, y = Rata_iesire, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Rata_respingere, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Medie_val_pag, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Nr_pag_info, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Nr_pag_prod, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Nr_pag_admin, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Timp_pag_prod, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Timp_pag_info, fill = Intentia)) +
 theme(text = element_text(size=20))
ggplot(online_shoppers_intention) +
 geom_boxplot(aes(x = Intentia, y = Timp_pag_admin, fill = Intentia)) +
 theme(text = element_text(size=20))

###calculam numarul de cazuri "True" si "False" pentru variabila "Intentia"
by_Intentia <- group_by(online_shoppers_intention, Intentia)
summarize(by_Intentia, count=n())

###Rata_iesire
mod <-glm (data = online_shoppers_intention, Intentia ~ Rata_iesire, family = binomial)
summary(mod)
grid <- online_shoppers_intention %>%
     data_grid(Rata_iesire = seq_range(Rata_iesire, 100)) %>%
        add_predictions(mod, "prob_Intentia", type="response") 
grid
ggplot() + 
 geom_line(data = grid, aes(Rata_iesire, prob_Intentia), color = "red", size = 2)
 nd <- tribble(~Rata_iesire, 0.00, 0.05, 0.10, 0.15, 0.20)
 predicted <- predict(mod, newdata = nd, type = "response")
 predicted

#Rata_respingere
mod <-glm (data = online_shoppers_intention, Intentia ~ Rata_respingere, family = binomial)
summary(mod)
grid <- online_shoppers_intention %>%
     data_grid(Rata_respingere = seq_range(Rata_respingere, 100)) %>%
         add_predictions(mod, "prob_Intentia", type="response") 
grid
ggplot() + 
 geom_line(data = grid, aes(Rata_respingere, prob_Intentia), color = "red", size = 2)
 nrata_resp <- tribble(~Rata_respingere, 0.00, 0.05, 0.10, 0.15, 0.20)
 predicted <- predict(mod, newdata = nrata_resp, type = "response")
 predicted

#Medie_val_pag
mod <-glm (data = online_shoppers_intention, Intentia ~ Medie_val_pag, family = binomial)
summary(mod)
grid <- online_shoppers_intention %>%
 data_grid(Medie_val_pag = seq_range(Medie_val_pag, 100)) %>%
 add_predictions(mod, "prob_Intentia", type="response") 
 grid

ggplot() +
 geom_line(data = grid, aes(Medie_val_pag, prob_Intentia), color = "red", size = 2)
 nmedie_pag <- tribble(~Medie_val_pag, 0, 100, 200, 300)
 predicted <- predict(mod, newdata = nmedie_pag, type = "response")
 predicted

#Regresie multipla
mod_iesire_vizitator <- glm(data=online_shoppers_intention, Intentia ~ Rata_iesire + Tip_vizitator, family = binomial)
summary(mod_iesire_vizitator)

grid_vizitator_nou <- online_shoppers_intention %>%
data_grid(Rata_iesire = seq_range(Rata_iesire, 100)) %>%
mutate(Tip_vizitator = "New_Visitor") %>%
add_predictions(mod_iesire_vizitator, "prob_Intentia", type = "response")
grid_vizitator_vechi <- online_shoppers_intention %>%
data_grid(Rata_iesire = seq_range(Rata_iesire, 100)) %>%
mutate(Tip_vizitator = "Returning_Visitor") %>%
add_predictions(mod_iesire_vizitator, "prob_Intentia", type = "response")
grid_vizitator_necunoscut <- online_shoppers_intention %>%
data_grid(Rata_iesire = seq_range(Rata_iesire, 100)) %>%
mutate(Tip_vizitator = "Other") %>%
add_predictions(mod_iesire_vizitator, "prob_Intentia", type = "response")
ggplot(grid_vizitator_nou, aes(Rata_iesire, prob_Intentia)) + geom_line(color = "purple", size = 2) + geom_line(data = grid_vizitator_vechi, color = "pink", size=2) + geom_line(data = grid_vizitator_necunoscut, color = "blue", size=2)

online_shoppers_intention %>%
  select_if(is.numeric)

online_shoppers_intention %>%
  + ggplot(aes(Rata_iesire)) +
  + geom_density(show.legend = TRUE)

online_shoppers_intention %>%
  + ggplot(aes(Rata_respingere)) +
  + geom_density(show.legend = TRUE)

online_shoppers_intention %>%
  + ggplot(aes(Medie_val_pag)) +
  + geom_density(show.legend = TRUE)

table(online_shoppers_intention$Intentia)

online_shoppers_intention %>%
  + select_if(is.numeric) %>%
  + gather(metric, value) %>%
  + ggplot(aes(value, fill=metric)) +
  + geom_density(show.legend = FALSE) +
  + facet_wrap(~metric, scales = "free")

online_shoppers_intention <- online_shoppers_intention %>%
  + mutate(Rata_iesire = factor(Rata_iesire), Rata_respingere = factor(Rata_respingere), Medie_val_pag = factor(Medie_val_pag))

online_shoppers_intention %>%
  + filter(Intentia == "TRUE") %>%
  + select_if(is.numeric) %>%
  + cor() %>%
  + corrplot::corrplot()

set.seed(123)

split <- initial_split(online_shoppers_intention, prop = 0.7, strata = "Intentia")
train <- training(split)

online_shoppers_intention %>%
  + filter(Intentia == "FALSE") %>%
  + select_if(is.numeric) %>%
  + cor() %>%
  + corrplot::corrplot()

table(train$Intentia)
table(test$Intentia)

features <- setdiff(names(train), "Intentia")
x <- train[,features]
y <- train$Intentia

fitControl <- trainControl(method = "cv", number = 10)

online_shoppers_intention <- online_shoppers_intention %>%
  + mutate(Intentia = factor(Intentia))

y <- as.factor(y)

modNbSimpleCV <- train(x = x, y = y, method = "nb", trControl = fitControl)
modNbSimpleCV
confusionMatrix(modNbSimpleCV)

searchGrid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0.5, adjust = seq(0, 5, by = 1))

modNbCVSearch <- train(x = x, y = y, method = "nb", trControl = fitControl, tuneGrid = searchGrid)
modNbCVSearch
confusionMatrix(modNbCVSearch)

modNbCVSearch$results %>%
  + top_n(5, wt = Accuracy) %>%
  + arrange(desc(Accuracy))

pred <- predict(modNbCVSearch, test) 
pred
predProb <- predict(modNbCVSearch, test, type = "prob")
predProb
confusionMatrix(pred, test$Intentia)
test$Intentia[sapply(test$Intentia, is.logical)] <-lapply(test$Intentia[sapply(test$Intentia, is.logical)], as.factor)

