# Барамия Никита Э-301
# Домашнее задание №5

C_param <- 5
gamma_param <- 2
k_param <- 10

# библиотеки
library(dplyr)
library(caret)
library(e1071)
library(tictoc)
library(stargazer)


## Пункт 1
data <- read.csv("winequality-red.csv", header = TRUE)

# описательная статистика
stargazer(data, summary = TRUE, type = "tex")
# посмотрим на распределение качества вина
ggplot(data, aes(x = factor(quality))) + geom_bar() +
  geom_text(size = 10, stat = 'count', aes(label = ..count..), vjust = -0.1) + 
  labs(x = "Качество вина", y = "Количество наблюдений с данным качеством") + 
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20))

# перекодируем quality на бинарную, 0, если меньше либо равно 5, 1 -- иначе
df <- data %>% mutate(quality = as.factor(ifelse(quality <= 5, 0, 1)))
ggplot(df, aes(x = quality)) + geom_bar() + 
  geom_text(size = 10, stat = 'count', aes(label = ..count..), vjust = -0.1) + 
  labs(x = "Класс вина", y = "Количество наблюдений в классе") + 
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20))

# Графический анализ, безумный, слишком много графиков :)
# colors <- c('red2', 'green3')[df$quality]
# pairs(df[, 1:11], col = colors, pch = 1, cex = 1, cex.labels = 1.5)


## Пункт 2
set.seed(1)

# Разделяем на обучающую и тестовую
index_split <- createDataPartition(y = df$quality, p = 0.75, list = FALSE)
train <- df[index_split, ]
test <- df[-index_split, ]

# обучаем модель
svm_model_1 <- svm(quality ~ ., data = train, kernel = "radial",
                   cost = C_param, gamma = gamma_param)
svm_model_1

# предскажем классы для теста
pred_1 <- predict(svm_model_1, test[, -12])
# и посчитаем ошибка прогноза
mean(pred_1 != test$quality)

# количество опорных векторов в каждом классе
train[svm_model_1$index, ] %>% group_by(quality) %>% summarise(number = n()) %>% data.frame()


## Пункт 3
svm_model_2 <- svm(quality ~ alcohol + residual.sugar, data = train, kernel = "radial",
                 cost = C_param, gamma = gamma_param, cross = k_param)
svm_model_2

# ошибка на обучающей выборке с кросс-валидацией
1 - svm_model_2$tot.accuracy / 100
# таблица с ошибками
# cross_val_scores <- data.frame(k_fold = c(1:k_param), mse = 1 - svm_model_2$accuracies / 100)
# средняя ошибка прогноза на кросс-валидации
# mean(cross_val_scores$mse) # тот же результат, самопроверка пройдена

# графическая интерпретация
plot(svm_model_2, data = train[c("quality", "alcohol", "residual.sugar")],
     svSymbol = "x", dataSymbol = 1, symbolPalette = c("red2", "green3"))


## Пункт 4
tic()
ctrl <- tune.control(sampling = "cross", cross = k_param)
svm_tune <- tune(svm, train.x = train[, -12], train.y = train[, 12], kernel = "radial", 
                 ranges = list(cost = seq(from = 1, to = 30, by = 1), 
                               gamma = seq(from = 0.1, to = 3, by = 0.1)),
                 tunecontrol = ctrl)
svm_tune
toc() # около 20 минут

svm_tune$best.parameters$cost
svm_tune$best.parameters$gamma

# сохраняем и обучаем модель с лучшими параметрами
tuned_model <- svm(quality ~ ., data = train, kernel = "radial",
                   cost = svm_tune$best.parameters$cost, gamma = svm_tune$best.parameters$gamma)

# опорные вектора
train[tuned_model$index, ] %>% group_by(quality) %>% summarise(number = n()) %>% data.frame()

# используем лучшие параметры для проверки на обучающей
pred_train <- predict(tuned_model, train[, -12])
mean(pred_train != train$quality)
# и на тестовой выборке
pred_test <- predict(tuned_model, test[, -12])
mean(pred_test != test$quality)
# матрица ошибок
table(pred_test, test$quality)


## Пункт 5
q_uniqie <- sort(unique(data$quality))[2:4]
crazy_list <- list()
error_train <- rep(Inf, 3)
error_test <- rep(Inf, 3)
timer <- list()
i <- 1
for (k in q_uniqie) {
  tic()
  # печатаем столбец деления и сохраняем деление в таблицу
  print(k)
  df_k <- data %>% mutate(quality = as.factor(ifelse(quality <= k, 0, 1)))
  # делим на обучающую и тестовую
  set.seed(1)
  index_split <- createDataPartition(y = df_k$quality, p = 0.75, list = FALSE)
  train_k <- df_k[index_split, ]
  test_k <- df_k[-index_split, ]
  # тюнинг гиперпараметров
  ctrl <- tune.control(sampling = "cross", cross = 7)
  svm_tune_k <- tune(svm, train.x = train_k[, -12], train.y = train_k[, 12], kernel = "radial", 
                     ranges = list(cost = seq(from = 1, to = 100, by = 3), 
                                   gamma = seq(from = 0.1, to = 10, by = 0.3)),
                     tunecontrol = ctrl)
  # принтуем лучшие параметры 
  print(svm_tune_k$best.parameters$cost)
  print(svm_tune_k$best.parameters$gamma)
  # используем и сохраняем
  tuned_model_k <- svm(quality ~ ., data = train_k, kernel = "radial",
                       cost = svm_tune_k$best.parameters$cost, gamma = svm_tune_k$best.parameters$gamma)
  crazy_list[[i]] <- tuned_model_k
  # ошибка на обучающей
  pred_train_k <- predict(tuned_model_k, train_k[, -12])
  print(mean(pred_train_k != train_k$quality))
  error_train[i] <- round(mean(pred_train_k != train_k$quality), digits = 3)
  # и на тестовой выборке
  pred_test_k <- predict(tuned_model_k, test_k[, -12])
  print(mean(pred_test_k != test_k$quality))
  error_test[i] <- round(mean(pred_test_k != test_k$quality), digits = 3)
  # сохраняем время
  timer[[i]] <- toc()
  i <- i + 1
} # занимает чуть более 50 минут

error_train
error_test

# то же самое, только для определённых делений и с более точечным диапозоном
q_uniqie <- sort(unique(data$quality))[c(2, 4)]
crazy_list <- list()
error_train <- rep(Inf, 2)
error_test <- rep(Inf, 2)
timer <- list()
i <- 1
for (k in q_uniqie) {
  tic()
  # печатаем столбец деления и сохраняем деление в таблицу
  print(k)
  df_k <- data %>% mutate(quality = as.factor(ifelse(quality <= k, 0, 1)))
  # делим на обучающую и тестовую
  set.seed(1)
  index_split <- createDataPartition(y = df_k$quality, p = 0.75, list = FALSE)
  train_k <- df_k[index_split, ]
  test_k <- df_k[-index_split, ]
  # тюнинг гиперпараметров
  ctrl <- tune.control(sampling = "cross", cross = 7)
  svm_tune_k <- tune(svm, train.x = train_k[, -12], train.y = train_k[, 12], kernel = "radial", 
                     ranges = list(cost = seq(from = 1, to = 8, by = 0.5), 
                                   gamma = seq(from = 0.1, to = 3, by = 0.1)),
                     tunecontrol = ctrl)
  # принтуем лучшие параметры 
  print(svm_tune_k$best.parameters$cost)
  print(svm_tune_k$best.parameters$gamma)
  # используем и сохраняем
  tuned_model_k <- svm(quality ~ ., data = train_k, kernel = "radial",
                       cost = svm_tune_k$best.parameters$cost, gamma = svm_tune_k$best.parameters$gamma)
  crazy_list[[i]] <- tuned_model_k
  # ошибка на обучающей
  pred_train_k <- predict(tuned_model_k, train_k[, -12])
  print(mean(pred_train_k != train_k$quality))
  error_train[i] <- round(mean(pred_train_k != train_k$quality), digits = 3)
  # и на тестовой выборке
  pred_test_k <- predict(tuned_model_k, test_k[, -12])
  print(mean(pred_test_k != test_k$quality))
  error_test[i] <- round(mean(pred_test_k != test_k$quality), digits = 3)
  # сохраняем время
  timer[[i]] <- toc()
  i <- i + 1
} # занимает около 20 минут

error_train
error_test


df_k <- data %>% mutate(quality = as.factor(ifelse(quality <= 4, 0, 1)))
set.seed(1)
index_split <- createDataPartition(y = df_k$quality, p = 0.75, list = FALSE)
train_k <- df_k[index_split, ]
test_k <- df_k[-index_split, ]
tuned_model_1 <- svm(quality ~ ., data = train_k, kernel = "radial",
                     cost = 2, gamma = 1.6)

pred_test_1 <- predict(tuned_model_1, test_k[, -12])
table(pred_test_1, test_k$quality)


df_k <- data %>% mutate(quality = as.factor(ifelse(quality <= 6, 0, 1)))
set.seed(1)
index_split <- createDataPartition(y = df_k$quality, p = 0.75, list = FALSE)
train_k <- df_k[index_split, ]
test_k <- df_k[-index_split, ]
tuned_model_2 <- svm(quality ~ ., data = train_k, kernel = "radial",
                     cost = 2.5, gamma = 1.0)

pred_test_2 <- predict(tuned_model_2, test_k[, -12])
table(pred_test_2, test_k[, 12])
