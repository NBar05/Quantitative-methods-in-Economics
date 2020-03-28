# Барамия Никита Э-301
# Домашнее задание №1
# Изучаемая страна - Польша
# n = 2
# XXX - «Свобода делать жизненный выбор» (столбец 7)
# YYY - «Ожидание здоровой жизни» (столбец 6)

# библиотеки
library(FNN)
library(stats)
library(tictoc)
library(ggplot2)
library(stargazer)


# Пункт 1
data <- read.csv("WHI2019.csv", sep=";", header=TRUE)
data$Country.or.region <- as.character(data$Country.or.region)
# описательная статистика
stargazer(data[3:10], summary = TRUE, type = "text")
# Графический анализ
colors <- c('red2', 'green3')[data$Advanced+1]
pairs(data[, 4:9], col=colors, pch = 1, cex = 1, cex.labels = 1.5)

# пригодится
country <- "Poland"


# Пункт 2
# 5 ближайших соседей Польши по компонентам индекса
k_neighbours <- 5
algor <- get.knnx(data[data$Country.or.region != country, 4:9], 
                  query = data[data$Country.or.region == country, 4:9], 
                  k = k_neighbours, algorithm = "kd_tree")
data[algor$nn.index, "Country.or.region"]
# развитые страны, оказавшиеся «соседями» Польши
data[(data$Advanced == 1) & (data$Overall.rank %in% c(algor$nn.index)), "Country.or.region"]


# Пункт 3
# время работы двух алгоритмов поиска ближайших соседей: полного перебора и kd-tree
k_neighbours <- 5
# время работы kd-tree
tic("kd_tree")
t1 <- get.knnx(data[data$Country.or.region != country, 4:9], 
               query = data[data$Country.or.region == country, 4:9], 
               k = k_neighbours, algorithm = "kd_tree")
toc()
# время работы полного перебора
tic("brute")
t2 <- get.knnx(data[data$Country.or.region != country, 4:9], 
               query = data[data$Country.or.region == country, 4:9], 
               k = k_neighbours, algorithm = "brute")
toc()


# Пункт 4
# соотносится ли уровень счастья в стране с уровнем ее экономического развития?
k_neighbours <- 5
pred <- knn(train = data[data$Country.or.region != country, 4:9], 
            test = data[data$Country.or.region == country, 4:9],
            cl = as.factor(data[data$Country.or.region != country, 10]), 
            k = k_neighbours, prob = FALSE, algorithm = "kd_tree")

as.numeric(as.character(pred)) == data[data$Country.or.region == country, "Advanced"]


# Пункт 5
# сделаем предсказания по каждой стране из выборки, кроме Польши
k_neighbours <- 5
pred <- knn.cv(train = data[data$Country.or.region != country, 4:9],
               cl = as.factor(data[data$Country.or.region != country, 10]), 
               k = k_neighbours, algorithm = "kd_tree")

# функцмя для посчёта ошибки
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
# считаем ошибку
calc_class_err(data[data$Country.or.region != country, 10], pred)

# считаем ошибку для каждого k
k_to_try = c(1:20)
err_k = rep(x = 0, times = length(k_to_try))

for (k_neighbours in k_to_try) {
  pred = knn.cv(train = data[data$Country.or.region != country, 4:9],
                cl = as.factor(data[data$Country.or.region != country, 10]), 
                k = k_neighbours, algorithm = "kd_tree")
  err_k[k_neighbours] <- calc_class_err(data[data$Country.or.region != country, 10], pred)
}

# строим график ошибок для каждого k 
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors",
     ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")


# Пункт 6
# Кластеризируем страны на n кластеров по двум компонентам – 𝑿𝑿𝑿 и 𝒀𝒀𝒀.
n <- 2
xxx <- "Freedom.to.make.life.choices"
yyy <- "Healthy.life.expectancy"

kmean <- kmeans(data[, c(xxx, yyy)], centers = n, 
                nstart = 25, iter.max = 10, algorithm = "Lloyd")
# Смотрим полученные центры
kmean$centers

# Страны, самые близкие к соотвествующим центрам
algor <- get.knnx(data[, c(xxx, yyy)], query = kmean$centers, k = 1, algorithm = "kd_tree")
data[algor$nn.index, "Country.or.region"]

# Определяем кластер, в котором оказалась Польша
kmean$cluster[data$Country.or.region == country]

# Графический результат кластеризации
plot(data[, c(xxx, yyy)], col = (kmean$cluster+1), las = 1,
     main = "k-means результат с 2-мя кластерами",
     xlab = "Свобода делать жизненный выбор",
     ylab = "Ожидание здоровой жизни")
points(kmean$centers, col = "black", pch = 9, cex = 1)


# Пункт 7
# Кластеризуем страны по всем компонентам индекса счастья (столбцы 4:9) на n кластеров
kmean <- kmeans(data[, 4:9], centers = n, nstart = 25, iter.max = 10, algorithm = "Lloyd")
kmean$centers
# Определяем, в каком кластере Польша
kmean$cluster[data$Country.or.region == country]


# Пункт 8
# Переберём задаваемое число кластеров и посчитаем аккуратность для каждого числа
k_to_try = 2:20
acc_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = kmeans(data[, 4:9], centers = k_to_try[i], 
                nstart = 30, iter.max = 30, algorithm = "Lloyd")
  acc_k[i] = pred$betweenss/pred$totss
}

# График аккуратности кластеризации
plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number clusters", ylab = "classification accuracy",
     main = "Accuracy Rate vs Clusters")


# Переберём задаваемое число кластеров и посчитаем суммарное внутригрупповое расстояние
k_to_try = 2:20
within_total = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = kmeans(data[, 4:9], centers = k_to_try[i], 
                nstart = 30, iter.max = 30, algorithm = "Lloyd")
  within_total[i] = pred$tot.withinss
}

# График суммарных втутригрупповых расстояний
plot(within_total, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number clusters", ylab = "total within sum of square",
     main = "Accuracy Rate vs Clusters")
