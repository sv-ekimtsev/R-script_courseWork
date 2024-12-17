# Определим диапазон значений для x1 и x2
x1_vals <- seq(-2, 5, length.out = 100)  # Увеличиваем диапазон для оси X
x2_vals_1 <- (15 - 3 * x1_vals) / 5 
x2_vals_2 <- (15 - 5 * x1_vals) / 3

# Построение графиков ограничений с увеличенным масштабом
plot(x1_vals, x2_vals_1, type = "l", col = "blue", xlab = "X", ylab = "Y", xlim = c(-1, 4), ylim = c(-1, 4), cex.lab = 1, cex.axis = 1, lwd = 2)
lines(x1_vals, x2_vals_2, col = "red", lwd = 2)  # Увеличили толщину линии
abline(h = 0, v = 0, col = "#0000FF", lwd = 2)  # Увеличили толщину осей

# Заштриховываем область ОДЗ с координатами (0,0), (0,3), (1.875,1.875), (3,0)
polygon(c(0, 0, 1.875, 3), c(0, 3, 1.875, 0), col = "#FF00B3", border = "#FF00B3", density = 10, angle = 45, lwd =3)  # Розовая штриховка


# Добавляем подписи к точкам
text(0, 0, "(0, 0)", pos = 1, cex = 1.2, col = "black")
text(0, 3, "(0, 3)", pos = 4, cex = 1.2, col = "black")
text(1.875, 1.875, "(1.875, 1.875)", pos = 4, cex = 1.2, col = "black")
text(3, 0, "(3, 0)", pos = 1, cex = 1.2, col = "black")
text(1, 1, "(1, 1)", pos = 1, cex = 1.2, col = "black")






# Построение графиков целевой функции (семейство окружностей с центром в (1, 1))
for (r in seq(1, 10, by = .61)) {  # Увеличили шаг на 0.7
  # Параметр r — радиус окружности, равный sqrt(F)
  theta <- seq(0, 2*pi, length.out = 100)
  x_circle <- 1 + r * cos(theta)  # Центр окружности в (1, 1)
  y_circle <- 1 + r * sin(theta)
  lines(x_circle, y_circle, col = "#0000FF", lty = 2, lwd = 2) # Увеличили толщину окружностей
}

# Определение целевой функции
objective_function <- function(x1, x2) {
  return((x1 + 1)^2 + (x2 - 1)^2)
}

# Максимизация функции цели
max_F <- -Inf
best_x1 <- 0
best_x2 <- 0
for (x1 in seq(0, 5, 0.1)) {
  for (x2 in seq(0, 5, 0.1)) {
    if (3*x1 + 5*x2 <= 15 && 5*x1 + 3*x2 <= 15) {
      F_value <- objective_function(x1, x2)
      if (F_value > max_F) {
        max_F <- F_value
        best_x1 <- x1
        best_x2 <- x2
      }
    }
  }
}

# Нарисуем точку оптимального решения
points(1,1 , col = "black", pch = 19)
points(0,0 , col = "#FF00B3", pch = 19)
points(0,3 , col = "#FF00B3", pch = 19)
points(3,0 , col = "#FF00B3", pch = 19)
points(1.875,1.875 , col = "#FF00B3", pch = 19)


# Результаты
cat("Оптимальное значение x1:", best_x1, "\n")
cat("Оптимальное значение x2:", best_x2, "\n")

