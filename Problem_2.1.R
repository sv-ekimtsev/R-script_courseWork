# Задание ограничений
library(ggplot2)

# Функция для отображения линии ограничений
plot_constraint <- function(a, b, c, x_range) {
  x <- seq(0, x_range, by = 0.1)  # Ограничим x_range
  y <- (c - a * x) / b
  y <- pmax(y, 0)  # Убираем отрицательные значения y
  return(data.frame(x, y))
}

# Ограничения
constraint1 <- plot_constraint(2, 3, 720, 350)      # 2x1 + 3x2 <= 720
constraint2 <- plot_constraint(2.7, 3, 780, 350)    # 2.7x1 + 3x2 <= 780
constraint3 <- plot_constraint(1, 1.2, 324, 350)    # x1 + 1.2x2 <= 324

# Точки для области допустимых значений
polygon_data <- data.frame(
  x = c(0, 0, 85.71, 288.89), 
  y = c(0, 240, 182.86, 0)
)

# Функция для построения контурных линий целевой функции
plot_objective_contours <- function(a, b) {
  x_vals <- seq(0, 350, by = 1)  # Уменьшаем шаг для увеличения количества контурных линий
  y_vals <- seq(0, 350, by = 1)  # Уменьшаем шаг для увеличения количества контурных линий
  
  # Целевая функция F = 160x1 + 210x2
  F_vals <- outer(x_vals, y_vals, FUN = function(x1, x2) 160 * x1 + 210 * x2)
  
  contour_data <- data.frame(
    x = rep(x_vals, length(y_vals)),
    y = rep(y_vals, each = length(x_vals)),
    z = as.vector(F_vals)
  )
  
  return(contour_data)
}

# Строим график
ggplot() +
  # Ограничения
  geom_line(data = constraint1, aes(x = x, y = y), color = "red", size = 1) + 
  geom_line(data = constraint2, aes(x = x, y = y), color = "blue", size = 1 ) + 
  geom_line(data = constraint3, aes(x = x, y = y), color = "green", size = 1) +
  annotate("text", x = 250, y = 100, label = "2x1 + 3x2 = 720", color = "red", size = 5) + 
  annotate("text", x = 250, y = 140, label = "2.7x1 + 3x2 = 780", color = "blue", size = 5) + 
  annotate("text", x = 150, y = 200, label = "x1 + 1.2x2 = 324", color = "green", size = 5) + 
  xlim(0, 350) +
  ylim(0, 300) +  # Увеличиваем пределы оси x2
  labs(x = "x1", y = "x2") +
  theme_minimal() +
  theme(
    #panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Основная сетка
    #panel.grid.minor = element_line(color = "lightgray", size = 0.25), # Мелкая сетка
    #panel.grid.major.x = element_line(color = "lightgray", size = 0.5), # Вертикальная сетка
    #panel.grid.major.y = element_line(color = "lightgray", size = 0.5)  # Горизонтальная сетка
  ) +
  ggtitle("Графическое решение задачи линейного программирования") +
  geom_rug(sides = "b") + 
  geom_rug(sides = "l") +
  # Добавляем линии для ограничений по x1 и x2 (толще и линейные)
  geom_vline(xintercept = 0, size = 1, color = "black") +  # x1 >= 0
  geom_hline(yintercept = 0, size = 1, color = "black") +  # x2 >= 0
  # Добавляем область допустимых значений с фиолетовой границей
  geom_polygon(data = polygon_data, aes(x = x, y = y), fill = "#ff00c0", alpha = 0.2, color = "#ff00c0", size = 1.2) +
  # Добавляем контурные линии целевой функции (dashed)
  geom_contour(data = plot_objective_contours(160, 210), aes(x = x, y = y, z = z), color = "black", size = .5, linetype = "dashed") +
  # Добавляем точки полигона
  geom_point(data = polygon_data, aes(x = x, y = y), color = "black", size = 3)