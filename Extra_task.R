#     Подключение библиотек
library(tidyverse)
library(corrplot)  
library(grid)

#     Чтение датасета
data <- read.csv("./scientific_publications_dataset.csv") # датасет, полученный в результате выполнения основной задачи (см. файл Main_task.R)



#     Выбираем данные об авторах и их признаках
author_data <- data %>%
  filter(!is.na(successful)) %>%
  select(
    successful,
    experience_years,
    co_authors,
    has_phd,
    is_open_access
  ) %>%
  mutate(
    has_phd = as.numeric(has_phd),
    is_open_access = as.numeric(is_open_access),
  )


#     Преобразуем факторы в числа (0/1 или уровни)
author_data <- author_data %>%
  mutate(
    has_phd = as.numeric(has_phd),  # если был TRUE/FALSE или 0/1
    is_open_access = as.numeric(is_open_access),
  )


#     Вычисляем корреляцию
cor_matrix <- cor(author_data, use = "complete.obs", method = "pearson")

#     Выводим на экран округлённо
print(round(cor_matrix, 2))

#     Построение корреляционной матрицы без заголовка внутри corrplot
corrplot::corrplot(
  cor_matrix,
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  mar = c(0, 0, 2, 0)  # оставляем место сверху (bottom, left, top, right)
)

#     Добавляем заголовок отдельно, иначе он не влезает
grid.text("Корреляция между признаками авторов и успешностью публикаций",
          x = 0.5, y = 0.98, gp = gpar(fontsize = 12, fontface = "bold"))


#     Успешность по наличию учёной степени
#     Рассчитываем среднюю цитируемость по наличию PhD
success_by_phd <- data %>%
  group_by(has_phd = ifelse(has_phd == 1, "Есть PhD", "Нет PhD")) %>% 
  summarise(
    avg_citations = mean(citations, na.rm = TRUE),
    .groups = 'drop'
  )

#     Строим график
ggplot(success_by_phd, aes(x = has_phd, y = avg_citations, fill = has_phd)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Нет PhD" = "#FFA07A", "Есть PhD" = "#6CBF91")) + # мягкие приятные цвета
  labs(
    title = "Средняя цитируемость по наличию учёной степени",
    x = "Учёная степень",
    y = "Среднее число цитирований"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"  # скрываем легенду — она лишняя тут
  ) +
  ylim(0, max(success_by_phd$avg_citations) * 1.1)

#     Зависимость успешности публикации от стажа автора
#     Делим стаж на группы
data_grouped <- data %>%
  mutate(
    exp_group = cut(experience_years, 
                    breaks = c(0, 5, 10, 15, 20, 30),
                    labels = c("0–5", "6–10", "11–15", "16–20", "21+"),
                    include.lowest = TRUE)
  ) %>%
  group_by(exp_group) %>%
  summarise(
    success_rate = mean(successful, na.rm = TRUE)
  )

#     График
ggplot(data_grouped, aes(x = exp_group, y = success_rate)) +
  geom_bar(stat = "identity", fill = "#6CBF91") +
  labs(
    title = "Успешность публикаций по стажу автора",
    x = "Стаж автора (лет)",
    y = "Доля успешных публикаций"
  ) +
  theme_minimal()


#     Зависимость успешности публикации от открытого доступа к статье
#     Убираем строки с NA в is_open_access перед группировкой
success_by_open_access <- data %>%
  filter(!is.na(is_open_access)) %>% 
  group_by(is_open_access) %>%
  summarise(
    total_pubs = n(),
    successful_pubs = sum(successful, na.rm = TRUE),
    success_rate = successful_pubs / total_pubs
  ) %>%
  mutate(
    label = ifelse(is_open_access == 1, "Open Access", "Closed Access")
  )

#     Строим график
ggplot(success_by_open_access, aes(x = label, y = success_rate, fill = label)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Доля успешных публикаций по наличию Open Access",
    x = "Тип доступа",
    y = "Доля успешных публикаций"
  ) +
  scale_fill_manual(values = c("Open Access" = "#6CBF91", "Closed Access" = "#FFA07A")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  ylim(0, max(success_by_open_access$success_rate) * 1.1)





