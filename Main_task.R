#     Подключение пакета tidyverse для анализа
library(tidyverse)

#     Чтение датасета
data <- read.csv("./dataset_final.csv")


#     Преобразуем citations в числовой формат
data <- data %>%
  mutate(
    citations = as.character(citations),
    citations = as.numeric(ifelse(citations %in% c("", "NA", "none"), NA, citations))
  )


#     Преобразуем is_open_access в числовой формат
data <- data %>%
  mutate(
    is_open_access = as.character(is_open_access),
    is_open_access = as.numeric(ifelse(is_open_access %in% c("", "NA", "none"), NA, is_open_access))
  )

#     Преобразуем experience_years в числовой формат
data <- data %>%
  mutate(
    experience_years = as.character(experience_years),
    experience_years = as.numeric(ifelse(experience_years %in% c("", "NA", "none"), NA, experience_years))
  )

#     Преобразуем co_authors в числовой формат
data <- data %>%
  mutate(
    co_authors = as.character(co_authors),
    co_authors = as.numeric(ifelse(co_authors %in% c("", "NA", "none"), NA, co_authors))
  )

#     Преобразуем year в числовой формат
data <- data %>%
  mutate(
    year = as.character(year),
    year = as.numeric(ifelse(year %in% c("", "NA", "none"), NA, year))
  )

#     Добавляем новую логическую переменную succecssful 
#     Если цитирования выше медианного значения за год → публикация успешная
data <- data %>%
  group_by(year) %>%
  mutate(median_citations_year = median(citations)) %>%  # находим медиану по году
  ungroup() %>%
  mutate(successful = ifelse(citations > median_citations_year, 1, 0)) %>%  # успешность
  select(-median_citations_year)  # убираем вспомогательный столбец

#     Добавляем новую переменную co-autors - уровень кооперации 
#     Значение переменной зависит от числа соавторов
data <- data %>%
  mutate(
    collaboration_level = case_when(
      co_authors <= 2 ~ "Low",
      co_authors <= 5 ~ "Medium",
      co_authors > 5 ~ "High"
    ),
    collaboration_level = factor(collaboration_level, levels = c("Low", "Medium", "High"))
  )

#     Добавляем переменную is_senior
#     Если у автора есть 15 лет стажа и PhD → считаем его старшим
data <- data %>%
  mutate(is_senior = ifelse(experience_years > 15 & has_phd == 1, TRUE, FALSE))


#     Строим модель условной стоимости публикации
#     Базовая стоимость в зависимости от квартиля
base_cost <- case_when(
  data$quartile == "Q1" ~ 100000,
  data$quartile == "Q2" ~ 80000,
  data$quartile == "Q3" ~ 50000,
  data$quartile == "Q4" ~ 30000,
  TRUE ~ 10000
)


#     Добавляем корректировки:
#     +30% за Open Access
data$cost_final <- base_cost * 
  (1 + 0.3 * data$is_open_access)

#     Выводим первые строки для проверки
head(data)

#     Сохраняем в CSV, чтобы можно было загрузить позже
write.csv(data, "scientific_publications_dataset.csv", row.names = FALSE)


#     Начинаем визуализацию и анализ
#     Гистограмма: количество публикаций по годам
ggplot(data, aes(x = year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Количество публикаций по годам", x = "Год", y = "Число публикаций")

#     Boxplot: цитируемость по квартилям
#     Подготовим таблицу со средними по квартилям
mean_by_quartile <- data %>%
  group_by(quartile) %>%
  summarise(mean_citations = mean(citations, na.rm = TRUE))

#     Красивый график
ggplot(data, aes(x = quartile, y = citations)) +
  geom_boxplot(fill = "#6CBF91", color = "black", outlier.alpha = 0.3) +
  geom_point(data = mean_by_quartile, aes(x = quartile, y = mean_citations), 
             color = "red", shape = 23, fill = "white", size = 3, stroke = 1.5) +
  scale_y_log10() +
  labs(
    title = "Цитируемость по квартилям журналов (медиана и среднее)",
    subtitle = "Красные точки — среднее значение цитирований в квартиле",
    x = "Квартиль журнала",
    y = "Цитирования"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold")
  )
#     Диаграмма: влияние числа авторов
ggplot(data, aes(x = collaboration_level)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Частота публикаций по уровню кооперации",
    x = "Уровень кооперации",
    y = "Число публикаций"
  ) +
  theme_minimal()

#     Рассчитываем метрики успешности
#     Доля успешных публикаций
success_rate <- mean(data$successful, na.rm = TRUE)
cat("🔹 Доля успешных публикаций:", round(success_rate * 100, 2), "%\n")


#     Рассчитываем общие расходы института
total_cost <- sum(data$cost_final, na.rm = TRUE)
cat("🔹 Общие затраты института на все публикации: ", format(total_cost, big.mark = " "), "₽\n")

#     Средняя стоимость успешной публикации
successful_pubs <- filter(data, successful == 1)
mean_cost_successful <- mean(successful_pubs$cost_final, na.rm = TRUE)
cat("🔹 Средняя стоимость успешной публикации: ", format(mean_cost_successful, big.mark = " "), "₽\n")

#     Эффективность: число успешных публикаций на 100 000 ₽
efficiency <- nrow(successful_pubs) / total_cost * 100000
cat("🔹 Эффективность: ", round(efficiency, 2), " публикаций на 100 000 рублей\n")


#     Зависимость успеха от возраста автора
# Строим среднюю успешность по стажу (binned)
data %>%
  # Удаляем NA в нужных столбцах
  filter(!is.na(experience_years), !is.na(successful)) %>%
  
  # Группируем стаж на интервалы
  mutate(
    experience_group = cut(experience_years,
                           breaks = c(0, 5, 10, 15, 20, 30),
                           labels = c("0–5", "6–10", "11–15", "16–20", "21+"),
                           include.lowest = TRUE)
  ) %>%
  
  # Группировка и расчёт доли успешных публикаций
  group_by(experience_group) %>%
  summarise(success_rate = mean(successful, na.rm = TRUE)) %>%
  
  # Построение графика
  ggplot(aes(x = experience_group, y = success_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Доля успешных публикаций по стажу автора",
    x = "Стаж автора (лет)",
    y = "Доля успешных публикаций"
  ) +
  theme_minimal() +
  ylim(0, 1)


