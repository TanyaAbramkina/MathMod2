# Абрамкина Татьяна - создайте модель множественной линейной регрессии дневных потоков 
# углекислого газа за осенний период 2013 года по данным измерений методом турбулентной пульсации
library("tidyverse")# целая вселенная
library("readr")# функция read_csv()
library("stringr")# функция str_replace_all
library("dplyr")# функции: filter(),arrange(),select(),mutate(),summarize(),group_by(),sample_n()
library("ggplot2")# графики функций qplot()
# Скачиваем файл онлайн
# Или считываем файл офлайн
# При этом пропускаем первую строку, заменяем все на числовые значения на NA, и игнорируем с символом "["
# eddypro=read_csv("https://www.dropbox.com/s/erhs9hoj4vhr0b/eddypro.csv?dl=1", skip = 1, na = c("","NA","-9999","-9999.0"), comment = c("["))
getwd()
eddypro = read.csv("eddypro.csv", skip = 1, na = c ("","NA","-9999","-9999.0"), comment = c("["))
#
# Блок подготовки данных
#
# Удаляем ненужную пустую первую строку
eddypro = eddypro [-1,]
# Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro,-(roll))
# Преобразуем в факторы (factor) столбы типа char(символ)
eddypro = eddypro %>% mutate_if(is.character,factor)
#Заменим специальные символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")
#Возвратим столбцы таблицы в виде векторов для проверки
glimpse(eddypro)
#Удалим строки в которых содержится NA, так как они содержат неполные данные и только мешают
eddypro = drop_na(eddypro)
# Отфильтруем по заданию данные только за осенний период. С начала сентября (245 день) по конец ноября(335 день)
eddypro = filter(eddypro,DOY >= 245 & DOY < 335)
# Отфильтруем данные по заданию только за дневное время
eddypro = filter(eddypro, daytime ==TRUE)
# Получим таблицу, состоящую только из чисел. Будем работать с ней.
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]
# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]
# Создадим обучающую и тестирующую непересекающиеся выборки с помощью базового функционала
row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]
# Создадим модель добавив в нее все переменные с помощью "(.)" и используя обучающую выборку
mod = lm(co2_flux~ (.) , data = teaching_tbl)
