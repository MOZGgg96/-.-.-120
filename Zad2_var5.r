library(factoextra)
library(readr)
library(dplyr)
#считали таблицу
table = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1",
                 skip=1, comment = "[")
#убрали самую первую строку значений, тк она пустая
table = table[-1,]
#приравняли значения -9999 к NA
table[table == -9999] = NA
#оставили строки дневные
table <- table %>% filter(daytime == "T")
#оставили строки 2013 года
table <- table %>% filter(year(date) == 2013)
#оставили строки весны
table <- table %>% filter(month(date) %in% c(03, 04, 05))
#оставили строки только числовые
table = table %>% select(where(is.numeric))
#оставили столбцы без тех что относятся к h2o
table =-table %>% select(-contains("h2o"), h2o_flux)
#создали таблицу var_data со значениями дисперсии каждого показателя
var_data = table %>% summarise_all( ~var(.x,na.rm=T))
#там где результат получился NA, заменяем на 0
var_data[is.na(var_data)] = 0
#создаем таблицу cpa_data используя var_data чтобы исключить показатели с нулевой дисперсией
cpa_data = table[,as.logical(var_data != 0)]
#создали корреляционную таблицу на основе cpa_data исключив столбцы содержащие NA
cor_matrix = cor(na.exclude(cpa_data))
#создали тепловую карту корреляционной матрицы
heatmap(cor_matrix)
#сделали выборку значений из матрицы для H2O
h2o_cor = as.numeric(cor_matrix[82,])
#проименовали эту выборку
names(h2o_cor) = names(cor_matrix[82,])
#выбрали только те значения, что входят в диапазон коэффициента влияния
cpa_dataf = cpa_data[,h2o_cor > 0.35 | h2o_cor < -.35]

#Чтобы не возникало ошибок со специальными символами, переименуем некоторые переменные
table = table %>% rename(z_sub_d__L = `(z-d)/L`)
table = table %>% rename_with( 
  ~gsub("-","_sub_",.x, fixed = T))
table = table %>% rename_with( 
  ~gsub("*","_star",.x, fixed = T))
table = table %>% rename_with( 
  ~gsub("%","_p",.x, fixed = T))
table = table %>% rename_with( 
  ~gsub("/","__",.x, fixed = T))

cpa_dataf = cpa_dataf %>% rename(z_sub_d__L = `(z-d)/L`)
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("-","_sub_",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("*","_star",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("%","_p",.x, fixed = T))
cpa_dataf = cpa_dataf %>% rename_with( 
  ~gsub("/","__",.x, fixed = T))

#произвели анализ основных компонентов
data_pca = prcomp(na.exclude(cpa_dataf),scale=TRUE)
#отобразили розу ветров для влияния различных показателей
fviz_pca_var(data_pca,repel = TRUE, col.var = "steelblue")
#создали модель линейной регрессии исходя из показателей, максимально приближенных к h2o_flux по влиянию
model = lm(h2o_flux ~ flowrate+air_temperature+LE+co2_molar_density+RH, table)
#вывели сводную по получившейся модели
summary(model)
#Далее последовательно несколько раз проводим создания линейных моделей, убирая переменные из формулы, оказывающие наименьшее воздействие на модель 
#Начиная с третьей итерации опираемся на дисперсионный анализ ANOVA для исключения переменных

formula = paste(c("h2o_flux ~ ",
                  paste(names(cpa_dataf)[-40], collapse = "+")),
                collapse = "")
formula = as.formula(formula)


model_first = lm(formula, cpa_dataf)
summary(model_first)
formula2 = formula(h2o_flux ~ Tau + H + rand_err_H + LE + co2_mixing_ratio + air_density + air_molar_volume + RH + 
                     T_star + w__ts_cov + co2_mole_fraction + z_sub_d__L + x_offset + co2_flux + sonic_temperature + 
                     air_temperature + co2...127)
model2 = lm(formula2, cpa_dataf)
summary(model2)
formula3 = formula(h2o_flux ~ H + LE + co2_mixing_ratio + air_density + air_molar_volume + RH + 
                     T_star + w__ts_cov + co2_mole_fraction + x_offset + sonic_temperature + 
                     air_temperature)
model3 = lm(formula3, cpa_dataf)
summary(model3)
anova(model3)
formula4 = formula(h2o_flux ~ H + LE + co2_mixing_ratio + air_density + air_molar_volume + RH + 
                     w__ts_cov + sonic_temperature + air_temperature)
model4 = lm(formula4, cpa_dataf)
summary(model4)
anova(model4)
