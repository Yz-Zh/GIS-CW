#3

library(broom)
library(corrr)
library(tidypredict)

##########

#人口密度与well being的关系
Regressiondata<- wardsinfo%>%
  clean_names()%>%
  dplyr::select(wellbeing_2013, 
                population_density_persons_per_sq_km_2013)

model1 <- Regressiondata %>%
  lm(wellbeing_2013 ~
       population_density_persons_per_sq_km_2013,
     data=.)

tidy(model1)
glance(model1)

Regressiondata %>%
  tidypredict_to_column(model1)

##########

#人口密度的残差
model_data <- model1 %>%
  augment(., Regressiondata)

model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

##########

#所有自变量加入的回归
Regressiondata2<- wardsinfo%>%
  clean_names()%>%
  dplyr::select(wellbeing_2013,
                median_household_income_2013,
                population_density_persons_per_sq_km_2013,
                obese_2013_new,
                open_space_with_access_rate_2013)

model2 <- lm(wellbeing_2013 ~ log(median_household_income_2013) + 
               population_density_persons_per_sq_km_2013+
               obese_2013_new+
               open_space_with_access_rate_2013, 
             data = Regressiondata2)

tidy(model2)

#残差图
par(mfrow=c(2,2))
plot(model2)

##########

Correlation <- wardsinfo %>%
  st_drop_geometry()%>%
  dplyr::select(wellbeing_2013,
                median_household_income_2013,
                population_density_persons_per_sq_km_2013,
                open_space_with_access_rate_2013) %>%
  correlate() %>%
  # just focus on GCSE and house prices
  focus(-wellbeing_2013, mirror = TRUE) 


#visualize the correlation matrix
rplot(Correlation)

vif(model2)

position <- c(8:11)

Correlation_all<- wardsinfo %>%
  st_drop_geometry()%>%
  dplyr::select(position)%>%
  correlate()

rplot(Correlation_all)

##########

Regressiondata3<- wardsinfo%>%
  clean_names()%>%
  dplyr::select(wellbeing_2013,
                median_household_income_2013,
                population_density_persons_per_sq_km_2013,
                open_space_with_access_rate_2013)

model3 <- lm(wellbeing_2013 ~ log(median_household_income_2013) + 
               population_density_persons_per_sq_km_2013+
               open_space_with_access_rate_2013, 
             data = Regressiondata3)

tidy(model3)

model_data3 <- model3 %>%
  augment(., Regressiondata3)

wardsinfo <- wardsinfo %>%
  mutate(model3resids = residuals(model3))

#残差图
par(mfrow=c(2,2))
plot(model3)

##########

DW <- durbinWatsonTest(model3)
tidy(DW)


tmap_mode("view")
tm_shape(wardsinfo) +
  tm_polygons("model3resids",
              palette = "RdYlBu")

##########















