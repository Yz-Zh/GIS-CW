#4

library(spatialreg)
library(spgwr)

##########

geocenter <- wardsinfo%>%
  st_centroid()%>%
  st_geometry()
LWard_nb <- wardsinfo %>%
  poly2nb(., queen=T)

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")

##########

knn_wards <-geocenter %>%
  knearneigh(., k=4)
LWard_knn <- knn_wards %>%
  knn2nb()

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

##########

Queen <- wardsinfo %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()


Nearest_neighbour <- wardsinfo %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Queen

##########
##########
##########

myvars <- wardsinfo %>%
  dplyr::select(wellbeing_2013,
                population_density_persons_per_sq_km_2013,
                median_household_income_2013,
                open_space_with_access_rate_2013)

#check their correlations are OK
Correlation_myvars <- myvars %>%
  st_drop_geometry()%>%
  correlate()

#run a final OLS model
model_final <- lm(wellbeing_2013~
                  population_density_persons_per_sq_km_2013+
                  median_household_income_2013+
                  open_space_with_access_rate_2013,
                  data = myvars)

tidy(model_final)

wardsinfo <- wardsinfo %>%
  mutate(residuals = residuals(model_final))

par(mfrow=c(2,2))
plot(model_final)

qtm(wardsinfo, fill = "residuals", alpha=0.2)

##########

final_model_Moran <- wardsinfo %>%
  st_drop_geometry()%>%
  dplyr::select(residuals)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

final_model_Moran

##########

st_crs(wardsinfo) = 27700
wardsinfosp <- wardsinfo %>%
  as(., "Spatial")

st_crs(geocenter) = 27700
geocentersp <- geocenter %>%
  as(., "Spatial")

##########

bandwidth <- gwr.sel(wellbeing_2013~
                       population_density_persons_per_sq_km_2013+
                       median_household_income_2013+
                       open_space_with_access_rate_2013,
                        data = wardsinfosp, 
                        coords=geocentersp,
                        adapt=T)

gwr.model = gwr(wellbeing_2013~
                  population_density_persons_per_sq_km_2013+
                  median_household_income_2013+
                  open_space_with_access_rate_2013, 
                data = wardsinfosp, 
                coords=geocentersp, 
                adapt=bandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

results <- as.data.frame(gwr.model$SDF)
#names(results)

wardsinfo <- wardsinfo %>%
  mutate(coefincome = (results$median_household_income_2013)*1000,
         coefpopdens = (results$population_density_persons_per_sq_km_2013)*1000,
         coefopenspac = results$open_space_with_access_rate_2013,
         )

tm_shape(wardsinfo) +
  tm_polygons(col = "coefincome", 
              palette = "RdYlBu", 
              alpha = 0.65)

tm_shape(wardsinfo) +
  tm_polygons(col = "coefpopdens", 
              palette = "RdYlBu", 
              alpha = 0.65)

tm_shape(wardsinfo) +
  tm_polygons(col = "coefopenspac", 
              palette = "RdYlBu", 
              alpha = 0.65)

################

#Sig 检验
sigTest1 = abs(gwr.model$SDF$"population_density_persons_per_sq_km_2013")-2 * gwr.model$SDF$"population_density_persons_per_sq_km_2013_se"
wardsinfo <- wardsinfo %>%
  mutate(popdensitySig = sigTest1)
tm_shape(wardsinfo) +
  tm_polygons(col = "popdensitySig", 
              palette = "RdYlBu")



sigTest2 = abs(gwr.model$SDF$"open_space_with_access_rate_2013")-2 * gwr.model$SDF$"open_space_with_access_rate_2013_se"
wardsinfo <- wardsinfo %>%
  mutate(openspaceSig = sigTest2)
tm_shape(wardsinfo) +
  tm_polygons(col = "openspaceSig", 
              palette = "RdYlBu")



sigTest3 = abs(gwr.model$SDF$"median_household_income_2013")-2 * gwr.model$SDF$"median_household_income_2013_se"
wardsinfo <- wardsinfo %>%
  mutate(incomeSig = sigTest3)
tm_shape(wardsinfo) +
  tm_polygons(col = "incomeSig", 
              palette = "RdYlBu")