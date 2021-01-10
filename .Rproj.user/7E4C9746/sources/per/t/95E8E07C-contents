#1

library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)

##########

#载入wards地图轮廓
wardsmap <- dir_info(here::here("original-data", 
                                 "statistical-gis-boundaries-london", 
                                 "ESRI"))%>%
  
  dplyr::filter(str_detect(path, 
                           "London_Ward_CityMerged.shp$"))%>%
  dplyr::select(path)%>%
  
  pull()%>%
  st_read()

#查看地图
#tmap_mode("view")
#qtm(wardsmap)

##########

#载入wards的wellbeing情况
wardswellbeing <- read_csv(here::here("processed_data_new",
                                 "wellbeing.csv"), 
                      na = c("", "NA", "n/a"), 
                      locale = locale(encoding = 'Latin1'), 
                      col_names = TRUE)


#将wards的信息与地图结合
wardsinfo <- wardsmap%>%
  left_join(.,
            wardswellbeing, 
            by = c("GSS_CODE" = "Ward Code"))%>%
  clean_names()

#检查wellbeing的结合情况
#tmap_mode("view")
#qtm(wardsinfo, 
#    fill = "wellbeing_2013", 
#    borders = NULL,  
#    fill.palette = "Blues")


##########

#载入wards的2013年家庭收入中位数情况文件
wardsincome <- read_csv(here::here("processed_data_new",
                                   "income.csv"))
#将家庭收入加入wardsinfo
wardsinfo <- wardsinfo%>%
  left_join(.,
            wardsincome, 
            by = c("gss_code" = "Code"))%>%
  clean_names()

#检查household income的结合情况
#tmap_mode("view")
#qtm(wardsinfo, 
#    fill = "Median_household_income_2013", 
#    borders = NULL,  
#    fill.palette = "Greens")

##########

#载入wards的2013年open space and access rate的文件
wardsossrate <- read_csv(here::here("processed_data_new",
                                   "space.csv"))
#将open space and access rate加入wardsinfo
wardsinfo <- wardsinfo%>%
  left_join(.,
            wardsossrate, 
            by = c("gss_code" = "Ward_GSS_CODE"))%>%
  clean_names()

#检查open space and access rate的结合情况
#tmap_mode("view")
#qtm(wardsinfo, 
#    fill = "Open Space with access rate 2013", 
#    borders = NULL,  
#    fill.palette = "Greens")

##########

#载入wards的2013年人口密度情况文件
wardspopden <- read_csv(here::here("processed_data_new",
                                   "density.csv"))
#将人口密度加入wardsinfo
wardsinfo <- wardsinfo%>%
  left_join(.,
            wardspopden, 
            by = c("gss_code" = "New code"))%>%
  clean_names()

#检查人口密度的结合情况
#tmap_mode("view")
#qtm(wardsinfo, 
#    fill = "Population density persons per sq km - 2013", 
#    borders = NULL,  
#    fill.palette = "Reds")

##########

#载入wards的2013年小学生肥胖情况文件
wardsobeschool <- read_csv(here::here("processed_data_new",
                                   "obesity.csv"))
#将人口密度加入wardsinfo
wardsinfo <- wardsinfo%>%
  left_join(.,
            wardsobeschool, 
            by = c("gss_code" = "Code"))%>%
  clean_names()

#检查小学生肥胖的结合情况

#tmap_mode("view")
#qtm(wardsinfo, 
#    fill = "obese_2013", 
#    borders = NULL,  
#    fill.palette = "Blues")


##########

##完成数据载入##













