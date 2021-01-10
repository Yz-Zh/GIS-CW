#2


##########

#wellbeing（因变量y）的分布
ggplot(wardsinfo, aes(x=wellbeing_2013)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 3) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)
#非常好！是正态分布！

##########

#income（自变量x1）的分布
ggplot(wardsinfo, aes(x=median_household_income_2013)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1000) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)

#还行,但需要log-transformation
ggplot(wardsinfo, aes(x=log(median_household_income_2013))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.025) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)
#好些

##########

#open space & access rate（自变量x2）的分布
ggplot(wardsinfo, aes(x=open_space_with_access_rate_2013)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 2.5) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)

#相当差劲！急需数据转换！这次试试squre
ggplot(wardsinfo, aes(x=(open_space_with_access_rate_2013)^(1/2)/5)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)
#好极了！

##########

#population density（自变量x3）的分布
ggplot(wardsinfo, aes(x=population_density_persons_per_sq_km_2013)) + 
  geom_histogram(aes(y = ..density..),
                  binwidth = 500) + 
  geom_density(colour="blue", 
                size=1, 
                adjust=1)

#同自变量2，squre
ggplot(wardsinfo, aes(x=(population_density_persons_per_sq_km_2013)^(1/2)/100)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)
#可以

##########

#obesity rate（自变量x4）的分布
ggplot(wardsinfo, aes(x=obese_2013_new)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.01) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)

#偏右,1.5次方
ggplot(wardsinfo, aes(x=(obese_2013_new)^(1.5)*10)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="blue", 
               size=1, 
               adjust=1)
#差强人意

##########








