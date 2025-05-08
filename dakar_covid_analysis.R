rm(list=ls())
library(dplyr)
library(INLA)
library(tidyr)
#library(devtools)
#install_github('timcdlucas/INLAutils', force=TRUE)

library(INLAutils)
#library(maptools)
library(mapview)
#library(osmdata)
library(raster)
#library(rgdal)
library(sf)
library(sp)
library(tidyverse)
library(ggplot2)
#install.packages("season")
library(season)
#install.packages("COVID19.analytics")
library(exactextractr)

# load covid data
#cd <- read.csv("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covid_neigh.csv")
cd <- read.csv("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covid_neigh2.csv")
#cd <- read.csv("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covid_neigh_dpg.csv")
#cd <- read.csv("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/base_covid_final.csv")
#cd <- read.csv("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/base_covid_lebon.csv")
head(cd)
cd$infection <- cd$NOMBRE.DE.CAS.ISSUS.DE.LA.TRANSMISION.COMMUNAUTAIRE
#cd <- cd[cd$dept!="RUFISQUE",]

#str(cd)
#unique(cd$dept)
#unique(cd$neighbourhood)
#cd <- cd[cd$neighbourhood!="RUFISQUE",]

# Transformer la date de l'infection en format date
covid_dat <- cd %>%
  as_tibble() %>% 
  mutate(H_Zone = Code,
         date_infection = as.Date(as.character(DATE), format = "%d/%m/%Y"),
         month = as.numeric(substr(date_infection, 6, 7)),
         year = as.numeric(substr(date_infection, 1, 4)),
         n_index = as.character(substr(H_Zone, 1,2)),
         n_idx = as.character(substr(H_Zone, 8,12)),
         neigh_index = paste0(n_index,n_idx),
         timeperiod = paste0(month, year)) %>%
  arrange(date_infection) %>% 
  glimpse()

# Extract month and year from date
#covid_dat$month <- format(covid_dat$date_infection, "%m")
#covid_dat$year <- format(covid_dat$date_infection, "%Y")

# Aggregate infection by H_zone, timeperiod, year, and month
aggregated_df <- aggregate(infection ~ H_Zone + Code + year + month + date_infection + neigh_index, covid_dat, sum)
aggregated_df

tail(aggregated_df)

# load the shapefile
dkr_shp <- st_read("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/Dakar_quartiers.shp")
st_make_valid(dkr_shp)
names(dkr_shp)
dkr_shp<-dkr_shp[dkr_shp$Department!="RUFISQUE",]
dkr_shp<-dkr_shp[dkr_shp$Area4!="Goree",]
dkr_shp<-dkr_shp[dkr_shp$Area5!="ILE DE NGOR",]
plot(st_geometry(dkr_shp))


#st_write(dkr_shp, "C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/Dk_pk_gu_quartiers.shp")

# load rasters

# extract covariates
built_areas <- raster("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covs/dkr_built_up.tif")
pop <- raster("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covs/dakar10.tif")
residential <- raster("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covs/residential.tif")
acs <- raster("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covs/acs.tif")
market_dist <- raster("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covs/SEN_DT_Marketplaces.tif")
temperature <- raster("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covs/SEN_grid_100m_wclim_temp.tif")
#
dkr_shp$built <- exact_extract(built_areas, dkr_shp, 'sum')
dkr_shp$res <- exact_extract(residential, dkr_shp, 'mean')
dkr_shp$acs <- exact_extract(acs, dkr_shp, 'mean')
dkr_shp$pop <- exact_extract(pop, dkr_shp, 'sum')
dkr_shp$mkt <- exact_extract(market_dist, dkr_shp, 'mean')
dkr_shp$tmp <- exact_extract(temperature, dkr_shp, 'mean')


#dkr_shp$area <- st_area(dkr_shp)
#dkr_shp$area_ha <- set_units(dkr_shp$area, ha)
dkr_shp$built_density <- dkr_shp$built/dkr_shp$Area
dkr_shp$population_density <- dkr_shp$pop/dkr_shp$built_density

#write.csv(st_drop_geometry(dkr_shp), 
#          file = "C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covs_covid_data.csv",
#          row.names = FALSE)


# Add all health zone names or, Health zone id
H_Zones <- dkr_shp$Code
#residential <- dkr_shp$res
#acs <- dkr_shp$acs
#gridded_pop <- dkr_shp$pop

# Add years and months
years <- c(2020, 2021)
months <- 1:12

# Create a complete table for all spatial and temporal combinations
complete_data <- expand.grid(H_Zone = H_Zones, year = years, month = months)

# Merge the complete table with your original data where there is data available
final_data <- merge(complete_data, aggregated_df, by = c("H_Zone", "year", "month"), all = TRUE)
summary(final_data)
final_data$Code <- final_data$H_Zone

# Replace NA values in 'infec' column with 0
final_data$infection[is.na(final_data$infection)] <- 0

# If you want to filter the data for specific months for 2021 or other years (e.g., months 1-6)
# As you mentioned 15 months from 2020, so I did like this 
# 2020 - 12 months and 
# 2021 - 6 months
#final_data <- subset(final_data, !(year == 2021 & month > 7))
#final_data <- subset(final_data, !(year == 2020 & month < 5))

# Final dataframe
#print(final_data)

# subset covs
names(dkr_shp)


covs <- dkr_shp[,c("Code","res","acs","pop","built_density","population_density","mkt","tmp")]
final_data2 <- merge(final_data, covs, by = c("H_Zone" =  "Code"), all.x = TRUE)
#final_data2 <- full_join(final_data, covs, by = c("H_Zone" =  "Code"))


#
#write.csv(st_drop_geometry(final_data2), 
#          file = "C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covid_data.csv",
#          row.names = FALSE)

#dkr_map <- full_join(dkr_shp, aggregated_df, by = c("Code" = "H_Zone"))
#st_write(dkr_map, "C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/Dk_pk_gu_map.shp")

#write.csv(st_drop_geometry(df_dat2), 
#          file = "C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/df_dat2.csv",
#          row.names = FALSE)
#
##----Extract the coordinates
dkr_shp2 <- as(dkr_shp, "Spatial")
coords = coordinates(dkr_shp2)
dkr_shp2$longitude = coords[,1]
dkr_shp2$latitude = coords[,2]

#names(dkr_shp2)

dkr_coords <- dkr_shp2 %>% 
  as_tibble() %>% 
  dplyr::select(Code,longitude,latitude)

df_dat2 <- merge(final_data2, dkr_coords, by = c("H_Zone" =  "Code"), all.x = TRUE)
df_dat2$rw1_month_all <- df_dat2$month
df_dat2$infection

plot(dkr_shp2)
points(coords, col='red')

# split the data into training and prediction set
df_dat2$infection[df_dat2$month == 6 & df_dat2$year == 2021] = NA
df_dat2$infection[df_dat2$month == 7 & df_dat2$year == 2021] = NA

df_fit <- df_dat2[!is.na(df_dat2$infection),]
df_pred <- df_dat2[is.na(df_dat2$infection),]


coords_fit = as.matrix(df_fit[,16:17])
coords_pred = as.matrix(df_pred[,16:17])


summary(df_dat2)
df_dat2$res[is.na(df_dat2$res)] <- 0
df_dat2$acs[is.na(df_dat2$acs)] <- 0
df_dat2$pop[is.na(df_dat2$pop)] <- 0
df_dat2$built_density[is.na(df_dat2$built_density)] <- 0
df_dat2$population_density[is.na(df_dat2$population_density)] <- 0
df_dat2$mkt[is.na(df_dat2$mkt)] <- 0
df_dat2$tmp[is.na(df_dat2$tmp)] <- 0
df_dat2$longitude[is.na(df_dat2$longitude)] <- 0
df_dat2$latitude[is.na(df_dat2$latitude)] <- 0


names(df_dat2)
coords_dat2 = as.matrix(df_dat2[,16:17])

### SPDE Model
### SPDE Model
#-------Extract boundary for mesh
bdry <- inla.sp2segment(dkr_shp2)
bdry$loc <- inla.mesh.map(bdry$loc)

###----Build non-hull mesh
non_convex_bdry <- inla.nonconvex.hull(coords, -0.07, -0.004, resolution = c(100, 100))
plot(non_convex_bdry)
plot(dkr_shp2, add = TRUE)
points(coords, pch = 19, col='red', cex = 0.5)

max.edge = max(diff(range(dkr_coords$longitude)), diff(range(dkr_coords$latitude)))/15

mesh <- inla.mesh.2d( 
  boundary = non_convex_bdry,
  coords_dat2,
  max.edge = c(0.05, 0.1),
  offset = c(0.01,0.05),
  cutoff = 0.01)



plot(mesh)
plot(dkr_shp2, add = TRUE)
points(coords, pch = 19, col='red', cex = 0.5)


summary(df_dat2)

spde12 <- inla.spde2.pcmatern(mesh, constr = TRUE, 
                              prior.range = c(0.01, 0.01), 
                              prior.sigma = c(1, 0.1))

#A12_dat <- inla.spde.make.A(mesh, loc = coords_dat2)
A12_fit <- inla.spde.make.A(mesh, loc = coords_fit)
A12_pred <- inla.spde.make.A(mesh, loc = coords_pred)

idx_dat <- inla.spde.make.index('i', n.spde = spde12$n.spde)

stk.fit <- inla.stack(data=list(y = df_fit$infection), A = list(A12_fit, 1), tag = 'dat',
                      effects=list(idx_dat, 
                                   data.frame(Intercept = 1,
                                              mean_residential = df_fit$res,
                                              mean_acs = df_fit$acs,
                                              month_number = df_fit$month,
                                              rw1_month_all = df_fit$rw1_month_all,
                                              population_density = df_fit$ppltn_d,
                                              built_density = df_fit$blt_dns,
                                              neigh_id_index = df_fit$neigh_index,
                                              mean_mkt = df_fit$mkt,
                                              mean_tmp = df_fit$tmp
                                   )))


stk.pred <- inla.stack(data=list(y = df_pred$infection), A = list(A12_pred, 1), tag = 'pred',
                       effects=list(idx_dat, 
                                    data.frame(Intercept = 1,
                                               mean_residential = df_pred$res,
                                               mean_acs = df_pred$acs,
                                               month_number = df_pred$month,
                                               rw1_month_all = df_pred$rw1_month_all,
                                               population_density = df_pred$ppltn_d,
                                               built_density = df_pred$blt_dns,
                                               neigh_id_index = df_pred$neigh_index,
                                               mean_mkt = df_pred$mkt,
                                               mean_tmp = df_pred$tmp
                                    )))

stk_all <- inla.stack.join(stk.fit,stk.pred)


## Formula for INLA model
formula12_fit = y ~ 0 + Intercept +   
  f(neigh_id_index, model = "iid",
    hyper = list( prec = list( prior = "pc.prec", param = c(0.5, 0.01)))) +
  f(rw1_month_all, model = "rw1", scale.model = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) +
  factor(month_number) +
  mean_residential + 
  mean_acs +
  population_density +
  built_density +
  mean_mkt +
  mean_tmp +
  f(i, model = spde12)


## Execute INLA
M12_fit = inla(formula12_fit, family = 'zeroinflatedpoisson1',
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
               inla.mode = "experimental",
               data = inla.stack.data(stk_all), verbose=TRUE, 
               control.predictor = list(A = inla.stack.A(stk_all), compute = TRUE, link = 1))

































#-------Extract boundary for mesh
bdry <- inla.sp2segment(dkr_shp2)
bdry$loc <- inla.mesh.map(bdry$loc)

###----Build non-hull mesh
non_convex_bdry <- inla.nonconvex.hull(coords, -0.05, -0.05, resolution = c(100, 100))

max.edge = max(diff(range(dkr_coords$longitude)), diff(range(dkr_coords$latitude)))/30

mesh <- inla.mesh.2d(boundary = non_convex_bdry, max.edge = c(max.edge, max.edge), 
                     offset = c(max.edge/2, max.edge),
                     cutoff = max.edge/10)

mesh$n
plot(mesh)

#names(df_dat2)
df_dat2$infection[df_dat2$month == 6 & df_dat2$year == 2021] = NA
df_dat2$infection[df_dat2$month == 7 & df_dat2$year == 2021] = NA

df_fit <- df_dat2[!is.na(df_dat2$infection),]
df_pred <- df_dat2[is.na(df_dat2$infection),]

names(df_fit)
coords_fit = as.matrix(df_fit[,16:17])
coords_pred = as.matrix(df_pred[,16:17])

summary(df_dat2)

spde12 <- inla.spde2.pcmatern(mesh, constr = TRUE, 
                              prior.range = c(0.01, 0.01), 
                              prior.sigma = c(1, 0.1))

#A12_dat <- inla.spde.make.A(mesh, loc = coords_dat2)
A12_fit <- inla.spde.make.A(mesh, loc = coords_fit)
A12_pred <- inla.spde.make.A(mesh, loc = coords_pred)

#idx_dat <- inla.spde.make.index('i', n.spde = spde12$n.spde)

#stk.dat12 <- inla.stack(data=list(y = df_dat2$infection), A = list(A12_dat, 1), tag = 'dat',
#                        effects=list(idx_dat, 
#                                     data.frame(Intercept = 1,
#                                                mean_residential = df_dat2$res,
#                                                mean_acs = df_dat2$acs,
#                                                month_number = df_dat2$month,
#                                                rw1_month_all = df_dat2$rw1_month_all,
#                                                population_density = df_dat2$population_density,
#                                                built_density = df_dat2$built_density,
#                                                neigh_id_index = df_dat2$neigh_index,
#                                                mean_mkt = df_dat2$mkt,
#                                                mean_tmp = df_dat2$tmp
#                                     )))



idx_dat <- inla.spde.make.index('i', n.spde = spde12$n.spde)
stk.fit <- inla.stack(data=list(y = df_fit$infection), A = list(A12_fit, 1), tag = 'dat',
                        effects=list(idx_dat, 
                                     data.frame(Intercept = 1,
                                                mean_residential = df_fit$res,
                                                mean_acs = df_fit$acs,
                                                month_number = df_fit$month,
                                                rw1_month_all = df_fit$rw1_month_all,
                                                population_density = df_fit$population_density,
                                                built_density = df_fit$built_density,
                                                neigh_id_index = df_fit$neigh_index,
                                                mean_mkt = df_fit$mkt,
                                                mean_tmp = df_fit$tmp
                                     )))


stk.pred <- inla.stack(data=list(y = df_pred$infection), A = list(A12_pred, 1), tag = 'pred',
                      effects=list(idx_dat, 
                                   data.frame(Intercept = 1,
                                              mean_residential = df_pred$res,
                                              mean_acs = df_pred$acs,
                                              month_number = df_pred$month,
                                              rw1_month_all = df_pred$rw1_month_all,
                                              population_density = df_pred$population_density,
                                              built_density = df_pred$built_density,
                                              neigh_id_index = df_pred$neigh_index,
                                              mean_mkt = df_pred$mkt,
                                              mean_tmp = df_pred$tmp
                                   )))

stk_all <- inla.stack.join(stk.fit,stk.pred)


## Formula for INLA model
formula12_fit = y ~ 0 + Intercept +   
  f(neigh_id_index, model = "iid",
    hyper = list( prec = list( prior = "pc.prec", param = c(0.5, 0.01)))) +
  f(rw1_month_all, model = "rw1", scale.model = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) +
  factor(month_number) +
  mean_residential + 
  mean_acs +
  population_density +
  built_density +
  mean_mkt +
  mean_tmp +
  f(i, model = spde12)


## Execute INLA
M12_fit = inla(formula12_fit, family = 'zeroinflatedpoisson1',
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
               inla.mode = "experimental",
               data = inla.stack.data(stk_all), verbose=TRUE, 
               control.predictor = list(A = inla.stack.A(stk_all), compute = TRUE, link = 1))

summary(M12_fit)

p <- autoplot(M12_fit)
p$random.effects.line
p$fixed.marginals
p$hyper.marginals
p$marginal.fitted

##-------Posterior Prediction at EA level using the approaches

index_code <- inla.stack.index(stk.dat12, "dat")$data #---extract the data location indices
fit_covid <- exp(M12_fit$summary.linear.predictor[index_code,"mean"]) #--predicted mean count
summary(fit_covid)

plot(df_fit$infection - fit_covid[1:27231])
plot(df_dat2$infection[27232:29833] - fit_covid[27232:29833])

df_pred <- as.data.frame(cbind(df_dat2$infection, fit_covid, df_dat2$H_Zone))
names(df_pred)
df_pred$obs_covid <- df_pred$V1
ggplot(df_pred, aes(x = obs_covid, y = fit_covid)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE)

df_pred2 <- as.data.frame(cbind(df_dat2$year,df_dat2$month, df_dat2$H_Zone, df_dat2$infection, fit_covid))
names(df_pred2)
df_pred2$year <- df_pred2$V1
df_pred2$month <- df_pred2$V2
df_pred2$Code <- df_pred2$V3
df_pred2$obs_covid <- df_pred2$V4

ggplot(df_pred2, aes(x = obs_covid, y = fit_covid)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE)



head(df_pred2)
cor.test(df_pred2$V1, df_pred2$fit_covid)
plot(df_pred2$V1, df_pred2$fit_covid)

#
head(df_pred)

# s <- summary(M12_fit)
# capture.output(s, file = "hurdle_with_all_covariates.txt")
cpo1 = mean(log(M12_fit$cpo$cpo[M12_fit$cpo$cpo>0]), na.rm=TRUE)
cpo1

dic = M12_fit$dic$deviance.mean
dic 

#


df_pred22<-subset(df_pred2, (year == 2021 & month > 5))
head(df_pred22)
aggregated_df22 <- aggregate(fit_covid ~ Code + year + month, df_pred22, sum)

#final_data <- subset(final_data, !(year == 2021 & month > 7))
#final_data <- subset(final_data, !(year == 2020 & month < 5))

dkr_map_pred <- full_join(dkr_shp, aggregated_df22, by = "Code")
#st_write(dkr_map_pred, "C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/Dk_risk_map2.shp")




















# Assuming you have weekly frequency data stored in a variable called 'weekfreq'
# and a vector of labels for each day of the week called 'daysoftheweek'

# Load the necessary library
library(season)

# Example data for days of the week
daysoftheweek <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Random weekly data as an example
set.seed(123) # for reproducibility
weekfreq <- table(round(runif(100, min=1, max=7)))

# Plotting the circular plot
plotCircular(area1=weekfreq, labels=daysoftheweek, dp=0)

###
# Observed number of AFL players with expected values
data(AFL)
plotCircular(area1=AFL$players, area2=AFL$expected, scale=0.72,
             labels=month.abb, dp=0, lines=TRUE, legend=FALSE)
plotCircular(area1=AFL$players, area2=AFL$expected, scale=0.72,
             labels=month.abb, dp=0, lines=TRUE, pieces.col=c("green","red"),
             auto.legend=list(labels=c("Obs","Exp"), title="# players"),
             main="Observed and Expected AFL players")
###
y <- aggregated_df
#y = y[y$Dc_anRegistre %in% c(2002, 2013),]
y1 = y[y$year %in% c(2020),]
y1$m_abb <- NA
y1$m_abb[y1$month == 5] = "May"
y1$m_abb[y1$month == 6] = "June"
y1$m_abb[y1$month == 7] = "July"
y1$m_abb[y1$month == 8] = "August"
y1$m_abb[y1$month == 9] = "September"
y1$m_abb[y1$month == 10] = "October"
y1$m_abb[y1$month == 11] = "November"
y1$m_abb[y1$month == 12] = "December"
y1$count = 1
k = 1
mmean1 = tapply(y1$count[y1$infection == k], y1$month[y1$infection == k], function(x) sum(x, na.rm = T))
plotCircular(area1 = mmean1, labels = month.abb, scale = 0.7)
mmean1 = tapply(y1$count[y1$infection == k], y1$m_abb[y1$infection == k], function(x) sum(x, na.rm = T))
plotCircular(area1 = mmean1, labels = month.abb, scale = 0.7)
#
y2 = y[y$year %in% c(2021),]
y2$count = 1
k = 1
mmean2 = tapply(y2$count[y2$infection == k], y2$month[y2$infection == k], function(x) sum(x, na.rm = T))
plotCircular(area1 = mmean2, labels = month.abb, scale = 0.7)

unique(y1$month)
#
y$count = 1
k = 1

mmean11 = tapply(y$count[y$infection == k], y$month[y$infection == k], function(x) sum(x, na.rm = T))
plotCircular(area1 = mmean11, labels = month.abb, scale = 0.7)


yy <- covid_dat %>%
  as_tibble() %>% 
  mutate(H_Zone = Code,
         date_infection = as.Date(as.character(DATE), format = "%d/%m/%Y"),
         my = format(as.Date(date_infection), "%Y-%m"),
         month = as.numeric(substr(date_infection, 6, 7)),
         year = as.numeric(substr(date_infection, 1, 4)),
         n_index = as.character(substr(H_Zone, 1,2)),
         n_idx = as.character(substr(H_Zone, 8,12)),
         neigh_index = paste0(n_index,n_idx),
         timeperiod = paste0(month, year)) %>% 
  glimpse()

ggplot(yy, aes(x = my, y = infection)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Month", y = "Cas communautaires", title = "Distribution mensuelle des cas d'infection de COVID-19 a Dakar entre Juin 2020 - Juin 2021") +
  theme_minimal()


#################################################################################
#################################################################################
#################################################################################  Months, Road Surface
################################################################################# Road Type (3 Covariates)
#################################################################################
#################################################################################
rm(list=ls())
setwd("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal")

### Accessing data set
load("example_df.RData")

plot(mesh_nw_all)
plot(mesh_reg_all)
## Accident Data 
names(df_dat)
head(df_dat)

coords_dat = as.matrix(df_dat[,8:9])

## INLA Model 6
###########################################
########################################### INLA (Covariates) + SPDE (Region Mesh) + Time + iid 
###########################################               NO INTERACTION
###########################################
###########################################

### SPDE Model

spde12 <- inla.spde2.pcmatern(mesh_reg_all, constr = TRUE, 
                              prior.range = c(0.01, 0.01), 
                              prior.sigma = c(1, 0.1))

A12_dat <- inla.spde.make.A(mesh_reg_all, loc = coords_dat)

idx_dat <- inla.spde.make.index('i', n.spde = spde12$n.spde)


stk.dat12 <- inla.stack(data=list(y = df_dat$acc_num), A = list(A12_dat, 1), tag = 'dat',
                        effects=list(idx_dat, 
                                     data.frame(Intercept = 1,
                                                road_type = df_dat$road_type,
                                                road_surface = df_dat$road_surface,
                                                month_number = df_dat$month,
                                                rw1_month_all = df_dat$rw1_month_all,
                                                osm_id_index = df_dat$osm_id_index
                                     )))

## Formula for INLA model
formula12_fit = y ~ 0 + Intercept +   
  f(osm_id_index, model = "iid",
    hyper = list( prec = list( prior = "pc.prec", param = c(0.5, 0.01)))) +
  f(rw1_month_all, model = "rw1", scale.model = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) +
  factor(month_number) +
  factor(road_type) + 
  factor(road_surface) + 
  f(i, model = spde12)


## Execute INLA
M12_fit = inla(formula12_fit, family = 'zeroinflatedpoisson0',
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
               inla.mode = "experimental",
               data = inla.stack.data(stk.dat12), verbose=TRUE, 
               control.predictor = list(A = inla.stack.A(stk.dat12), compute = TRUE, link = 1))




summary(M12_fit)

# s <- summary(M12_fit)
# capture.output(s, file = "hurdle_with_all_covariates.txt")
cpo1 = mean(log(M12_fit$cpo$cpo[M12_fit$cpo$cpo>0]), na.rm=TRUE)
cpo1

dic = M12_fit$dic$deviance.mean
dic 


#### interactive web-based data visualisation
library(plotly)
#install.packages("gapminder")
library(gapminder)

data(gapminder, package = "gapminder")
gg <-ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10() ggplotly(gg)


base <-gapminder %>% 
  plot_ly(x = ~gdpPercap, y = ~lifeExp, size = ~pop, 
          text = ~country, hoverinfo = "text") %>% 
  layout(xaxis = list(type = "log"))

base %>% 
  add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red")) 
    )

#
library(crosstalk)
library(leaflet)
eqs <-highlight_key(quakes) 
stations <-filter_slider(
  "station", "Number of Stations",
  eqs, ~stations
)
p <-plot_ly(eqs, x = ~depth, y = ~mag) %>% 
  add_markers(alpha = 0.5) %>% 
  highlight("plotly_selected")

map <-leaflet(eqs) %>% addTiles() %>% 
  addCircles()
bscols( 
  widths = c(6, 6, 3), 
  p, map, stations
)










df <- data.frame(
  player = rep(c('A', 'B'), each = 4),
  year = rep(c(1, 1, 2, 2), times = 2),
  stat = rep(c('points', 'assists'), times = 4),
  amount = c(14, 6, 18, 7, 22, 9, 38, 4)
)

df_wide <- df %>%
  pivot_wider(names_from = stat, values_from = amount)

print(df_wide)
print(df)







