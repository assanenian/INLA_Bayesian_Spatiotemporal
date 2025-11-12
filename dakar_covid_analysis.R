rm(list=ls())
library(dplyr)
library(INLA)
library(tidyr)

library(INLAutils)
library(mapview)
library(raster)
library(sf)
library(sp)
library(tidyverse)
library(ggplot2)
library(exactextractr)

# load covid data
cd <- read.csv("C:/Local/Assane/R_stuff/IGU_2024/INLA_spatio_temporal/dakar/covid_neigh2.csv")

head(cd)
cd$infection <- cd$NOMBRE.DE.CAS.ISSUS.DE.LA.TRANSMISION.COMMUNAUTAIRE


# Change date format
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


# Add years and months
years <- c(2020, 2021)
months <- 1:12

# Create a complete table for all spatial and temporal combinations
complete_data <- expand.grid(H_Zone = H_Zones, year = years, month = months)

# Merge the complete table with your original data where there is data available
final_data <- merge(complete_data, aggregated_df, by = c("H_Zone", "year", "month"), all = TRUE)
summary(final_data)
final_data$Code <- final_data$H_Zone

names(dkr_shp)


covs <- dkr_shp[,c("Code","res","acs","pop","built_density","population_density","mkt","tmp")]
final_data2 <- merge(final_data, covs, by = c("H_Zone" =  "Code"), all.x = TRUE)
#
##----Extract the coordinates
dkr_shp2 <- as(dkr_shp, "Spatial")
coords = coordinates(dkr_shp2)
dkr_shp2$longitude = coords[,1]
dkr_shp2$latitude = coords[,2]


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


names(df_dat2)
coords_dat2 = as.matrix(df_dat2[,16:17])

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


## Formula for INLA model (non-spatial)
formula11_fit = y ~ 0 + Intercept +   
  f(neigh_id_index, model = "iid",
    hyper = list( prec = list( prior = "pc.prec", param = c(0.5, 0.01)))) +
  f(rw1_month_all, model = "rw1", scale.model = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) +
  factor(month_number) +
  mean_residential + 
  mean_acs +
  population_density +
  built_density +
  social_distancing_index +
  #  mean_mkt +
  mean_tmp
#f(i, model = spde12)

## Execute INLA
M11_fit = inla(formula11_fit, family = 'zeroinflatedpoisson0',
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
               inla.mode = "experimental",
               data = inla.stack.data(stk_all), verbose=TRUE, 
               control.predictor = list(A = inla.stack.A(stk_all), compute = TRUE, link = 1))

summary(M11_fit)

p11 <- autoplot(M11_fit)
p11$fixed.marginals
p11$hyper.marginals
p11$random.effects.line
p11$marginal.fitted

## Formula for INLA model (space-time)
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
  social_distancing_index +
  #  mean_mkt +
  mean_tmp +
  f(i, model = spde12)


## Execute INLA
M12_fit = inla(formula12_fit, family = 'zeroinflatedpoisson0',
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
               inla.mode = "experimental",
               data = inla.stack.data(stk_all), verbose=TRUE, 
               control.predictor = list(A = inla.stack.A(stk_all), compute = TRUE, link = 1))

summary(M12_fit)

p12 <- autoplot(M12_fit)
p12$fixed.marginals
p12$hyper.marginals
p12$random.effects.line
p12$marginal.fitted

## Formula for INLA model (spatio-temporal)
formula13_fit = y ~ 0 + Intercept +   
  f(neigh_id_index, model = "iid",
    hyper = list( prec = list( prior = "pc.prec", param = c(0.5, 0.01)))) +
  f(rw1_month_all, model = "rw1", scale.model = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))) +
  f(rw1_month_all_ii, model="iid",
    hyper = list( prec = list( prior="pc.prec", param = c(0.5, 0.01)))) + # Spatiotemporal Interaction
  factor(month_number) +
  mean_residential + 
  mean_acs +
  population_density +
  built_density +
  social_distancing_index +
  #  mean_mkt +
  mean_tmp +
  f(i, model = spde12)


## Execute INLA
M13_fit = inla(formula13_fit, family = 'zeroinflatedpoisson0',
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
               inla.mode = "experimental",
               data = inla.stack.data(stk_all), verbose=TRUE, 
               control.predictor = list(A = inla.stack.A(stk_all), compute = TRUE, link = 1))

summary(M13_fit)

p13 <- autoplot(M13_fit)
p13$fixed.marginals
p13$hyper.marginals
p13$random.effects.line
p13$marginal.fitted

##-------Posterior Prediction on test set

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


