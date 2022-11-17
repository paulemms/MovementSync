rm(list=ls())
library(lmtest)
library(grangers)
library(dplyr)
devtools::load_all(".")

## Which came first: the chicken or the egg?
data(ChickEgg)
grangertest(egg ~ chicken, order = 3, data = ChickEgg)
grangertest(chicken ~ egg, order = 3, data = ChickEgg)

## alternative ways of specifying the same test
grangertest(ChickEgg, order = 3)
grangertest(ChickEgg[, 1], ChickEgg[, 2], order = 3)

# Get recording meta data
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)

# Get raw views
rv_list <- get_raw_views(r1)
pv_list <- lapply(rv_list, get_processed_view)
get_data_points(pv_list$Central_Sitar)
fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = "Nose", n = 41, p = 3)

# Put individual ts into one data frame using a JoinedView
jv <- get_joined_view(fv_list)
plot(jv, yax.flip=TRUE)

# Subset the time line
jv_sub <- subset(jv, Time < 10*60)
plot(jv_sub, yax.flip=TRUE)

# Do Grangers - time domain
grangertest(Nose_x_Central_Sitar ~ Nose_x_Central_Tabla, order = 3, data = jv_sub$df)
grangertest(Nose_x_Central_Tabla ~ Nose_x_Central_Sitar, order = 3, data = jv_sub$df)

# Grangers example - frequency domain
RealGdp.rate.ts <- euro_area_indicators[,1]
m3.rate.ts <- euro_area_indicators[,2]
plot(zoo(euro_area_indicators[,1:2]))
bc_test_uncond(RealGdp.rate.ts, m3.rate.ts, ic.chosen="SC", max.lag=2)
#bc_test_uncond(RealGdp.rate.ts,m3.rate.ts,ic.chosen="SC",plot=T, max.lag=2)

# frequency domain
Granger.unconditional(RealGdp.rate.ts, m3.rate.ts, "SC", 4)

# movement sync
gc_un <- Granger.unconditional(jv_sub$df$Nose_x_Central_Sitar, jv_sub$df$Nose_x_Central_Tabla, plot = T, "SC", 4)
bc_un <- bc_test_uncond(jv_sub$df$Nose_x_Central_Sitar, jv_sub$df$Nose_x_Central_Tabla, ic.chosen="SC", max.lag=2)
#bc_test_uncond(jv_sub$df$Nose_x_Central_Sitar, jv_sub$df$Nose_x_Central_Tabla, ic.chosen="SC", plot = T, max.lag=2)

plot(bc_un$significant_frequencies)
