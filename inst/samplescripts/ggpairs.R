library(ggplot2)
library(GGally)
library(movementsync)

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
o1 <- get_onsets_selected_data(r1)
autoplot(o1)
r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
o2 <- get_onsets_selected_data(r2)
autoplot(o2)
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)
o3 <- get_onsets_selected_data(r3)
autoplot(o3, instrument = 'Onset')
r4 <- get_recording("NIRP1_VS_Hams", fps = 25)
o4 <- get_onsets_selected_data(r4)
autoplot(o4, instrument = 'Onset')
r5 <- get_recording("Gagaku_5_Juha", fps = 60)
o5 <- get_onsets_selected_data(r5)
autoplot(o5, instrument = 'Hichiriki', tactus = 'SD_T')

instruments <- c("Shoko_L", "Shoko_R", "Taiko", "Kakko", "Kakko_1", "So", "Biwa",
                 "Ryuteki", "Hichiriki", "Sho", "Biwa_RW", "Shoko_RW", "Taiko_LW",
                 "Taiko_RW")

# Difference in onsets for each instrument pair
po1 <- difference_onsets(o1, instruments = c('Inst', 'Tabla'))
ggpairs(po1, columns = 2:4, aes(colour = Metre))


primary_var <- "Inst-Tabla"
pairs <- ggpairs(po1, columns = 2:4, aes(colour = Metre))
pvar_pos <- match(primary_var, pairs$xAxisLabels)
plots <- lapply(1:pairs$ncol, function(j) getPlot(pairs, i = pvar_pos, j = j))
ggmatrix(
  plots,
  nrow = 1,
  ncol = pairs$ncol,
  xAxisLabels = pairs$xAxisLabels,
  yAxisLabels = primary_var
)


po5 <- difference_onsets(o5, instruments = instruments)
ggpairs(po5, columns = 2:5) # only one Metre in plot

primary_var <- c("Shoko_L-Shoko_R", "Shoko_L-Taiko")
col_nums <- c(2, match(primary_var, colnames(po5)))
pairs <- ggpairs(po5, columns = col_nums)
pvar_pos <- match(primary_var, pairs$xAxisLabels)
plots1 <- lapply(1:pairs$ncol, function(j) getPlot(pairs, i = pvar_pos[1], j = j))
plots2 <- lapply(1:pairs$ncol, function(j) getPlot(pairs, i = pvar_pos[2], j = j))
ggmatrix(
  c(plots1, plots2),
  nrow = 2,
  ncol = pairs$ncol,
  xAxisLabels = pairs$xAxisLabels,
  yAxisLabels = primary_var
)
