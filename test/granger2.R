rm(list=ls())
library(lmtest)
library(dplyr)
devtools::load_all(".")

# Get recording meta data
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)

# Get filtered views
fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p =3)

# Put individual ts into one data frame using a JoinedView
jv <- get_joined_view(fv_list)
plot(jv, yax.flip=TRUE)

# Get duration annotation object
d1 <- get_duration_annotation_data(r1)

# Subset the time line to first 10 minutes
jv_sub <- subset(jv, Time <= 10*60)
plot(jv_sub, yax.flip=TRUE)

# Do Grangers - time domain
grangertest(Nose_x_Central_Sitar ~ Nose_x_Central_Tabla, order = 1*25, data = jv_sub$df)
grangertest(Nose_x_Central_Tabla ~ Nose_x_Central_Sitar, order = 1*25, data = jv_sub$df)
# Sitar Nose follows Tabla Nose at 5% confidence level with one second lag

# function to take jv_sub and generated a spliced_df
splicing_df <- splice_time(jv_sub, win_size = 30, step_size = 5)

# apply spliced_df to jv_sub
sv <- get_spliced_view(jv_sub, splicing_df)

# it will have a lot of sub-divisions so to view each piece need to subset
sv_sub <- subset(sv, Tier %in% paste0('w', 1:5))
autoplot(sv_sub)

# apply granger_test to each Tier in spliced_df using 0.1s lag
g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 0.1)
g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1)
g

# p-value plot both forward and backwards
autoplot(g, splicing_df = splicing_df)

# p-value plot with annotation from duration data
autoplot(g, splicing_df = splicing_df) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Tier")

autoplot(g, splicing_df = splicing_df) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Comments")

# data for arrows showing causality direction
library(dplyr)
library(tidyr)
obj <- g
df <- obj$df
sig_level <- 0.05
#df$P_Value[df$P_Value > sig_level] <- NA
#df$mlog10p <- ifelse(df$Var1 == "Nose_x_Central_Sitar", -log10(df$P_Value), log10(df$P_Value))
splicing_df$Centre <- (splicing_df$Start + splicing_df$End) / 2
df <- dplyr::inner_join(df, splicing_df[c('Tier', 'Centre')], by = 'Tier')
x <- df[c("Var1", "Centre", "P_Value")]
wide_df <- pivot_wider(x, names_from = "Var1", values_from = "P_Value") %>%
  mutate(Value = if_else(
    Nose_x_Central_Sitar < sig_level | Nose_x_Central_Tabla < sig_level,
    log10(Nose_x_Central_Tabla/Nose_x_Central_Sitar), NA_real_)) %>%
  select(Centre, Value)

ggplot2::ggplot(wide_df) +
  geom_segment(colour="black", aes(x=Centre, xend=Centre, y=0, yend=Value),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  ggplot2::labs(title = "Influence Diagram", subtitle = obj$recording$stem) +
  ggplot2::xlab("Time / min:sec") +
  ggplot2::ylab("-log10(P_Value) difference if one significant") +
  ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
  annotate("text", label = unique(df$Var1)[1], x=max(df$Centre)/2, y=max(-log10(df$P_Value))) +
  annotate("text", label = unique(df$Var1)[2], x=max(df$Centre)/2, y=min(log10(df$P_Value))) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Tier")

ggplot2::ggplot(wide_df) +
  geom_segment(colour="black", aes(x=Centre, xend=Centre, y=0, yend=Value),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  ggplot2::labs(title = "Influence Diagram", subtitle = obj$recording$stem) +
  ggplot2::xlab("Time / min:sec") +
  ggplot2::ylab("-log10(P_Value) difference if one significant") +
  ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
  annotate("text", label = unique(df$Var1)[1], x=max(df$Centre)/2, y=max(-log10(df$P_Value))) +
  annotate("text", label = unique(df$Var1)[2], x=max(df$Centre)/2, y=min(log10(df$P_Value))) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Comments")

# facet stack interactions on top of each time line

# igraph for data_point causality on time segments

