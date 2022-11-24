library(WaveletComp)

### univariate

# constant period

x = periodic.series(start.period = 50, length = 1000)
x = x + 0.2*rnorm(1000) # add some noise
plot(x, type = "l")

my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 10)

wt.image(my.w, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))

reconstruct(my.w, plot.waves = FALSE, lwd = c(1,2),
            legend.coords = "bottomleft", ylim = c(-1.8, 1.8))

# variable period

x = periodic.series(start.period = 20, end.period = 100, length = 1000)
x = x + 0.2*rnorm(1000)
plot(x, type = "l")

my.data <- data.frame(x = x)
my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 10)

wt.image(my.w, n.levels = 250,
         legend.params = list(lab = "wavelet power levels"))

my.rec <- reconstruct(my.w, plot.waves = TRUE)
x.rec <- my.rec$series$x.r # x: name of original series
plot(x.rec, type = "l")

# two periods
x1 <- periodic.series(start.period = 80, length = 1000)
x2 <- periodic.series(start.period = 30, length = 1000)
x <- x1 + x2 + 0.2*rnorm(1000)

plot(x, type = "l")
my.data <- data.frame(x = x)

my.w <- analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt = 1, dj = 1/250,
                        lowerPeriod = 16,
                        upperPeriod = 128,
                        make.pval = TRUE, n.sim = 10)
wt.image(my.w, n.levels = 250,
         legend.params = list(lab = "wavelet power levels") )

reconstruct(my.w, sel.period = 80, plot.waves = TRUE, lwd = c(1,2),
            legend.coords = "bottomleft")

# average power

x1 <- periodic.series(start.period = 100, length = 500)
x2 <- 1.2*periodic.series(start.period = 60, length = 500)
x <- c(x1, x2) + 0.3*rnorm(1000)
y1 <- periodic.series(start.period = 100, length = 1000)
y2 <- 1.2*periodic.series(start.period = 60, length = 1000)
y <- (y1 + y2)/2 + 0.3*rnorm(1000)

my.data <- data.frame(x = x, y = y)
my.wx <- analyze.wavelet(my.data, "x", loess.span = 0,
                         dt = 1, dj = 1/20,
                         lowerPeriod = 16, upperPeriod = 256,
                         make.pval = TRUE, n.sim = 10)
my.wy <- analyze.wavelet(my.data, "y", loess.span = 0,
                         dt = 1, dj = 1/20,
                         lowerPeriod = 16, upperPeriod = 256,
                         make.pval = TRUE, n.sim = 10)

maximum.level = 1.001*max(my.wx$Power.avg, my.wy$Power.avg)
wt.avg(my.wx, maximum.level = maximum.level)
wt.avg(my.wy, maximum.level = maximum.level)


### bivariate

x1 <- periodic.series(start.period = 1*24, length = 24*96)
x2 <- periodic.series(start.period = 2*24, length = 24*96)
x3 <- periodic.series(start.period = 4*24, length = 24*96)
x4 <- periodic.series(start.period = 8*24, length = 24*96)
x5 <- periodic.series(start.period = 16*24, length = 24*96)
x <- x1 + x2 + 3*x3 + x4 + x5 + 0.5*rnorm(24*96)
y <- x1 + x2 - 3*x3 + x4 + 3*x5 + 0.5*rnorm(24*96)
plot(x, type = "l")
plot(y, type = "l")

my.data <- data.frame(x = x, y = y)
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0,
                           dt = 1/24, dj = 1/100,
                           lowerPeriod = 1/2,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (days)")

wc.avg(my.wc, siglvl = 0.01, sigcol = "red", sigpch = 20,
       periodlab = "period (days)")

wt.image(my.wc, my.series = "x")
wt.image(my.wc, my.series = "y")


# phase differences at period 10

wc.sel.phases(my.wc, sel.period = 4,
              only.sig = TRUE,
              which.sig = "wc",
              siglvl = 0.05,
              phaselim = c(-pi,+pi), ## default if legend.horiz = FALSE
              legend.coords = "topright", legend.horiz = FALSE,
              main = "", sub = "", timelab = "")

# global image of phase differences
wc.phasediff.image(my.wc, which.contour = "wc", use.sAngle = TRUE,
                   n.levels = 250, siglvl = 0.1,
                   legend.params = list(lab = "phase difference levels",
                                        lab.line = 3),
                   timelab = "")
