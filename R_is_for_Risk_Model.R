
# Poisson - rpois(n events, lambda average events per year)
# Events that are expected to occur several times a year
# Examples - Unbudgeted compensations, storms, client defaults, accidents
# *
# Log-normal - rlnorm(n events, meanlog, sdlog)
# Events that are expected to produce unusually large losses (heavy tail)
# Corruption fine, class action, compliance scandal

# Source Cyentia IRIS 2022
# STDEV: SQRT((LN(mean)-LN(median))*2)
# mu: LN(median)
loss_mean <- 25000000
loss_median <- 259000
loss_stdev <- sqrt((log(loss_mean)-log(loss_median))*2)
loss_mu <- log(loss_median)

# Frequency parameters: Poisson log-normal
# Source Cyentia IRIS 2022
# x <- rpois(10000, .2) * rlnorm(10000, loss_mu, loss_stdev)
x <- rpois(10000, 4)
library(poilog)
z <- rpoilog(10000, -2.284585, 0.8690759, condS = FALSE, keep0 = TRUE)
#y <- rlnorm(10000, loss_mu, loss_stdev)
y <- rlnorm(10000, 12.55949, 3.068723)
options(scipen = 999)

mycurrency <- function(x){
  return(paste0("€", formatC(as.numeric(x), format="f", digits=0, big.mark=",")))
}

df <- data.frame("Loss_sims" = z * y)
colnames(df) <- "Loss_Sims"
p <- ggplot(df) +
  aes(x = Loss_Sims) +
  geom_histogram(bins = 30L, fill = "#112446") +
  geom_vline(aes(xintercept = mean(df$Loss_Sims),
                 text = paste("Mean:", mean(df$Loss_Sims))),
             linetype = "dashed",
             color = "navyblue",
             size = 1.1) + 
  scale_x_continuous(trans = "log", breaks = c(100,10000,10000,100000,1000000,10000000,1000000000)) +
  theme_minimal()

library(plotly)
ggplotly(p)

summary(df$Loss_Sims)
median_loss_sims <- median(df$Loss_Sims)



df <- (z*y)
hist(log(df))
summary(log(df))
quantile(log(df), c(0.9))

summary(log(x))
summary(x)
hist(log(x))
quantile(log(x), c(0.9))



# You created the variable x with 10,000 random combinations of the 
# Poisson probabilities and the log-normal impacts
# Summary < displays mean, median, max, and min
# Hist < creates a histogram
# Quantile < calculates the value 