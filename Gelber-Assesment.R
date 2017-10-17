# adding dependencies
library(Quandl)
library(ggplot2)

# reading the data in
grek <- read.csv("data/GREK_2yrs.csv")
ex_rates <- Quandl("FED/RXI_US_N_B_EU", api_key="sfk1toiCxwvhsvgMwwFe", start_date="2011-12-07")

# cleaning into the formats we'd like
grek$volume <- as.vector(grek$volume)
grek <- grek[2:nrow(grek),]
grek <- grek[order(grek$date),]
grek$Month <- paste(substr(grek$date, 1, 4), substr(grek$date, 6, 7), sep = "-")
ex_rates$date <- format(ex_rates$Date, format = "%Y/%m/%d")
names(ex_rates)[names(ex_rates) == "Value"] <- "ex_rate"

# looking at exchange rate
data <- merge(grek, ex_rates, by.x = "date")
lo <- loess(data$close ~ data$ex_rate, model = TRUE)
data$loess_prediction <- predict(lo, data.frame(close = seq(5, 25, length.out = nrow(data))))

prediction <- function(x) {
  while (nrow(data[data$date == x, ]) == 0) {
    if (substr(x, nchar(x), nchar(x)) != "9") {
      x <- paste(substr(x, 1, nchar(x) - 1), as.numeric(substr(x, nchar(x), nchar(x))) + 1, sep="")
    } else {
      x <- paste(substr(x, 1, nchar(x) - 2), as.numeric(substr(x, nchar(x) - 1, nchar(x) - 1)) + 1, 0, sep= "")
    }
  }
  return(lo$fitted[which(data$date == x)])
}
  
ggplot(data) + 
  aes(ex_rate, close) +
  geom_point(position = "jitter", size = 0.7, alpha = 0.8) +
  ggtitle("GREK Closing Price vs USD/EURO Exchange Rate\n") +
  xlab("\n USD per Euro") +
  ylab("GREK Closing Price (USD)\n") +
  theme(plot.title=element_text(size=18), 
        axis.title=element_text(size=14),
        plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm")) 
#ggsave("figures/close_vs_ex_rate.png")

ggplot(data) + 
  aes(ex_rate, close) +
  geom_point(position = "jitter", size = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("GREK Closing Price vs USD/EURO Exchange Rate\n") +
  xlab("\n USD per Euro") +
  ylab("GREK Closing Price (USD)\n") +
  theme(plot.title=element_text(size=18), 
        axis.title=element_text(size=14),
        plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm")) 
#ggsave("figures/close_vs_ex_rate_loess.png")

