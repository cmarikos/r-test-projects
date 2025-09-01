cinnamon <- abs(rnorm(n=50, mean=2.5, sd=2))
pine <- abs(rnorm(n=50, mean=2.3, sd=2.1))
clove <- abs(rnorm(n=50, mean=1, sd=4))

tasty_candles <- data.frame("Cinnamon Apple"= cinnamon, "Pine" = pine, "Clove" = clove)

correlation_matrix <- cor(tasty_candles[, c("Cinnamon.Apple", "Pine", "Clove"])

                                        