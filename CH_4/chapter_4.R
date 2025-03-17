setwd("C:/Users/nicho/OneDrive - Augustana College/Data 332")
write.csv(deck, file = "cards.csv", row.names = FALSE)

#exercise 1
deal <- function(cards) {
  cards[1, ]
}
#exercise 3
shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}
