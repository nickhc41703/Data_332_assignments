deck2 <- deck

shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}

deck3 <- shuffle(deck)
#exercise 1
deck2$face
deck2$face == "ace"
sum(deck2$face == "ace")
deck3$face == "ace"
deck3$value[deck3$face == "ace"]
deck3$value[deck3$face == "ace"] <- 14
head(deck3)
#end
deck4 <- deck
deck4$value <- 0
#exercise 2
deck4$suit == "hearts"
deck4$value[deck4$suit == "hearts"]
deck4$value[deck4$suit == "hearts"] <- 1
deck4$value[deck4$suit == "hearts"]
deck4[deck4$face == "queen", ]
deck4[deck4$suit == "spades", ]
#exercise 3
w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

w > 0
10 < x & x < 20
y == "February"
all(z %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
             "Saturday", "Sunday"))
