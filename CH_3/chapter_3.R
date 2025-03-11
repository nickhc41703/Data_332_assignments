hand <- c("ace", "king", "queen", "jack", "ten")
hand

hand1 <- c("ace", "king", "queen", "jack", "ten", "spades", "spades",
           "spades", "spades", "spades")
matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5, 2)

card <- c("ace", "hearts", 1)
card

card <- c("ace", "hearts", 1)
card