library(stringr)
s <- c("70","5 ft","4'11","",".","Six feet")
pattern <- "\\d|ft"
str_view_all(s, pattern)
# wrong variant 
# library(stringr)
# s <- c("70","5 ft","4'11","",".","Six feet")
# pattern <- "\d|ft"
# str_view_all(s, pattern)
library(stringr)
s <- c("70","5 ft","4'11","",".","Six feet")
pattern <- "\\d\\d|ft"
str_view_all(s, pattern)
library(stringr)
s <- c("70","5 ft","4'11","",".","Six feet")
pattern <- "\\d|feet"
str_view_all(s, pattern)