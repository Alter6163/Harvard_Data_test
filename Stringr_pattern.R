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
library(stringr)
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)
library(stringr)
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)
library(stringr)
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)
library(stringr)
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo*"
str_detect(animals, pattern)
library(stringr)
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "moo*"
str_detect(animals, pattern)
library(stringr)
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo+"
str_detect(animals, pattern)
library(stringr)
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo?"
str_detect(animals, pattern)
# wrong variant
library(stringr)
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools %>% 
  str_replace("Univ\\.?|U\\.?", "University ") %>% 
  str_replace("^University of |^University ", "University of ")
#right answer
library(stringr)
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")
# wrong variant
library(stringr)
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools %>% 
  str_replace("^Univ\\.\\s|^U\\.\\s", "University") %>% 
  str_replace("^University of |^University ", "University of ")
# wrong variant
library(stringr)
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University") %>% 
  str_replace("University ", "University of ")
# q1
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")
# q2
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")
# right variant
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")
# q2 v2 right
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)
# q2 v2 wrong
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
  str_replace("\\s*|feet|foot|ft", "'") %>% 
  str_replace("\\s*|inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
str_detect(converted, pattern)
# Using Groups and Quantifiers
s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}
# We can also write a function that converts words to numbers:
  
  words_to_numbers <- function(s){
    str_to_lower(s) %>%  
      str_replace_all("zero", "0") %>%
      str_replace_all("one", "1") %>%
      str_replace_all("two", "2") %>%
      str_replace_all("three", "3") %>%
      str_replace_all("four", "4") %>%
      str_replace_all("five", "5") %>%
      str_replace_all("six", "6") %>%
      str_replace_all("seven", "7") %>%
      str_replace_all("eight", "8") %>%
      str_replace_all("nine", "9") %>%
      str_replace_all("ten", "10") %>%
      str_replace_all("eleven", "11")
  }
# Now we can see which problematic entries remain:
  
#converted <- problems %>% words_to_numbers %>% convert_format
#remaining_problems <- converted[not_inches_or_cm(converted)]
#pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
#index <- str_detect(remaining_problems, pattern)
#remaining_problems[!index]
  extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
          regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")    
  str_detect(converted, pattern)
  print(s)
  s
  
