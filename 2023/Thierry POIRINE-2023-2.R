source("src/functions.R")
library(readr)
library(stringi)


input <- aoc_get_inputfile(2,2023,"Thierry POIRINE")

input <- read_lines(input)

#PART 1

isGameValid <- function(lineinput){
  # lineinput <- "Game 100: 8 green, 7 blue, 1 red; 10 blue, 2 green, 5 red; 12 blue, 1 green, 1 red; 9 green, 9 blue, 2 red; 1 blue, 5 red, 3 green"
  blues <- all(as.integer(stri_match_all_regex(lineinput,"(\\d*) blue")[[1]][,2]) <= 14)
  reds <- all(as.integer(stri_match_all_regex(lineinput, "(\\d*) red")[[1]][,2]) <= 12)
  greens <- all(as.integer(stri_match_all_regex(lineinput, "(\\d*) green")[[1]][,2]) <= 13)
  return (blues && reds && greens)
}

IDsValid <- list()

for (i in input) {
  print(i)
  print(isGameValid(i))
  if(isGameValid(i)){
    IDsValid <- append(IDsValid,stri_match_first_regex(i, "Game (\\d*):")[,2])
  }
}

sum(as.integer(IDsValid))

# PART 2

maxCubes <- function(lineinput){
  # lineinput <- "Game 100: 8 green, 7 blue, 1 red; 10 blue, 2 green, 5 red; 12 blue, 1 green, 1 red; 9 green, 9 blue, 2 red; 1 blue, 5 red, 3 green"
  blues <- max(as.integer(stri_match_all_regex(lineinput,"(\\d*) blue")[[1]][,2]))
  reds <- max(as.integer(stri_match_all_regex(lineinput, "(\\d*) red")[[1]][,2]))
  greens <- max(as.integer(stri_match_all_regex(lineinput, "(\\d*) green")[[1]][,2]))
  return (blues * reds * greens)
}

sumMAX = 0

for (i in input) {
  print(i)
  print(maxCubes(i))
  sumMAX = sumMAX + maxCubes(i)
}

sumMAX
