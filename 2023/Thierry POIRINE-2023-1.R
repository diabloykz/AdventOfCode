source("src/functions.R")
library(readr)
library(stringi)


input <- aoc_get_inputfile(1,2023,"Thierry POIRINE")

#1st part

dtinput <- fread(input,header=FALSE)

dtinput[,V2:=stri_extract_first_regex(V1,"^*[1-9]")]
dtinput[,V3:=stri_extract_last_regex(V1,"^*[1-9]")]
dtinput[,V4:=paste0(V2,V3)]
sum(as.integer(dtinput[,V4]))


#2nd part

one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                   six=6, seven=7, eight=8, nine=9)

dtinput2 <- fread(input,header=FALSE)

dtinput2[,V2:=stri_match_first_regex(V1,paste0("(?=(\\d|",paste(digits, collapse = "|"),")).*(\\d|",paste(digits, collapse = "|"),")"))[,2]]
dtinput2[,V3:=stri_match_first_regex(V1,paste0("(?=(\\d|",paste(digits, collapse = "|"),")).*(\\d|",paste(digits, collapse = "|"),")"))[,3]]
dtinput2[,firstdigit:= fifelse(V2%in%digits,as.character(one_digits[V2]),V2)]
dtinput2[,lastdigit:= fifelse(V3%in%digits,as.character(one_digits[V3]),V3)]
dtinput2[,calibration:= paste0(firstdigit,lastdigit)]

sum(as.integer(dtinput2[,calibration]))


