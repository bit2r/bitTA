##==============================================================================
## Load packages
##==============================================================================
library(shiny)
library(dplyr)
library(bitTA)
library(RMeCab)

##==============================================================================
## User defined function
##==============================================================================
getCoCollocate <- function(x, node, span = 2) {
  library(dplyr)
  
  collocate_filter <- function(x) {
    
    # one character
    filter_char1 <- nchar(x[, "Term"]) == 1
    x <- x[!filter_char1, ]
    
    # asterisk
    filter_asta <- grep("\\*", x[, "Term"], invert = TRUE)
    x <- x[filter_asta, ]
    
    # tag
    filter_tag <- grep("^E", x[, "Term"], invert = TRUE)
    x[filter_tag, ]
  }
  
  fname <- "tmp.txt"
  cat(x, file = fname)
  
  coll <- collocate(fname, node = node, span = span) %>%
    collocate_filter
  
  if (is.null(coll)) return(NULL)
  
  coll <- collScores(coll, node = node, span = span) %>%
    filter(!is.na(MI) & MI > 2)
  coll
}

##==============================================================================
## Global variables for app
##==============================================================================
list_df <- Filter(
  function(x) is(x, "data.frame"), 
  mget(ls(pos = ".GlobalEnv"), envir = .GlobalEnv)) %>% 
  names()

## 만약에 데이터 프레임 객체가 없다면, 대통령 연설문인 president_speech를 로드
if (length(list_df) == 0) {
  data("president_speech")
  
  list_df <- "president_speech"
}

vnames <- names(get(list_df[1]))

ucnt <- apply(get(list_df[1]), 2, function(x) length(unique(x)))
cnames <- vnames[ucnt > 1 & ucnt <= 30]
cnames <- c("전체", cnames)

node <- "통일"
span <- 2 

docs <- N <- idx <- NULL
