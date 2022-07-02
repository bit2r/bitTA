#' Naver sentiment movie corpus v1.0
#' 
#' @description 
#' 네이버 영화 리뷰에서 스크랩한 데이터이며, 모두 140자 미만의 길이고, 
#' 0(Negative)과 1(Positive)로 라벨링 되어있음
#' 
#' @format 3개의 변수와 150,000개(train), 50,000(test)의 관측치로 구성된 티블(tibble) 객체.:
#' \describe{
#'   \item{id}{character. 리뷰 아이디}
#'   \item{document}{character. 영화 리뷰}
#'   \item{label}{integer. 긍부정의 정보. 부정(0), 긍정(1)}
#' }
#' @docType data
#' @keywords datasets
#' @name movie_ratings_train
#' @usage data(movie_ratings_train)
#' @usage data(movie_ratings_test)
#' @source 
#' "Naver sentiment movie corpus v1.0" in github <https://github.com/e9t/nsmc>
#' @examples
#' \dontrun{
#' data(movie_ratings_train)
#' data(movie_ratings_test)
#' 
#' head(movie_ratings_train)
#' }
NULL

#' @rdname movie_ratings_train
#' @name movie_ratings_test
NULL

# library(dplyr)
# 
# path <- here::here("data", "nsmc-master")
# fname_train <- glue::glue("{path}/ratings_train.txt")
# fname_test <- glue::glue("{path}/ratings_test.txt")
# 
# movie_ratings_train <- readr::read_delim(fname_train, delim = "\t") %>% 
#   mutate(id = as.character(id)) %>% 
#   mutate(label = as.integer(label)) 
# movie_ratings_test <- readr::read_delim(fname_test, delim = "\t") %>% 
#   mutate(id = as.character(id)) %>% 
#   mutate(label = as.integer(label)) 
# 
# save(movie_ratings_train, file = glue::glue("data/movie_ratings_train.rda"))
# save(movie_ratings_test, file = glue::glue("data/movie_ratings_test.rda"))

