#' Extract Collapsed Noun
#' @description 텍스트 문서에서 명사들을 토큰화한 후, 토큰화된 명사들을 공백으로
#' 묶어서 텍스트 문서를 만듦
#' 조회한다.
#' @param doc character. 명사로만 구성될 문서를 만들 대상 텍스트 데이터
#' @param type character. 토큰화할 명사의 유형을 지정. "noun", "noun2"중에서 선택. 
#' 기본값은 "noun"로 일반명사를 의미하며, "noun2"는 모든 명사를 의미함.
#' @param user_dic mecab-ko 형태소 분석기의 사용자 정의 사전 파일.
#' 기본값은 NULL로 사용자 사전파일을 지정하지 않음.
#' 시스템 사전인 "/usr/local/lib/mecab/dic/mecab-ko-dic"(Linux, Mac)를 보완하여 사용됨.
#' 사용자 사전 파일은 mecab-dict-index 명령어로 생성되며, 확장자가 "dic"임.
#' @param chunk integer. 병렬 작업 수행 시 처리 단위인 chunk
#' @param mc.cores integer. 병렬 작업 수행 시 사용할 코어의 개수
#' @return character. 명사로만 구성된 텍스트
#' @examples
#' \donttest{
#' collapse_noun(president_speech$doc[1:7])
#' 
#' # Collaboration with tidytext
#' library(dplyr)
#' 
#' nho_noun <- president_speech %>%
#'   filter(president %in% "노무현") %>%
#'   filter(str_detect(category, "^외교")) %>%
#'   mutate(doc_noun = collapse_noun(doc)) %>%
#'     tidytext::unnest_ngrams(
#'       noun_bigram,
#'       doc_noun,
#'       n = 2
#'    )
#' nho_noun
#'  
#' nho_noun$noun_bigram[1:5]
#' }
#' @export
#' @import dplyr
#' @import parallel
#' @importFrom rlang arg_match
#' @importFrom purrr map_chr
#' @importFrom tibble is_tibble
collapse_noun <- function(doc, user_dic = NULL, type = c("noun", "noun2"),
                          chunk = round(length(if (tibble::is_tibble(doc)) dplyr::pull(doc) else doc) / mc.cores),
                          mc.cores = parallel::detectCores()) {
  
  type <- rlang::arg_match(type)
  
  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }
  
  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)
  
  get_collapse_noun <- function(chunk_id, doc) {
    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]
    
    tmp <- doc[start:end]
    
    tmp %>% 
      purrr::map_chr(
        function(x) {
          morpho_mecab(x, type = type, user_dic = user_dic) %>% 
            paste(collapse = " ")
        }
      )
  }
  
  collapsed <- parallel::mclapply(
    seq(chunk_idx$idx_start), 
    get_collapse_noun, 
    doc = doc, 
    mc.cores = mc.cores
  )
  
  do.call("c", lapply(collapsed, function(x) x))
}


#' Tokenization with N-gram 
#' @description n-gram 토큰화 및 n-gram 토큰화 집계.
#' @param x character. n-gram 토큰화에 사용할 document.
#' @param n integer. n-gram 토큰화에서의 n.
#' @param token character. n-gram 토큰화에서 토큰의 종류. "noun", "noun2", "word"
#' 에서 선택. 기본값은 "noun"로 일반명사, "noun2"는 명사, "word"는 단어를 의미함.
#' @param type character. 반환하는 결과물의 종류. "raw"는 토큰화된 n-gram 자체를 반환하며,
#' "table"은 토큰화된 n-gram 집계 정보를 반환.
#' @param user_dic mecab-ko 형태소 분석기의 사용자 정의 사전 파일.
#' 기본값은 NULL로 사용자 사전파일을 지정하지 않음.
#' 
#' @return n-gram 토큰화된 character 벡터, 혹은 n-gram 집계 정보를 담은 데이터 프레임
#' @section n-gram 집계 정보:
#' n-gram 집계 정보를 담은 데이터 프레임 변수는 다음과 같음.:
#' \itemize{
#' \item ngrams : n-gram 토큰. character.
#' \item freq : n-gram 토큰의 도수. integer.
#' \item prop : n-gram 토큰의 상대도수. numeric.
#' }
#' @examples
#' \donttest{
#' str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."
#' 
#' # bi-gram
#' get_ngrams(str)
#' 
#' # tri-gram
#' get_ngrams(str, n = 3)
#' 
#' # 워드(띄어쓰기) 기반 토큰화
#' get_ngrams(str, token = "word")
#' 
#' # 집계정보
#' get_ngrams(str, type = "table")
#' 
#' # 사용자 정의 사전 사용
#' dic_path <- system.file("dic", package = "bitTA")
#' dic_file <- glue::glue("{dic_path}/buzz_dic.dic")
#' get_ngrams(str, user_dic = dic_file)
#' 
#' }
#' 
#' @importFrom rlang arg_match
#' @importFrom ngram ngram get.ngrams ng_order get.phrasetable
#' @export
get_ngrams <- function(x, n = 2L, token = c("noun", "noun2", "word"), 
                       type = c("raw", "table"),
                       user_dic = NULL) {
  token <- rlang::arg_match(token)
  type <- rlang::arg_match(type)
  
  ngram_delim <- " "
  
  if (token %in% "word") {
    ng <- ngram::ngram(str, n = n, sep = ngram_delim)
  } else {
    ng <- morpho_mecab(x, type = token, user_dic = user_dic) %>% 
      paste(collapse = " ") %>% 
      ngram::ngram(n = n, sep = ngram_delim)
  }
  
  if (type %in% "raw") {
    ngram::get.ngrams(ng)[ngram::ng_order(ng)]
  } else if (type %in% "table") {
    ngram::get.phrasetable(ng)
  }
}

#' N-gram Tokenizer
#' @description 명사를 추출하여 n-gram으로 토큰화합니다. 
#' @param x character. 토큰화할 문자열 벡터
#' @param n integer. n-gram의 단어 수입니다. 1 이상의 정수.
#' @param n_min integer. 이것은 1보다 크거나 같고 n보다 작거나 같은 정수여야 함
#' @param stopwords character. n-그램에서 제외할 불용어의 문자형 벡터
#' @param ngram_delim character. 생성된 n-gram에서 단어 사이의 구분 기호
#' @param simplify logical. 기본값은 FALSE로 입력 길이에 관계없이 일관된 값이 
#' 반환되도록 list 객체로 반환. TRUE인 경우 x가 단일 값일경우에는 문자 벡터를 반환
#' @return 토큰화된 character 벡터를 성분으로 갖는 list. simplify값이 TRUE이고 
#' x가 단일값일 때에는 character 벡터   
#' @examples
#' \donttest{
#' tokenize_noun_ngrams(president_speech$doc[1:2])
#' 
#' # simplify = TRUE
#' tokenize_noun_ngrams(president_speech$doc[1], simplify = TRUE)
#' 
#' str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."
#' 
#' tokenize_noun_ngrams(str)
#'
#' # 불용어 처리
#' tokenize_noun_ngrams(str, stopwords = "구매")
#'  
#' # 사용자 정의 사전 사용
#' dic_path <- system.file("dic", package = "bitTA")
#' dic_file <- glue::glue("{dic_path}/buzz_dic.dic")
#' tokenize_noun_ngrams(str, simplify = TRUE, user_dic = dic_file)
#' 
#' # n_min
#' tokenize_noun_ngrams(str, n_min = 1, user_dic = dic_file)
#' 
#' # ngram_delim
#' tokenize_noun_ngrams(str, ngram_delim = ":", user_dic = dic_file)
#' }
#' 
#' @export
#' @importFrom tokenizers tokenize_ngrams
tokenize_noun_ngrams <- function (x, n = 3L, n_min = n, 
                                 stopwords = character(), 
                                 ngram_delim = " ", simplify = FALSE,
                                 type = c("noun", "noun2"),
                                 user_dic = NULL) {
  words <- collapse_noun(x, type = type, user_dic = user_dic)
  tokenizers::tokenize_ngrams(
    words, n = n, n_min = n_min, stopwords = stopwords,
    ngram_delim = ngram_delim, simplify = simplify)
}


#' Wrapper around unnest_tokens for n-grams of noun
#' @description 명사를 추출하여 n-gram으로 토큰화합니다. 
#' @param tbl A data frame. 
#' @param output character or symbol. 출력열로 새로 만들 변수 이름
#' @param input character or symbol. 입력으로 사용할 변수 이름
#' @param n integer. n-gram의 단어 수입니다. 1 이상의 정수.
#' @param n_min integer. 이것은 1보다 크거나 같고 n보다 작거나 같은 정수여야 함
#' @param stopwords character. n-그램에서 제외할 불용어의 문자형 벡터
#' @param ngram_delim character. 생성된 n-gram에서 단어 사이의 구분 기호
#' @param drop logical. 원래 입력 열을 삭제해야 하는지 여부. 기본값은 TRUE이며 
#' 원래 입력 열과 새 출력 열의 이름이 같은 경우 무시됨.
#' @param collapse A character vector. 결과에서 개별 n-gram들을 그룹핑할 변수 이름. 
#' 기본값은 NULL로 개별 n-gram들을 묶지 않음.
#' @param ... 토크나이저(tokenize_noun_ngrams)에 전달되는 추가 인수
#' @return 토큰화된 character 벡터를 성분으로 갖는 list. simplify값이 TRUE이고 
#' x가 단일값일 때에는 character 벡터   
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' president_speech %>%
#'   select(title, doc) %>% 
#'   filter(row_number() <= 2) %>%
#'   unnest_noun_ngrams(
#'     noun_bigram,
#'     doc,
#'     n = 2,
#'     ngram_delim = ":",
#'     type = "noun2"
#'   )
#'   
#' president_speech %>%
#'   select(title, doc) %>% 
#'   filter(row_number() <= 2) %>%
#'   unnest_noun_ngrams(
#'     noun_bigram,
#'     doc,
#'     n = 2,
#'     ngram_delim = ":",
#'     drop = FALSE
#'   )   
#'  
#' # grouping using group_by() function
#' president_speech %>%
#'   filter(row_number() <= 4) %>%
#'   mutate(speech_year = substr(date, 1, 4)) %>% 
#'   select(speech_year, title, doc) %>% 
#'   group_by(speech_year) %>%
#'   unnest_noun_ngrams(
#'     noun_bigram,
#'     doc,
#'     n = 2,
#'     ngram_delim = ":"
#'   )
#'   
#' # grouping using collapse argument
#' president_speech %>%
#'   filter(row_number() <= 4) %>%
#'   mutate(speech_year = substr(date, 1, 4)) %>% 
#'   select(speech_year, title, doc) %>% 
#'   unnest_noun_ngrams(
#'     noun_bigram,
#'     doc,
#'     n = 2,
#'     ngram_delim = ":",
#'     collapse = "speech_year"
#'   )
#' }
#' 
#' @export
#' @importFrom tidytext unnest_tokens
unnest_noun_ngrams <- function (tbl, output, input, n = 3L, n_min = n, 
                                ngram_delim = " ", drop = TRUE, collapse = NULL, 
                                ...) {
  tidytext::unnest_tokens(
    tbl, !!enquo(output), !!enquo(input), format = "text", 
    drop = drop, collapse = collapse, token = "noun_ngrams", n = n, 
    n_min = n_min, ngram_delim = ngram_delim, ...)
}




