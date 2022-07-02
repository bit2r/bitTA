#' 텍스트 데이터 전처리를 위한 메타정보 등록 및 조회
#' @description 텍스트 데이터의 전처리 과정인 패턴 일치되는 데이터 삭제, 문자열
#' 대체, 불필요 문자열 제거, 문자열 연결 등을 수행하기 위한 메타 정보를 등록하고
#' 조회한다.
#' @param id character. 메타 정보의 아이디.
#' @param filename character. 등록할 메타 정보가 포함된 파일의 이름
#' @param sep character. 메타 정보를 기술한 파일의 컬럼 구분자
#' @param fileEncoding character. 파일의 인코딩
#' @param append	logical. 메타 정보의 추가 여부. TRUE이면, 기 등록 메타에 추가한다.
#' @return data.frame 등록된 메타정보를 담은 data.frame
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @examples
#' \dontrun{
#' meta_path <- system.file("meta", package = "bitTA")
#' fname <- glue::glue("{meta_path}/preparation_filter.csv")
#'
#' ## 데이터 필터링 메타 신규 등록
#' set_meta("filter", fname, fileEncoding = "utf8")
#'
#' ## 기 등록된 데이터 필터링 메타 조회
#' get_meta("filter")
#'
#' ## 데이터 필터링 메타 추가 등록
#' #fname <- "preparation_filter2.csv"
#' #set_meta("filter", fname, fileEncoding = "utf8", append = TRUE)
#' }
#' @export
get_meta <- function(id = c("filter", "replace", "remove", "concat", "split")) {
  id <- match.arg(id)

  get_taenv(paste("META", toupper(id), sep = "_"))
}


#' @rdname get_meta
#' @export
#' @importFrom stringr str_detect
set_meta <-function(id = c("filter", "replace", "remove", "concat", "split"),
                    filename, sep = ",", fileEncoding = "utf-8", append = FALSE) {
  id <- match.arg(id)

  if (id %in% c("replace")) {
    col.names <- c("rule_nm", "rule_class", "pattern", "replace", "use")
  } else if (id %in% c("concat", "split")) {
    col.names <- c("rule_nm", "pattern", "replace", "use")
  } else if (id == "remove") {
    col.names <- c("rule_nm", "pattern", "use")
  } else if (id == "filter") {
    col.names <- c("rule_nm", "pattern", "accept", "use")
  }

  newmeta <- read.table(filename, sep = sep, fileEncoding = fileEncoding,
                        col.names = col.names, stringsAsFactors = FALSE)

  if (append) {
    meta <- .getMeta(id)

    dup <- base::intersect(meta$pattern, newmeta$pattern)

    if (length(dup) > 0) {
      message("이미 등록된 메터 정보와 아래의 메타가 중복됩니다.\n")
      flag <- stringr::str_detect(dup, newmeta$pattern)
      print(newmeta[flag, ])
      message("기존과 중복된 메타가 있기 때문에 메타를 등록하지 않았습니다.\n")
    } else {
      meta <- rbind(meta, newmeta)

      set_taenv(paste("META", toupper(id), sep = "_"), meta)
    }
  } else {
    set_taenv(paste("META", toupper(id), sep = "_"), newmeta)
  }
}


#' 텍스트 데이터의 문자열 매치 데이터 필터링
#' @description 텍스트 데이터의 전처리 과정 중 패턴 일치되는 문자열이 있는
#' 데이터를 취하거나 제거한다.
#' @param doc character. 문자열 필터링을 수행할 문자열 벡터
#' @param chunk integer. 병렬 작업 수행 시 처리 단위인 chunk
#' @param mc.cores integer. 병렬 작업 수행 시 사용할 코어의 개수
#' @param verbos logical. 메타의 Rule 당 처리된 건수를 화면에 출력할 지의 여부
#' @return character. 문자열 필터링이 수행된 문자열 벡터.
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @examples
#' \dontrun{
#' ##======================================================
#' ## 문자열 매치 데이터 필터링
#' ##======================================================
#'
#' # 매치 데이터 필터링 메타 신규 등록
#' meta_path <- system.file("meta", package = "bitTA")
#' fname <- glue::glue("{meta_path}/preparation_filter.csv")
#' set_meta("filter", fname, fileEncoding = "utf8")
#'
#' # 등록된 필터링 룰 확인하기
#' get_meta("filter")
#'
#' doc_content <- buzz[, "CONTENT"]
#'
#' # 필터링, verbos = FALSE, chunk = 200
#' doc_content_after <- filter_text(doc_content, verbos = FALSE, chunk = 200)
#'
#' # 필터링, chunk = 500, mc.cores = 8
#' doc_content_after <- filter_text(doc_content, chunk = 500, mc.cores = 8)
#'
#' # 필터링 전/후 비교
#' NROW(doc_content)
#' length(doc_content_after)
#' }
#'
#' @export
#' @import dplyr
#' @import parallel
#' @importFrom purrr walk
#' @importFrom stringr str_detect
#' @importFrom cli cli_rule
#' @importFrom tibble is_tibble
filter_text <- function(
    doc,
    chunk = round(length(if (tibble::is_tibble(doc)) dplyr::pull(doc) else doc) / mc.cores),
    mc.cores = parallel::detectCores(),
    verbos = TRUE
  ) {
  filter_patterns <- get_meta("filter")
  filter_patterns <- filter_patterns[filter_patterns$use, ]

  if (is.null(filter_patterns)) {
    stop("문자열 필터링 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }

  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)

  filtering <- function(chunk_id, data, pattern) {
    cnt <- integer(nrow(pattern))

    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]

    tmp <- data[start:end]

    for (idx in seq(cnt)) {
      rule <- pattern[idx, "pattern"]
      accept <- pattern[idx, "accept"]

      detect <- stringr::str_detect(tmp, rule)

      if (verbos)
        cnt[idx] <- sum(detect, na.rm = TRUE) * ifelse(accept, 1, -1)

      if (accept) {
        tmp <- tmp[detect]
      } else {
        tmp <- tmp[!detect]
      }
    }

    if (verbos)
      list(docs = tmp, cnt = cnt)
    else
      list(docs = tmp)
  }

  doc <- parallel::mclapply(seq(chunk_idx$idx_start), filtering, data = doc,
                            pattern = filter_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)
    # message(paste(filter_patterns[, "rule_nm"], ":",
    #               ifelse(cnt > 0, "accepts", "rejects"), abs(cnt), "\n"))

    job_summary <- data.frame(
      rule_nm = filter_patterns[, "rule_nm"],
      flag = ifelse(cnt > 0, "accepts", "rejects"),
      cnt = abs(cnt),
      stringsAsFactors = FALSE
    ) %>%
      group_by(rule_nm, flag) %>%
      summarise(cnt = sum(cnt), .groups = "drop")

    job_summary %>%
      NROW() %>%
      seq() %>%
      purrr::walk(
        function(x) {
          cli::cli_rule(
            left = "{job_summary$flag[x]}: {job_summary$rule_nm[x]}",
            right = "{format(job_summary$cnt[x], big.mark = ',')}건"
          )
        }
      )
  }

  do.call("c", lapply(doc, function(x) x$docs))
}


#' 텍스트 데이터의 문자열 대체/제거/결합/분리
#' @description 텍스트 데이터의 전처리 과정 중 패턴 일치되는 문자열에 대해서
#' 다른 문자열로 대체하거나 제거, 혹은 결합한다.
#' @param doc character. 문자열 대체/제거/결합/분리를 수행할 문자열 벡터
#' @param chunk integer. 병렬 작업 수행 시 처리 단위인 chunk
#' @param mc.cores integer. 병렬 작업 수행 시 사용할 코어의 개수
#' @param verbos logical. 메타의 Rule 당 처리된 건수를 화면에 출력할 지의 여부
#' @return character. 문자열 대체/제거/결합이 수행된 문자열 벡터.
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @examples
#' \dontrun{
#' ##======================================================
#' ## 문자열 대체
#' ##======================================================
#'
#' # 문자열 대체 메타 신규 등록
#' meta_path <- system.file("meta", package = "bitTA")
#' fname <- glue::glue("{meta_path}/preparation_replace.csv")
#' set_meta("replace", fname, fileEncoding = "utf8")
#'
#' # 등록된 문자열 대체 룰 확인하기
#' get_meta("replace")
#'
#' doc_content <- buzz[, "CONTENT"]
#'
#' # 문자열 대체, verbos = FALSE, chunk = 200
#' doc_content_after <- replace_text(doc_content, verbos = FALSE, chunk = 200)
#'
#' # 문자열 대체, chunk = 500, mc.cores = 8
#' doc_content_after <- replace_text(doc_content, chunk = 500, mc.cores = 8)
#' }
#' @export
#' @import dplyr
#' @import parallel
#' @importFrom purrr walk
#' @importFrom stringr str_detect str_replace_all
#' @importFrom cli cli_rule
#' @importFrom tibble is_tibble
replace_text <- function(
    doc,
    chunk = round(length(if (tibble::is_tibble(doc)) dplyr::pull(doc) else doc) / mc.cores),
    mc.cores = parallel::detectCores(),
    verbos = TRUE
  ) {
  filter_patterns <- get_meta("replace")
  filter_patterns <- filter_patterns[filter_patterns$use, ]

  if (is.null(filter_patterns)) {
    stop("문자열 대체 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }

  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)

  replace <- function(chunk_id, data, pattern) {
    cnt <- integer(nrow(pattern))

    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]

    tmp <- data[start:end]

    for (idx in seq(cnt)) {
      rule <- pattern[idx, "pattern"]
      replace <- pattern[idx, "replace"]

      if (verbos)
        cnt[idx] <- sum(stringr::str_detect(tmp, rule), na.rm = TRUE)

      tmp <- stringr::str_replace_all(tmp, rule, replace)
    }

    if (verbos)
      list(docs = tmp, cnt = cnt)
    else
      list(docs = tmp)
  }

  doc <- parallel::mclapply(seq(chunk_idx$idx_start), replace, data = doc,
                            pattern = filter_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)

    job_summary <- data.frame(
      rule_nm = filter_patterns[, "rule_nm"],
      rule_class = filter_patterns[, "rule_class"],
      cnt = abs(cnt),
      stringsAsFactors = FALSE
    ) %>%
      mutate(rule_nm = glue::glue("[{rule_class}] - {rule_nm}")) %>%
      group_by(rule_nm) %>%
      summarise(cnt = sum(cnt), .groups = "drop")

    job_summary %>%
      NROW() %>%
      seq() %>%
      purrr::walk(
        function(x) {
          cli::cli_rule(
            left = "Replace: {job_summary$rule_nm[x]}",
            right = "{format(job_summary$cnt[x], big.mark = ',')}건"
          )
        }
      )
  }

  do.call("c", lapply(doc, function(x) x$docs))
}



#' @rdname replace_text
#' @examples
#' \dontrun{
#' ##======================================================
#' ## 문자열 결합
#' ##======================================================
#'
#' # 문자열 결합 메타 신규 등록
#' meta_path <- system.file("meta", package = "bitTA")
#' fname <- glue::glue("{meta_path}/preparation_concat.csv")
#' set_meta("concat", fname, fileEncoding = "utf8")
#'
#' # 등록된 문자열 결합 룰 확인하기
#' get_meta("concat")
#'
#' doc_content <- buzz[, "CONTENT"]
#'
#' ## verbos = FALSE, chunk = 200
#' doc_content_after <- concat_text(doc_content, verbos = FALSE, chunk = 200)
#'
#' ## chunk = 500, mc.cores = 8
#' doc_content_after <- concat_text(doc_content, chunk = 500, mc.cores = 8)
#' }
#' @export
#' @import dplyr
#' @import parallel
#' @importFrom purrr walk
#' @importFrom stringr str_detect str_replace_all str_split
#' @importFrom cli cli_rule
#' @importFrom tibble is_tibble
concat_text <- function(
    doc,
    chunk = round(length(if (tibble::is_tibble(doc)) dplyr::pull(doc) else doc) / mc.cores),
    mc.cores = parallel::detectCores(),
    verbos = TRUE
  ) {
  filter_patterns <- get_meta("concat")
  filter_patterns <- filter_patterns[filter_patterns$use, ]

  if (is.null(filter_patterns)) {
    stop("문자열 결합 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }

  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)

  replace <- function(chunk_id, data, pattern) {
    cnt <- integer(nrow(pattern))

    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]

    tmp <- data[start:end]

    for (idx in seq(cnt)) {
      rule <- pattern[idx, "pattern"] %>%
        stringr::str_split(" ", simplify = TRUE) %>%
        paste(collapse = "[[:space:]]+")

      replace <- pattern[idx, "replace"]

      if (verbos)
        cnt[idx] <- sum(stringr::str_detect(tmp, rule), na.rm = TRUE)

      tmp <- stringr::str_replace_all(tmp, rule, replace)
    }

    if (verbos)
      list(docs = tmp, cnt = cnt)
    else
      list(docs = tmp)
  }

  doc <- parallel::mclapply(seq(chunk_idx$idx_start), replace, data = doc,
                            pattern = filter_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)

    job_summary <- data.frame(
      rule_nm = filter_patterns[, "rule_nm"],
      cnt = abs(cnt),
      stringsAsFactors = FALSE
    ) %>%
      group_by(rule_nm) %>%
      summarise(cnt = sum(cnt), .groups = "drop")

    job_summary %>%
      NROW() %>%
      seq() %>%
      purrr::walk(
        function(x) {
          cli::cli_rule(
            left = "Concat: {job_summary$rule_nm[x]}",
            right = "{format(job_summary$cnt[x], big.mark = ',')}건"
          )
        }
      )
  }

  do.call("c", lapply(doc, function(x) x$docs))
}


#' @rdname replace_text
#' @examples
#' \dontrun{
#' ##======================================================
#' ## 문자열 분리
#' ##======================================================
#'
#' # 문자열 분리 메타 신규 등록
#' meta_path <- system.file("meta", package = "bitTA")
#' fname <- glue::glue("{meta_path}/preparation_split.csv")
#' set_meta("split", fname, fileEncoding = "utf8")
#'
#' # 등록된 문자열 분리 룰 확인하기
#' get_meta("split")
#'
#' doc_content <- buzz[, "CONTENT"]
#'
#' # 문자열 분리, verbos = FALSE, chunk = 200
#' doc_content_after <- split_text(doc_content, verbos = FALSE, chunk = 200)
#'
#' # 문자열 분리, chunk = 500, mc.cores = 8
#' doc_content_after <- split_text(doc_content, chunk = 500, mc.cores = 8)
#' }
#' @export
#' @import dplyr
#' @import parallel
#' @importFrom purrr walk
#' @importFrom stringr str_detect str_replace_all
#' @importFrom cli cli_rule
#' @importFrom tibble is_tibble
split_text <- function(
    doc,
    chunk = round(length(if (tibble::is_tibble(doc)) dplyr::pull(doc) else doc) / mc.cores),
    mc.cores = parallel::detectCores(),
    verbos = TRUE
  ) {
  filter_patterns <- get_meta("split")
  filter_patterns <- filter_patterns[filter_patterns$use, ]

  if (is.null(filter_patterns)) {
    stop("문자열 분리 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- dplyr::pull(doc)
  }

  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)

  replace <- function(chunk_id, data, pattern) {
    n_pattern <- nrow(pattern)
    cnt <- integer(n_pattern)

    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]

    tmp <- data[start:end]

    for (idx in seq(n_pattern)) {
      rule <- pattern[idx, "pattern"]
      replace <- pattern[idx, "replace"]

      if (verbos)
        cnt[idx] <- sum(stringr::str_detect(tmp, rule), na.rm = TRUE)

      tmp <- stringr::str_replace_all(tmp, rule, replace)
    }

    if (verbos)
      list(docs = tmp, cnt = cnt)
    else
      list(docs = tmp)
  }

  doc <- parallel::mclapply(seq(chunk_idx$idx_start), replace, data = doc,
                            pattern = filter_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- sum(sapply(doc, function(x) x$cnt))

    job_summary <- data.frame(
      rule_nm = filter_patterns[, "rule_nm"],
      cnt = abs(cnt),
      stringsAsFactors = FALSE
    ) %>%
      group_by(rule_nm) %>%
      summarise(cnt = sum(cnt), .groups = "drop")

    job_summary %>%
      NROW() %>%
      seq() %>%
      purrr::walk(
        function(x) {
          cli::cli_rule(
            left = "Split: {job_summary$rule_nm[x]}",
            right = "{format(job_summary$cnt[x], big.mark = ',')}건"
          )
        }
      )
  }

  do.call("c", lapply(doc, function(x) x$docs))
}


#' @rdname replace_text
#' @examples
#' \dontrun{
#' ##======================================================
#' ## 문자열 제거
#' ##======================================================
#'
#' # 문자열 제거 메타 신규 등록
#' meta_path <- system.file("meta", package = "bitTA")
#' fname <- glue::glue("{meta_path}/preparation_remove.csv")
#' set_meta("remove", fname, fileEncoding = "utf8")
#'
#' # 등록된 문자열 제거 룰 확인하기
#' get_meta("remove")
#'
#' doc_content <- buzz[, "CONTENT"]
#'
#' ## verbos = FALSE, chunk = 800
#' doc_content_after <- remove_text(doc_content, verbos = FALSE, chunk = 800)
#'
#' ## chunk = 500, mc.cores = 8
#' doc_content_after <- remove_text(doc_content, chunk = 500, mc.cores = 8)
#' }
#'
#' @export
#' @import dplyr
#' @import parallel
#' @importFrom purrr walk
#' @importFrom stringr str_detect str_remove_all
#' @importFrom cli cli_rule
#' @importFrom tibble is_tibble
remove_text <- function(
    doc,
    chunk = round(length(if (tibble::is_tibble(doc)) dplyr::pull(doc) else doc) / mc.cores),
    mc.cores = parallel::detectCores(),
    verbos = TRUE
  ) {
  filter_patterns <- get_meta("remove")
  filter_patterns <- filter_patterns[filter_patterns$use, ]

  if (is.null(filter_patterns)) {
    stop("문자열 제거 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }

  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)

  remove <- function(chunk_id, data, pattern) {
    cnt <- integer(nrow(pattern))

    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]

    tmp <- data[start:end]

    for (idx in seq(cnt)) {
      rule <- pattern[idx, "pattern"]

      if (verbos)
        cnt[idx] <- sum(stringr::str_detect(tmp, rule), na.rm = TRUE)

      tmp <- stringr::str_remove_all(tmp, rule)
    }

    if (verbos)
      list(docs = tmp, cnt = cnt)
    else
      list(docs = tmp)
  }

  doc <- parallel::mclapply(seq(chunk_idx$idx_start), remove, data = doc,
                            pattern = filter_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)

    job_summary <- data.frame(
      rule_nm = filter_patterns[, "rule_nm"],
      cnt = abs(cnt),
      stringsAsFactors = FALSE
    ) %>%
      group_by(rule_nm) %>%
      summarise(cnt = sum(cnt), .groups = "drop")

    job_summary %>%
      NROW() %>%
      seq() %>%
      purrr::walk(
        function(x) {
          cli::cli_rule(
            left = "Removes: {job_summary$rule_nm[x]}",
            right = "{format(job_summary$cnt[x], big.mark = ',')}건"
          )
        }
      )
  }

  do.call("c", lapply(doc, function(x) x$docs))
}


#' Mecab 형태소 분석기 기반 토큰화
#' @description Mecab 형태소 분석기 기반 형태소분석/품사 태깅을 통한 토큰화
#' @param x character. 형태소 분석에 사용할 document.
#' @param type character. 형태소 분석의 결과 유형.모든 품사, 명사, 동사 및 형용사와 같은
#'  토큰화 결과 유형을 지정.
#'  "morpheme", "noun", "noun2", "verb", "adj"중에서 선택. 기본값은 "noun"로
#'  일반명사만 추출함.
#' @param indiv logical. 복수개의 문서일 때 개별 문서를 리스트로 반환할 지를 선택함.
#' TRUE이면 개별 리스트로 반환하고, FALSE이면 하나의 문자 벡터로 반환함.
#' 기본값은 TRUE
#' @param dic Mecab 형태소 분석기의 사용자 정의 사전 파일.
#' 기본값은 NULL로 사용자 사전파일을 지정하지 않음.
#' 시스템 사전인 "/usr/local/lib/mecab/dic/mecab-ko-dic"를 보완하여 사용됨.
#' 사용자 사전 파일은 mecab-dict-index 명령어로 생성되며, 확장자가 "dic"임.
#' @details
#' type 인수에 따라 토큰화되는 품사의 종류는 다음과 같다.:
#' \itemize{
#' \item "morpheme" : 모든 품사 토큰화
#' \item "moun" : 일반명사(NNG) 토큰화
#' \item "moun" : 모든 명사 토큰화
#' \item "verb" : 동사 토큰화
#' \item "adj" : 형용사 토큰화
#' }
#'
#' Mecab 형태소 분석기의 시스템 사전의 경로는 "/usr/local/lib/mecab/dic/mecab-ko-dic"이며,
#' NIADic이 포팅되어 들어 있음. 그러나, "/usr/local/lib/mecab/dic/mecab-ko-dic2"에는
#' NIADic이 포함되어 있지 않음. 이것은 bitTA 패키지에서는 참조하지 않음.
#' @return Mecab 형태소 분석기 결과 구조의 character 벡터 혹은 character 벡터를
#' 원소로 갖는 list 객체.
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @seealso \code{\link{morpho_hann}}
#' @examples
#' \dontrun{
#' ## Mecab 형태소 분석
#' morpho_mecab("아버지가 방에 들어가신다.")
#' morpho_mecab("아버지가 방에 들어가신다.", type = "morpheme")
#' morpho_mecab("아버지가 방에 들어가신다.", type = "verb")
#'
#' dic_path <- system.file("dic", package = "bitTA")
#' dic_file <- glue::glue("{dic_path}/HPUSS.dic")
#'
#' str <- "2019년 11월 임플란트 시술하였습니다. 간편가입 종신보험 또는
#'         간편경영인정기보험을 가입하려고 합니다. 타사지급이력으로 청약서
#'         발행이 안됩니다. 가입이 안되는건가요?"
#' morpho_mecab(str)
#' morpho_mecab(str, dic = dic_file)
#'
#' morpho_mecab(c("무궁화꽃이 피었습니다.", "나는 어제 올갱이국밥을 먹었다."))
#' morpho_mecab(c("무궁화꽃이 피었습니다.", "나는 어제 올갱이국밥을 먹었다."), indiv = FALSE)
#' }
#' @export
#' @import dplyr
#' @importFrom RMeCab RMeCabC
#' @importFrom purrr map
#' @importFrom stringr str_detect
morpho_mecab <- function(x, type = c("morpheme", "noun", "noun2", "verb", "adj")[2],
                         indiv = TRUE, dic = NULL) {
  tokens <- x %>%
    purrr::map(
      function(doc) {
        if (is.null(dic)) {
          morpheme <- RMeCab::RMeCabC(doc)
        } else {
          morpheme <- RMeCab::RMeCabC(doc, dic = dic)
        }

        morpheme %>%
          unlist()
      }
    )

  if (type != "morpheme") {
    if (type %in% "noun") pattern <- "NNG"
    if (type %in% "noun2") pattern <- "^N"
    if (type == "verb") pattern <- "^VV"
    if (type == "adj") pattern <- "^VA"

    tokens <- tokens %>%
      purrr::map(
        function(token) {
          idx <- stringr::str_detect(names(token), pattern)
          token[idx]
        }
      )
  }

  if (!indiv) {
    tokens <- unlist(tokens)
  }

  if (length(tokens) == 1) {
    tokens <- unlist(tokens)
  }

  tokens
}

#' Hannnanum 형태소 분석기 기반 토큰화
#' @description KoNLP 패키지 내부에 포함된 한나눔 형태소 분석기의 호출
#' @param x character. 형태소 분석에 사용할 document.
#' @param type 형태소 분석의 결과 유형.모든 품사, 명사, 동사 및 형용사와 같은
#'  토큰화 결과 유형을 지정.
#'  "morpheme", "noun", "noun2", "verb", "adj"중에서 선택. 기본값은 "noun"로
#'  명사만 추출함.
#' @param indiv logical. 복수개의 문서일 때 개별 문서를 리스트로 반환할 지를 선택함.
#' TRUE이면 개별 리스트로 반환하고, FALSE이면 하나의 문자 벡터로 반환함.
#' 기본값은 TRUE
#' @return Hannnanum 형태소 분석기 결과 구조의 character 벡터.
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @seealso \code{\link{morpho_mecab}}
#' @examples
#' \dontrun{
#' ## Hannnanum 형태소 분석
#' morpho_hann("아버지가 방에 들어가신다.")
#' morpho_hann("아버지가 방에 들어가신다.", type = "verb")
#'
#' morpho_hann(c("무궁화꽃이 피었습니다.", "나는 어제 올갱이국밥을 먹었다."))
#' morpho_hann(c("무궁화꽃이 피었습니다.", "나는 어제 올갱이국밥을 먹었다."), indiv = FALSE)
#' }
#' @export
#' @import dplyr
#' @importFrom KoNLP SimplePos22
morpho_hann <- function(x, type = c("morpheme", "noun", "noun2", "verb", "adj")[2],
                        indiv = TRUE) {
  options(java.parameters = "-Xmx8g" )

  library(KoNLP)

  tokens <- x %>%
    seq() %>%
    purrr::map(
      function(i) {
        token <- KoNLP::SimplePos22(x[i]) %>%
          sapply(stringr::str_split, pattern = "\\+") %>%
          unlist() %>%
          sapply(stringr::str_split, pattern = "/") %>%
          unlist()

        result <- token[as.logical(seq(token) %% 2)]
        names(result) <- token[!as.logical(seq(token) %% 2)]

        if (type != "morpheme") {
          if (type %in% c("noun", "noun2")) pattern <- "^N"
          if (type == "verb") pattern <- "^PV"
          if (type == "adj") pattern <- "^PA"

          idx <- stringr::str_detect(names(result), pattern)
          result <- result[idx]
        }

        result
      }
    )

  if (!indiv) {
    tokens <- unlist(tokens)
  }

  if (length(tokens) == 1) {
    tokens <- unlist(tokens)
  }

  tokens
}


#' 한글 자동 띄어쓰기
#' @description 한글 문장을 띄어쓰기 규칙에 맞게 자동으로 띄어쓰기 보정.
#' @param x character. 띄어쓰기 보정에 사용할 document.
#' @return 띄어쓰기 보정된 character 벡터.
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @examples
#' \dontrun{
#' # 한글 자동 띄어쓰기
#' get_spacing("최근음성인식정확도가높아짐에따라많은음성데이터가Text로변환되고분석되기시작했는데,이를위해잘동작하는띄어쓰기엔진은거의필수적인게되어버렸다")
#'
#' str <- "글쓰기에서맞춤법과띄어쓰기를올바르게하는것은좋은글이될수있는요건중하나이다.하지만요즘학생들은부족한어문규정지식으로인해맞춤법과띄어쓰기에서많은오류를범하기도한다.본연구는그중띄어쓰기가글을인식하는데중요한역할을하는것으로판단하여,대학생들이띄어쓰기에대해서어느정도정확하게인식하고있는지,실제오류실태는어떠한지에대해살펴서그오류를개선할수있는교육방안을마련할필요가있다고판단하였다."
#' get_spacing(str)
#' }
#' @export
get_spacing <- function(x) {
  mor <- morpho_mecab(x, type = "morpheme")
  mor <- sapply(mor, c)

  ## 조사/어미/접미사/마침표,물음표,느낌표,컴마
  idx <- grep("^J|^E|^XS|SF|SE|NNBC|SC|VCP", names(mor))

  for (i in rev(idx)) {
    mor[i-1] <- paste(mor[i-1], mor[i], sep = "")
  }

  mor <- mor[-idx]

  paste(mor, collapse = " ")
}


#' @import dplyr
#' @importFrom stringr str_which str_detect
#' @importFrom purrr map_df
#' @export
get_opinion <- function(doc, n = 1, agg = TRUE) {
  data("polarity")

  get_ngram <- function(x, n = 1) {
    morp <- unlist(morpho_mecab(x, type = "morpheme"))
    morp <- morp[stringr::str_which(names(morp), "[^SF]")]

    morp <- paste(morp, names(morp), sep = "/")

    N <- length(morp)
    term <- character(N - n + 1)

    for (i in seq(term)) {
      term[i] <- paste(morp[i:(i+n-1)], collapse = ";")
    }

    term
  }

  get_polarity <- function(x, n = 1) {
    data.frame(ngram = get_ngram(x, n = n), stringsAsFactors = FALSE) %>%
      dplyr::left_join(polarity) %>%
      dplyr::filter(!is.na(freq)) %>%
      dplyr::filter(!stringr::str_detect(ngram, "^/J")) %>%
      dplyr::filter(!stringr::str_detect(ngram, "^/ETM")) %>%
      dplyr::filter(!stringr::str_detect(ngram, "^/MM")) %>%
      dplyr::summarise(complex = mean(COMP),
                       negative = mean(NEG),
                       positive = mean(POS),
                       neutral = mean(NEUT),
                       none = mean(None),
                       vote = ifelse(n() == 0, "NOMATCH",
                                     names(sort(table(max.value), decreasing = TRUE))[1]),
                       polarity = (positive - negative) / (positive + negative),
                       subjectivity = (positive + negative) /
                         (positive + negative + complex + neutral + none))
  }

  result <- doc %>%
    purrr::map_df(get_polarity, n = n) %>%
    dplyr::filter(vote != "NOMATCH")

  if (agg) {
    if (nrow(result) > 0) {
      result  %>%
        dplyr::summarise(Complex = mean(complex, na.rm = TRUE),
                         Negative = mean(negative, na.rm = TRUE),
                         Positive = mean(positive, na.rm = TRUE),
                         Neutral = mean(neutral, na.rm = TRUE),
                         None = mean(none, na.rm = TRUE),
                         vote = names(sort(table(vote), decreasing = TRUE))[1],
                         polarity = mean((positive - negative) / (positive + negative)),
                         subjectivity = mean((positive + negative) /
                                               (positive + negative + complex + neutral + none))) %>%
        dplyr::select(complex = Complex,
                      negative = Negative,
                      positive = Positive,
                      neutral = Neutral,
                      none = None,
                      vote,
                      polarity,
                      subjectivity) %>%
        return()
    } else {
      return(result)
    }
  } else {
    data.frame(result, doc = doc) %>%
      return()
  }
}


get_chunk_id <- function(N, chunk) {
  n <- N %/%  chunk

  if (N %%  chunk > 0) n <- n + 1

  idx_start <- (seq(n) - 1) * chunk + 1

  idx_end <- seq(n) * chunk
  idx_end[n] <- N

  list(idx_start = idx_start, idx_end = idx_end)
}


