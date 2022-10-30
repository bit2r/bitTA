#' Meta information processing for text data preprocessing
#' @description 텍스트 데이터의 전처리 과정인 패턴 일치되는 데이터 삭제, 문자열
#' 대체, 불필요 문자열 제거, 문자열 연결 등을 수행하기 위한 메타 정보를 등록하고
#' 조회한다.
#' @param id character. 메타 정보의 아이디.
#' @param filename character. 등록할 메타 정보가 포함된 파일의 이름
#' @param sep character. 메타 정보를 기술한 파일의 컬럼 구분자
#' @param fileEncoding character. 파일의 인코딩
#' @param append	logical. 메타 정보의 추가 여부. TRUE이면, 기 등록 메타에 추가한다.
#' @return data.frame 등록된 메타정보를 담은 data.frame
#' @examples
#' \donttest{
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


#' Filter data based on string matches of text data
#' @description 텍스트 데이터의 전처리 과정 중 패턴 일치되는 문자열이 있는
#' 데이터를 취하거나 제거한다.
#' @param doc character. 문자열 필터링을 수행할 문자열 벡터
#' @param as_logical logical. 반환값을 논리벡터로 반환할지의 여부. 
#' 기본값 TRUE이면 추출한 대상을 의미하는 논리값을 반환하고, 
#' FALSE이면 대상을 추출한 문자열 벡터를 반환. 
#' tidytext 패키지를 사용할 경우에는 기본값인 TRUE를 사용하면 됨
#' @param chunk integer. 병렬 작업 수행 시 처리 단위인 chunk
#' @param mc.cores integer. 병렬 작업 수행 시 사용할 코어의 개수
#' @param verbos logical. 메타의 Rule 당 처리된 건수를 화면에 출력할 지의 여부
#' @return character. 문자열 필터링이 수행된 문자열 벡터.
#' @details Windows 운영체제에서는 병력작업이 지원되지 않기 때문에, 사용자의 설정과는 무관하게 mc.cores의 값이 1로 적용됩니다.
#' @examples
#' \donttest{
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
#' doc_after_logical <- filter_text(doc_content, verbos = FALSE, chunk = 200)
#'
#' # 필터링, as_logical = FALSE,  mc.cores = 8, 
#' doc_after_character <- filter_text(doc_content, as_logical = FALSE, mc.cores = 8)
#'
#' # 필터링 전/후 비교
#' NROW(doc_content)
#' sum(doc_after_logical)
#' NROW(doc_after_character)
#' 
#' # tidyverse(혹은 tidytext)와의 협업
#' library(dplyr)
#' buzz %>% 
#'   filter(filter_text(CONTENT, verbos = FALSE)) %>% 
#'   select(KEYWORD, SRC, CONTENT)
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
    as_logical = TRUE,
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

  if (get_os() %in% "windows") {
    mc.cores <- 1
    
    msg <- "Window 운영체제에서는 병렬처리를 지원하지 않기 때문에 mc.cores = 1이 적용됩니다."
    cli_alert_info(msg)
  }
  
  chunk_idx <- get_chunk_id(N = length(doc), chunk = chunk)

  filtering <- function(chunk_id, data, pattern, as_logical) {
    cnt <- integer(nrow(pattern))

    start <- chunk_idx$idx_start[chunk_id]
    end <- chunk_idx$idx_end[chunk_id]

    chunks <- data[start:end]
    is_accept_allow <- rep(FALSE, length(start:end))
    is_accept_deny <- rep(TRUE, length(start:end))

    for (idx in seq(cnt)) {
      rule <- pattern[idx, "pattern"]
      accept <- pattern[idx, "accept"]

      detect_docs <- stringr::str_detect(chunks, rule)

      if (verbos)
        cnt[idx] <- sum(detect_docs, na.rm = TRUE) * ifelse(accept, 1, -1)

      if (accept) {
        is_accept_allow <- is_accept_allow | detect_docs
      } else {
        is_accept_deny <- is_accept_deny & !detect_docs
      }   
    }
    
    is_accept <- is_accept_allow | is_accept_deny

    if (!as_logical) {
      result <- chunks[is_accept]
    } else {
      result <- is_accept
    }
    
    if (verbos)
      list(docs = result, cnt = cnt)
    else
      list(docs = result)
  }

  doc <- parallel::mclapply(
    seq(chunk_idx$idx_start), 
    filtering, data = doc,
    as_logical = as_logical,
    pattern = filter_patterns, 
    mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)

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

  result <- do.call("c", lapply(doc, function(x) x$docs))
  
  idx_na <- is.na(result) %>% 
    which

  if (length(idx_na) > 0) {
    if (as_logical) {
      result[idx_na] <- FALSE
    } else {
      result <- result[-c(idx_na)]
    }  

    if (verbos) {
      cli::cli_rule(
        left = "Missing Check: Removing NA",
        right = "{format(length(idx_na), big.mark = ',')}건"
      )
    }    
  }
  
  result
}


#' Replace/remove/join/separate strings in text data
#' @description 텍스트 데이터의 전처리 과정 중 패턴 일치되는 문자열에 대해서
#' 다른 문자열로 대체하거나 제거, 혹은 결합한다.
#' @param doc character. 문자열 대체/제거/결합/분리를 수행할 문자열 벡터
#' @param chunk integer. 병렬 작업 수행 시 처리 단위인 chunk
#' @param mc.cores integer. 병렬 작업 수행 시 사용할 코어의 개수
#' @param verbos logical. 메타의 Rule 당 처리된 건수를 화면에 출력할 지의 여부
#' @return character. 문자열 대체/제거/결합이 수행된 문자열 벡터.
#' @details Windows 운영체제에서는 병력작업이 지원되지 않기 때문에, 사용자의 설정과는 무관하게 mc.cores의 값이 1로 적용됩니다.
#' @examples
#' \donttest{
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
  replace_patterns <- get_meta("replace")
  replace_patterns <- replace_patterns[replace_patterns$use, ]

  if (is.null(replace_patterns)) {
    stop("문자열 대체 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }

  if (get_os() %in% "windows") {
    mc.cores <- 1
    
    msg <- "Window 운영체제에서는 병렬처리를 지원하지 않기 때문에 mc.cores = 1이 적용됩니다."
    cli_alert_info(msg)
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
                            pattern = replace_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)

    job_summary <- data.frame(
      rule_nm = replace_patterns[, "rule_nm"],
      rule_class = replace_patterns[, "rule_class"],
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
#' \donttest{
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
  concat_patterns <- get_meta("concat")
  concat_patterns <- concat_patterns[concat_patterns$use, ]

  if (is.null(concat_patterns)) {
    stop("문자열 결합 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }

  if (get_os() %in% "windows") {
    mc.cores <- 1
    
    msg <- "Window 운영체제에서는 병렬처리를 지원하지 않기 때문에 mc.cores = 1이 적용됩니다."
    cli_alert_info(msg)
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
                            pattern = concat_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)

    job_summary <- data.frame(
      rule_nm = concat_patterns[, "rule_nm"],
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
#' \donttest{
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
  split_patterns <- get_meta("split")
  split_patterns <- split_patterns[split_patterns$use, ]

  if (is.null(split_patterns)) {
    stop("문자열 분리 메타 정보를 등록하지 않았습니다.")
  }

  if (get_os() %in% "windows") {
    mc.cores <- 1
    
    msg <- "Window 운영체제에서는 병렬처리를 지원하지 않기 때문에 mc.cores = 1이 적용됩니다."
    cli_alert_info(msg)
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
                            pattern = split_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- sum(sapply(doc, function(x) x$cnt))

    job_summary <- data.frame(
      rule_nm = split_patterns[, "rule_nm"],
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
#' \donttest{
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
  remove_patterns <- get_meta("remove")
  remove_patterns <- remove_patterns[remove_patterns$use, ]

  if (is.null(remove_patterns)) {
    stop("문자열 제거 메타 정보를 등록하지 않았습니다.")
  }

  if (tibble::is_tibble(doc)) {
    doc <- pull(doc)
  }
  
  if (get_os() %in% "windows") {
    mc.cores <- 1
    
    msg <- "Window 운영체제에서는 병렬처리를 지원하지 않기 때문에 mc.cores = 1이 적용됩니다."
    cli_alert_info(msg)
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
                            pattern = remove_patterns, mc.cores = mc.cores)

  if (verbos) {
    cnt <- apply(sapply(doc, function(x) x$cnt), 1, sum)

    job_summary <- data.frame(
      rule_nm = remove_patterns[, "rule_nm"],
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

