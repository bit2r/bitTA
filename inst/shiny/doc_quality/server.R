library(shiny)
library(bitTA)
library(RMeCab)

#################################################################
## User defined function
#################################################################
getCoCollocate <- function(x, node, span = 2) {
  library(dplyr)

  collocate_filter <- function(x) {

    # one character
    filter_char1 <- nchar(x[, "Term"]) == 1
    x <- x[!filter_char1, ]

    # asterisk
    filter_asta <- grep("\\*", x[, "Term"], invert = TRUE)
    x <- x[filter_asta, ]

    # asterisk
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

getNGram <- function(x, N = 3, pos = c("NNG", "NNB", "NNP")) {
  fname <- "tmp.txt"
  cat(x, file = fname)

  ngm <- Ngram(filename = fname, N = N, type = 1, pos = pos)

  ngm[order(ngm$Freq, decreasing = TRUE), ]
}

docs <- N <- idx <- NULL

shinyServer(function(input, output, session) {
  #################################################################
  ## Data Tab
  #################################################################
  observeEvent(input$dname, {
    assign(".docData", data.frame(get(input$dname)), pos = 1)

    obj_name <- names(.docData)

    updateSelectInput(session, "idname",
                      choices = obj_name,
                      selected = obj_name[1])

    updateSelectInput(session, "vname",
                      choices = obj_name,
                      selected = obj_name[2])

    updateSelectInput(session, "vname2",
                      choices = obj_name,
                      selected = obj_name[1])

    updateSelectInput(session, "vname3",
                      choices = obj_name,
                      selected = obj_name[1])

    updateSelectInput(session, "vname4",
                      choices = obj_name,
                      selected = obj_name[1])

    ## 조건 선택의 변수 목록 변경 로직 추가
    ## 2017-07-18
    ucnt <- apply(.docData, 2, function(x) length(unique(x)))
    cnames <- obj_name[ucnt > 1 & ucnt <= 30]
    cnames <- c("전체", cnames)

    updateSelectInput(session, "cname",
                      choices=cnames,
                      selected=cnames[1])

    output$dtable <-  renderDataTable({
      nms <- names(.docData)
      type <- mapply(.docData, FUN = class)
      data.frame(var_name = nms, var_type = type)
    }, options = list(searching = FALSE, paging = FALSE))
  })

  observeEvent(input$save, {
    assign(input$savename, .docData, pos = 1)
  })


  #################################################################
  ## Find / Replace Tab
  #################################################################
  output$ui <- renderUI({
    if (is.null(input$cname) | input$cname %in% "전체") {
      return()
    }


    cvalue <- unique(.docData[, input$cname])

    selectInput("cvalue", label = "조건변수값", choices = cvalue,
                selected = cvalue[1])
  })

  observeEvent(input$search, {
    if (!input$cname %in% "전체") {
      cidx <<- .docData[, input$cname] %in% input$cvalue
      docs <<- .docData[cidx, input$vname]
    } else {
      cidx <<- NULL
      docs <<- .docData[, input$vname]
    }

    N <<- length(docs)

    idx <<- grep(input$pattern, docs)
    n <- length(idx)

    if (n == 0) {
      showModal(modalDialog(
        title = "Important message",
        "검색된 문서가 없습니다."
      ))

      return(NULL)
    }

    curr_doc <- docs[idx[1]]

    updateTextInput(session, "cnt",
                    label = "",
                    value = sprintf("%d/%d/%d", 1, n, N))

    curr_doc <- gsub(sprintf("(%s)", input$pattern),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl = TRUE)

    output$ids <- renderText({
      ids <- ""
      for (i in seq(input$idname)) {
        if (is.null(cidx)) {
          ids <- sprintf("%s%s%s", ids, ifelse(i == 1, "", "<br>"),
                         paste(input$idname[i], .docData[, input$idname[i]][idx[1]],
                               sep = " : "))
        } else {
          ids <- sprintf("%s%s%s", ids, ifelse(i == 1, "", "<br>"),
                         paste(input$idname[i], .docData[cidx, input$idname[i]][idx[1]],
                               sep = " : "))
        }
      }

      return(ids)
    })

    output$doc <- renderUI({
      HTML(curr_doc)})
  })

  observeEvent(input$replace, {
    docs <<- gsub(input$pattern, input$replacement, docs)
    .docData[, input$vname] <<- docs

    updateTextInput(session, "pattern",
                    label = "",
                    value = input$replacement)

    updateTextInput(session, "replacement",
                    label = "",
                    value = "")
  })


  observeEvent(input$down, {
    next_idx <- as.integer(strsplit(input$cnt, "/")[[1]][1]) + 1
    n <- as.integer(strsplit(input$cnt, "/")[[1]][2])

    if (next_idx > n)
      return(NULL)

    curr_doc <- docs[idx[next_idx]]

    curr_doc <- gsub(sprintf("(%s)", input$pattern),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    updateTextInput(session, "cnt",
                    label = "",
                    value = sprintf("%d/%d/%d", next_idx, n, N))

    output$ids <- renderText({
      ids <- ""
      for (i in seq(input$idname)) {
        if (is.null(cidx)) {
          ids <- sprintf("%s%s%s", ids, ifelse(i == 1, "", "<br>"),
                         paste(input$idname[i], .docData[, input$idname[i]][idx[next_idx]],
                               sep = " : "))
        } else {
          ids <- sprintf("%s%s%s", ids, ifelse(i == 1, "", "<br>"),
                         paste(input$idname[i], .docData[cidx, input$idname[i]][idx[next_idx]],
                               sep = " : "))
        }
      }

      return(ids)
    })

    output$doc <- renderText({
      HTML(curr_doc)})
  })

  observeEvent(input$up, {
    prev_idx <- as.integer(strsplit(input$cnt, "/")[[1]][1]) - 1
    n <- as.integer(strsplit(input$cnt, "/")[[1]][2])

    if (prev_idx < 1)
      return(NULL)

    curr_doc <- docs[idx[prev_idx]]

    updateTextInput(session, "cnt",
                    label = "",
                    value = sprintf("%d/%d/%d", prev_idx, n, N))

    curr_doc <- gsub(sprintf("(%s)", input$pattern),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    output$ids <- renderText({
      ids <- ""
      for (i in seq(input$idname)) {
        if (is.null(cidx)) {
          ids <- sprintf("%s%s%s", ids, ifelse(i == 1, "", "<br>"),
                         paste(input$idname[i], .docData[, input$idname[i]][idx[prev_idx]],
                               sep = " : "))
        } else {
          ids <- sprintf("%s%s%s", ids, ifelse(i == 1, "", "<br>"),
                         paste(input$idname[i], .docData[cidx, input$idname[i]][idx[prev_idx]],
                               sep = " : "))
        }
      }

      return(ids)
    })

    output$doc <- renderUI({
      HTML(curr_doc)})
  })

  #################################################################
  ## 형태소 분석 Tab
  #################################################################
  observeEvent(input$search2, {
    docs <<- .docData[, input$vname2]
    N <<- length(docs)

    idx <<- grep(input$pattern2, docs)
    n <- length(idx)

    if (n == 0) {
      showModal(modalDialog(
        title = "Important message",
        "검색된 문서가 없습니다."
      ))

      return(NULL)
    }

    curr_doc <- docs[idx[1]]

    updateTextInput(session, "cnt2",
                    label = "",
                    value = sprintf("%d/%d/%d", 1, n, N))

    mor <- morpho_mecab(curr_doc)

    curr_doc <- gsub(sprintf("(%s)", input$pattern2),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    output$doc2 <- renderUI({
      HTML(paste(mor, collapse = " "))
    })
    output$doc21 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$down2, {
    next_idx <- as.integer(strsplit(input$cnt2, "/")[[1]][1]) + 1
    n <- as.integer(strsplit(input$cnt2, "/")[[1]][2])

    if (next_idx > n)
      return(NULL)

    curr_doc <- docs[idx[next_idx]]

    mor <- morpho_mecab(curr_doc)

    curr_doc <- gsub(sprintf("(%s)", input$pattern2),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    updateTextInput(session, "cnt2",
                    label = "",
                    value = sprintf("%d/%d/%d", next_idx, n, N))

    output$doc2 <- renderUI({
      HTML(paste(mor, collapse = " "))
    })
    output$doc21 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$up2, {
    prev_idx <- as.integer(strsplit(input$cnt2, "/")[[1]][1]) - 1
    n <- as.integer(strsplit(input$cnt2, "/")[[1]][2])

    if (prev_idx < 1)
      return(NULL)

    curr_doc <- docs[idx[prev_idx]]

    mor <- morpho_mecab(curr_doc)

    updateTextInput(session, "cnt2",
                    label = "",
                    value = sprintf("%d/%d/%d", prev_idx, n, N))

    curr_doc <- gsub(sprintf("(%s)", input$pattern2),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    output$doc2 <- renderUI({
      HTML(paste(mor, collapse = " "))
    })
    output$doc21 <- renderUI({
      HTML(curr_doc)
    })
  })

  #################################################################
  ## 공동발생분석 Tab
  #################################################################
  observeEvent(input$search3, {
    if (input$pattern3 == "") {
      showModal(modalDialog(
        title = "Important message",
        "검색할 단어를 입력해야 합니다."
      ))

      return(NULL)
    }

    docs <<- .docData[, input$vname3]
    N <<- length(docs)

    idx <<- grep(input$pattern3, docs)
    n <- length(idx)

    if (n == 0) {
      showModal(modalDialog(
        title = "Important message",
        "검색된 문서가 없습니다."
      ))

      return(NULL)
    }

    curr_doc <- docs[idx[1]]

    updateTextInput(session, "cnt3",
                    label = "",
                    value = sprintf("%d/%d/%d", 1, n, N))

    coll <- getCoCollocate(curr_doc, node = input$pattern3, span = input$span)

    curr_doc <- gsub(sprintf("(%s)", input$pattern3),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    output$ctable <-  renderDataTable({
      coll
    }, options = list(
      pageLength = 10))

    output$doc3 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$down3, {
    next_idx <- as.integer(strsplit(input$cnt3, "/")[[1]][1]) + 1
    n <- as.integer(strsplit(input$cnt3, "/")[[1]][2])

    if (next_idx > n)
      return(NULL)

    curr_doc <- docs[idx[next_idx]]

    coll <- getCoCollocate(curr_doc, node = input$pattern3, span = input$span)

    curr_doc <- gsub(sprintf("(%s)", input$pattern3),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    updateTextInput(session, "cnt3",
                    label = "",
                    value = sprintf("%d/%d/%d", next_idx, n, N))

    output$ctable <-  renderDataTable({
      coll
    }, options = list(
      pageLength = 10))

    output$doc3 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$up3, {
    prev_idx <- as.integer(strsplit(input$cnt3, "/")[[1]][1]) - 1
    n <- as.integer(strsplit(input$cnt3, "/")[[1]][2])

    if (prev_idx < 1)
      return(NULL)

    curr_doc <- docs[idx[prev_idx]]

    coll <- getCoCollocate(curr_doc, node = input$pattern3, span = input$span)

    updateTextInput(session, "cnt3",
                    label = "",
                    value = sprintf("%d/%d/%d", prev_idx, n, N))

    curr_doc <- gsub(sprintf("(%s)", input$pattern3),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    output$ctable <-  renderDataTable({
      coll
    }, options = list(
      pageLength = 10))

    output$doc3 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$span, {
    if (is.null(docs))
      return(NULL)

    curr_idx <- as.integer(strsplit(input$cnt3, "/")[[1]][1])
    curr_doc <- docs[idx[curr_idx]]

    coll <- getCoCollocate(curr_doc, node = input$pattern3, span = input$span)

    output$ctable <-  renderDataTable({
      coll
    }, options = list(
      pageLength = 10))

    output$doc3 <- renderUI({
      curr_doc <- gsub(sprintf("(%s)", input$pattern3),
                       "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                       curr_doc, perl=TRUE)

      HTML(curr_doc)
    })
  })

  #################################################################
  ## N-Gram 분석 Tab
  #################################################################
  observeEvent(input$search4, {
    docs <<- .docData[, input$vname4]
    N <<- length(docs)

    idx <<- grep(input$pattern4, docs)
    n <- length(idx)

    if (n == 0) {
      showModal(modalDialog(
        title = "Important message",
        "검색된 문서가 없습니다."
      ))

      return(NULL)
    }

    curr_doc <- docs[idx[1]]

    updateTextInput(session, "cnt4",
                    label = "",
                    value = sprintf("%d/%d/%d", 1, n, N))

    ngm <- getNGram(curr_doc, N = input$N)

    curr_doc <- gsub(sprintf("(%s)", input$pattern4),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    output$ntable <-  renderDataTable({
      ngm
    }, options = list(
      pageLength = 10))

    output$doc4 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$down4, {
    next_idx <- as.integer(strsplit(input$cnt4, "/")[[1]][1]) + 1
    n <- as.integer(strsplit(input$cnt4, "/")[[1]][2])

    if (next_idx > n)
      return(NULL)

    curr_doc <- docs[idx[next_idx]]

    ngm <- getNGram(curr_doc, N = input$N)

    curr_doc <- gsub(sprintf("(%s)", input$pattern4),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl=TRUE)

    updateTextInput(session, "cnt4",
                    label = "",
                    value = sprintf("%d/%d/%d", next_idx, n, N))

    output$ntable <-  renderDataTable({
      ngm
    }, options = list(
      pageLength = 10))

    output$doc4 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$up4, {
    prev_idx <- as.integer(strsplit(input$cnt4, "/")[[1]][1]) - 1
    n <- as.integer(strsplit(input$cnt4, "/")[[1]][2])

    if (prev_idx < 1)
      return(NULL)

    curr_doc <- docs[idx[prev_idx]]

    ngm <- getNGram(curr_doc, N = input$N)

    updateTextInput(session, "cnt4",
                    label = "",
                    value = sprintf("%d/%d/%d", prev_idx, n, N))

    curr_doc <- gsub(sprintf("(%s)", input$pattern4),
                     "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                     curr_doc, perl = TRUE)

    output$ntable <-  renderDataTable({
      ngm
    }, options = list(
      pageLength = 10))

    output$doc4 <- renderUI({
      HTML(curr_doc)
    })
  })

  observeEvent(input$N, {
    if (is.null(docs))
      return(NULL)

    curr_idx <- as.integer(strsplit(input$cnt4, "/")[[1]][1])
    curr_doc <- docs[idx[curr_idx]]

    ngm <- getNGram(curr_doc, N = input$N)

    output$ntable <-  renderDataTable({
      ngm
    }, options = list(
      pageLength = 10))

    output$doc4 <- renderUI({
      curr_doc <- gsub(sprintf("(%s)", input$pattern4),
                       "<font color=\"#FF0000\"><b>\\1</b></font>\\2",
                       curr_doc, perl = TRUE)
      HTML(curr_doc)
    })
  })

  observeEvent(input$run, {
    if (is.null(input$cmd))
      return(NULL)

    raw_cmd <- input$cmd
    cmds <- rlang::parse_exprs(raw_cmd)

    results <- lapply(cmds, rlang::eval_bare)

    strs <- ""
    for (i in seq(results)) {
      if (i == 1) {
        strs <- paste0("\n", capture.output(results[[i]]), collapse = "")
        strs <- sprintf("[Result %s]%s", i, strs)
      }

      else {
        tmp  <- paste0("\n", capture.output(results[[i]]), collapse = "")
        tmp <- sprintf("\n\n[Result %s]%s", i, tmp)
        strs <- paste(strs, tmp, collapse = "\n\n\n")
      }
    }

    output$result <- renderText(strs)

  })
})
