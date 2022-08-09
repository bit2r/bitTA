shinyUI(
  navbarPage("Docs Explorer",
             tabPanel("데이터",
                      sidebarPanel(
                        selectInput("dname", "데이터 로드: ", choices = list_df, 
                                    selected = list_df[1]),
                        HTML("<b>데이터 저장:</b>"),
                        fluidRow(
                          column(width = 9, textInput("savename", NULL, "newData")),
                          column(width = 3,
                                 actionButton("save", "", icon = icon("save"))))
                      ),
                      mainPanel(
                        dataTableOutput("dtable")
                      )
             ),
             tabPanel("검색/대체",
                      sidebarPanel(
                        selectInput("idname", "아이디 변수: ", choices=vnames,
                                    multiple = TRUE,  selected=vnames[1]),
                        selectInput("vname", "문자열 변수: ", choices=vnames, selected=vnames[2]),
                        selectInput("cname", "조건 변수: ", choices=cnames, selected=cnames[1]),
                        uiOutput("ui"),
                        HTML("<b>패턴 검색:</b>"),
                        fluidRow(
                          column(width = 9, textInput("pattern", NULL, "")),
                          column(width = 3,
                                 actionButton("search", "", icon = icon("search")))),
                        fluidRow(
                          column(width = 3,
                                 actionButton("up", "", icon = icon("angle-left"))),
                          column(width = 6, textInput("cnt", NULL)),
                          column(width = 3,
                                 actionButton("down", "", icon = icon("angle-right")))),
                        HTML("<b>패턴 변경:</b>"),
                        fluidRow(
                          column(width = 9, textInput("replacement", NULL, "")),
                          column(width = 3,
                                 actionButton("replace", "", icon = icon("wrench"))))
                      ),

                      # Show the simple table
                      mainPanel(
                        h4(htmlOutput("ids")),
                        htmlOutput("doc")
                      )
             ),
             tabPanel("형태소분석",
                      sidebarPanel(
                        selectInput("vname2", "문자열 변수: ", choices=vnames, selected=vnames[2]),
                        HTML("<b>패턴 검색:</b>"),
                        fluidRow(
                          column(width = 9, textInput("pattern2", NULL, "")),
                          column(width = 3,
                                 actionButton("search2", "", icon = icon("search")))),
                        fluidRow(
                          column(width = 3,
                                 actionButton("up2", "", icon = icon("angle-left"))),
                          column(width = 6, textInput("cnt2", NULL)),
                          column(width = 3,
                                 actionButton("down2", "", icon = icon("angle-right"))))
                      ),

                      # Show the simple table
                      mainPanel(
                        h4("형태소 단위"),
                        htmlOutput("doc2"),
                        h4("원문"),
                        htmlOutput("doc21")
                      )
             ),
             tabPanel("공동발생분석",
                      sidebarPanel(
                        selectInput("vname3", "문자열 변수: ", choices=vnames, selected=vnames[2]),
                        HTML("<b>단어 검색:</b>"),
                        fluidRow(
                          column(width = 9, textInput("pattern3", NULL, "")),
                          column(width = 3,
                                 actionButton("search3", "", icon = icon("search")))),
                        sliderInput("span", label = h5("Span"), min = 1,
                                    max = 5, value = 2),
                        fluidRow(
                          column(width = 3,
                                 actionButton("up3", "", icon = icon("angle-left"))),
                          column(width = 6, textInput("cnt3", NULL)),
                          column(width = 3,
                                 actionButton("down3", "", icon = icon("angle-right"))))
                      ),

                      # Show the simple table
                      mainPanel(
                        h4("공동발생 정보"),
                        dataTableOutput("ctable"),
                        h4("원문"),
                        htmlOutput("doc3")
                      )
             ),
             tabPanel("N-Gram",
                      sidebarPanel(
                        selectInput("vname4", "문자열 변수: ", choices=vnames, selected=vnames[2]),
                        HTML("<b>단어 검색:</b>"),
                        fluidRow(
                          column(width = 9, textInput("pattern4", NULL, "")),
                          column(width = 3,
                                 actionButton("search4", "", icon = icon("search")))),
                        sliderInput("N", label = h5("N"), min = 1,
                                    max = 5, value = 2),
                        fluidRow(
                          column(width = 3,
                                 actionButton("up4", "", icon = icon("angle-left"))),
                          column(width = 6, textInput("cnt4", NULL)),
                          column(width = 3,
                                 actionButton("down4", "", icon = icon("angle-right"))))
                      ),

                      # Show the simple table
                      mainPanel(
                        h4("N-Gram 정보"),
                        dataTableOutput("ntable"),
                        h4("원문"),
                        htmlOutput("doc4")
                      )
             ),
             tabPanel("R Command",
                      fluidPage(
                        textAreaInput("cmd", "R Command: ", width = "800px",
                                      height = "300px"),
                        actionButton("run", "Run"),
                        hr(),
                        verbatimTextOutput("result")
                      )
             )
  )
)
