server <- function(input, output, session) {
  
  # >>>>>>>>>>>>>>>>----
  # reactive values----
  # _page navigation----
  sm <- reactiveValues(
    go = 0, # password correct
    pg = 1, # selected page number
    mb = 0, # member number (1-5)
  )
  
  # reactive variables to store match values
  mtch <- reactiveValues(
    p = 0, # the point index for match
    s = 1, # the set index of the match (1 to 5)
    srv = NULL, # who is serving in the current game (1 for A, 2 for B)
    rtn = NULL, # who is returning in the current game (1 for A, 2 for B)
    status = TRUE, # TRUE = match still active, FALSE = match over
    pts = c(0, 0), # the points won by each player per game 
    gms = c(0, 0), # the games won by each player per set
    sts = c(0, 0), # the sets won by each player per match
    setGms = matrix(c(rep(0, 10)),
                    nrow = 5) # the number of games won in each set by player
  )
  
  # reactive variables to store player probabilities
  # p1 = prob of first serve in
  # p2 = prob of second serve in
  # p3 = prob of return first serve in
  # p4 = prob of return second serve in
  # p5 = prob of return non-serve win
  plyr <- reactiveValues(
    p = matrix(c(0.5, 0.5, 0.5, 0.5, 0.5,
                 0.5, 0.5, 0.5, 0.5, 0.5),
               nrow = 5)
  )
  
  # >>>>>>>>>>>>>>>>----
  # ui home page----
  output$uiHome <- renderUI(
    if (input$tabs == 1) {
      div(
        style = paste0(
          'height:100vh; ',
          'top:0; right:0; bottom:0; left:0; ',
          'background-image:url("smash1.jpg"); ',
          'background-repeat:no-repeat; ',
          'background-size:cover;'
        ),
        div(
          align = 'center',
          style = 'padding:40vh 5% 0 75%;',
          p(
            style = 'color:orange; font-size:60px;',
            'SMASH!'
          ),
          hr(),
          h2(
            style = 'color:white; padding:0; margin:0;',
            'The Ultimate'
          ),
          h2(
            style = 'color:white; padding:0; margin:0;',
            'Tennis Match'
          ),
          h2(
            style = 'color:white; padding:0; margin:0;',
            'Simulator'
          ),
          hr(),
          tags$a(
            href = 'https://www.atptour.com/',
            target = '_blank',
            img(
              src = 'atp.png',
              width = '100px'
            )
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>----
  # ui frop page----
  output$uiFrop <- renderUI(
    if (input$tabs == 2) {
      div(
        style = paste0(
          'height:100vh; ',
          'background-image:url("smash2.jpg"); ',
          'background-repeat:no-repeat; ',
          'background-size:cover;'
        ),
        align = 'center',
        div(
          style = 'padding:10vh 30% 10px 30%;',
          htmlOutput('scorebd'),
          br(),
          actionBttn(
            inputId = 'play',
            label = 'Play',
            style = 'jelly',
            color = 'warning',
            size = 'lg',
            icon = icon('play')
          )
        ),
        div(
          style = 'padding:0 20% 0 20%;',
          wellPanel(
            fluidRow(
              column(
                width = 6,
                h5('Player A'),
                sliderInput('A1', 'First Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('A2', 'Second Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('A3', 'Return First Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('A4', 'Return Second Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('A5', 'Return All Others In', 0, 1, 0.5, 0.1, ticks = FALSE)
              ),
              column(
                width = 6,
                h5('Player B'),
                sliderInput('B1', 'First Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('B2', 'Second Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('B3', 'Return First Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('B4', 'Return Second Serve In', 0, 1, 0.5, 0.1, ticks = FALSE),
                sliderInput('B5', 'Return All Others In', 0, 1, 0.5, 0.1, ticks = FALSE)
              )
            )
          )
        )
      )
    }
  )
  
  # _ui scoreboard----
  output$scorebd <- renderText(
    paste0(
      '<style>table.score {background-color: #000000; padding: 10px; ',
      'border: 5px solid black; border-radius:10px;} ',
      'th.score {color: #ffffff; padding: 10px; text-align: center; ',
      'font-size: 18px; border: 5px solid black; background-color: #c74cff;} ',
      'td.score1 {color: #ffffff; padding: 10px; text-align: center; ',
      'font-size: 18px; border: 5px solid black; background-color: #267f26;} ',
      'td.score2 {color: #ffffff; padding: 10px; text-align: center; ',
      'font-size: 18px; border: 5px solid black; background-color: #267f26;}</style>',
      '<table class = "score" width = "100%">',
      '<col width = "24%"><col width = "2%"><col width = "12%"><col width = "2%">',
      '<col width = "12%"><col width = "12%">',
      '<col width = "12%"><col width = "12%"><col width = "12%">',
      '<tr><th colspan = 4></th>',
      '<th class = "score" colspan = 5>Set</th></tr>',
      '<tr><th class = "score">Player</th><th></th>',
      '<th class = "score">Game</th><th></th>',
      '<th class = "score">1</th><th class = "score">2</th>',
      '<th class = "score">3</th><th class = "score">4</th>',
      '<th class = "score">5</th></tr>',
      '<tr><td class = "score1">A</td><td></td>',
      '<td class = "score1">',mtch$pts[1], '</td><td></td>',
      '<td class = "score1">', mtch$setGms[1, 1], '</td>',
      '<td class = "score1">', mtch$setGms[2, 1], '</td>',
      '<td class = "score1">', mtch$setGms[3, 1], '</td>',
      '<td class = "score1">', mtch$setGms[4, 1], '</td>',
      '<td class = "score1">', mtch$setGms[5, 1], '</td></tr>',
      '<tr><td class = "score2">B</td><td></td>',
      '<td class = "score2">',mtch$pts[2], '</td><td></td>',
      '<td class = "score2">', mtch$setGms[1, 2], '</td>',
      '<td class = "score2">', mtch$setGms[2, 2], '</td>',
      '<td class = "score2">', mtch$setGms[3, 2], '</td>',
      '<td class = "score2">', mtch$setGms[4, 2], '</td>',
      '<td class = "score2">', mtch$setGms[5, 2], '</td>',
      '</tr></table>'
    )
  )
  
  # event play----
  observeEvent(
    eventExpr = input$play,
    {
      
      # reset match
      mtch$p = 0
      mtch$s = 1
      mtch$status = TRUE
      mtch$pts = c(0, 0)
      mtch$gms = c(0, 0)
      mtch$sts = c(0, 0)
      mtch$setGms = matrix(c(rep(0, 10)), nrow = 5)
      
      while (mtch$status) {
        
        mtch$p <- mtch$p + 1
        
        # temp variables for game and set statuses
        gOver <- FALSE
        sOver <- FALSE
        
        # determine who serves first
        if (mtch$p == 1) {
          mtch$srv <- sample(1:2, 1) 
          mtch$rtn <- ifelse(mtch$srv == 1, 2, 1)
        }
        
        # play point
        r1 <- runif(1)
        if (plyr$p[1, mtch$srv] > r1) { # first serve in
          r12 <- runif(1)
          if (plyr$p[3, mtch$rtn] > r12) { # return first serve in
            r13 <- runif(1)
            if (plyr$p[5, mtch$srv] > r13) { # point won by srv
              mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
            } else { # point won by rtn
              mtch$pts[mtch$rtn] <- mtch$pts[mtch$rtn] + 1
            }
          } else { # point won by srv
            mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
          }
        } else { # first serve out
          r2 <- runif(1)
          if (plyr$p[2, mtch$srv] > r2) { # second serve in
            r22 <- runif(1)
            if (plyr$p[4, mtch$rtn] > r22) { # return second serve in
              r23 <- runif(1)
              if (plyr$p[5, mtch$srv] > r23) { # point won by srv
                mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
              } else { # point won by rtn
                mtch$pts[mtch$rtn] <- mtch$pts[mtch$rtn] + 1
              }
            } else { # point won by srv
              mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
            }
          } else { # second serve out aka double fault; point won by rtn
            mtch$pts[mtch$rtn] <- mtch$pts[mtch$rtn] + 1
          }
        }
        
        # is game over?
        ######## Deuce Logic ########
        if ((mtch$pts[mtch$srv] == 4) && (mtch$pts[mtch$srv] - mtch$pts[mtch$rtn] >= 2)) {
          mtch$gms[mtch$srv] <- mtch$gms[mtch$srv] + 1
          mtch$setGms[mtch$s, mtch$srv] <- mtch$setGms[mtch$s, mtch$srv] + 1
          mtch$pts <- c(0, 0)
          gOver <- TRUE
        }
        else if ((mtch$pts[mtch$rtn] == 4) && (mtch$pts[mtch$rtn] - mtch$pts[mtch$srv] >= 2))  {
          mtch$gms[mtch$rtn] <- mtch$gms[mtch$rtn] + 1
          mtch$setGms[mtch$s, mtch$rtn] <- mtch$setGms[mtch$s, mtch$rtn] + 1
          mtch$pts <- c(0, 0)
          gOver <- TRUE
        }
        ####################################
       
        # if (mtch$pts[mtch$srv] == 4) {
        #   mtch$gms[mtch$srv] <- mtch$gms[mtch$srv] + 1
        #   mtch$setGms[mtch$s, mtch$srv] <- mtch$setGms[mtch$s, mtch$srv] + 1
        #   mtch$pts <- c(0, 0)
        #   gOver <- TRUE
        # } else if (mtch$pts[mtch$rtn] == 4) {
        #   mtch$gms[mtch$rtn] <- mtch$gms[mtch$rtn] + 1
        #   mtch$setGms[mtch$s, mtch$rtn] <- mtch$setGms[mtch$s, mtch$rtn] + 1
        #   mtch$pts <- c(0, 0)
        #   gOver <- TRUE
        # }
        
        # is set over?
        if (gOver) {
          ######## Tie Breaking Logic ########
          # if ((mtch$gms[mtch$srv] == 6) && (mtch$gms[mtch$srv] - mtch$gms[mtch$rtn] > 2)){
          #   mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
          #   mtch$gms <- c(0, 0)
          #   mtch$s <- mtch$s + 1
          #   sOver <- TRUE
          # }
          #     
          # else if ((mtch$gms[mtch$rtn] == 6) && (mtch$gms[mtch$rtn] - mtch$gms[mtch$srv] > 2)){
          #   mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
          #   mtch$gms <- c(0, 0)
          #   mtch$s <- mtch$s + 1
          #   sOver <- TRUE
          # }
          ####################################
          
            
          if (mtch$gms[mtch$srv] == 6) {
            mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
            mtch$gms <- c(0, 0)
            mtch$s <- mtch$s + 1
            sOver <- TRUE
          } else if (mtch$gms[mtch$rtn] == 6) {
            mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
            mtch$gms <- c(0, 0)
            mtch$s <- mtch$s + 1
            sOver <- TRUE
          }

        }
        
        # is match over?
        if (sOver) {
          if (mtch$sts[mtch$srv] == 3) {
            sendSweetAlert(
              session = session,
              title = 'Match Over',
              text = paste0('Player ', ifelse(mtch$srv == 1, 'A', 'B'), ' Wins!'),
              type = 'success'
            )
            mtch$status = FALSE
          } else if (mtch$sts[mtch$rtn] == 3) {
            sendSweetAlert(
              session = session,
              title = 'Match Over',
              text = paste0('Player ', ifelse(mtch$rtn == 1, 'A', 'B'), ' Wins!'),
              type = 'success'
            )
            mtch$status = FALSE
          }
        }
        
        # swap service
        if (gOver) {
          mtch$srv <- ifelse(mtch$srv == 1, 2, 1)
          mtch$rtn <- ifelse(mtch$rtn == 1, 2, 1)
        }
        
      }
      
    }
    
  )
  
  # _events player settings----
  observeEvent(input$A1, {plyr$p[1, 1] = input$A1})
  observeEvent(input$A2, {plyr$p[2, 1] = input$A2})
  observeEvent(input$A3, {plyr$p[3, 1] = input$A3})
  observeEvent(input$A4, {plyr$p[4, 1] = input$A4})
  observeEvent(input$A5, {plyr$p[5, 1] = input$A5})
  observeEvent(input$B1, {plyr$p[1, 2] = input$B1})
  observeEvent(input$B2, {plyr$p[2, 2] = input$B2})
  observeEvent(input$B3, {plyr$p[3, 2] = input$B3})
  observeEvent(input$B4, {plyr$p[4, 2] = input$B4})
  observeEvent(input$B5, {plyr$p[5, 2] = input$B5})
  
  # >>>>>>>>>>>>>>>>----
  # ui abou page----
  output$uiAbou <- renderUI(
    if (input$tabs == 3) {
      div(
        align = 'center',
        style = paste0(
          'width:100%; height:100vh; ',
          'background-image:url("smash3.jpg"); ',
          'background-repeat:no-repeat; ',
          'background-size:cover;'
        ),
        div(
          align = 'center',
          style = 'padding:20vh 1% 0 15%;',
          fluidRow(
            lapply(
              1:5, 
              function(i) {
                column(
                  width = 2,
                  tags$button(
                    id = paste0('memBio', i),
                    class = 'btn action-button',
                    style = 'background-color:rgba(0,0,0,0);',
                    img(
                      src = paste0('mem', i, '.png'),
                      width = '100%',
                      style = paste0(
                        'border:solid ',
                        ifelse(sm$mb == i, 'orange  ', 'white '),
                        '5px; border-radius:50%;'
                      )
                    )
                  ),
                  h5(
                    style = paste0(
                      'color:',
                      ifelse(sm$mb == i, 'orange;', 'white;')
                    ),
                    mem1[i]
                  )
                )
              }
            )
          )
        ),
        uiOutput('uiBio')
      )
    }
  )
  
  # event mem bttns----
  observeEvent(input$memBio1, {sm$mb <- ifelse(sm$mb == 1, 0, 1)})
  observeEvent(input$memBio2, {sm$mb <- ifelse(sm$mb == 2, 0, 2)})
  observeEvent(input$memBio3, {sm$mb <- ifelse(sm$mb == 3, 0, 3)})
  observeEvent(input$memBio4, {sm$mb <- ifelse(sm$mb == 4, 0, 4)})
  observeEvent(input$memBio5, {sm$mb <- ifelse(sm$mb == 5, 0, 5)})
  
  # ui bio----
  output$uiBio <- renderUI(
    if (sm$mb > 0) {
      div(
        style = 'padding:5vh 25% 0 25%;',
        wellPanel(
          style = paste0(
            'background-color:rgba(50,50,50,0.5); ',
            'border:solid orange 2px;'
          ),
          h3(
            style = 'color:orange;',
            member[sm$mb]
          ),
          hr(),
          p(
            style = 'text-align:justify;',
            bio[sm$mb]
          ),
          br(),
          tags$a(
            href = paste0(
            'https://www.linkedin.com/in/', lnkdin[sm$mb], '/'
            ),
            target = '_blank',
            img(
              src = 'linkedin.png',
              width = '40px'
            )
          )
        )
      )
    }
  )
  
  memBio <- function(i) {
    shinyalert(
      title = member[i],
      text = paste0(
        '<style>.myDiv {text-align:left; padding:0 20px 20px 20px;}</style>',
        '<hr><table><tr><td>',
        '<img class = "myImg" src = "mem', i, '.png" width = "200px"></td>',
        '<td valign="top"><div class = "myDiv"><p>', bio[i], '</p></div>',
        '<a href="https://www.linkedin.com/in/', lnkdin[i], '/" target="_blank">',
        '<img src = "linkedin.png" width = "40px"></img></a>',
        '</td></tr></table>'
      )
    )
  }
  
}