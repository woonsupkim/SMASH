server <- function(input, output, session) {
  
  # >>>>>>>>>>>>>>>>----
  # reactive values----
  # _page navigation----
  sm <- reactiveValues(
    mb = 0, # member number (1-6)
  )
  
  # reactive variables to store sim values
  sim <- reactiveValues(
    i = 0, # simulation number
    n = 200, # number of sims per batch
    m = 0, # round of sim
    w = c(0,0), # matches won by A or B
    p = c(0,0), # points won by A or B
    l = matrix(rep(0,6), nrow = 3), # count of sets lost in wins
    lm = 0, # max of all sets lost values
    x = tibble(
      i = 0,
      as1 = 0,
      as2 = 0,
      as3 = 0,
      as4 = 0,
      as5 = 0,
      bs1 = 0,
      bs2 = 0,
      bs3 = 0,
      bs4 = 0,
      bs5 = 0
    )
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
  
  # reactive variables to store match log
  mlog <- reactiveValues(
    x = tibble(
      i = 0,          # play number of match
      g = 0,          # game number of set
      s = 0,          # set
      w = NULL,       # winner of point (1 for A, 2 for B)
      srv = NULL,     # server (1 for A, 2 for B)
    )
  )
  
  # reactive variables to store player probabilities
  # p1 = prob of first serve in
  # p2 = prob of second serve in
  # p3 = prob of return first serve in
  # p4 = prob of return second serve in
  # p5 = prob of return non-serve win
  # _plyrs french----
  plyr1 <- reactiveValues(
    p = matrix(c(0.75, 0.99, 0.75, 0.85, 0.5,
                 0.70, 0.98, 0.68, 0.85, 0.5),
               nrow = 5)
  )
  
  # _plyrs wimbledon----
  plyr2 <- reactiveValues(
    p = matrix(c(0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0),
               nrow = 5),
    id = c('0a','0b'), # id's of players in match
    nm = c(NULL, NULL), # names of players in match
  )
  
  # _french open options----
  fr <- reactiveValues(
    x = 0 # option for actual 1 vs simulated 2
  )
  
  # _wimbledon options----
  wi <- reactiveValues(
    m = 0 # option for slow 1 vs fast 2
  )
  
  # _ui background----
  output$uiBkgrd <- renderUI(
    if (input$tabs == 1) {
      setBackgroundImage('smash1.jpg')
    } else if (input$tabs == 2.1) {
      setBackgroundImage('smash2a.jpg')
    } else if (input$tabs == 2.2) {
      setBackgroundImage('smash2b.jpg')
    } else if (input$tabs == 2.3) {
      setBackgroundImage('smash2c.jpg')
    } else if (input$tabs == 3) {
      setBackgroundImage('smash3.jpg')
    }
  )
  
  # >>>>>>>>>>>>>>>>----
  # ui home page----
  output$uiHome <- renderUI(
    if (input$tabs == 1) {
      div(
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
    # if (input$tabs == 2.1) {
    {
      div(
        align = 'center',
        div(
          style = 'padding:80px 5% 10px 5%;',
          h3(
            style = 'color:white;',
            'Men\'s Singles Final'
          ),
          fluidRow(
            # _player a----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:5px; background-color:rgba(50,50,50,0.95); ',
                  'box-shadow: 10px 10px 10px 0 rgba(0,255,0,0.5);'
                ),
                img(
                  src = 'plyra.png',
                  width = '80%',
                  style = 'max-height:300px; object-position:top; object-fit:cover;'
                ),
                h6(
                  style = 'color:lime;',
                  paste0(
                    pp1[1,1], ' ', pp1[1,2], ' (', pp1[1,3], ')'
                  )
                )
              )
            ),
            column(
              width = 6,
              wellPanel(
                style = paste0(
                  'padding:20px; background-color:rgba(100,50,0,0.95); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,0,0.75);'
                ),
                # _scoreboard----
                uiOutput('scorebd1')
              ),
              # _control panel----
              wellPanel(
                style = paste0(
                  'padding:20px; background-color:rgba(0,0,128,0.75); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,0,0.75);'
                ),
                h4(
                  paste0(
                    'The match has concluded. '
                  )
                ),
                # hr(style = 'border-color:white; margin:2px 0 12px 0;'),
                hr(style = 'border-color:white; margin:24px 0 24px 0;'),
                h4(
                  paste0(
                    'Come back in May 2024 for more French Open action.'
                  )
                )
                # fluidRow(
                #   style = 'padding:15px 0 0 0;',
                #   column(
                #     width = 5,
                #     offset = 1,
                #     actionBttn(
                #       inputId = 'act1',
                #       label = 'Actual',
                #       style = 'simple',
                #       color = 'primary',
                #       size = 'md',
                #       block = TRUE,
                #       icon = icon('search')
                #     )
                #   ),
                #   column(
                #     width = 5,
                #     align = 'left',
                #     actionBttn(
                #       inputId = 'sim1',
                #       label = 'Simulated',
                #       style = 'simple',
                #       color = 'warning',
                #       size = 'md',
                #       block = TRUE,
                #       icon = icon('search')
                #     )
                #   )
                # )
              )
            ),
            # _player b----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:5px; background-color:rgba(50,50,50,0.95); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,0,255,0.5);'
                ),
                img(
                  src = 'plyrb.png',
                  width = '80%',
                  style = 'max-height:300px; object-position:top; object-fit:cover;'
                ),
                h6(
                  style = 'color:magenta;',
                  paste0(
                    pp1[2,1], ' ', pp1[2,2], ' (', pp1[2,3], ')'
                  )
                )
              )
            )
          ),
          div(
            uiOutput('uiResults1')
          )
        )
      )
    }
  )
  
  # _ui scoreboard1----
  output$scorebd1 <- renderUI(
    {
      ss <- matrix(
        c(7,6,7,0,0,6,3,5,0,0), # final score of french open final
        nrow = 5
      )
      div(
        # __header----
        fluidRow(
          column(
            width = 4,
            h4('Player')
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 2,
                NULL
                # h4('G')
              ),
              lapply(
                1:5, 
                function(i) {
                  column(
                    width = 2,
                    h4(i)
                  )
                }
              )
            )
          )
        ),
        hr(style = 'border-color:white; margin:2px 0 12px 0;'),
        # __player a score----
        fluidRow(
          column(
            width = 4,
            h4(
              style = 'color:lime;',
              pp1[1,2]
            )
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 2,
                NULL
                # h4(mtch$pts[1])
              ),
              lapply(
                1:5, 
                function(i) {
                  column(
                    width = 2,
                    style = 'padding-left:5px; padding-right:5px;',
                    div(
                      style = paste0(
                        'border:solid ',
                        ifelse(ss[i,1] > ss[i,2], 'lime ', 'rgba(100,50,0,0.95) '),
                        '1px; ',
                        'border-radius:5px;'
                      ),
                      h4(
                        style = 'color:lime;',
                        ifelse(sum(ss[i,]) > 0, ss[i, 1], '')
                      )
                    )
                  )
                }
              )
            )
          )
        ),
        # __player b score----
        fluidRow(
          column(
            width = 4,
            h4(
              style = 'color:magenta;',
              pp1[2,2]
            )
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 2,
                NULL
                # h4(mtch$pts[2])
              ),
              lapply(
                1:5, 
                function(i) {
                  column(
                    width = 2,
                    style = 'padding-left:5px; padding-right:5px;',
                    div(
                      style = paste0(
                        'border:solid ',
                        ifelse(ss[i,1] < ss[i,2], 'magenta ', 'rgba(100,50,0,0.95) '),
                        '1px; ',
                        'border-radius:5px;'
                      ),
                      h4(
                        style = 'color:magenta;',
                        ifelse(sum(ss[i,]) > 0, ss[i, 2], '')
                      )
                    )
                  )
                }
              )
            )
          )
        )
      )
    }
  )
  
  # _ui results 1----
  output$uiResults1 <- renderUI(
    if (fr$x > 0) {
      div(
        h4('Match Results'),
        fluidRow(
          # __matches won a----
          column(
            width = 3,
            wellPanel(
              style = paste0(
                'padding:15px 25px 15px 25px; border-width:0; ',
                'background-color:rgba(50,50,50,0.5); ',
                'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
              ),
              h4('Matches Won'),
              h1(
                style = 'color:lime; font-size:92px;',
                677
              ),
              div(
                style = paste0(
                  'padding:1px 0 1px 0; width:50%; ',
                  'border:solid lime 1px; border-radius:5px;'
                ),
                h4(
                  style = 'color:lime; font-size:30px;',
                  '68%'
                )
              )
            )
          ),
          # __central area----
          column(
            width = 6,
            wellPanel(
              style = paste0(
                'padding:15px 25px 15px 25px; border-width:0; ',
                'background-color:rgba(50,50,50,0.5); ',
                'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
              ),
              # radioGroupButtons(
              #   inputId = 'matchDisp1',
              #   label = NULL,
              #   choices = c(1:3),
              #   selected = 1,
              #   status = 'default',
              #   size = 'xs',
              #   justified = TRUE,
              #   individual = TRUE,
              #   checkIcon = list(
              #     yes = icon('check')
              #   )
              # ),
              uiOutput('uiMatchDisp1')
            )
          ),
          # __matches won b----
          column(
            width = 3,
            wellPanel(
              style = paste0(
                'padding:15px 25px 15px 25px; border-width:0; ',
                'background-color:rgba(50,50,50,0.5); ',
                'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
              ),
              h4('Matches Won'),
              h1(
                style = 'color:magenta; font-size:92px;',
                323
              ),
              div(
                style = paste0(
                  'padding:1px 0 1px 0; width:50%; ',
                  'border:solid magenta 1px; border-radius:5px;'
                ),
                h4(
                  style = 'color:magenta; font-size:30px;',
                  '32%'
                )
              )
            )
          )
        )
      )
    }
  )
  
  # __ui match disp 1----
  output$uiMatchDisp1 <- renderUI(
    # ___disp 1----
    # if (input$matchDisp1 == '1') {
      div(
        style = 'height:180px; border:solid grey 1px; border-radius:5px;',
        h5('Sets Lost in Wins'),
        lapply(
          0:2,
          function(i) {
            fluidRow(
              style = 'padding:12px 5px 0 5px;',
              column(
                width = 5,
                align = 'right',
                style = 'padding:1px 0 1px 5px;',
                uiOutput(paste0('setL', i, '_A'))
              ),
              column(
                width = 2,
                style = 'padding:1px 0 2px 0;',
                div(
                  align = 'center',
                  style = paste0(
                    'width:100%; ',
                    'padding: 2px 0 2px 0; ',
                    'border:solid white 1px; ',
                    'border-radius:5px;'
                  ),
                  h4(style = 'margin:2px;', i)
                )
              ),
              column(
                width = 5,
                align = 'left',
                style = 'padding:1px 5px 1px 0;',
                uiOutput(paste0('setL', i, '_B'))
              )
            )
          }
        )
      )
      # ___disp 2----
    # } else if (input$matchDisp1 == '2') {
    #   div(
    #     style = 'height:180px; border:solid grey 1px; border-radius:5px;',
    #     h5('Win Flow')
    #   )
    #   # ___disp 3----
    # } else if (input$matchDisp1 == '3') {
    #   div(
    #     style = 'height:180px; border:solid grey 1px; border-radius:5px;',
    #     h5('Match Stats')
    #   )
    # }
  )
  
  # >>>>>>>>>>>>>>>>----
  # ui wimb page----
  output$uiWimb <- renderUI(
    # if (input$tabs == 2.2) {
    {
      div(
        align = 'center',
        div(
          style = 'padding:80px 5% 10px 5%;',
          h3(
            style = 'color:white;',
            'Men\'s Singles'
          ),
          fluidRow(
            # _player a----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:5px; background-color:rgba(50,50,50,0.95); ',
                  'box-shadow: 10px 10px 10px 0 rgba(0,255,0,0.5);'
                ),
                pickerInput(
                  inputId = 'plyrA2',
                  label = NULL,
                  choices = plyrsW$name,
                  selected = plyr2$nm[1],
                  options = list(
                    title = 'Select Player A'
                  )
                ),
                tags$button(
                  id = 'profA2',
                  class = 'btn action-button',
                  style = 'background-color:rgba(0,0,0,0); padding:0; border-width:0;',
                  img(
                    src = paste0('plyrs/', plyr2$id[1], '.png'),
                    width = '80%',
                    style = 'max-height:275px; object-position:top; object-fit:cover;'
                  )
                ),
                # h6(
                #   style = 'color:lime;',
                #   paste0(
                #     pp2[1,1], ' ', pp2[1,2], ' (', pp2[1,3], ')'
                #   )
                # )
              )
            ),
            column(
              width = 6,
              wellPanel(
                style = paste0(
                  'padding:20px; background-color:rgba(0,75,0,0.95); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,0,0.75);'
                ),
                # _scoreboard----
                uiOutput('scorebd2')
              ),
              # _control panel----
              wellPanel(
                style = paste0(
                  'padding:20px; background-color:rgba(0,0,128,0.75); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,0,0.75);'
                ),
                h4(
                  ifelse(
                    sim$m > 0,
                    paste0(
                      'Simulation #: ', sim$i, ' of ', sim$m * sim$n
                    ),
                    paste0(
                      'Choose a simulation speed to get started.'
                    )
                  )
                ),
                hr(style = 'border-color:white; margin:2px 0 8px 0;'),
                fluidRow(
                  style = 'padding:15px 0 0 0;',
                  column(
                    width = 5,
                    offset = 1,
                    tags$button(
                      id = 'simSlow2',
                      class = 'btn action-button',
                      style = 'background-color:orange; padding:6px 45% 6px 45%;',
                      img(
                        src = 'icon_turtle.png',
                        height = '25px'
                      )
                    )
                  ),
                  column(
                    width = 5,
                    tags$button(
                      id = 'simFast2',
                      class = 'btn action-button',
                      style = 'background-color:orange; padding:6px 45% 6px 45%;',
                      img(
                        src = 'icon_rabbit.png',
                        height = '25px'
                      )
                    )
                  )
                  # column(
                  #   width = 2,
                  #   actionBttn(
                  #     inputId = 'reset',
                  #     label = NULL,
                  #     style = 'simple',
                  #     color = 'royal',
                  #     size = 'md',
                  #     block = TRUE,
                  #     icon = icon('undo')
                  #   )
                  # )
                )
              )
            ),
            # _player b----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:5px; background-color:rgba(50,50,50,0.95); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,0,255,0.5);'
                ),
                pickerInput(
                  inputId = 'plyrB2',
                  label = NULL,
                  choices = plyrsW$name,
                  selected = plyr2$nm[2],
                  options = list(
                    title = 'Select Player B'
                  )
                ),
                tags$button(
                  id = 'profB2',
                  class = 'btn action-button',
                  style = 'background-color:rgba(0,0,0,0); padding:0; border-width:0;',
                  img(
                    src = paste0('plyrs/', plyr2$id[2], '.png'),
                    width = '80%',
                    style = 'max-height:275px; object-position:top; object-fit:cover;'
                  )
                )
              )
            )
          ),
          div(
            uiOutput('uiSim')
          )
        )
      )
    }
  )
  
  # _event plyr a----
  observeEvent(
    input$plyrA2,
    if (sim$m == 0) {
      if (input$plyrA2 != '') {
        z1 <- plyrsW %>%
          filter(name == input$plyrA2)
        z2 <- plyrsW %>%
          filter(id == z1$wim_next)
        plyr2$id[1] <- z1$id
        plyr2$id[2] <- z2$id
        plyr2$p[1,1] <- z1$p1
        plyr2$p[2,1] <- z1$p2
        plyr2$p[3,1] <- z1$p3
        plyr2$p[4,1] <- z1$p4
        plyr2$p[5,1] <- z1$p5
        plyr2$p[1,2] <- z2$p1
        plyr2$p[2,2] <- z2$p2
        plyr2$p[3,2] <- z2$p3
        plyr2$p[4,2] <- z2$p4
        plyr2$p[5,2] <- z2$p5
        plyr2$nm[1] <- z1$name
        plyr2$nm[2] <- z2$name
        updatePickerInput(
          session = session,
          inputId = 'plyrA2',
          selected = z1$name
        )
        updatePickerInput(
          session = session,
          inputId = 'plyrB2',
          selected = z2$name
        )
      } else {
        updatePickerInput(
          session = session,
          inputId = 'plyrA2',
          selected = plyr2$nm[1]
        )
        updatePickerInput(
          session = session,
          inputId = 'plyrB2',
          selected = plyr2$nm[2]
        )
      }
    }
  )
  
  # _event plyr b----
  observeEvent(
    input$plyrB2,
    if (sim$m == 0) {
      if (input$plyrB2 != '') {
        z1 <- plyrsW %>%
          filter(name == input$plyrB2)
        z2 <- plyrsW %>%
          filter(id == z1$wim_next)
        plyr2$id[2] <- z1$id
        plyr2$id[1] <- z2$id
        plyr2$p[1,1] <- z2$p1
        plyr2$p[2,1] <- z2$p2
        plyr2$p[3,1] <- z2$p3
        plyr2$p[4,1] <- z2$p4
        plyr2$p[5,1] <- z2$p5
        plyr2$p[1,2] <- z1$p1
        plyr2$p[2,2] <- z1$p2
        plyr2$p[3,2] <- z1$p3
        plyr2$p[4,2] <- z1$p4
        plyr2$p[5,2] <- z1$p5
        plyr2$nm[1] <- z2$name
        plyr2$nm[2] <- z1$name
        updatePickerInput(
          session = session,
          inputId = 'plyrB2',
          selected = z1$name
        )
        updatePickerInput(
          session = session,
          inputId = 'plyrA2',
          selected = z2$name
        )
      }
    }
  )
  
  # _ui scoreboard2----
  output$scorebd2 <- renderUI(
    {
      ss <- matrix(c(rep(0, 10)),
                   nrow = 5)
      if (!mtch$status | wi$m == 1) {
        ss <- mtch$setGms
      }
      zA2 <- plyrsW %>%
        filter(name == input$plyrA2)
      zB2 <- plyrsW %>%
        filter(name == input$plyrB2)
      div(
        # __header----
        fluidRow(
          column(
            width = 4,
            h4('Player')
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 2,
                NULL
                # h4('G')
              ),
              lapply(
                1:5, 
                function(i) {
                  column(
                    width = 2,
                    h4(i)
                  )
                }
              )
            )
          )
        ),
        hr(style = 'border-color:white; margin:2px 0 12px 0;'),
        # __player a score----
        fluidRow(
          column(
            width = 4,
            h4(
              style = 'color:lime;',
              paste0(
                ifelse(nrow(zA2) == 0, 'Player A', zA2$last),
                ifelse(!is.na(zA2$us_seed), paste0(' (', zA2$us_seed, ')'), '')
              )
            )
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 2,
                NULL
                # h4(mtch$pts[1])
              ),
              lapply(
                1:5, 
                function(i) {
                  column(
                    width = 2,
                    style = 'padding-left:5px; padding-right:5px;',
                    div(
                      style = paste0(
                        'border:solid ',
                        ifelse(mtch$setGms[i,1] > mtch$setGms[i,2], 'lime ', 'rgba(0,75,0,0.95) '),
                        '1px; ',
                        'border-radius:5px;'
                      ),
                      h4(
                        style = 'color:lime;',
                        ifelse(sum(mtch$setGms[i,]) > 0, mtch$setGms[i, 1], '')
                      )
                    )
                  )
                }
              )
            )
          )
        ),
        # __player b score----
        fluidRow(
          column(
            width = 4,
            h4(
              style = 'color:magenta;',
              paste0(
                ifelse(nrow(zB2) == 0, 'Player B', zB2$last),
                ifelse(!is.na(zB2$us_seed), paste0(' (', zB2$us_seed, ')'), '')
              )
            )
          ),
          column(
            width = 8,
            fluidRow(
              column(
                width = 2,
                NULL
                # h4(mtch$pts[2])
              ),
              lapply(
                1:5, 
                function(i) {
                  column(
                    width = 2,
                    style = 'padding-left:5px; padding-right:5px;',
                    div(
                      style = paste0(
                        'border:solid ',
                        ifelse(mtch$setGms[i,1] < mtch$setGms[i,2], 'magenta ', 'rgba(0,75,0,0.95) '),
                        '1px; ',
                        'border-radius:5px;'
                      ),
                      h4(
                        style = 'color:magenta;',
                        ifelse(sum(mtch$setGms[i,]) > 0, mtch$setGms[i, 2], '')
                      )
                    )
                  )
                }
              )
            )
          )
        )
      )
    }
  )
  
  # _event profile a----
  observeEvent(
    input$profA2,
    if (plyr2$id[1] != '0a') {
      shinyalert(
        title = NULL,
        text = pProf2(1),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 'm',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
    }
  )
  
  # _event profile b----
  observeEvent(
    input$profB2,
    if (plyr2$id[2] != '0b') {
      shinyalert(
        title = NULL,
        text = pProf2(2),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 'm',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
    }
  )
  
  pProf2 <- function(ab) {
    return(
      paste0(
        '<table style = "width:90%; background-color:white;">',
          '<tr>',
          '<td style = "width:35%">',
            '<div>',
            '<img class = "myImg1" src = "plyrs/', plyr2$id[ab], '.png" height = "250px"></img>',
            '<h4>', plyr2$nm[ab], '</h4></div>',
          '</td>',
          '<td style = "width:65%">',
            '<div style = "background-color:rgba(0,100,0,0.2); border-radius:10px; padding:15px 25px 15px 25px;">',
              '<h5 style = "color:black;">Performance Ratings (Grass Surface)</h5>',
              '<div style = "background-color:#e5ffe5; border-radius:3px 6px 6px 3px; ',
                'width:100%; height:40px; margin:5px 0 5px 0; position:relative;">',
                '<div style = "background-color:#00b200; border-radius:3px 6px 6px 3px; ',
                'position:absolute; top:0; left:0; ',
                'width:', round(100 * plyr2$p[1,ab] + 20, 0), '%; height:100%; ',
                'padding:5px 0 5px 0;">',
                '<h5 style = "text-align:left; color:white; padding-left:10px;">1st Serve',
                '<span style = "float:right; color:white; padding-right:10px;">',
                round(100 * plyr2$p[1,ab] + 20, 0),
                '</span></h5>',
                '</div>',
              '</div>',
              '<div style = "background-color:#e5ffe5; border-radius:3px 6px 6px 3px; ',
                'width:100%; height:40px; margin:5px 0 5px 0; position:relative;">',
                '<div style = "background-color:#00b200; border-radius:3px 6px 6px 3px; ',
                'position:absolute; top:0; left:0; ',
                'width:', round(100 * plyr2$p[2,ab] * plyr2$p[1,ab] + 10, 0), '%; height:100%; ',
                'padding:5px 0 5px 0;">',
                '<h5 style = "text-align:left; color:white; padding-left:10px;">2nd Serve',
                '<span style = "float:right; color:white; padding-right:10px;">',
                round(100 * plyr2$p[2,ab] * plyr2$p[1,ab] + 10, 0),
                '</span></h5>',
                '</div>',
              '</div>',
              '<div style = "background-color:#e5ffe5; border-radius:3px 6px 6px 3px; ',
                'width:100%; height:40px; margin:5px 0 5px 0; position:relative;">',
                '<div style = "background-color:#00b200; border-radius:3px 6px 6px 3px; ',
                'position:absolute; top:0; left:0; ',
                'width:', round(100 * plyr2$p[3,ab], 0), '%; height:100%; ',
                'padding:5px 0 5px 0;">',
                '<h5 style = "text-align:left; color:white; padding-left:10px;">1st Serve Return',
                '<span style = "float:right; color:white; padding-right:10px;">',
                round(100 * plyr2$p[3,ab], 0),
                '</span></h5>',
                '</div>',
              '</div>',
              '<div style = "background-color:#e5ffe5; border-radius:3px 6px 6px 3px; ',
                'width:100%; height:40px; margin:5px 0 5px 0; position:relative;">',
                '<div style = "background-color:#00b200; border-radius:3px 6px 6px 3px; ',
                'position:absolute; top:0; left:0; ',
                'width:', round(100 * plyr2$p[4,ab], 0), '%; height:100%; ',
                'padding:5px 0 5px 0;">',
                '<h5 style = "text-align:left; color:white; padding-left:10px;">2nd Serve Return',
                '<span style = "float:right; color:white; padding-right:10px;">',
                round(100 * plyr2$p[4,ab], 0),
                '</span></h5>',
                '</div>',
              '</div>',
              '<div style = "background-color:#e5ffe5; border-radius:3px 6px 6px 3px; ',
                'width:100%; height:40px; margin:5px 0 5px 0; position:relative;">',
                '<div style = "background-color:#00b200; border-radius:3px 6px 6px 3px; ',
                'position:absolute; top:0; left:0; ',
                'width:', round(100 * plyr2$p[5,ab] * 1.8, 0), '%; height:100%; ',
                'padding:5px 0 5px 0;">',
                '<h5 style = "text-align:left; color:white; padding-left:10px;">Volley',
                '<span style = "float:right; color:white; padding-right:10px;">',
                round(100 * plyr2$p[5,ab] * 1.8, 0),
                '</span></h5>',
                '</div>',
              '</div>',
            '</div>',
          '</td>',
          '</tr>',
        '</table>'
      )
    )
  }
  
  # >>>>>>>>>>>>>>>>----
  # _ui sim results----
  output$uiSim <- renderUI(
    if (wi$m > 0) {
      # __pt by pt results----
      if (wi$m == 1 & nrow(mlog$x) > 1) {
        pa <- nrow(mlog$x[mlog$x$w == plyr2$nm[1],])
        pb <- nrow(mlog$x[mlog$x$w == plyr2$nm[2],])
        div(
          fluidRow(
            column(
              width = 8,
              offset = 2,
              h4('Point-by-Point Results')
            ),
            column(
              width = 2,
              if (wi$m == 2) {
                actionBttn(
                  inputId = 'reset2',
                  label = 'Reset',
                  style = 'simple',
                  color = 'royal',
                  size = 'sm',
                  block = TRUE,
                  icon = icon('undo')
                )
              }
            )
          ),
          fluidRow(
            # ___points won a----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:15px 25px 15px 25px; border-width:0; ',
                  'background-color:rgba(50,50,50,0.5); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
                ),
                h4('Points Won'),
                h1(
                  style = 'color:lime; font-size:92px;',
                  pa
                ),
                div(
                  style = paste0(
                    'padding:1px 0 1px 0; width:50%; ',
                    'border:solid lime 1px; border-radius:5px;'
                  ),
                  h4(
                    style = 'color:lime; font-size:30px;',
                    ifelse(
                      pa + pb > 0,
                      paste0(
                        round(100 * pa / (pa + pb), 0), '%'
                      ),
                      '0%'
                    )
                  )
                )
              )
            ),
            # ___central area----
            column(
              width = 6,
              wellPanel(
                style = paste0(
                  'padding:15px 25px 15px 25px; border-width:0; ',
                  'background-color:rgba(50,50,50,0.5); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
                ),
                # radioGroupButtons(
                #   inputId = 'simDisp',
                #   label = NULL,
                #   choices = c(1:3),
                #   selected = 1,
                #   status = 'default',
                #   size = 'xs',
                #   justified = TRUE,
                #   individual = TRUE,
                #   checkIcon = list(
                #     yes = icon('check')
                #   )
                # ),
                uiOutput('uiPbpDisp')
              )
            ),
            # ___points won b----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:15px 25px 15px 25px; border-width:0; ',
                  'background-color:rgba(50,50,50,0.5); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
                ),
                h4('Points Won'),
                h1(
                  style = 'color:lime; font-size:92px;',
                  pb
                ),
                div(
                  style = paste0(
                    'padding:1px 0 1px 0; width:50%; ',
                    'border:solid lime 1px; border-radius:5px;'
                  ),
                  h4(
                    style = 'color:lime; font-size:30px;',
                    ifelse(
                      pa + pb > 0,
                      paste0(
                        round(100 * pb / (pa + pb), 0), '%'
                      ),
                      '0%'
                    )
                  )
                )
              )
            )
          )
        )
        # __match results----
      } else if (wi$m == 2) {
        ww <- c(0,0)
        if (!mtch$status) {
          ww <- sim$w
        }
        div(
          fluidRow(
            column(
              width = 8,
              offset = 2,
              h4('Match Results')
            ),
            column(
              width = 2,
              actionBttn(
                inputId = 'reset2',
                label = 'Reset',
                style = 'simple',
                color = 'royal',
                size = 'sm',
                block = TRUE,
                icon = icon('undo')
              )
            )
          ),
          fluidRow(
            # ___matches won a----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:15px 25px 15px 25px; border-width:0; ',
                  'background-color:rgba(50,50,50,0.5); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
                ),
                h4('Matches Won'),
                h1(
                  style = 'color:lime; font-size:92px;',
                  ww[1]
                ),
                div(
                  style = paste0(
                    'padding:1px 0 1px 0; width:50%; ',
                    'border:solid lime 1px; border-radius:5px;'
                  ),
                  h4(
                    style = 'color:lime; font-size:30px;',
                    ifelse(
                      sim$i > 0,
                      paste0(
                        round(100 * ww[1] / sum(ww), 0), '%'
                      ),
                      '0%'
                    )
                  )
                )
              )
            ),
            # ___central area----
            column(
              width = 6,
              wellPanel(
                style = paste0(
                  # 'padding:15px 25px 15px 25px; border-width:0; ',
                  'padding:50px 25px 15px 25px; border-width:0; ',
                  'background-color:rgba(50,50,50,0.5); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
                ),
                # radioGroupButtons(
                #   inputId = 'simDisp',
                #   label = NULL,
                #   choices = c(1:3),
                #   selected = 1,
                #   status = 'default',
                #   size = 'xs',
                #   justified = TRUE,
                #   individual = TRUE,
                #   checkIcon = list(
                #     yes = icon('check')
                #   )
                # ),
                uiOutput('uiSimDisp')
              )
            ),
            # ___matches won b----
            column(
              width = 3,
              wellPanel(
                style = paste0(
                  'padding:15px 25px 15px 25px; border-width:0; ',
                  'background-color:rgba(50,50,50,0.5); ',
                  'box-shadow: 10px 10px 10px 0 rgba(255,255,255,0.5);'
                ),
                h4('Matches Won'),
                h1(
                  style = 'color:magenta; font-size:92px;',
                  ww[2]
                ),
                div(
                  style = paste0(
                    'padding:1px 0 1px 0; width:50%; ',
                    'border:solid magenta 1px; border-radius:5px;'
                  ),
                  h4(
                    style = 'color:magenta; font-size:30px;',
                    ifelse(
                      sim$i > 0,
                      paste0(
                        round(100 * ww[2] / sum(ww), 0), '%'
                      ),
                      '0%'
                    )
                  )
                )
              )
            )
          )
        )
      }
    }
  )
  
  # __ui pbp disp----
  output$uiPbpDisp <- renderUI(
    if (TRUE) {
      div(
        style = 'height:220px;',
        # h5('Point Log'),
        dataTableOutput('tblPtLog')
      )
    }
  )
  
  # ___table pt log----
  output$tblPtLog <- renderDataTable(
    {
      c <- paste0(
        '<style>th {text-align: center; color:white;}</style>',
        '<table><thead><tr>',
        '<th>Pt</th>',
        '<th>Gm</th>',
        '<th>Set</th>',
        '<th>Winner</th>',
        '<th>Serve</th>',
        '</tr></thead></table>'
      )
      datatable(
        data = mlog$x %>% filter(i > 0),
        class = 'cell-border stripe',
        container = c,
        rownames = FALSE,
        selection = 'single',
        options = list(
          pageLength = 1000,
          ordering = FALSE,
          scrollY = '180px',
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:4)
          ),
          initComplete = JS(
            "function(settings, json) {$(this.api().table().container()).css({'font-size': '8pt'});} "
          )
          # initComplete = JS(
          #   "function(){$(this).addClass('compact');}"
          # )
        )
      ) 
    }
  )
  
  # __ui sim disp----
  output$uiSimDisp <- renderUI(
    # ___disp 1----
    # if (input$simDisp == '1') {
    if (TRUE) {
      div(
        style = 'height:180px; border:solid grey 1px; border-radius:5px;',
        h5('Sets Lost in Wins'),
        lapply(
          0:2,
          function(i) {
            fluidRow(
              style = 'padding:12px 5px 0 5px;',
              column(
                width = 5,
                align = 'right',
                style = 'padding:1px 0 1px 5px;',
                uiOutput(paste0('setL', i, '_A'))
              ),
              column(
                width = 2,
                style = 'padding:1px 0 2px 0;',
                div(
                  align = 'center',
                  style = paste0(
                    'width:100%; ',
                    'padding: 2px 0 2px 0; ',
                    'border:solid white 1px; ',
                    'border-radius:5px;'
                  ),
                  h4(style = 'margin:2px;', i)
                )
              ),
              column(
                width = 5,
                align = 'left',
                style = 'padding:1px 5px 1px 0;',
                uiOutput(paste0('setL', i, '_B'))
              )
            )
          }
        )
      )
      # ___disp 2----
    } else if (input$simDisp == '2') {
      div(
        style = 'height:180px; border:solid grey 1px; border-radius:5px;',
        h5('Win Flow')
      )
      # ___disp 3----
    } else if (input$simDisp == '3') {
      div(
        style = 'height:180px; border:solid grey 1px; border-radius:5px;',
        h5('Match Stats')
      )
    }
  )
  
  setL <- function(i, j) {
    if (sim$l[i,j] > 0) {
      w <- floor(90 * (sim$l[i,j] / sim$lm))
      return(
        div(
          align = switch(j, 'left', 'right'),
          style = paste0(
            'width:', w, '%; ',
            'background-color:',
            switch(j, 'lime; ', 'magenta; '),
            'padding: ', 
            switch(j, '2px 0 2px 3px; ', '2px 3px 2px 0; '),
            switch(j,'border-radius:5px 0 0 5px;', 'border-radius:0 5px 5px 0;')
          ),
          h4(style = 'color:black; margin:2px;', sim$l[i,j])
        )
      )
    } else {
      return(
        div(
          align = switch(j, 'left', 'right'),
          style = paste0(
            'width:10%; ',
            'background-color:',
            switch(j, 'lime; ', 'magenta; '),
            'padding: ', 
            switch(j, '2px 0 2px 3px; ', '2px 3px 2px 0; '),
            switch(j,'border-radius:5px 0 0 5px;', 'border-radius:0 5px 5px 0;')
          ),
          h4(style = 'color:black; margin:2px;', sim$l[i,j])
        )
      )
    }
  }
  
  # ___set losses----
  output$setL0_A <- renderUI(setL(1,1))
  output$setL1_A <- renderUI(setL(2,1))
  output$setL2_A <- renderUI(setL(3,1))
  output$setL0_B <- renderUI(setL(1,2))
  output$setL1_B <- renderUI(setL(2,2))
  output$setL2_B <- renderUI(setL(3,2))
  
  # >>>>>>>>>>>>>>>>----
  # _event simulate slow----
  observeEvent(
    eventExpr = input$simSlow2,
    if (plyr2$id[1] != '0a') {
      wi$m <- 1
      # sim$m <- sim$m + 1
      # determine who serves first
      mtch$srv <- sample(1:2, 1)
      mtch$rtn <- ifelse(mtch$srv == 1, 2, 1)
      observe(
        {
          invalidateLater(20, session) # millisecs each iteration
          isolate(
            while (mtch$status) {
              mtch$p <- mtch$p + 1
              # temp variables for game and set statuses
              gOver <- FALSE
              sOver <- FALSE
              # play point
              r1 <- runif(1)
              if (plyr2$p[1, mtch$srv] > r1) { # first serve in
                r12 <- runif(1)
                if (plyr2$p[3, mtch$rtn] > r12) { # return first serve in
                  r13 <- runif(1)
                  if (plyr2$p[5, mtch$srv] > r13) { # point won by srv
                    plusLog(mtch$srv)
                    mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
                  } else { # point won by rtn
                    plusLog(mtch$rtn)
                    mtch$pts[mtch$rtn] <- mtch$pts[mtch$rtn] + 1
                  }
                } else { # point won by srv
                  plusLog(mtch$srv)
                  mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
                }
              } else { # first serve out
                r2 <- runif(1)
                if (plyr2$p[2, mtch$srv] > r2) { # second serve in
                  r22 <- runif(1)
                  if (plyr2$p[4, mtch$rtn] > r22) { # return second serve in
                    r23 <- runif(1)
                    if (plyr2$p[5, mtch$srv] > r23) { # point won by srv
                      plusLog(mtch$srv)
                      mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
                    } else { # point won by rtn
                      plusLog(mtch$rtn)
                      mtch$pts[mtch$rtn] <- mtch$pts[mtch$rtn] + 1
                    }
                  } else { # point won by srv
                    plusLog(mtch$srv)
                    mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
                  }
                } else { # second serve out aka double fault; point won by rtn
                  plusLog(mtch$rtn)
                  mtch$pts[mtch$rtn] <- mtch$pts[mtch$rtn] + 1
                }
              }
              # __is game over?----
              if (mtch$pts[mtch$srv] == 4) {
                mtch$gms[mtch$srv] <- mtch$gms[mtch$srv] + 1
                mtch$setGms[mtch$s, mtch$srv] <- mtch$setGms[mtch$s, mtch$srv] + 1
                mtch$pts <- c(0, 0)
                gOver <- TRUE
              } else if (mtch$pts[mtch$rtn] == 4) {
                mtch$gms[mtch$rtn] <- mtch$gms[mtch$rtn] + 1
                mtch$setGms[mtch$s, mtch$rtn] <- mtch$setGms[mtch$s, mtch$rtn] + 1
                mtch$pts <- c(0, 0)
                gOver <- TRUE
              }
              # __is set over?----
              if (gOver) {
                # if server reaches 6
                if (mtch$gms[mtch$srv] == 6) {
                  # if return is 4 or less
                  if (mtch$gms[mtch$rtn] < 5) {
                    mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
                    mtch$gms <- c(0, 0)
                    mtch$s <- mtch$s + 1
                  } else {
                    r6 <- runif(1)
                    if (r6 < 0.75) {
                      r7 <- runif(1)
                      if (r7 < 0.33) {
                        mtch$setGms[mtch$s, mtch$rtn] <- 6
                      }
                      mtch$setGms[mtch$s, mtch$srv] <- 7
                      mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
                      mtch$gms <- c(0, 0)
                      mtch$s <- mtch$s + 1
                    } else {
                      mtch$setGms[mtch$s, mtch$rtn] <- 7
                      mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
                      mtch$gms <- c(0, 0)
                      mtch$s <- mtch$s + 1
                    }
                  }
                  sOver <- TRUE
                } else if (mtch$gms[mtch$rtn] == 6) {
                  if (mtch$gms[mtch$srv] < 5) {
                    mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
                    mtch$gms <- c(0, 0)
                    mtch$s <- mtch$s + 1
                  } else {
                    r6 <- runif(1)
                    if (r6 < 0.75) {
                      r7 <- runif(1)
                      if (r7 < 0.33) {
                        mtch$setGms[mtch$s, mtch$srv] <- 6
                      }
                      mtch$setGms[mtch$s, mtch$rtn] <- 7
                      mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
                      mtch$gms <- c(0, 0)
                      mtch$s <- mtch$s + 1
                    } else {
                      mtch$setGms[mtch$s, mtch$srv] <- 7
                      mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
                      mtch$gms <- c(0, 0)
                      mtch$s <- mtch$s + 1
                    }
                  }
                  sOver <- TRUE
                }
              }
              # __is match over?----
              if (sOver) {
                if (mtch$sts[mtch$srv] == 3) {
                  mtch$status = FALSE
                  # sim$w[mtch$srv] <- sim$w[mtch$srv] + 1
                  # sl <- mtch$sts[mtch$rtn]
                  # sim$l[sl+1, mtch$srv] <- sim$l[sl+1, mtch$srv] + 1
                  # sim$lm <- max(sim$l)
                } else if (mtch$sts[mtch$rtn] == 3) {
                  mtch$status = FALSE
                  # sim$w[mtch$rtn] <- sim$w[mtch$rtn] + 1
                  # sl <- mtch$sts[mtch$srv]
                  # sim$l[sl+1, mtch$rtn] <- sim$l[sl+1, mtch$rtn] + 1
                  # sim$lm <- max(sim$l)
                }
              }
              # swap service
              if (gOver) {
                mtch$srv <- ifelse(mtch$srv == 1, 2, 1)
                mtch$rtn <- ifelse(mtch$rtn == 1, 2, 1)
              }
            }
          )
        }
      )
    }  else {
      sendSweetAlert(
        session = session,
        title = 'No Players',
        text = 'Select the players before simulating match results.',
        html = TRUE,
        type = 'error'
      )
    }
  )
  
  plusLog <- function(ab) {
    mlog$x <- mlog$x %>% 
      bind_rows(
        tibble(
          i = mlog$x$i[nrow(mlog$x)] + 1,
          g = sum(mtch$gms) + 1,
          s = sum(mtch$sts) + 1,
          w = switch(ab, plyr2$nm[1], plyr2$nm[2]),
          srv = switch(mtch$srv, plyr2$nm[1], plyr2$nm[2])
        )
      )
  }
  
  # >>>>>>>>>>>>>>>>----
  # _event simulate fast----
  observeEvent(
    eventExpr = input$simFast2,
    if (plyr2$id[1] != '0a') {
      wi$m <- 2
      sim$m <- sim$m + 1
      observe(
        {
          invalidateLater(20, session) # millisecs each iteration
          isolate(
            # if (sim$i < as.numeric(input$nSim)) {
            if (sim$i < sim$m * sim$n) {
              sim$i <- sim$i + 1
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
                if (plyr2$p[1, mtch$srv] > r1) { # first serve in
                  r12 <- runif(1)
                  if (plyr2$p[3, mtch$rtn] > r12) { # return first serve in
                    r13 <- runif(1)
                    if (plyr2$p[5, mtch$srv] > r13) { # point won by srv
                      mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
                    } else { # point won by rtn
                      mtch$pts[mtch$rtn] <- mtch$pts[mtch$rtn] + 1
                    }
                  } else { # point won by srv
                    mtch$pts[mtch$srv] <- mtch$pts[mtch$srv] + 1
                  }
                } else { # first serve out
                  r2 <- runif(1)
                  if (plyr2$p[2, mtch$srv] > r2) { # second serve in
                    r22 <- runif(1)
                    if (plyr2$p[4, mtch$rtn] > r22) { # return second serve in
                      r23 <- runif(1)
                      if (plyr2$p[5, mtch$srv] > r23) { # point won by srv
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
                # __is game over?----
                if (mtch$pts[mtch$srv] == 4) {
                  mtch$gms[mtch$srv] <- mtch$gms[mtch$srv] + 1
                  mtch$setGms[mtch$s, mtch$srv] <- mtch$setGms[mtch$s, mtch$srv] + 1
                  mtch$pts <- c(0, 0)
                  gOver <- TRUE
                } else if (mtch$pts[mtch$rtn] == 4) {
                  mtch$gms[mtch$rtn] <- mtch$gms[mtch$rtn] + 1
                  mtch$setGms[mtch$s, mtch$rtn] <- mtch$setGms[mtch$s, mtch$rtn] + 1
                  mtch$pts <- c(0, 0)
                  gOver <- TRUE
                }
                # __is set over?----
                if (gOver) {
                  # if server reaches 6
                  if (mtch$gms[mtch$srv] == 6) {
                    # if return is 4 or less
                    if (mtch$gms[mtch$rtn] < 5) {
                      mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
                      mtch$gms <- c(0, 0)
                      mtch$s <- mtch$s + 1
                    } else {
                      r6 <- runif(1)
                      if (r6 < 0.75) {
                        r7 <- runif(1)
                        if (r7 < 0.33) {
                          mtch$setGms[mtch$s, mtch$rtn] <- 6
                        }
                        mtch$setGms[mtch$s, mtch$srv] <- 7
                        mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
                        mtch$gms <- c(0, 0)
                        mtch$s <- mtch$s + 1
                      } else {
                        mtch$setGms[mtch$s, mtch$rtn] <- 7
                        mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
                        mtch$gms <- c(0, 0)
                        mtch$s <- mtch$s + 1
                      }
                    }
                    sOver <- TRUE
                  } else if (mtch$gms[mtch$rtn] == 6) {
                    if (mtch$gms[mtch$srv] < 5) {
                      mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
                      mtch$gms <- c(0, 0)
                      mtch$s <- mtch$s + 1
                    } else {
                      r6 <- runif(1)
                      if (r6 < 0.75) {
                        r7 <- runif(1)
                        if (r7 < 0.33) {
                          mtch$setGms[mtch$s, mtch$srv] <- 6
                        }
                        mtch$setGms[mtch$s, mtch$rtn] <- 7
                        mtch$sts[mtch$rtn] <- mtch$sts[mtch$rtn] + 1
                        mtch$gms <- c(0, 0)
                        mtch$s <- mtch$s + 1
                      } else {
                        mtch$setGms[mtch$s, mtch$srv] <- 7
                        mtch$sts[mtch$srv] <- mtch$sts[mtch$srv] + 1
                        mtch$gms <- c(0, 0)
                        mtch$s <- mtch$s + 1
                      }
                    }
                    sOver <- TRUE
                  }
                }
                # __is match over?----
                if (sOver) {
                  if (mtch$sts[mtch$srv] == 3) {
                    mtch$status = FALSE
                    sim$w[mtch$srv] <- sim$w[mtch$srv] + 1
                    sl <- mtch$sts[mtch$rtn]
                    sim$l[sl+1, mtch$srv] <- sim$l[sl+1, mtch$srv] + 1
                    sim$lm <- max(sim$l)
                  } else if (mtch$sts[mtch$rtn] == 3) {
                    mtch$status = FALSE
                    sim$w[mtch$rtn] <- sim$w[mtch$rtn] + 1
                    sl <- mtch$sts[mtch$srv]
                    sim$l[sl+1, mtch$rtn] <- sim$l[sl+1, mtch$rtn] + 1
                    sim$lm <- max(sim$l)
                  }
                  # if (!mtch$status) {
                  #   sim$x <- sim$x %>% 
                  #     bind_rows(
                  #       tibble(
                  #         i =sim$i,
                  #         as1 = mtch$setGms[1,1],
                  #         as2 = mtch$setGms[2,1],
                  #         as3 = mtch$setGms[3,1],
                  #         as4 = mtch$setGms[4,1],
                  #         as5 = mtch$setGms[5,1],
                  #         bs1 = mtch$setGms[1,2],
                  #         bs2 = mtch$setGms[2,2],
                  #         bs3 = mtch$setGms[3,2],
                  #         bs4 = mtch$setGms[4,2],
                  #         bs5 = mtch$setGms[5,2]
                  #       )
                  #     )
                  # }
                }
                # swap service
                if (gOver) {
                  mtch$srv <- ifelse(mtch$srv == 1, 2, 1)
                  mtch$rtn <- ifelse(mtch$rtn == 1, 2, 1)
                }
              }
            } else {
              # write_csv(
              #   x = sim$x, 
              #   file = '_stuff/sim_results.csv'
              # )
            }
          )
        }
      )
    } else {
      sendSweetAlert(
        session = session,
        title = 'No Players',
        text = 'Select the players before simulating match results.',
        html = TRUE,
        type = 'error'
      )
    }
  )
  
  # _event reset----
  observeEvent(
    eventExpr = input$reset2,
    {
      wi$m = 0
      sim$i = 0
      sim$m = 0
      sim$w = c(0,0)
      sim$l = matrix(rep(0,6), nrow = 3)
      mtch$p = 0
      mtch$s = 1
      mtch$srv = NULL
      mtch$rtn = NULL
      mtch$status = TRUE
      mtch$pts = c(0, 0)
      mtch$gms = c(0, 0)
      mtch$sts = c(0, 0)
      mtch$setGms = matrix(
        c(rep(0, 10)),
        nrow = 5
      )
      plyr2$id = c('0a', '0b')
      plyr2$nm = c(NULL, NULL)
      updatePickerInput(
        session = session,
        inputId = 'plyrA2',
        selected = NULL
      )
      updatePickerInput(
        session = session,
        inputId = 'plyrB2',
        selected = NULL
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>----
  # ui usop page----
  output$uiUsop <- renderUI(
    if (input$tabs == 2.3) {
      div(
        align = 'center',
        div(
          style = 'padding:80px 5% 10px 5%;',
          h3(
            style = 'color:white;',
            'Coming in September 2023'
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>----
  # ui abou page----
  output$uiAbou <- renderUI(
    if (input$tabs == 3) {
      div(
        align = 'center',
        div(
          align = 'center',
          style = 'padding:80px 10% 0 10%;',
          h3(
            style = 'color:lime;',
            'The Development Team'
          ),
          p('Click on an image to view the member\'s bio.'),
          fluidRow(
            lapply(
              1:6, 
              function(i) {
                column(
                  width = 2,
                  style = 'padding:3px;',
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
  observeEvent(input$memBio6, {sm$mb <- ifelse(sm$mb == 6, 0, 6)})
  
  # ui bio----
  output$uiBio <- renderUI(
    if (sm$mb > 0) {
      div(
        style = 'padding:5vh 20% 0 20%;',
        wellPanel(
          style = paste0(
            'background-color:rgba(50,50,50,0.5); ',
            'border:solid orange 2px;'
          ),
          h3(
            style = 'color:orange; margin-bottom:30px;',
            member[sm$mb]
          ),
          # hr(),
          HTML(bio[sm$mb]),
          # br(),
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
  
}