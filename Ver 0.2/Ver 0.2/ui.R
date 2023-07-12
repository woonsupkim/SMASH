ui <- navbarPage(
  
  id = 'tabs',
  windowTitle = 'SMASH!',
  position = 'fixed-top',
  collapsible = TRUE,
  inverse = FALSE,
  theme = shinytheme('slate'), # https://rstudio.github.io/shinythemes/
  
  title = div(
    style = 'padding:10px 0 0 0;',
    'SMASH!'
  ),
  
  # setBackgroundImage(paste0('smash1.jpg')),
  
  tabPanel(
    value = 1,
    title = div(
      img(src = 'ball.png', height = '40px'), 
      HTML('&nbsp;'), 'Home'
    ),
    uiOutput('uiHome')
  ),
  
  tabPanel(
    value = 2,
    title = div(
      img(src = 'rg_logo.avif', height = '40px'), 
      HTML('&nbsp;'), 'The French Open 2023'
    ),
    uiOutput('uiFrop')
  ),
  
  tabPanel(
    value = 3,
    title = div(
      img(src = 'racquet.png', height = '40px'), 
      HTML('&nbsp;'), 'About Us'
    ),
    uiOutput('uiAbou')
  )

)
