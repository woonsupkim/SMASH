ui <- navbarPage(
  
  id = 'tabs',
  windowTitle = 'SMASH!',
  position = 'fixed-top',
  collapsible = TRUE,
  inverse = FALSE,
  theme = shinytheme('slate'), # https://rstudio.github.io/shinythemes/
  
  uiOutput('uiBkgrd'),
  
  # home page----
  tabPanel(
    value = 1,
    title = div(
      img(
        src = 'ball.png', 
        height = '40px',
        style = 'border-radius:50%; box-shadow:5px 10px 10px 0 rgba(0,0,0,0.5);'
      ), 
      HTML('&nbsp;'), 'Home'
    ),
    uiOutput('uiHome')
  ),
  
  # french open page----
  tabPanel(
    value = 2.1,
    title = div(
      img(
        src = 'logo_rg.png', 
        height = '40px',
        style = 'border-radius:50%; box-shadow:5px 10px 10px 0 rgba(0,0,0,0.5);'
      ), 
      HTML('&nbsp;'), 'French Open'
    ),
    uiOutput('uiFrop')
  ),
  
  # wimbledon page----
  tabPanel(
    value = 2.2,
    title = div(
      img(
        src = 'logo_wb.png', 
        height = '40px',
        style = 'border-radius:50%; box-shadow:5px 10px 10px 0 rgba(0,0,0,0.5);'
      ), 
      HTML('&nbsp;'), 'Wimbledon'
    ),
    uiOutput('uiWimb')
  ),
  
  # us open page----
  tabPanel(
    value = 2.3,
    title = div(
      img(
        src = 'logo_us.png', 
        height = '40px',
        style = 'border-radius:50%; box-shadow:5px 10px 10px 0 rgba(0,0,0,0.5);'
      ), 
      HTML('&nbsp;'), 'US Open'
    ),
    uiOutput('uiUsop')
  ),
  
  # about us page----
  tabPanel(
    value = 3,
    title = div(
      img(
        src = 'lion.png', 
        height = '40px',
        style = 'border-radius:50%; box-shadow:5px 10px 10px 0 rgba(0,0,0,0.5);'
      ), 
      HTML('&nbsp;'), 'About Us'
    ),
    uiOutput('uiAbou')
  )

)
