# the necessary packages
library(DT)
library(highcharter)
library(leaflet)
library(RPostgres)
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# connect to the swish database
# con <- dbConnect(
#   drv = dbDriver('Postgres'),
#   dbname = 'swishdb',
#   host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com',
#   port = 25061,
#   user = 'nba_user',
#   password = 'AVNS_EjCjTHjSTZQ-oqhiu80',
#   sslmode = 'require'
# )

# plyr qry
# stmt <- paste0(
#   'SELECT *, date_part(\'year\', age(CURRENT_DATE, dob)) age FROM plyrs ',
#   'ORDER BY fire DESC;'
# )


# team members
member <- c(
  'Lanru Fu',
  'Wendi Hu',
  'Woon Sup Kim',
  'Emily Pham',
  'Vivian Yin',
  'Day Yi'
)

mem1 <- c(
  'Lanru',
  'Wendi',
  'Woon Sup',
  'Emily',
  'Vivian',
  'Day'
)

# member linkedin ids
lnkdin <- c(
  'lanru-fu-a55376162',
  'wendihu-wendy',
  'woonsup-kim',
  'emily-tpham',
  'vivianryin'
)

# member bios
bio <- c(
  paste0(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do ',
    'eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut ',
    'enim ad minim veniam, quis nostrud exercitation ullamco laboris ',
    'nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ',
    'reprehenderit in voluptate velit esse cillum dolore eu fugiat ',
    'nulla pariatur.'
  ),
  paste0(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do ',
    'eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut ',
    'enim ad minim veniam, quis nostrud exercitation ullamco laboris ',
    'nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ',
    'reprehenderit in voluptate velit esse cillum dolore eu fugiat ',
    'nulla pariatur.'
  ),
  paste0(
    'Woon Sup is a data scientist with a Master\'s degree from ',
    'Columbia University. He has a strong background in data science, ',
    'product management, and engineering and has been working in the ',
    'industry for over six years. Sports has always been a big part ',
    'of his life. He regularly competes in boxing, swimming, and ',
    'recently became a fan of tennis after witnessing Alcaraz’s ',
    'intense win against Tiafoe at US Open. Woon Sup frequently ',
    'intersects his data science skills with his interests and ',
    'participates in various projects. More about his experiences ',
    'can be found on his LinkedIn page, and he can be reached at ',
    'wk2371@columbia.edu for further inquiries.'
  ),
  paste0(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do ',
    'eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut ',
    'enim ad minim veniam, quis nostrud exercitation ullamco laboris ',
    'nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ',
    'reprehenderit in voluptate velit esse cillum dolore eu fugiat ',
    'nulla pariatur.'
  ),
  paste0(
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do ',
    'eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut ',
    'enim ad minim veniam, quis nostrud exercitation ullamco laboris ',
    'nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ',
    'reprehenderit in voluptate velit esse cillum dolore eu fugiat ',
    'nulla pariatur.'
  )
)
