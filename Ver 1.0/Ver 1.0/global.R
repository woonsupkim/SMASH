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

pp <- matrix(
  c(
    'Novak', 'Casper',
    'Djokovic', 'Ruud',
    3, 4
  ),
  nrow = 2
)

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
  'vivianryin',
  'dayhyi'
)

# member bios
bio <- c(
  paste0(
    '<p style = "text-align:left;">Lanru is a graduate student ',
    'at Columbia University. She earned her BA in Economics from ',
    'UC Irvine (home of the Anteaters!). She holds a certification ',
    'in AWS Cloud Foundations among other advanced technical skills. ',
    'She can be reached at ',
    '<a href="mailto:lf2752@columbia.edu">lf2752@columbia.edu</a></p>'
  ),
  paste0(
    '<p style = "text-align:left;">Wendi is currently pursuing her MSc in Applied Analytics at ',
    'Columbia University, focusing on honing her skills in ',
    'Machine Learning, Statistical Modeling, and Econometrics. ',
    'As a Data Scientist, she brings expertise in these areas, ',
    'along with a strong aptitude for learning and staying ',
    'abreast of the latest advancements. She possesses a strong ',
    'learning ability and is driven by her passion for math ',
    'and coding. Additionally, in her leisure time, Wendi indulges ',
    'in her love for photography, capturing captivating moments ',
    'through her lens. She can be reached at ',
    '<a href="mailto:wh2521@columbia.edu">wh2521@columbia.edu</a></p>'
  ),
  paste0(
    '<p style = "text-align:left;">Woon Sup is a data scientist with a Master\'s degree from ',
    'Columbia University. He has a strong background in data science, ',
    'product management, and engineering and has been working in the ',
    'industry for over six years. Sports has always been a big part ',
    'of his life. He regularly competes in boxing, swimming, and ',
    'recently became a fan of tennis after witnessing Alcaraz’s ',
    'intense win against Tiafoe at US Open. Woon Sup frequently ',
    'intersects his data science skills with his interests and ',
    'participates in various projects. More about his experiences ',
    'can be found on his LinkedIn page, and he can be reached at ',
    '<a href="mailto:wk2371@columbia.edu">wk2371@columbia.edu</a> ',
    'for further inquiries.</p>'
  ),
  paste0(
    '<p style = "text-align:left;">Emily Pham is currently a research assistant at Columbia ',
    'GSAPP and a data analyst intern at Endear. Prior to her ',
    'enrollment at Columbia, she studied statistics and ',
    'gained experience as a data scientist at MGN Microgrid ',
    'Networks in New York. She is enthusiastic about applying ',
    'her expertise in data analytics to the field of sports. ',
    'For any additional inquiries, please feel free to reach ',
    'out via email at ',
    '<a href="mailto:tp2701@columbia.edu">tp2701@columbia.edu</a>.</p>'
  ),
 paste0(
    '<p style = "text-align:left;">Vivian is a passionate data scientist and a current ',
    'graduate student in Columbia University\'s Applied ',
    'Analytics program. She graduated from NYU with a bachelor\'s ',
    'degree in Mathematics and Economics and has experience ',
    'working in both the financial and gaming industries. ',
    'Vivian is a sports enthusiast. In addition to tennis, ',
    'she enjoys watching soccer, basketball, and hockey games. ',
    'She is always eager to apply her data analytics skills to ',
    'diverse fields. The opportunity to create a tennis simulation ',
    'game has ignited her excitement as it allows her to merge ',
    'her love for sports with her technical expertise. Vivian ',
    'can be contacted at ',
    '<a href="mailto:vivian.yin@columbia.edu">vivian.yin@columbia.edu</a> ',
    'or via her LinkedIn page.</p>'
  ),
  paste0(
    '<p style = "text-align:left;">Day is a lecturer at Columbia University in the Applied ',
    'Analytics department. He is a passionate sports fan and ',
    'loves to apply his analytic ideas toward sports scenarios. ',
    'This SMASH! app was made possible with the help of his ',
    'core development team and their dedication to devote their  ',
    'free time toward side projects. He can be contacted at ',
    '<a href="mailto:dy2365@columbia.edu">dy2365@columbia.edu</a>.</p>'
  )
)
