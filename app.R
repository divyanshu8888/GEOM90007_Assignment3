# Required libraries
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shiny)
library(shinyjs)
library(wordcloud2)
library(tidytext)

#Incorporating Tableau
source("tableau-in-shiny-v1.0.R")

#**************************************************
# Code for word cloud
#**************************************************

# Load your data and perform necessary preprocessing
poi1 <- read_csv("Datasets/POI_1.csv")
poi2 <- read_csv("Datasets/POI_2.csv")
events <- read.csv("Datasets/Events.csv")

# Combine data from different sources into a single data frame
all_data <- bind_rows(poi1, poi2, events)

# Define a function to create the word cloud
vibe_word_df <- function(df) {
  tidy_txt <- df %>%
    select(`Feature Name`) %>%
    unnest_tokens(word, `Feature Name`) %>%
    anti_join(rbind(stop_words, c("br"), c("railway"), c("level"), c("ground"), c("gnd"),c("basement"),c("Hardware")))
  
  tidy_txt_df <- tidy_txt %>%
    filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
    dplyr::count(word, sort = TRUE) %>%
    na.omit(word) %>%
    mutate(word = reorder(word, n))
  
  return(tidy_txt_df)
}

#Hotels tab
#***********

hotels <- tabPanel(
  tags$head(
    tags$style(HTML(".tableauPlaceholder { width: 100% !important; }"))
  ),
  tags$style(
    HTML(
      "
    body {
      background: linear-gradient(to bottom, #E6F7FF, #FFFFFF); /* Gradient from Even Lighter Blue to White */
    }

      .hotel-tab {
        background-color: #f0f0f0;
        padding: 2px;
      }
      .hotel-heading {
        font-family: 'Times New Roman', Times, serif;
        color: black;
        text-align: center;
      }
      "
    )
  ),
  tags$div(class = "hotel-tab",
           tags$h2("HOTELS IN MELBOURNE", class = "hotel-heading")),
  tableauPublicViz(
    id = 'tableauviz',
    url = "https://public.tableau.com/views/HotelsinMelbourne/HotelDashboard?:language=en-US&:display_count=n&:origin=viz_share_link",
    height = "800px"
  )
)

#Poi tab
#**********

poi <- tabPanel(
  tags$head(
    tags$style(
      HTML(".tableauPlaceholder { width: 100% !important; }")
    )
  ),
  tags$style(
    HTML(
      "
      .poi-tab {
        background-color: #f0f0f0;
        padding: 2px; /* Light Grey */
      }
      .poi-heading {
        font-family: 'Times New Roman', Times, serif;
        color: black;
        text-align: center;
      }
      "
    )
  ),
  tags$div(class = "poi-tab",
           tags$h2("POINT OF INTERESTS IN MELBOURNE", class = "poi-heading")),
  tableauPublicViz(
    id = 'tableauviz_poi',
    url = "https://public.tableau.com/views/transportation_16971021842490/Dashboard2?:language=en-GB&publish=yes&:display_count=n&:origin=viz_share_link",
    height = "800px"
  )
)

#Rest tab
#**************

rest <- tabPanel(
  tags$head(
    tags$style(HTML(".tableauPlaceholder { width: 100% !important; }"))
  ),
  tags$style(
    HTML(
      "
      .rest-tab {
        background-color: #f0f0f0;
        padding: 2px; /* Light Grey */
      }
      .rest-heading {
        font-family: 'Times New Roman', Times, serif;
        color: black;
        text-align: center;
      }
      "
    )
  ),
  tags$div(class = "rest-tab",
           tags$h2("RESTAURANTS AND CAFES IN MELBOURNE", class = "rest-heading")),
  tableauPublicViz(
    id = 'tableauviz_cafe',
    url = "https://public.tableau.com/views/Restaurants_16969515690420/Story1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
    height = "800px"
  )
)

#Routes tab
#******************

routes <- tabPanel(
  tags$head(
    tags$style(HTML(".tableauPlaceholder { width: 100% !important; }"))
  ),
  tags$style(
    HTML(
      "
      .routes-tab {
        background-color: #f0f0f0;
        padding: 2px; /* Light Grey */
      }
      .routes-heading {
        font-family: 'Times New Roman', Times, serif;
        color: black;
        text-align: center;
      }
      "
    )
  ),
  tags$div(class = "routes-tab",
           tags$h2("TRANSPORTATION ROUTES IN MELBOURNE", class = "rest-heading")),
  tableauPublicViz(
    id = 'tableauviz_routes',
    url = "https://public.tableau.com/views/TransportationRoutesMelbourne/Dashboard3?:language=en-GB&:display_count=n&:origin=viz_share_link",
    height = "800px"
  )
)

#Emergency tab info
#*******************

emergency_info <- div(
  style = "background-color: #f0f0f0; padding: 10px; text-align: center;",
  h4("Emergency Contacts"),
  p("Police: 000"),
  p("Fire: 000"),
  p("Hospital: 000")
)


# "Overview" tab panel content
#********************************

overview_tab <- tabPanel("Overview",
                         tags$style(HTML(
                           "
    .over-tab {
      background-color: #f0f0f0;
      margin-top: 20px; /* Increase the space above */
      padding: 2px; /* Light Grey */
    }
    
    .over-text {
      font-family: 'Times New Roman', Times, serif;
      color: black;
      text-align: center;
    }
    
    .image-container {
      display: flex;
      flex-direction: row;
      align-items: center;
      gap: 20px; /* Gap between images and text */
    }
    
    .image-container img {
      width: 300px;
      height: 200px;
      transition: transform 0.2s;
    }
    
    .image-container img:hover {
      transform: scale(1.1);
      z-index: 1;
    }
    
    .image-container img::after {
      content: attr(alt);
      display: block;
      text-align: center;
      background-color: rgba(255, 255, 255, 0.8);
      position: absolute;
      bottom: 0;
      left: 0;
      right: 0;
      transform: scaleY(0);
      transition: transform 0.2s;
    }
    
    .image-container img:hover::after {
      transform: scaleY(1);
    }
    
    .caption {
      text-align: center;
      font-weight: bold;
    }
    
    .p-container {
      margin-top: 20px; /* Add margin to the top of the <p> element */
    }
    
    .wordcloud-container {
      margin-top: 20px; /* Add margin to the top of the word cloud container */
    }
    "
                         )),
                         
                         tabsetPanel(
                           id = "overview-submenu",  # Unique ID for the tabsetPanel
                           type = "tabs",
                           tabPanel("About Melbourne",class="About",
                                    div(class = "image-container",
                                        div(
                                          class = "image-title",
                                          tags$img(src = "Collins.jpeg", alt = "Collins Street"),
                                          p(class = "caption", "Collins Street")
                                        ),
                                        div(
                                          class = "image-title",
                                          tags$img(src = "history1.jpg", alt = "Flinders Railway Station"),
                                          p(class = "caption", "Flinders Railway Station")
                                        ),
                                        div(
                                          class = "image-title",
                                          tags$img(src = "flinders_his.jpg", alt = "Flinders Railway Station"),
                                          p(class = "caption", "Flinders Railway Station")
                                        ),
                                        div(
                                          class = "image-title",
                                          tags$img(src = "street.jpg", alt = "Flinders Street"),
                                          p(class = "caption", "Flinders Street")
                                        )
                                    ),
                                    div(class = "p-container",
                                        tags$p(
                                          "The City of Melbourne respectfully acknowledges that it is located on the traditional land of the Kulin Nation. ",
                                          "This special place is now known by its European name of Melbourne.",
                                          br(),
                                          "Today, Melbourne is one of the great multicultural cities of the world and is a significant meeting place.",
                                          br(),
                                          "For the Wurundjeri, Boonerwrung, Taungurong, Djajawurrung and Wathaurung which make up the Kulin Nation, Melbourne has always been an important meeting place and location for events of social, educational, sporting, and cultural significance.",
                                          br(),
                                          "Since its European settlement in 1835, different nationalities have migrated to Melbourne at various stages of its history and have contributed significantly to the city's growing identity.",
                                          br(),
                                          "Find out more about Melbourne’s ", tags$a(href = "https://www.melbourne.vic.gov.au/about-melbourne/melbourne-profile/aboriginal-culture/Pages/aboriginal-culture.aspx", "Aboriginal and Torres Strait Islander culture "),
 "and its", tags$a(href = "https://www.melbourne.vic.gov.au/about-melbourne/melbourne-profile/multicultural-communities/Pages/multicultural-communities.aspx", "its multicultural history."),
                                          br(),
                                          br(),
                                          "Download the PDF: ",
                                          tags$a(href = "https://www.melbourne.vic.gov.au/SiteCollectionDocuments/history-city-of-melbourne.pdf", "The History of the City of Melbourne")
                                        )
                                    ),
                                    # Add other content related to 'About Melbourne'
                           ),
 tabPanel("Attraction Places",
                                   tags$style(HTML(
                                     "
    .attraction-container {
      display: flex;
      flex-wrap: wrap;
      gap: 20px;
    }
    .attraction {
      display: flex;
      flex-direction: row;
      align-items: center;
      gap: 20px;
    }
    .attraction img {
      max-width: 45vw;
      max-height: 45vh;
      transition: transform 0.2s;
    }
    .attraction img:hover {
      transform: scale(1.5);
      z-index: 1;
    }
    .attraction img::after {
      content: attr(alt);
      display: block;
      text-align: center;
      background-color: rgba(255, 255, 255, 0.8);
      position: absolute;
      bottom: 0;
      left: 0;
      right: 0;
      transform: scaleY(0);
      transition: transform 0.2s;
    }
    .attraction img:hover::after {
      transform: scaleY(1);
    }
    .attraction-info {
      flex: 1;
    }
    .attraction-info p {
      margin: 0;
    }
    .attraction-info h4 { font-weight: bold; }
    "
                                   )),
                                   tags$div(
                                     class = "attraction-container",
                                     div(
                                       class = "attraction",
                                       img(src = "1.jpg", alt = "Federation Square", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Federation Square"),
                                         p("Federation Square, also known as 'Fed Square,' is Melbourne's central gathering place. It features a unique architectural design with a mix of cultural institutions, restaurants, bars, and event spaces. It's a hub for cultural events, art exhibitions, and a popular spot to enjoy the city's vibrant atmosphere."),
                                         a(href = "https://www.viator.com/Melbourne-attractions/Federation-Square/d384-a396?mcid=33953&tsem=true&supci=-1126458424&supag=1225955543976705&supsc=kwd-76622283963788&supai=76622353963360&supdv=c&supnt=o&supkw=federation_square%20website&supti=kwd-76622283963788&suplp=112413&supli=112413&m=33953&supag=1225955543976705&supsc=kwd-76622283963788&supai=76622353963360&supdv=c&supnt=nt:o&suplp=112413&supli=112413&supti=kwd-76622283963788&tsem=true&supci=kwd-76622283963788&supkw=federation_square%20website&msclkid=e5c2a7e939da1d1f4294c2c0333a4f52", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "royal_botanic_gardens.jpg", alt = "Royal Botanic Gardens", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Royal Botanic Gardens"),
                                         p("The Royal Botanic Gardens in Melbourne offer a peaceful escape from the bustling city. Spread over 38 hectares, it's home to a wide variety of plant species, beautifully manicured gardens, and scenic walking paths."),
                                         a(href = "https://www.rbg.vic.gov.au/", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "national_gallery_of_victoria.jpg", alt = "National Gallery of Victoria", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("National Gallery of Victoria (NGV)"),
                                         p("NGV is Australia's premier art museum, featuring an extensive collection of art, including Australian, European, Asian, and contemporary works. The museum is known for its impressive exhibitions and is a must-visit for art enthusiasts."),
                                         a(href = "https://www.ngv.vic.gov.au/", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "melbourne_museum.jpg", alt = "Melbourne Museum", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Melbourne Museum"),
                                         p("Located in the Carlton Gardens, the Melbourne Museum showcases Australian cultural history, indigenous cultures, and natural history. It's an educational and interactive experience for visitors of all ages."),
                                         a(href = "https://museumsvictoria.com.au/melbournemuseum/", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "eureka_tower.jpg", alt = "Eureka Tower", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Eureka Tower"),
                                         p("Eureka Tower is an iconic skyscraper in Melbourne, and its Eureka Skydeck on the 88th floor offers panoramic views of the city. It's a great place to get a bird's-eye view of Melbourne."),
                                         a(href = "https://en.wikipedia.org/wiki/Eureka_Tower", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "queen_victoria_market.jpg", alt = "Queen Victoria Market", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Queen Victoria Market"),
                                         p("One of the largest open-air markets in the Southern Hemisphere, Queen Victoria Market is a bustling place where you can shop for fresh produce, gourmet foods, clothing, souvenirs, and more."),
                                         a(href = "https://qvm.com.au/", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "zoo.jpg", alt = "Melbourne Zoo", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Melbourne Zoo"),
                                         p("Melbourne Zoo is home to a wide range of animals from around the world. It's known for its conservation efforts and provides an opportunity to see and learn about various species."),
                                         a(href = "https://www.zoo.org.au/melbourne", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "southbank_promenade.jpg", alt = "Southbank Promenade", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Southbank Promenade"),
                                         p("Southbank is a lively waterfront area along the Yarra River. It's filled with restaurants, cafes, and cultural institutions. You can enjoy a leisurely walk, dine with a view, or attend a show at the Arts Centre Melbourne."),
                                         a(href = "https://www.melbourne.vic.gov.au/building-and-development/shaping-the-city/city-projects/Pages/southbank-promenade.aspx", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "state_library.jpg", alt = "State Library of Victoria", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("State Library of Victoria"),
                                         p("The State Library is not only a library but a historical and architectural gem. Visitors can explore its beautiful reading rooms, exhibitions, and a vast collection of books and manuscripts."),
                                         a(href = "https://www.slv.vic.gov.au/", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "st_kilda_beach.jpg", alt = "St. Kilda Beach", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("St. Kilda Beach"),
                                         p("St. Kilda is a vibrant beachside suburb known for its beach, Luna Park amusement park, and the famous St. Kilda Pier. It's a great place to relax by the sea and enjoy a lively atmosphere."),
                                         a(href = "https://www.visitvictoria.com/regions/Melbourne/See-and-do/Nature-and-wildlife/Beaches-and-coastlines/VV-St-Kilda-Beach", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "melbourne_street_art.jpg", alt = "Melbourne Street Art", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Melbourne Street Art"),
                                         p("Melbourne is known for its vibrant street art scene, with laneways like Hosier Lane and AC/DC Lane adorned with colorful murals and graffiti art."),
                                         a(href = "https://www.melbourne.vic.gov.au/arts-and-culture/art-outdoors/pages/street-art.aspx", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "old_melbourne_gaol.jpg", alt = "Old Melbourne Gaol", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Old Melbourne Gaol"),
                                         p("This historic site was once a prison and is now a museum that tells the story of Melbourne's criminal past, including the life and crimes of the infamous outlaw Ned Kelly."),
                                         a(href = "https://www.tiqets.com/en/old-melbourne-gaol-tickets-l150298/?utm_account=154002385&utm_source=bing&utm_medium=cpc&utm_campaign=377363721&utm_content=1255642780193484&msclkid=0fdcfbf0af05100392239ed9bfcedc6d&utm_term=Old+Melbourne+Gaol", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "royal_arcade.jpg", alt = "Royal Arcade", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("Royal Arcade"),
                                         p("Royal Arcade is known for its elegant architecture, boutique shops, and charming atmosphere, making it popular for shopping and exploring."),
                                         a(href = "https://royalarcade.com.au/", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "the_block_arcade.jpg", alt = "The Block Arcade", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("The Block Arcade"),
                                         p("The Block Arcade is known for its elegant architecture, boutique shops, and charming atmosphere, making it popular for shopping and exploring."),
                                         a(href = "https://theblock.com.au/", "More Info")
                                       )
                                     ),
                                     div(
                                       class = "attraction",
                                       img(src = "acmi.jpg", alt = "ACMI (Australian Centre for the Moving Image)", width = "300px", height = "200px"),
                                       div(
                                         class = "attraction-info",
                                         h4("ACMI (Australian Centre for the Moving Image)"),
                                         p("ACMI is a museum dedicated to film, television, video games, and digital culture. It features interactive exhibits and a wealth of information about the moving image."),
                                         a(href = "https://www.acmi.net.au/", "More Info")
                                       )
                                     )
                                   )
 ),
 
 tabPanel("Nearby Attractions",
          tags$style(HTML(
            "
    .attraction-container {
      display: flex;
      flex-wrap: wrap;
      gap: 20px;
    }
    .attraction {
      display: flex;
      flex-direction: row;
      align-items: center;
      gap: 20px;
    }
    .attraction img {
      max-width: 45vw; /* Adjust to 45% of the viewport width */
      max-height: 45vh; /* Adjust to 45% of the viewport height */
      transition: transform 0.2s;
    }
    .attraction img:hover {
      transform: scale(1.2);
      z-index: 2;
    }
    .attraction img::after {
      content: attr(alt);
      display: block;
      text-align: center;
      background-color: rgba(255, 255, 255, 0.8);
      position: absolute;
      bottom: 0;
      left: 0;
      right: 0;
      transform: scaleY(0);
      transition: transform 0.2s;
    }
    .attraction img:hover::after {
      transform: scaleY(1);
    }
    .attraction-info {
      flex: 1;
    }
    .attraction-info p {
      margin: 0;
    }
    "
          )),
          tags$div(
            class = "attraction-container",
            div(
              class = "attraction",
              img(src = "great_ocean_road.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Great Ocean Road"),
                p("The Great Ocean Road, stretching 243 kilometers (151 miles) from Torquay to Allansford in Victoria, Australia, is a renowned scenic coastal drive. Built by World War I returned soldiers between 1919 and 1932, it's the world's largest war memorial. Key attractions include the iconic Twelve Apostles limestone stacks, Loch Ard Gorge, and the surf hotspot, Bells Beach. The road's start in Torquay is roughly 100 kilometers (62 miles) or a 1.5 to 2-hour drive from Melbourne."),
                a(href = "https://visitgreatoceanroad.org.au", "More Info")
              )
            ),
            div(
              class = "attraction",
              img(src = "phillip_island.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Phillip Island"),
                p("Located 140 kilometers southeast of Melbourne, Phillip Island is famed for its diverse wildlife and motor sports. Highlights include the evening Penguin Parade, the Koala Conservation Centre, and Seal Rocks. The island also hosts the MotoGP at its Grand Prix Circuit and offers prime surfing at Cape Woolamai. A visit typically entails a 2-hour drive from Melbourne."),
                a(href = "https://www.visitphillipisland.com.au", "More Info")
              )
            ),
            div(
              class = "attraction",
              img(src = "mornington_peninsula.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Mornington Peninsula"),
                p("Situated 60-90 kilometers south of Melbourne, the Mornington Peninsula offers a mix of bay and surf beaches, boutique wineries, and local gourmet dining. Highlights include the Peninsula Hot Springs, scenic national parks like Point Nepean, and a variety of walking trails. The region is a 1 to 1.5-hour drive from Melbourne, making it a favorite for relaxation and outdoor adventures."),
                a(href = "https://www.mornpen.vic.gov.au/Home", "More Info")
              )
            ),
            div(
              class = "attraction",
              img(src = "lakes_entrance.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Lakes Entrance"),
                p("Located 320 kilometers east of Melbourne, Lakes Entrance is a coastal town known for its intersection of the Gippsland Lakes and the Southern Ocean. Popular for fishing, boating, and the expansive Ninety Mile Beach, the town is a hotspot for fresh seafood and offers scenic views, especially from the Entrance Walk. A visit typically involves a 3.5 to 4-hour drive from Melbourne."),
                a(href = "https://www.visitmelbourne.com/regions/gippsland/destinations/lakes-entrance", "More Info")
              )
            ),
            div(
              class = "attraction",
              img(src = "mount_buller.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Mount Buller"),
                p("Located 230 kilometers northeast of Melbourne, Mount Buller is a prime Victorian ski resort in winter and a mountain biking and hiking destination in summer. The mountain village offers accommodation, dining, and entertainment. A visit typically involves a 3-hour drive from Melbourne, making it a popular choice for both seasonal snow enthusiasts and summer adventurers."),
                a(href = "https://www.mtbuller.com.au", "More Info")
              )
            ),
            div(
              class = "attraction",
              img(src = "mount_baw_baw.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Mount Baw Baw"),
                p("Situated 120 kilometers east of Melbourne, Mount Baw Baw is a versatile alpine resort. Popular for skiing and snowboarding in winter, it transitions to hiking and mountain biking in summer. The alpine village provides essential amenities and accommodations. Typically, it's a 2.5-hour drive from Melbourne, making it a convenient nature retreat for city dwellers."),
                a(href = "https://www.mountbawbaw.com.au", "More Info")
              )
            ),
            div(
              class = "attraction",
              img(src = "brighton_beach.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Brighton Beach"),
                p("Located 11 kilometers south of Melbourne's CBD, Brighton Beach is famed for its iconic colorful bathing boxes. The sandy shore offers views of Melbourne's skyline across Port Phillip Bay and is a popular spot for swimming and relaxation. Its proximity to the city makes it a favorite seaside escape for locals and tourists alike."),
                a(href = "https://www.tripadvisor.com.au/Attraction_Review-g954016-d2501131-Reviews-Brighton_Beach-Brighton_Bayside_Greater_Melbourne_Victoria.html", "More Info")
              )
            ),
            div(
              class = "attraction",
              img(src = "sandringham_beach.jpg", width = "300px", height = "200px"),
              div(
                class = "attraction-info",
                h4("Sandringham Beach"),
                p("Located 16 kilometers southeast of Melbourne's CBD, Sandringham Beach is a tranquil coastal spot known for its calm waters and family-friendly ambiance. The beach features a scenic walking and cycling path, with Sandringham Village nearby offering eateries and shops. It's a favored seaside retreat close to the city."),
                a(href = "https://www.tripadvisor.com.au/Attraction_Review-g1838700-d10088338-Reviews-Sandringham_Beach-Sandringham_Bayside_Greater_Melbourne_Victoria.html", "More Info")
              )
            )
          )
 ),                          
 
 tabPanel("Fun Facts",
                                      tags$div(style="margin-top: 20px;"),  # Add space above the list
                                      tags$ul(
                                        tags$li(HTML("<b>The World's Most Livable City:</b> Melbourne has been consistently ranked as one of the most livable cities in the world. It often competes with cities like Vienna and Vancouver for the top spot."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Four Seasons in One Day:</b> Melbourne is known for its unpredictable weather. It's not uncommon to experience four seasons in one day, so locals often carry clothing for all weather conditions."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Coffee Capital:</b> Melbourne has a strong coffee culture. It's home to numerous cafes and is known for its quality coffee. You'll find a diverse range of coffee styles, from espressos to flat whites."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Laneway Art:</b> Melbourne's laneways are famous for their street art. Hosier Lane and AC/DC Lane are well-known for their ever-changing graffiti art. It's a dynamic and colorful part of the city's culture."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Sports Capital:</b> Melbourne is often referred to as the sports capital of the world. It hosts major events like the Australian Open (tennis), the Melbourne Cup (horse racing), and the AFL Grand Final (Australian rules football)."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Trams Everywhere:</b> Melbourne boasts the largest tram network in the world. The iconic trams are a common mode of transportation and are a symbol of the city."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Multicultural Melting Pot:</b> Melbourne is a multicultural city with a diverse population. This diversity is reflected in its cuisine, with a wide range of international foods available."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Gardens Galore:</b> The city is home to many beautiful gardens, including the Royal Botanic Gardens and Fitzroy Gardens, which offer a serene escape from the urban hustle and bustle."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Hidden Bars:</b> Melbourne is famous for its hidden bars. Some of them are tucked away in alleyways and require a bit of exploration to find. They are often cozy and have a unique atmosphere."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Shopping and Fashion:</b> Melbourne is a fashion-forward city. Chapel Street and Bourke Street Mall are popular shopping destinations, and the city hosts the Melbourne Fashion Festival."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Australian Rules Football:</b> Melbourne is the birthplace of Australian rules football (AFL). The sport is a major part of the city's culture, and games are held at iconic venues like the Melbourne Cricket Ground (MCG)."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Cultural Hub:</b> The city has a vibrant arts and cultural scene. It's home to the National Gallery of Victoria (NGV), the Melbourne Symphony Orchestra, and many theaters and galleries."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Education Hub:</b> Melbourne is known for its prestigious universities. It's a hub for international students and has a strong academic reputation."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Great Ocean Road:</b> Just a short drive from Melbourne, the Great Ocean Road is one of the world's most scenic coastal drives, featuring attractions like the Twelve Apostles."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>St. Kilda's Penguins:</b> St. Kilda Beach is famous for its little penguin colony. You can watch these cute creatures return from the sea at dusk."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Melbourne Cup:</b> The Melbourne Cup is one of the most famous horse races in the world. It's often called 'the race that stops a nation' because it's a public holiday in Melbourne."), style = "margin-bottom: 10px;"),
                                        tags$li(HTML("<b>Artistic Expression:</b> Melbourne hosts many cultural festivals throughout the year, celebrating music, arts, food, and more. The Melbourne International Comedy Festival is a popular event."), style = "margin-bottom: 10px;")
                                      ),
                                      tags$div(style="margin-bottom: 20px;")  # Add space below the list
 )
                         ),
                         div(class = "over-tab",
                             h3("WORD CLOUD", class = "over-text")
                         ),
                         fluidRow(class = "wordcloud-container",
                           column(12,
                                  wordcloud2Output("word_cloud", width = "100%", height = "400px")
                           )
                         )
)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Light black background for the navbar and pure white font color */
      .navbar {
        color: #000000;
        width:100%;
        border: 2px solid #000000; /* Create a 3D border effect on the bottom */
      }
      
  .title-section {
    display: flex;
    justify-content: center; /* Center horizontally */
    font-size: 48px; /* Adjust the font size as needed */
    align-items: center; /* Center vertically */
    font-weight: bold; /* Make the text bold */
    font-family: 'Roboto', sans-serif; /* Use the 'Roboto' Sans-Serif font, or replace it with your preferred Sans-Serif font */
  }
  .logo {
    max-width: 300px; /* Adjust the size of the logo as needed */
    margin-right: 35px;/* Adjust the margin for spacing */
    margin-top:15px;
    height: 150px;
  }
      
      /* Title text style */
      
      .navbar .navbar-brand {
        font-size: 25px; /* Adjust the font size */
        font-weight: bold; /* Make it bold */
        color: #000000; /* Set font color to pure white */
      }

      /* Thicker and 3D-look horizontal rule */
      .hr-3d {
        border: none;
        height: 4px;
        background: linear-gradient(to right, #000000 0%, #333333 25%, #666666 50%, #999999 75%, #CCCCCC 100%);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
      }


      /* Zoom effect for buttons on hover */
      .nav-tabs>li>a:hover {
        transform: scale(1.1); /* Increase the scale on hover (zoom effect) */
        transition: transform 0.3s; /* Add a smooth transition effect */
      }

    "))
  ),
  # Title section
  div(
    class = "title-section",
    img(src = "MM.jpeg", class = "logo"),
    h1("Melbourne Magic",style = "font-size: 54px; font-weight: bold; font-family: 'Roboto', sans-serif; color: #000000;"),
    
  ),
  hr(class = "hr-3d"),
  navbarPage(
    id = "mypage",
    header = setUpTableauInShiny(),
    # Horizontal rule with 3D effect
    hr(class = "hr-3d"),
    tabPanel("Welcome",
             tags$style(HTML(
               ".welcome-page {",
               "  background-image: url('Melbourne.jpg');",
               "  background-size: cover;",
               "  background-repeat: no-repeat;",
               "  background-attachment: fixed;",
               "  background-position: center center;",
               "  height: 130vh;",
               "font-weight: bold;", 
               "color: #000000;",
               "  opacity: 0.9;",  # Set the height to full viewport height
               "}"
             )),
             div(style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;"),
             div(class = "welcome-page",
                 style = "text-align: left; padding: 20px; color: white; border: 4px solid white;",
                 h3("Explore the Best of Melbourne!"),
                 p("Discover hidden gems and make the most of your visit."),
                 htmlOutput("timer")
             )
    ),
    # All tabs here
    tabPanel("Overview", overview_tab), # No CSS styles for this tab
    tabPanel("Hotels", hotels),
    tabPanel("Point of Interests", poi),
    tabPanel("Cafes and Restaurants", rest),
    tabPanel("Transportation Routes", routes),
    tabPanel("About Us",fluidPage(
      HTML("<h2>About Our Team</h2>"),
      
      HTML('<div style="display: flex; flex-wrap: wrap;">'), # Start a flex container
      
      # Team Member 1
      HTML('<div style="flex: 1; width: 25%; padding: 10px; text-align: center;">'),
      HTML('<div class="circle-img"><img src="team_member1.jpeg" alt="Team Member 1"></div>'),
      HTML("<h3>Divyanshu Mishra</h3>"),
      HTML("<p>1281413</p>"),
      HTML("<p>divyanshum@student.unimelb.edu.au</p>"),
      HTML('</div>'),
      
      # Team Member 2
      HTML('<div style="flex: 1; width: 25%; padding: 10px; text-align: center;">'),
      HTML('<div class="circle-img"><img src="team_member2.jpeg" alt="Team Member 2"></div>'),
      HTML("<h3>Meghana Ganapa</h3>"),
      HTML("<p>1436378</p>"),
      HTML("<p>mganapa@student.unimelb.edu.au</p>"),
      HTML('</div>'),
      
      # Team Member 3
      HTML('<div style="flex: 1; width: 25%; padding: 10px; text-align: center;">'),
      HTML('<div class="circle-img"><img src="team_member3.jpeg" alt="Team Member 3"></div>'),
      HTML("<h3>Satvik Chandra Saxena</h3>"),
      HTML("<p>1414681</p>"),
      HTML("<p>satvikchandra@student.unimelb.edu.au</p>"),
      HTML('</div>'),
      
      # Team Member 4
      HTML('<div style="flex: 1; width: 25%; padding: 10px; text-align: center;">'),
      HTML('<div class="circle-img"><img src="team_member4.jpeg" alt="Team Member 4"></div>'),
      HTML("<h3>Akashdeep Singh</h3>"),
      HTML("<p>1387018</p>"),
      HTML("<p>akashdeepsi2@student.unimelb.edu.au</p>"),
      HTML('</div>'),
      
      HTML('</div>') # End the flex container
      
      # CSS for circular images
      ,
      tags$style(HTML('
    .circle-img {
      width: 200px;
      height: 200px;
      border-radius: 50%;
      overflow: hidden;
      margin: 0 auto 10px auto;
    }
    .circle-img img {
      width: 100%;
      height: 100%;
      object-fit: cover;
    }
  '))
    )
    )
  ),
  br(),
  br(),
  br(), # Keep the <br>() tags
  # 3D horizontal rule
  hr(class = "hr-3d"),
  
  emergency_info,
  div(
    class = "additional-info",
    tags$style(
      HTML(
        ".additional-info p { margin: 0; }"  # Apply style to paragraphs within the div
      )
    ),
    HTML(
      "<p>Data from:
        <a href='https://datashare.maps.vic.gov.au/search?q=uuid%3D871a5706-7282-5897-accd-cff576e71055/'>Routes and stations</a>,
        <a href='https://datashare.maps.vic.gov.au/search?q=uuid%3D871a5706-7282-5897-accd-cff576e71055/'>Train Stations</a>,
        <a href='https://drive.google.com/file/d/1crnV653ZCGWhkBIzKo-mJCc9sY6ldBmN/view'>POI</a>,
        <a href='http://insideairbnb.com/get-the-data/'>Hotels</a>
      </p>"
    )
  )
)



server <- function(input, output, session) {
  observeEvent(input$mypage, {
    runjs('dispatchEvent(new Event("resize"))')
  })
  output$timer <- renderUI({
    # Use JavaScript to switch to the next tab after 3 seconds
    tags$script(HTML(
      "setTimeout(function() {",
      "  Shiny.onInputChange('next_tab', '1');",  # Switch to the next tab
      "}, 5000);"  # Delay in milliseconds (5 seconds)
    ))
  })
  observeEvent(input$next_tab, {
    # Switch to the next tab
    runjs('dispatchEvent(new Event("resize"))')
    updateNavbarPage(session, "mypage", selected = "Overview")
  })
  
  # Rendering Word Cloud
  output$word_cloud <- renderWordcloud2({
    set.seed(1234)
    wordcloud_df <- vibe_word_df(all_data)
    wordcloud2(wordcloud_df, fontFamily = 'helvetica nue', font = 2, color = 'random-dark', size = 6)  # Increase the size
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
