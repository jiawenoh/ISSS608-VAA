#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(tidyverse)
library(jsonlite)
library(tidytext)
library(shiny.semantic)
library(shinydashboard)
library(shinycssloaders)
options(scipen = 999) 

#importing the data 
MC3_challenge <- fromJSON("data/MC3.json")

#extracting edges 
MC3_edges <-as_tibble(MC3_challenge$links) %>%
  distinct() %>%
  mutate(source = as.character(source),
         target = as.character(target),
         type = as.character(type)) %>%
  group_by(source,target, type) %>%
  summarise(weights = n()) %>%
  filter (source != target) %>%
  ungroup()

#extrading nodes 
MC3_nodes <-as_tibble(MC3_challenge$nodes) %>%
  mutate(country = as.character(country),
         id = as.character(id),
         product_services = as.character(product_services),
         revenue_omu = as.numeric(as.character(revenue_omu)),
         type = as.character(type)) %>%
  select(id,country,type,revenue_omu,product_services)

#default masterlist 
id1 <- MC3_edges %>%
  select(source) %>%
  rename(id = source)
id2 <- MC3_edges %>%
  select(target) %>%
  rename(id = target)
MC3_nodes_master <- rbind(id1, id2) %>%
  distinct() %>%
  left_join(MC3_nodes,
            unmatched = "drop")

#create new node df to include id number
MC3_nodes_Masterlist <- MC3_nodes_master %>%
  select(id) %>%
  distinct() %>%
  rename(label = id) %>%
  ungroup()

#add ID to nodes dataframe
MC3_masternodes <- MC3_nodes_Masterlist %>%
  mutate(id = as.character(1:nrow(MC3_nodes_Masterlist))) %>%
  relocate(id,label) %>%
  ungroup()

#to append correspoinding id through left_join 
MC3_edges_addID <- MC3_edges %>%
  rename(sourcelabel = source, targetlabel = target) %>%
  left_join(MC3_masternodes, by = c("sourcelabel" = "label")) %>%
  rename(source = id) %>%
  left_join(MC3_masternodes, by = c("targetlabel" = "label")) %>%
  rename(target = id) %>%
  relocate(source,target)

#word related code from this line
#unnest words
token_nodes <- MC3_nodes %>%
  unnest_tokens(word, product_services)

#remove stop_words
stopwords_removed <- token_nodes %>% 
  anti_join(stop_words)

#remove generic words 
remove_characters <- c("character", "0","unknown","products","services",
                       "including", "source", "offers","range", "related")

#create dataframe of each word with frequency 
stopwords_removed_freq <- stopwords_removed %>%
  filter(!word %in% remove_characters) %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ungroup()

#generate word cloud 
set.seed(1234) #for reproductibility
wordcloud2(data=stopwords_removed_freq, size=1.6, color='random-dark')


#custom stopwords list to remove generic words
toremove_stopwords <- c("character", "0", "unknown", "yellow", "pink", "red", 
                        "â","added", "adding", "additional", "additions",
                        "addition", "aid", "aids", "air", "related", "including", 
                        "range","products","products","service", "services", 
                        "source", "offers","range")

#new df with category column 
stopwords_removed_addcategory <- stopwords_removed %>%
  filter(!word %in% toremove_stopwords) %>% #filter list without generic wordd
  filter(!grepl("\\d", word)) %>%  #to remove numbers
  filter(!grepl("[^[:alnum:]\\s]", word)) %>% #to remove words with symbols
  mutate(category = case_when(
    word %in% c("automobiles", "automobile", "car", "cars", "bus", "buses", 
                "trucks", "truck", "trucking", "van", "vans",
                "aircraft", "airjet", "airfrieght", "airline","airlines",
                "airport","vehicle", "vehicles","powertrain",
                "transportation", "trains", "automotive", 
                "freight","transport") ~ "Transportation",
    
    grepl("fish", word) ~ "Fishing",
    word %in% c("pangasius", "angunagas","seafood","seafoods",
                "shrimp", "shrimps", "tuna", "tunas", "pollock", "fin", "roe",
                "fillet", "haddock", "cod", "hake", "saithe", "scallops",
                "arrowtooth", "flounder", "chum", "salmon", "mahi", "tilapia",
                "crab", "oyster", "scallop", "clam", "cephalopods","eel",
                "squid", "loligo", "illex", "cuttle", "abalone", "prawns",
                "lobster", "clams", "octopus", "oysters", "crabs", "halibut",
                "crustaceans", "sockeye", "trout", "herring", "sardines",
                "mackerel", "mussels", "mollusks", "lobsters", "molluscs",
                "caviar","seabass", "shark", "yellowfin","fillets") ~ "Fishing",
    
    word %in% c("oil", "oils", "gas", "cars", "gasoline", "energy",
                "evs", "electric", "biodiesel", "turbines", "steam",
                "power", "plants", "acidizing") ~ "Oil & Gas",
    
    word %in% c("chrysogaster", "insect", "insecticides", "pest",
                "pesticides") ~ "Insects",
    word %in% c("meat", "meats", "chicken","beef", "food", "lamb", "foods",
                "pork", "vegetables", "fruit", "vegetable", "salad", "tea",
                "fruits", "poultry","rice", "cakes", "sugarbeets",
                "bakery", "lambs", "lamb's", "meatballs", "bread", "cream",
                "crabmeat", "steak","soup", "tomato","cheese", "chocolate",
                "burger", "corn","pasta") ~ "Other Food",
    
    grepl("beverage", word) ~ "Beverages",
    word %in% c("milk","milks","soymilk", "tea", "teas", "coffee", "juices",
                "water","drinks","drink","juice", "wine","wines") ~ "Beverages",
    
    word %in% c("production","manufacturing", "manufacture") ~ "Manufacturer", 
    word %in% c("dance", "zumba", "yoga") ~ "Dance", 
    word %in% c("abrasive", "abrasives", "metal", "materials", "material",
                "steel", "plastic", "plastics", "paper", "rubber", "iron",
                "sealants") ~"Materials",
    word %in% c("accessories", "accessory") ~ "Accessories",
    word %in% c("acid", "acids", "deacidification") ~"Chemicals",
    grepl("chemical", word) ~ "Chemicals",
    grepl("animal", word) ~ "Animals",
    word %in% c("tech","technology", "technologies", "nanotechnology", 
                "biotech") ~ "Technology", 
    word %in% c("explosive", "explosives", "pyrotechnics",
                "combustible", "powders", "gun") ~"Explosives",
    
    word %in% c("shoes", "adidas", "footwear", "apparel", "bags", "shirts",
                "clothing", "fabric", "fabrics","fashion", "textiles", "bag",
                "boots","garments","jewelry", "beauty", "wear", "belts",
                "slippers", "gloves","caps","dress", "short", "apparels",
                "skirts","cosmetic","garment", "jackets", "socks", "shoe",
                "pants", "cosmetics") ~"Apparel & Fashion",
    
    word %in% c("toys") ~ "Toys",
    word %in% c("pharmaceutical", "construction", "leather", "dental",
                "stationery", "textile", "building", "optical", "researcher",
                "pharmaceuticals", "engineering", "freelance",
                "management") ~"Other Industries", 
    TRUE ~ "other")) 

#create new df to to filter away others 
stopwords_removed_new <- stopwords_removed_addcategory %>%
  filter(category != "other" ) #filter for all except other

library(ggplot2)
data(penguins, package = "palmerpenguins")

ui <- page_sidebar(
  title = "Penguins dashboard",
  sidebar = sidebar(
    title = "Histogram controls",
    varSelectInput(
      "var", "Select variable", 
      dplyr::select_if(penguins, is.numeric)
    ),
    numericInput("bins", "Number of bins", 30)
  ),
  card(
    card_header("Histogram"),
    plotOutput("p")
  )
)

server <- function(input, output) {
  output$p <- renderPlot({
    ggplot(penguins) +
      geom_histogram(aes(!!input$var), bins = input$bins) +
      theme_bw(base_size = 20)
  })
}

shinyApp(ui, server)
