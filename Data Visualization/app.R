#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(tidyverse)
library(ggrepel)
library(directlabels)
library(colorspace)
library(sysfonts)
library(extrafont)
library(showtext)
library(readr)
library(dplyr)
library(treemap) # tree map visualization
library(treemapify)
library(ggmap)
library(plotly)
library(leaflet)
library(gridExtra)


## --------------------------------------------------------------------------------------------
kyoto <- read_csv("archive_kyoto_restaurant/Kyoto_Restaurant_Info.csv", col_types=cols(
    id = col_integer(),
    name = col_character(),
    japanese_name = col_character(),
    station = col_character(),
    first_category = col_character(),
    second_category = col_character(),
    dinner_price = col_character(),
    lunch_price = col_character(),
    total_rating = col_double(),
    dinner_rating = col_double(),
    lunch_rating = col_double(),
    review_num = col_integer(),
    lat = col_double(),
    long = col_double()
),

# rename the columns instead of the original ones in the CSV file
col_names = c("id", "name",	"japanese_name",	"station",	"first_category",	"second_category",	"dinner_price",	"lunch_price",	"total_rating",	"dinner_rating",	"lunch_rating",	"review_num", 	"lat",	"long"),

locale = locale(encoding = "UTF-8"),
skip=1)

kyoto <- 
    kyoto %>% 
    select(-id)



## --------------------------------------------------------------------------------------------
# kyoto %>% 
#     head(5) %>% 
#     DT::datatable(options = list(
#         lengthMenu = c(5,3,1)
#     ))


## --------------------------------------------------------------------------------------------
tilde <- strsplit(kyoto$dinner_price[1], split="")[[1]][6]
yen <- strsplit(kyoto$dinner_price[1], split="")[[1]][1]

kyoto$DinnerPrice <- as.character(kyoto$dinner_price)
kyoto$DinnerPrice <- gsub(yen, "JPY", kyoto$DinnerPrice)
kyoto$DinnerPrice <- gsub(tilde, "~", kyoto$DinnerPrice)
kyoto$DinnerPrice <- as.factor(kyoto$DinnerPrice)

# levels(kyoto$DinnerPrice)

kyoto$DinnerPrice <- 
    factor(kyoto$DinnerPrice, 
           levels = c("~JPY999", "JPY1000~JPY1999", "JPY2000~JPY2999", 
                      "JPY3000~JPY3999", "JPY4000~JPY4999", "JPY5000~JPY5999", 
                      "JPY6000~JPY7999", "JPY8000~JPY9999", "JPY10000~JPY14999", 
                      "JPY15000~JPY19999", "JPY20000~JPY29999", "JPY30000~"))
# head(kyoto$DinnerPrice)


## --------------------------------------------------------------------------------------------
# Setting location data of Kyoto
Kyoto.geo <- data.frame(135.768, 35.01164)
Kyoto.geo <- data.frame(Kyoto.geo)
names(Kyoto.geo) <- c("lon", "lat")

all_restaurant_map <- 
    leaflet(data = kyoto) %>% 
    addProviderTiles("Stamen.TonerLite") %>% 
    setView(lng = Kyoto.geo$lon, lat = Kyoto.geo$lat, zoom = 13) %>% 
    addCircleMarkers(~long, ~lat, popup = ~japanese_name,
                     clusterOptions = markerClusterOptions()) %>% 
    addLegend("topright", colors= "blue", 
              labels="Restaurant Name", title="Food Restaurants in Kyoto")


## --------------------------------------------------------------------------------------------
    # treemap(data.frame(table(kyoto$station)),
    #     index="Var1",
    #     vSize="Freq",
    #     type="index",
    #     title = "Number of Food Restaurants")


all_stations <-  
    data.frame(table(kyoto$station)) %>% 
        ggplot(aes(label=Var1, area=Freq, fill=Freq)) + 
            geom_treemap() + 
             geom_treemap_text(fontface = "italic",
                               colour = "white", 
                               place = "centre",
                               grow = TRUE) + 
            labs(title="Tree Map of Stations by Number of Restaurants")
    
## --------------------------------------------------------------------------------------------
top15_stations_barchart <- 
    data.frame(table(kyoto$station)) %>% 
    arrange(desc(Freq)) %>% 
    head(15) %>% 
    ggplot(aes(x = reorder(Var1, Freq), y = Freq, 
               fill = Var1, label = Freq)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(x = "", y = "", title = "Top 15 Areas") +
      geom_label(show.legend = F, nudge_y = -5) + 
      theme(axis.text.x = element_text(angle = 0)) +
      coord_flip()


## --------------------------------------------------------------------------------------------
top.areas <- 
    data.frame(table(kyoto$station)) %>% 
    arrange(desc(Freq)) %>% 
    head(15) %>% 
    select(Var1)

top.areas <- top.areas$Var1

kyoto.top.areas <- 
    kyoto %>% 
    filter(station %in% top.areas)


## --------------------------------------------------------------------------------------------
# finding the categories in the top 15 areas (stations)
category1 <- data.frame(kyoto.top.areas$first_category)
category2 <- data.frame(kyoto.top.areas$second_category)
names(category1) <- "Category"
names(category2) <- "Category"

food.category <- rbind(category1, category2)

all_food_categories <- 
    plot_ly(as.data.frame(table(food.category$Category))[-1,], 
        labels = ~Var1, values = ~Freq, type = 'pie', 
        textposition = "inside") %>%
    layout(title = 'Food Categories',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



## --------------------------------------------------------------------------------------------
food.df <- data.frame(table(food.category$Category)) %>% 
  arrange(desc(Freq)) %>% 
  head(20)

top20_food_categories_barchart <- 
  food.df %>% 
    ggplot(aes(x = reorder(Var1, Freq),
               y = Freq, 
               fill = Var1, 
               label = Freq)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_label(show.legend = F, nudge_y = -0.4*food.df$Freq, 
                 label.size=0.15, 
                 label.padding=unit(0.15, "lines"), 
                 label.r=unit(0.1, "lines")) +
      coord_flip() +
      labs(y = "", x = "Food Category",
           title = "Top 20 Popular Food Categories in Kyoto")


## --------------------------------------------------------------------------------------------
top.category <- data.frame(table(food.category$Category)) %>% 
    arrange(desc(Freq)) %>% 
    head(20)

# top.restaurant.kyoto <- 
#     kyoto.top.areas %>% 
#     filter(first_category %in% top.category$Var1 | second_category %in% top.category$Var1)


## --------------------------------------------------------------------------------------------


# Define UI for the Kyoto Restaurant application 
ui <-  navbarPage(title = "Kyoto Restaurants",
                  
  tabPanel(title = "Overview",
           "Welcome! Start here to get yourself familiarized with the locations of
           nearly 900 restaurants in Kyoto. Navigate to the other tabs to find out 
           more about popular stations, food categories and more! You can even find 
           the top restuarants in your favorite food category ranked by their 
           dinner ratings. We hope that the information on this page can be useful
           for your next trip to Kyoto. Bon appétit!",  
           emo::ji('food'), emo::ji('food'), emo::ji('food'),
           
           leafletOutput("all_restaurant_map"),
           br(),
           fluidRow(column(4, offset=8, "Data sourced from and some codes inspired by Kaggle."))
          ),
  tabPanel(title = "Popular Stations and Top Food Categories",
           
           "In this tab, you will find:", br(), 
           "1) All stations represented in a tree map where
           the area and the relative shade of blue of a tile reflect the number of 
           restaurants near that particular station", br(), 
           "2) The 15 most popular stations where
           restaurants tend to be concentrated", br(), 
           "3) All food categories among these 15 Most
           popular stations in a pie chart, ordered by the number of restaurants featuring
           a particular food type, and", br(),
           "4) The top 20 food categories in that pie chart;
           that is, the top 20 most served food categories among the 15 most popular
           stations.",
           
           fluidRow(column(6, plotOutput("all_stations")),
                    column(6, plotOutput("top15_stations_barchart")),
                    style = "padding-top:20px" 
                  ),
           fluidRow(column(6, plotlyOutput("all_food_categories")),
                    column(6, plotOutput("top20_food_categories_barchart")),
                    style = "padding-top:20px" 
                  )
          ),
  tabPanel(title = "Price and Rating",
           
           "Wanna have a sense of how dinner prices and ratings are 
           distributed in your favorite food category? Does a higher
           price range necessarily imply a better rating score? 
           Start here to find out.",
           
           # This tab is dependent on the food category selected by user,
           # but independent of the rating threshold set by user
           selectInput('food_category', 'Food Category', top.category$Var1),
           fluidRow(column(12, "Once you've selected a desired food cateogory, we'll find, 
              from the top 15 areas (stations), all restaurants serving that 
              food category. The following sets of plots are all based on restaurants
              in these 15 areas.")),
           br(),
           br(),
           fluidRow(column(12, "In the first set of plots, we plot the ",
                           strong("dinner price "),
              "distribution by station. The vertical axis displays the 
              dinner price range and the horizontal axis shows the number 
              of restaurants in a particular price range. The bars are also color-coded
              with lighter colors corresponding to lower price ranges and thicker
              colors corresponding to higher price ranges.")),
           br(),
           fluidRow(column(12, plotOutput("dinner_price_hist"))),
           br(),
           fluidRow(column(12, "In the second set of plots, we plot the ",
                                     strong("dinner rating"),
              " distribution by station. The horizontal axis displays the 
              dinner rating and the vertical axis shows the number of restaurants
              in a particular (narrow) range of dinner ratings (each little range 
              is called a \"bin\", and the number of restaurants associated with a 
              particular bin is found by counting the number of restaurants 
              with dinner ratings that fall in that bin. The blue dashed line
              represents the average dinner rating for that area.")),
           br(),
           fluidRow(column(12, plotOutput("dinner_rating_hist"))),
           br(),
           fluidRow(column(12, "Now that you have had a glimpse into the distributions
              of dinner prices and dinner ratings, a natural question comes to mind:
              is there any correlation between dinner prices and dinner ratings?
              That is, are higher prices asscociated with higher ratings, and vice versa?
              Putting it more concretely, is that highly rated restaurant you've longed 
              to try out a pricy one as well?
              We certainly had this doubt! In the following graph, we plot ",
                           strong("dinner rating"), "vs", strong("dinner price"),
              "based on all of the top 15 stations (we could plot the relationships per 
              station, but that would result in too few points (restaurants) per station, 
              which does not make interpretations particularly meaningful). Also added 
              is a best linear fit (in",
              
              span("blue", style="color:blue; margin-right: -4px;"),
              
              ") to the data and the error 
              bands (in",
              
              span("grey", style="color:grey; margin-right: -4px;"),
              
              "), which reflects our undertainty about the true relationship
              between price and rating inferred from limited and random data on the 
              available dinner ratings.")),
           br(),
           fluidRow(column(10, offset=1, plotOutput("dinner_price_rating"))),
           br(),
           fluidRow(column(12, "We'll leave it to you to explore whether there is a
                positive/negative relationship for any of the food categories. What we must
                note is that due to outliers and possibly dubious model assumptions,
                some of the linear fits might not represent the relationship in all 
                of the restaurants, and the error bands can be quite wide in some
                cases. Looking at the raw data points can be a better bet when that 
                happens! Also, keep in mind that there are often enough variations 
                in the data that flout the naive linear relationship: for example,
                even when the blue line suggests a positive relationship (that is, 
                on average, you need to pay more for a higher rating), still 
                listen to the data and notice how they deviate from the line; 
                you can often find a restaurant with a high rating at a low price 
                despite what the blue linear fit says!"))
         ),
  tabPanel(title = "Find Top-rated Restaurants in YOUR Food Category!",
           
           # This tab is dependent on both the food category and the rating threshold
           # set by user
           
           fluidRow(column(12, "In this tab, you can choose your favorite food category
                and enter a minimum acceptable dinner rating, and we will find the 
                qualified restaurants from the top 15 areas (stations) for you. 
                The histogram below shows the distribution
                of dinner ratings in your chosen food category based on restaurants in
                all of the top 15 areas (as opposed to one histogram for each area as
                in the previous tab), and the pink region highlights the chosen 
                restaurants.")),
                           
           selectInput('food_category_rank', 'Food Category', top.category$Var1),
           
           numericInput('rating_threshold',
                        'What would be the minimum acceptable dinner rating for
                        the food category you have selected?' , 3,
                        min = 0, max = 5),
           actionButton("update_rating_threshold", "Update Rating Threshold"),
           
           fluidRow(plotOutput("dinner_rating_highlight_hist")),
           br(),
           fluidRow(column(12, "These restaurants are then ranked below according
                to their dinner ratings. Choose a suitable minimum rating threshold 
                that is not so low that makes the plot crowded, and not so high that 
                no restaurant makes the cut!")),
           fluidRow(plotOutput("top.UserFoodRestaurants.rankRatings")),
           br(),
           fluidRow(column(12, "We take these top restaurants just found and locate
                  their nearby station. We then count the occurrence of each station
                  and rank them based on the number of top restaurants scattered 
                  around it. We only present the top 8 stations. The resulting plot
                  will give you a rough sense about what stations are the best bets 
                  to accidentally come across a top restaurant!")),
           fluidRow(plotOutput("top.UserFoodRestaurants.rankStations")),
           br(),
           fluidRow(column(12, "Finally, we've marked those top restaurants on the 
                      map. Here's to some tasty adventures!",
                           emo::ji('food'), emo::ji('food'), emo::ji('food'),)),
           fluidRow(leafletOutput("top.UserFoodRestaurants.map")) 
           )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Tab 1
    output$all_restaurant_map <- renderLeaflet({all_restaurant_map})
    
    # Tab 2
    output$all_stations <- renderPlot({all_stations})
    output$top15_stations_barchart <- renderPlot({top15_stations_barchart})
    output$all_food_categories <- renderPlotly({all_food_categories})
    output$top20_food_categories_barchart <- renderPlot({top20_food_categories_barchart})
    
    
    # Tab 3
    # the use of reactive() is probably unnecessary here
    user_food_category <-  reactive({input$food_category})
  
    
    UserFoodRestaurants <- reactive({
        kyoto.top.areas %>%
        filter(first_category == user_food_category() |
                   second_category == user_food_category()) %>% 
        group_by(station) %>%
            mutate(AverageDinerRating = mean(dinner_rating, na.rm = TRUE)) %>%
        ungroup() %>% 
        mutate(DinnerApproxPrice =
                   case_when(DinnerPrice == "JPY1000~JPY1999" ~ 1500,
                             DinnerPrice == "JPY2000~JPY2999" ~ 2500,
                             DinnerPrice == "JPY3000~JPY3999" ~ 3500,
                             DinnerPrice == "JPY4000~JPY4999" ~ 4500,
                             DinnerPrice == "JPY5000~JPY5999" ~ 5500,
                             DinnerPrice == "JPY6000~JPY7999" ~ 7000,
                             DinnerPrice == "JPY8000~JPY9999" ~ 9000,
                             DinnerPrice == "JPY10000~JPY14999" ~ 12500,
                             DinnerPrice == "JPY15000~JPY19999" ~ 17500,
                             DinnerPrice == "JPY20000~JPY29999" ~ 25000,
                             DinnerPrice == "JPY30000~" ~ 35000)
            )
    })
        
    output$dinner_price_hist <- renderPlot(
        {

        UserFoodRestaurants() %>%
            ggplot(aes(DinnerPrice, fill = DinnerPrice)) +
                geom_histogram(stat = "count") +
                facet_wrap(~station, scales = "free") +
                theme() +
                labs(title = paste("Dinner Price Distributions for ", user_food_category(),
                " in the 15 Most Popular Areas"),
                     y = "", x = "Average Dinner Price") +
                # scale_fill_discrete(name = "Average Price for 1 person")
                scale_fill_discrete_sequential("OrYel") +
            coord_flip()
        })
       
    
    output$dinner_rating_hist <- 
        renderPlot(
            {ggplot(data = UserFoodRestaurants()) +
                geom_histogram(aes(dinner_rating, fill = station), show.legend = FALSE) +
                geom_vline(aes(xintercept = AverageDinerRating), color = "blue",
                           linetype = "longdash", size = 1.3) +
                facet_wrap(~station, scales = "free") +
                labs(title = paste("Dinner Rating Distributions for ", user_food_category(),
                                   " in the 15 Most Popular Areas"),
                     y = "", x = "Average Dinenr Rating") 
            })

       
    output$dinner_price_rating <- renderPlot(
        {  UserFoodRestaurants() %>%
             ggplot(aes(x = DinnerApproxPrice, y = dinner_rating)) +
                geom_point() +
                geom_smooth(method = "lm") +
                # facet_wrap(~station, scales="free") + # too few points to facet wrap
                labs(title = paste("Dinner Rating vs Dinner Price for the \"",
                                       user_food_category(),
                                         "\" food category in the 15 Most Popular Areas"),
                     x = "Average Price for 1 person", y = "Rating Score")
        })
    
    
    # Tab 4
    
    user_food_category_rank <-  reactive({input$food_category_rank})
    
    
    UserFoodRestaurants_to_rank <- reactive({
      kyoto.top.areas %>%
        filter(first_category == user_food_category_rank() |
                 second_category == user_food_category_rank()) %>% 
        group_by(station) %>%
        mutate(AverageDinerRating = mean(dinner_rating, na.rm = TRUE)) %>%
        ungroup() %>% 
        mutate(DinnerApproxPrice =
                 case_when(DinnerPrice == "JPY1000~JPY1999" ~ 1500,
                           DinnerPrice == "JPY2000~JPY2999" ~ 2500,
                           DinnerPrice == "JPY3000~JPY3999" ~ 3500,
                           DinnerPrice == "JPY4000~JPY4999" ~ 4500,
                           DinnerPrice == "JPY5000~JPY5999" ~ 5500,
                           DinnerPrice == "JPY6000~JPY7999" ~ 7000,
                           DinnerPrice == "JPY8000~JPY9999" ~ 9000,
                           DinnerPrice == "JPY10000~JPY14999" ~ 12500,
                           DinnerPrice == "JPY15000~JPY19999" ~ 17500,
                           DinnerPrice == "JPY20000~JPY29999" ~ 25000,
                           DinnerPrice == "JPY30000~" ~ 35000)
        )
    })  
    
    
  rating_threshold <- eventReactive(input$update_rating_threshold, 
                                    {input$rating_threshold}, 
                                    ignoreNULL=F)  # want a default initial value

  ## --------------------------------------------------------------------------------------------
    output$dinner_rating_highlight_hist <- renderPlot(
        {  UserFoodRestaurants_to_rank() %>%
             ggplot() +
                geom_histogram(aes(x = dinner_rating, y=..density..),
                               position = "identity", fill = "blue", alpha = .9) +
                geom_density(aes(x = dinner_rating, y=..density..),
                             fill = "skyblue", alpha = .7) +
                annotate("rect",xmin=rating_threshold(),xmax=Inf,ymin=0,ymax= Inf,
                         alpha=.4,fill="red") +
                geom_vline(xintercept = rating_threshold()) +
                # geom_hline(yintercept = 3.28) +
                labs(y = "", x = "Rating Score of Dinner",
                     title = "Targeting only Top Restaurants")
        })


    
  # we make sure that user's changes in the rating threshold
  #  (as well as in the food category) are reactively reflected 
  #  in the plots in real time
    top.UserFoodRestaurants <- reactive({
        UserFoodRestaurants_to_rank() %>%
              filter(dinner_rating >= rating_threshold()) %>%
              arrange(desc(dinner_rating), desc(review_num))
 
    })
        
                
    
      output$top.UserFoodRestaurants.rankRatings <- renderPlot(
          {
          
            top.UserFoodRestaurants() %>%
              ggplot(aes(x = reorder(name, dinner_rating),
                         y = dinner_rating,
                         label = dinner_rating,
                         fill = DinnerApproxPrice)) +
                geom_bar(stat = "identity") +
                geom_text(angle = 0, nudge_y = 0.1, label.size=0.15) +
                scale_fill_gradient(low = "white", high = "red",
                                    name = "Average Price") +
                coord_flip() +
                labs(title = "The Most Popular Restaurants in Kyoto",
                     x = "",
                     y = "Rating Score") +
                theme(axis.text.y = element_text(size = 15))
        })

      output$top.UserFoodRestaurants.rankStations <- renderPlot(
          {
          
            data.frame(table(top.UserFoodRestaurants()$station)) %>%
                 arrange(desc(Freq)) %>%
                 head(8) %>%
            ggplot(aes(x = reorder(Var1, Freq),
                       y = Freq,
                       label = Var1,
                       fill = Var1)) +
                geom_bar(stat = "identity", show.legend = FALSE) +
                geom_text(position = position_stack(vjust = 0.5),
                          show.legend = FALSE, size = 5, hjust=0) +
                coord_flip() +
                labs(x ="", y = "Count") +
                labs(title = "Most Popular Areas") +
                theme(axis.text.y = element_blank())
       
     })

        # ## --------------------------------------------------------------------------------------------
        # kyoto %>% arrange(desc(dinner_rating))

  
## --------------------------------------------------------------------------------------------
      output$top.UserFoodRestaurants.map <- renderLeaflet(
        {
          
         top.UserFoodRestaurants() %>%
              leaflet() %>%
              addProviderTiles("Esri.WorldStreetMap") %>%
              #addProviderTiles("Stamen.TonerLite") %>%
              addMarkers(~long, ~lat,
                         popup = ~paste0("Name: ", "<b>", japanese_name, "</b>",
                       "</br>Food Category 1: ", first_category,
                       "</br>Food Category 2: ", second_category,
                       "</br>Rating: ", dinner_rating,
                       "</br>Dinner Price: ", dinner_price))
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
