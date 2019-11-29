

get_country <- function(location){
  mapdata <- map_data("world") 
  regions <- c(as.character(unique(mapdata$region)), "United States", "United kingdom")
  
  strex <- sapply(regions, function(x){
    t <- str_extract(as.character(location), x)
    return(t)
  })
  
  country <- lapply(1:nrow(strex), function(x){
    c <- regions[which(!is.na(strex[x,]))]
    if(length(c) == 0){
      return(NA)}
    else if (length(c) == 1){
      return(c)
    }else{
        return(NA)
      }
  }) %>% unlist()
  
  return(country)
}

combine <- function(beers, ratings, countries){
  dplyr::left_join(beers, ratings) %>% 
    mutate(region = countries) %>%
    filter(!is.na(region))
}

summarize_region <- function(data){
  sum <- data %>% group_by(region) %>% 
    summarize(rating = median(rating), abv = median(abv), ibu = median(ibu), nbeers = length(style))
}


get_worldmap <- function(data){
  
  mapdata <- map_data("world") %>% mutate(region = case_when(region == "USA" ~ "United States",
                                                             TRUE ~ region))
  beerdata_sp <- inner_join(data, mapdata)
  ggplot(mapdata) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = "lightgrey", color = "black") +
    geom_polygon(aes(x = long, y = lat, group = group, fill = rating), color = "black", data = beerdata_sp) +
    coord_quickmap() +
    scale_fill_fish(option = "Trimma_lantana", direction = -1, trans = "log") +
    theme_bw()
}

regplot <-function(beerdata, model){
fitted <- fitted(model)
ggplot(beerdata) +
  geom_point(aes(x = abv, y = rating, color = ibu), alpha = 0.6, size = 1) +
  geom_smooth(aes(x = abv, y = fitted), se = FALSE, color = "black", linetype = 2) +
  scale_color_fish(trans = "log") +
  theme_bw()
}

eu_top50 <- function(beerdata){
  sub <- filter(beerdata, !region %in% c("United States", "Canada", "Mexico", "Australia", "New Zealand",
                                         "Japan", "Brazil")) %>%
    arrange(desc(rating)) %>% top_n(n = 50, wt = rating)
  return(sub)
}










