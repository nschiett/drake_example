

plan <- drake_plan(
  #load data
  beers = read.csv("data/beers.csv"),
  ratings = read.csv("data/beer_ratings.csv")
)
