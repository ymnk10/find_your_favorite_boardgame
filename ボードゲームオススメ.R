library("tidyverse")
boardgame <- read_csv("bgg-15m-reviews.csv")

boardgame %>% 
  group_by(name) %>% 
  summarise(•]‰¿=n()) -> boardgame1

boardgame1 %>% 
  filter(•]‰¿ >= 8000) -> boardgame2

boardgame %>% 
  group_by(user) %>% 
  summarise(•]‰¿=n()) -> boardgame3

boardgame3 %>% 
  filter(•]‰¿ >= 150) -> boardgame4

boardgame %>% 
  select(user, rating, name) -> boardgame5

boardgame5 %>% 
  semi_join(boardgame2, by="name") %>% 
  semi_join(boardgame4, by="user") %>% 
  pivot_wider(names_from = name ,
              values_from=rating , values_fn=mean) %>% 
  select(!user)-> user_rating_mat

write_csv(user_rating_mat, "user_rating_mat.csv")

library("magrittr")
user_rating_mat %<>% 
  mutate(across(everything(),
                ~ replace_na(.x, 0)))
user_rating_mat %>% as.matrix() -> urmat

me <- c(10, 9, 8, 0, 7, 7, 0, 4, 9, 0, 8, 7)

me.vect <- rep(0, ncol(urmat))
me.item <- c("Catan", "Ticket to Ride", "Agricola", "Splendor",
             "Power Grid","Citadels", "Patchwork", "Munchkin",
             "Bohnanza","Hanabi","Jaipur", "Magic: The Gathering")
me.loc <- match(me.item, names(user_rating_mat))
for(i in 1:length(me)){
  me.vect[me.loc[i]] <- me[i]
}

library("lsa")
n <- nrow(urmat)
ruijido <- rep(0, n)
for(i in 1:n){
  ruijido[i] <- cosine(me.vect , as.vector(urmat[i,]))
}

user_rating_mat %<>% tibble(ruijido) 

user_rating_mat %>% top_n(10, ruijido) ->upper
upper %>% select(!ruijido) %>% 
  as.matrix() -> upper.rating
upper %>% pull(ruijido) -> upper.ruijido

tibble(name=me.item, rating=me) %>% 
  filter(rating!=0) -> notrecommend

upper.ruijido %>% sum() -> sum.ruijido
upper.rating %>% t() %*%
  upper.ruijido / sum.ruijido -> yosoku


yosoku %>% pred.V1=yosoku[,1] %>%
  arrange(desc(pred.V1)) %>%
  anti_join(notrecommend , by="name") %>%
  filter(pred.rating >=5) %>%
  left_join(boardgame, by="name") -> recommend.boardgame
