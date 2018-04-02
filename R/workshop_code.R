
library(tidyverse)

## an untidy example
df1 <- data.frame(names = c("Adrienne", "Alex"),
                  favorite_food = c("burritos", "pizza"),
                  favorite_color = c("yellow", "turquoise"))
df1

## Tidy data example

df2 <- reshape2::melt(df1, id.vars = "names")
df2

# Example data
ufo <- read_csv("../data/ufo_dat.csv")
ufo1 <- read.csv("../data/ufo_dat.csv")

## `read_csv()` vs `read.csv()`
system.time(ufo <- read_csv("../data/ufo_dat.csv"))
system.time(ufo1 <- read.csv("../data/ufo_dat.csv"))

## Rename columns
ufo <- ufo %>% 
  rename("duration_sec" = `duration (seconds)`)

# ufo <- rename(ufo, "duration_sec" = `duration (seconds)`)

## Create a new column: 
ufo <- ufo %>%
  mutate(duration_min = duration_sec/60) %>%
  mutate(duration_hr = duration_min/60)

head(ufo[, c("duration_sec", "duration_min", "duration_hr")])

## Subset your data: 
ufo %>% 
filter(duration_hr > 10) %>%
select(datetime, duration_sec, duration_hr, `duration (hours/min)`) %>%
arrange(desc(duration_hr)) %>%
head()


comment <- ufo %>% 
filter(duration_sec > 90000000) %>% 
select(comments)
print(comment$comments)

#Challenge: Get only the observations shorter than a duration of your choice. Save the results to a new data frame called `short_sight`. 


## Do calculations on subsets:
n_per_state <- ufo %>%
group_by(state) %>% 
count(sort = T)

head(n_per_state)

## Example `for` loops
states <- unique(ufo$state)

answer_df <- data.frame(state = states, n = NA)

for(i in 1:length(states)){
  temporary_data <- ufo %>% filter(state == states[i])
  answer_df$n[i] <- nrow(temporary_data)
}
head(answer_df)

## Example `apply` function

states <- unique(ufo$state)

count_obs <- function(state_id){
nrow(ufo %>% filter(state == state_id))
}

counts <- lapply(states, count_obs)

## Example `map` function

states <- unique(ufo$state)

count_obs <- function(state_id){
nrow(ufo %>% filter(state == state_id))
}

ans_list <- map(states, count_obs)
ans_vect <- map_int(states, count_obs)

## More with `map`
head(mtcars)
models <- mtcars %>% 
split(.$cyl) %>% 
map(function(df) lm(mpg ~ wt, data = df))

## What did we get? 
summary(models[[1]])

## Extract coefficients
names(models[[1]])

## Extract coeffients
map(models, "coefficients")

## Merge data from multiple sources: `join()`
?economics
head(economics)

## `join()`
library(lubridate)

ufo <- ufo %>% 
separate(datetime, into = c("date", "time"), sep = " ") %>%
mutate(date = mdy(date)) %>% 
mutate(year = year(date))

## `join()`
ufo %>% 
  select(date, time, year) %>% 
  head() 

## `join()`
ufo_counts <- ufo %>% 
group_by(year) %>%
count()

head(ufo_counts)

## `join()`
econ_summary <- economics %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise_all(mean) %>%
  ungroup() %>%
  select(-date)

## At last: `join()`
df_new <- left_join(ufo_counts, econ_summary, by = "year") %>% 
  filter(year >= min(econ_summary$year)) 
head(df_new)

## Why join?
ggplot(df_new, aes(x = unemploy, y = n)) + 
  geom_point()

## Wide to long: `melt`
library(reshape2)
df_long <- df_new %>% 
  melt(id.vars = c("year", "n"))

head(df_long)

## Wide to long: `melt`
ggplot(df_long, aes(x = value, y = n)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(~variable, scales = "free_x")

## Text data: `stringr` functions
colors <- c(" red ", " orange ", " yellow ", " green ", " blue ", " purple ", " pink ", " white ", " black ")

ufo_colors <- ufo %>% 
  mutate(color_ans = str_detect(comments, colors)) %>% 
  filter(color_ans == T)

## Look at some comments
ufo_colors$comments[1:10]

## Extract the colors
colors_ans <- map(colors, function(color){str_extract(ufo_colors$comments, color)})

colors_df <- as.data.frame(colors_ans) 
colors_df <- colors_df %>%
  unite("colors_included", 1:ncol(colors_df), sep = ";") %>%
  mutate(colors_included = str_replace_all(colors_included, "NA;", "")) %>%
  mutate(colors_included = str_replace_all(colors_included, ";NA", ""))

head(colors_df)

## Add colors to original data
ufo_colors <- ufo_colors %>% mutate(colors = colors_df$colors_included)

## Put it together to answer a question
library(tidytext)
colors_df <- ufo_colors %>% 
  unnest_tokens(output = colors, input = colors, 
                token = stringr::str_split, pattern = ";") %>%
  mutate(colors = str_replace_all(colors, " ", ""))

colors_df %>% 
  select(date, colors) %>%
  head() 

## Most common UFO colors
colors_df %>% 
  group_by(colors) %>% 
  count(sort = T) %>% 
  ggplot(aes(x = colors, y = n)) + 
  geom_col() + 
  coord_flip()

## Is there a relationship between color and shape? 
## First, we should just get the 10 most common shapes.

## Challenge: How would you do that? 

## Most common shapes
shapes <- colors_df %>% 
  filter(!is.na(shape)) %>%
  group_by(shape) %>%
  count(sort = TRUE) %>%
  ungroup() %>%
  slice(1:10)

## Keep only data with shapes in the top 10.

## Challenge: How would you do that? 

shape_colors <- colors_df %>%
filter(shape %in% shapes$shape)

## Relationship between shape and color: 

ggplot(shape_colors, aes(x = shape, fill = colors)) + 
geom_bar(position = "fill")

