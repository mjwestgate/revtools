# draw a hex sticker for `revtools`
library(tibble)
library(dplyr)
library(string2path)
library(showtext)
library(sf)
library(ggplot2)
library(hexSticker)
library(viridisLite)
# library(viridis)
# remotes::install_github("johannesbjork/LaCroixColoR")
# library(LaCroixColoR)

# get 'synthesisr' text as a polygon
final_size <- 1.2
synth_line <- string2path("revtools",
                          font = "inst/hex/Space_Mono/SpaceMono-Bold.ttf") |>
  tibble::rowid_to_column() |>
  tibble() |>
  mutate(x = x - min(x), y = y - min(y)) |> # place both mins at 0
  mutate(y = y / max(x), x = x / max(x)) |> # now at x = c(0, 1)
  mutate(x = (x * final_size) - (final_size * 0.5), y = y * final_size) |> # scale to required size
  mutate(y = y - (max(y) * 0.5)) # |> # centre vertically
  # mutate(x = x + 0.1) #  right-most x value at 0.7

# convert to `sf` object to allow calculation of spatial properties
text_polygons <- synth_line %>%
  st_as_sf(coords = c("x", "y")) |>
  group_by(path_id) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON")

# need to clip out inside of letters
words <- bind_rows(
  text_polygons[1, ],
  st_difference(text_polygons[2, ], text_polygons[3, ]),
  text_polygons[c(4, 5), ],
  st_difference(text_polygons[6, ], text_polygons[7, ]),
  st_difference(text_polygons[8, ], text_polygons[9, ]),
  text_polygons[c(10, 11), ]
) |>
  select(path_id, geometry)

## check
# ggplot(words) + geom_sf(fill = "blue", color = "white")

# clean up
rm(final_size, synth_line, text_polygons)

# now create hexagons
# from hexSticker, but using sf objects
create_hexagon <- function(scale = 1){
  hexd <- data.frame(x = 1+c(rep(-sqrt(3)/2, 2), 0, rep(sqrt(3)/2, 2), 0),
                     y = 1+c(0.5, -0.5, -1, -0.5, 0.5, 1))
  rbind(hexd, hexd[1, ]) |>
    tibble() |>
    mutate(x = (x - 1) * scale,
           y = (y - 1) * scale) |>
    st_as_sf(coords = c("x", "y")) |>
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
}

external_hexagon <- create_hexagon(scale = 1.00)
internal_hexagon <- create_hexagon(scale = 0.935)


# extract all vertices in `words` or `internal_hexagon` to coordinates tibble
#polygon_points <- words |>
#  select(geometry) |>
#  bind_rows(internal_hexagon) |>
#  st_cast("POINT") |>
#  st_coordinates() |>
#  as_tibble()

# create an origin point
origin <- tibble(x = -0.935, # mean(c(-0.935, -0.5)),
                 y = -0.935) |>
  st_as_sf(coords = c("x", "y"))

# make a quarter circle with, say 500 points
n <- 1000 # number of points
k <- 3 # radius
alpha <- 0 # <- (-90 - (180 / n)) * (pi/180)
theta <- (2 * (pi / 4 / n) * seq(0, (n - 1))) - alpha
segment_df <- tibble(theta = theta,
		                 x = k * cos(theta) - 1,
		                 y = k * sin(theta) - 1)

edge_color <- "#000000"

### Looks bad - try with lines instead

line_list <- vector(mode = "list", length = nrow(segment_df))
for(i in 1:nrow(segment_df)){
  result <- segment_df[i, ] |>
    st_as_sf(coords = c("x", "y")) |>
    bind_rows(origin) |>
    select(geometry) |>
    summarize() |>
    st_cast("LINESTRING") |>
    mutate(id = i)

  # clip segment by hexagon
  line_list[[i]] <- st_intersection(result, internal_hexagon)
}
line_df <- line_list |> bind_rows()

# cut letters from background
joint_lines <- st_difference(
  line_df, 
  st_sf(st_combine(words))) |>
  st_cast("MULTILINESTRING") |>
  mutate(full_length = st_length(line_df),
         word_length = st_length(joint_lines)) |>
  st_cast("LINESTRING")

# area added for disply purposes at this point
final_lines <- mutate(joint_lines, 
  source_id = id,
  final_id = row_number(),
  length = st_length(joint_lines),
  distance_from_origin = st_distance(origin, joint_lines)[1, ]) |>
  select(-id)

# next: calculate which polygons are 1. split by letters and 2. 'after' those letters.
# messy code as summarize() isn't working for me today
source_ids_preword <- purrr::map(
  split(final_lines, final_lines$source_id), 
  \(a){a$final_id[which.min(a$distance_from_origin)]}) |>
  unlist()
final_lines$pre_word <- final_lines$final_id %in% source_ids_preword

# scaling
# background colors
midline <- 500
mid_levels <- abs(final_lines$source_id - midline)
scaled_levels <- (mid_levels / max(mid_levels)) * 0.8
# shadow
in_shadow <- 1 - as.numeric(final_lines$pre_word)
amount_shadow <- 1 - (final_lines$word_length / final_lines$full_length)
shadow_levels <- (in_shadow * 1.6) * (amount_shadow * 1.4)
# combine
final_lines$color_levels <- scaled_levels + shadow_levels

# colors
simple_palette <- viridisLite::magma(n = 8, begin = 0.15, end = 0.90) |> rev()

# font
font_add("spacemono", "inst/hex/Space_Mono/SpaceMono-Regular.ttf")
showtext_auto()

p <- ggplot() +
  geom_sf(data = external_hexagon, fill = "white", color = NA) +
  geom_sf(data = final_lines,
          aes(color = color_levels),
          linewidth = 0.1) +
  geom_sf(data = internal_hexagon, fill = NA, color = edge_color, linewidth = 0.1) +
  geom_sf(data = words, fill = "white", color = edge_color, linewidth = 0.1) +
  scale_color_gradientn(colors = simple_palette) +
  annotate(geom = "text",
           x = 0.7,
           y = -0.19,
           label = "mjwestgate",
           family = "spacemono",
           size = 8,
           hjust = 1,
           color = "#ffffff") +
  theme_void() + 
  theme(legend.position = "none")

ggsave("man/figures/logo.png",
       p,
       width = 43.9,
       height = 50.8,
       units = "mm",
       bg = "transparent",
       dpi = 600)


### END

## POLYGONS CODE

# loop to create segments from pairs of lines, rather than just single lines
polygon_list <- vector(mode = "list", length = nrow(segment_df))
for(i in 2:nrow(segment_df)){
  result <- segment_df[c(i-1, i), ] |>
    st_as_sf(coords = c("x", "y")) |>
    bind_rows(origin) |>
    select(geometry) |>
    summarize() |>
    st_cast("POLYGON") |>
    mutate(id = i)

  # clip segment by hexagon
  polygon_list[[i]] <- st_intersection(result, internal_hexagon)
}
polygon_df <- polygon_list |> bind_rows()

# cut letters from background
joint_polygons <- st_difference(
  polygon_df,
  # {st_combine(polygon_df) |> st_sf()}, 
  st_sf(st_combine(words))) |>
  st_cast("MULTIPOLYGON") |>
  # mutate(source_polygon = id) |>
  st_cast("POLYGON")

# area added for disply purposes at this point
final_polygons <- mutate(joint_polygons, 
  source_id = id,
  final_id = row_number(),
  area = st_area(joint_polygons),
  distance_from_origin = st_distance(origin, joint_polygons)[1, ]) |>
  select(-id)

# next: calculate which polygons are 1. split by letters and 2. 'after' those letters.
# messy code as summarize() isn't working for me today
source_ids_preword <- purrr::map(
  split(final_polygons, final_polygons$source_id), 
  \(a){a$final_id[which.min(a$distance_from_origin)]}) |>
  unlist()
final_polygons$pre_word <- final_polygons$final_id %in% source_ids_preword

# scaling
# background colors
mid_polygon <- 12
mid_levels <- abs(final_polygons$source_id - mid_polygon)
scaled_levels <- (mid_levels / max(mid_levels)) * 0.8
# shadow
shadow_levels <- (1 - as.numeric(final_polygons$pre_word)) * 1.6
# combine
final_polygons$color_levels <- scaled_levels + shadow_levels
# final_polygons$color_levels <- scaled_levels

# plot
ggplot() +
  geom_sf(data = external_hexagon, fill = "white", color = NA) +
  geom_sf(data = final_polygons,
          aes(fill = color_levels, 
              color = color_levels),
          linewidth = 10,
          color = NA) +
  geom_sf(data = internal_hexagon, fill = NA, color = edge_color, linewidth = 0.1) +
  geom_sf(data = words, fill = "white", color = edge_color, linewidth = 0.1) +
  scale_fill_gradientn(colors = simple_palette) +
  scale_color_gradientn(colors = simple_palette) +
  # scale_fill_viridis_d() +
  # scale_alpha(range = c(0.5, 1)) +
  theme_void() + 
  theme(legend.position = "none")



## OLD
# ggplot(segment_df, aes(x = x, y= y)) + geom_point() + coord_cartesian()
segment_lines <- purrr::map(
  split(segment_df, seq_len(nrow(segment_df))),
  \(a){
    # create a single line from origin to edge
    line_df <- a |>
      st_as_sf(coords = c("x", "y")) |>
      bind_rows(origin) |>
      summarise(geometry = st_combine(geometry)) |>
      st_cast("LINESTRING")

    # split line by words
    word_difference <- st_difference(line_df, words)

    # ggplot(word_difference) + geom_sf() +
    #   geom_sf(data = words, fill = "white", color = edge_color, linewidth = 0.1)
    
    # work out cumulative interruption
    st_length(word_difference$geometry)

    word_intersect <- st_intersection(line_df, words) 
    if(nrow(word_intersect) > 0){
      result <- line_df |>
        mutate(length = sum(st_length(word_intersect$geometry))) |>
        st_difference(words)
    }else{
      result <- line_df |> mutate(length = 0)
    }
    st_intersection(result, internal_hexagon)  
  }) |>
  bind_rows()

# then crop to inner polygon







# below old

x_vec <- seq(-0.87, 0.87, by = 0.005)
result_internal <- lapply(x_vec, function(a){
  b <- data.frame(x = a, y = c(-1, 1)) |>
    st_as_sf(coords = c("x", "y")) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("LINESTRING") |>
    st_intersection(words)

  tibble(x = a, length = sum(st_length(b)))
}) |>
  bind_rows()

result_external <- lapply(x_vec, function(a){
  b <- data.frame(x = a, y = c(-1, 1)) |>
    st_as_sf(coords = c("x", "y")) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("LINESTRING") |>
    st_intersection(internal_hexagon)

  b |> mutate(x = a)
}) |>
  bind_rows()

# merge
background_lines <- left_join(result_external,
                              result_internal,
                              by = "x")

# clean up
rm(x_vec)



edge_color <- "#000000" # "#b951c9"
# palette <- lacroix_palette("CranRaspberry", n = 15, type = "continuous") |>
#   as.character()

# example colors:
# x <- lacroix_palette("CranRaspberry", n = 7, type = "continuous") |> as.character()
simple_palette <- c("#c92029",
                    "#a3086a",
                    "#6c159e",
                    "#0a238a")

p <- ggplot() +
  geom_sf(data = external_hexagon, fill = "white", color = NA) +
  geom_sf(data = background_lines,
          mapping = aes(
            color = x,
            alpha = (length ^ 1.2)),
          linewidth = 0.3) +
  geom_sf(data = internal_hexagon, fill = NA, color = edge_color, linewidth = 0.1) +
  geom_sf(data = words, fill = "white", color = edge_color, linewidth = 0.1) +
  geom_sf(data = segment_lines) +
  annotate(geom = "text",
           x = 0.7,
           y = -0.19,
           label = "mjwestgate",
           family = "spacemono",
           size = 8,
           hjust = 1,
           color = "#ffffff") +
  # geom_vline(xintercept = 0.35) +
  scale_colour_gradientn(colors = simple_palette) +
  # scale_color_viridis(option = "H") +
  scale_alpha(range = c(0.5, 1)) +
  # scale_color_gradient(low = "#800194", high = "#b951c9") +
  theme_void() +
  theme(legend.position = "none")

ggsave("man/figures/logo.png",
       p,
       width = 43.9,
       height = 50.8,
       units = "mm",
       bg = "transparent",
       dpi = 600)
