
# set up ------------------------------------------------------------------

name <- "lissajous"
version <- 08

# define common helper functions & core tools
source(here::here("source", "common.R"), echo = FALSE)
source(here::here("source", "core.R"), echo = FALSE)
source(here::here("source", "bezier.R"), echo = FALSE)

make_art <- function(seed, name, version) {
  
  # specify the output path and message the user
  output <- output_path(name, version, seed, format = "png")
  message("generating art at ", output)
  
  set.seed(seed)
  
  # set up palettes
  palettes <- c("palette_01.csv", "palette_02.csv", "palette_02.csv") |> 
    purrr::map(
      \(x) here::here("source", x) |> 
        readr::read_csv(show_col_types = FALSE)
    ) |> 
    dplyr::bind_rows()
  
  # select a palette
  row <- sample.int(nrow(palettes), 1)
  palette <- unlist(palettes[row, ])
  palette <- sample(palette)
  
  # parameters that affect all bezier objects
  pull_1 <- runif(1, min = -.1, max = .1) * .75
  pull_2 <- runif(1, min = 0, max = .2) * .75
  x_mid <- runif(1, min = -2, max = 2)
  y_mid <- runif(1, min = -2, max = 2)
  width_scale <- runif(1, min = 1, max = 2)
  max_arc <- runif(1, min = 1, max = 2) * pi/96
  
  get_curl <- function(x, y, seed = NULL) {
    ambient::curl_noise(
      generator = ambient::fracture,
      noise = ambient::gen_simplex,
      fractal = ambient::fbm,
      x = x,
      y = y,
      frequency = 1,
      seed = seed,
      octaves = 5
    )
  }

  n_ribbons <- 2400L
  
  base <- tibble::tibble(
    x = runif(n_ribbons, min = -2, max = 2),
    y = runif(n_ribbons, min = -2, max = 2)
  )
  curl <- get_curl(base$x, base$y, seed) |> 
    dplyr::mutate(
      x = ambient::normalize(x, to = c(-1, 1)),
      y = ambient::normalize(y, to = c(-1, 1))
    )
  
  values <- tibble::tibble(
    x = base$x,
    y = base$y,
    xend = x + curl$x,
    yend = y + curl$y,
    xctr_1 = (1 - pull_1) * (x + xend)/2 + pull_1 * x_mid,
    yctr_1 = (1 - pull_1) * (y + yend)/2 + pull_1 * y_mid,
    xctr_2 = (x + xend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
    yctr_2 = (y + yend) / 2 + pull_2 * runif(n_ribbons, min = -2, max = 2),
    width = width_scale * runif(n_ribbons, min = .01, max = .2),
    smooth = 6L,
    n = 100L,
    fill = sample(palette, n_ribbons, replace = TRUE),
    color = fill
  )

  # list of things to draw
  drawables <- purrr::pmap(values, bezier_ribbon)
  
  # draw sketch and save it
  r <- 2
  seed_str <- stringr::str_pad(seed, width = 4, pad = "0")
  png(
    filename = output,
    width = 2000,
    height = 2000,
    units = "px",
    bg = palette[1]
  )
  drawables |>
    sketch() |>
    draw(xlim = c(-r, r), ylim = c(-r, r))
  dev.off()
  
}

for(s in 800:899) make_art(s, name, version)

