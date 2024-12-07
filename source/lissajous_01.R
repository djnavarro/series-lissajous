
# set up ------------------------------------------------------------------

name <- "lissajous"
version <- 01

# define common helper functions & core tools
source(here::here("source", "common.R"), echo = FALSE)
source(here::here("source", "core.R"), echo = FALSE)
source(here::here("source", "bezier.R"), echo = FALSE)

make_art <- function(seed, name, version) {

  # specify the output path and message the user
  output <- output_path(name, version, seed, format = "png")
  message("generating art at ", output)

  # data frame with one row per twist
  set.seed(seed)
  palettes <- list(
    # readr::read_csv(
    #   here::here("source", "palette_02.csv"),
    #   show_col_types = FALSE
    # ),
    # readr::read_csv(
    #   here::here("source", "palette_01.csv"),
    #   col_select = tidyselect::starts_with("col"),
    #   show_col_types = FALSE
    # ),
    readr::read_csv(
      here::here("source", "palette_03.csv"),
      show_col_types = FALSE
    )
  ) |>
    dplyr::bind_rows()

  row <- sample.int(nrow(palettes), 1)
  palette <- unlist(palettes[row, ])
  palette <- sample(palette)
  n_ribbons <- 500L
  angle <- runif(1, min = 0, max = 2*pi)
  pull_1 <- runif(1, min = -.1, max = .1)
  pull_2 <- runif(1, min = 0, max = .2)
  x_mid <- runif(1, min = -2, max = 2)
  y_mid <- runif(1, min = -2, max = 2)
  width_scale <- runif(1, min = .5, max = 1.5)
  max_arc <- runif(1, min = pi/18, max = pi/6)

  A <- 1
  B <- 1
  delta <- pi/2
  a <- 5
  b <- 4

  values <- tibble::tibble(

    theta_1 = runif(n_ribbons, min = 0, max = 2 * pi),
    theta_2 = theta_1 + runif(n_ribbons, min = 0, max = max_arc),
    x = A * sin(a * theta_1 + delta),
    y = B * sin(b * theta_1),
    xend = A * sin(a * theta_2 + delta),
    yend = B * sin(b * theta_2),
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

  values <- dplyr::select(values, -theta_1, -theta_2)

  # list of things to draw
  drawables <- purrr::pmap(values, bezier_ribbon)

  # create a sketch from the ribbons and then draw the sketch
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
    draw(xlim = c(-2, 2), ylim = c(-2, 2))
  dev.off()

}

for(s in 1301:1400) make_art(s, name, version)

