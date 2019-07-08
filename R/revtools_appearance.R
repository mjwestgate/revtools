# add custom css
revtools_css <- function(){
  shiny::tag("style", shiny::HTML("
    .navbar-custom-menu {
      width: 800px;
    }
    .navbar-nav {
      width: 800px;
    }
    .content-wrapper,
      .right-side {
        background-color: #e2e2e2;
      }
      .skin-black .main-header .logo {
        background-color: #27114D;
        color: #ffffff;
      }
      .skin-black .main-header .logo:hover {
        background-color: #27114D;
      }
      .skin-black .main-header .navbar {
        background-color: #afafaf;
      }
      .dropdown {
        font-size: 120%;
        padding-right: 20px;
        padding-top: 12px;
      }
      .action-button {
        color: #fff;
        background: #4a3384;
        border-width: 0px;
      }
      .action-button:hover {
        color: #fff;
        background: black;
        border-width: 0px;
      }
      .irs-bar {
        background: #4a3384;
        border: #4a3384;
      }
      .irs-bar-edge {
        background: #4a3384;
        border: #4a3384;
      }
      .irs-grid-text {
        color: white;
      }
      .irs-grid-pol{
        background: white;
      }
      .irs-single {
        color: white;
        background: #4a3384;
      }
      .irs-from {
        color: white;
        background: #4a3384;
      }
      .irs-to {
        color: white;
        background: #4a3384;
      }
  "))
}

# add a logo with optional added text
revtools_logo <- function(text){
  graphics::par(
    mar = rep(0, 4),
    oma = rep(0, 4),
    bg = "#251256FF")
  graphics::plot(
    x = 1,
    y = 1,
    xlim = c(0, 1),
    ylim = c(0, 1),
    type = "n",
    ann = FALSE,
    axes = FALSE
  )
  graphics::rasterImage(logo,
    xleft = -0.04,
    ybottom = 0.905,
    xright = 1.04,
    ytop = 1.04)

  if(!missing(text)){
    graphics::text(
      x = 1.04,
      y = 0.93,
      pos = 2,
      label = text,
      col = "white",
      cex = 0.8
    )
  }
}