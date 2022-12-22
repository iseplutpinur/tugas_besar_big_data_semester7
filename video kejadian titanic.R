# kejadian tragedi titanic
library(shiny)
xy <- c(784,479) #output of grDevices::dev.size("px")
url <- "https://www.youtube.com/watch?v=9xoqXVjBEF8" #copy yt link here
url <- gsub("watch\\?v=","embed/",url)
ui <- fluidPage(
  HTML(paste0('<iframe width="',xy[1],
              '" height="',
              xy[2],
              '" src="',
              url,
              '" frameborder="0"></iframe>')
  )
)
server <- function(input, output, session) {
}
# tampilkan video
# runGadget(shinyApp(ui, server,options=c("launch.browser"=FALSE,"port"=1111)),port=1111,viewer = paneViewer())

