# Keep this file as a small entry point. The app logic lives in the
# neighbouring files so UI, server logic, and TWI processing can be edited
# independently.
app_dir <- if (file.exists("global.R")) "." else "app"

source(file.path(app_dir, "global.R"), local = TRUE, encoding = "UTF-8")
source(file.path(app_dir, "ui.R"), local = TRUE, encoding = "UTF-8")
source(file.path(app_dir, "server.R"), local = TRUE, encoding = "UTF-8")

shiny::shinyApp(ui = ui, server = server)
