library(connectapi)

rsconnect::writeManifest(appDir = "./public")

file.copy(from = "./public/articles/logo.png",
          to = "./public/reference/logo.png")

client <- connect(server = Sys.getenv("CONNECT_SERVER"),
                  api_key = Sys.getenv("CONNECT_API_KEY"))

bundle <- bundle_dir("./public")

content <- client %>%
  deploy(bundle, name = "dataRetrieval") %>%
  poll_task()