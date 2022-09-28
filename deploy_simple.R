library(connectapi)

file.copy(from = "./docs/articles/logo.png", 
          to = "./docs/reference/logo.png")

client <- connect(server = Sys.getenv("CONNECT_SERVER"),
                  api_key = Sys.getenv("CONNECT_API_KEY"))

bundle <- bundle_dir("./docs")

content <- client %>% 
  deploy(bundle, name = "wqReport") %>% 
  poll_task()

