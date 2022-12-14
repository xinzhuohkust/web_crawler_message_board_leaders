require("pacman")

p_load(magrittr, sentimentr, rsvg, stm, ggthemes, SnowballC, stopwords, pluralize, stm, ldatuning, stopwords, quanteda, tm, textclean, stringr, reticulate, purrr, furrr, progress, tictoc, lubridate, progress, rio, tidyverse, RSelenium, rvest, foreach, tryCatchLog, stringi, cld2, tidytext, quanteda, tidyfst, httr, tictoc, miceadds, sandwich, urltools) 

driver<- rsDriver(port = 4446L, browser=c("firefox"))
remDr <- driver[["client"]]
remDr$open()

remDr$getWindowHandles() -> windows
remDr$switchToWindow(windows[[1]])

c(555, 943, 947, 949, 951, 953, 957, 959, 961, 963, 965, 967, 969, 971, 973, 977, 979) -> leader
c(leader, leader + 1) -> leader

sprintf("http://liuyan.people.com.cn/threads/list?checkStatus=0&fid=%s", leader) -> leaders
prase <- function(x){x$getElementText() %>% 
    unlist()}

shanghai_id <- tibble()
for (leader in leaders) {
remDr$navigate(leader)
containers <- tibble()
for (h in 2:4) {
  remDr$findElements(using = "xpath", value = "//*[contains(@role, 'tab')]") -> tab
  tab[[h]]$clickElement()
  
  button <- list()
  i = 0
  repeat {
  i = i + 1
  tryCatch(
    remDr$findElement(using = "xpath", value = "//*[contains(@class, 'mord')]") -> button[[i]], 
    error = function(e){button[[i]] <<- "error!"})
  if (!is_character(button[[i]])) {button[[i]]$clickElement()}
  Sys.sleep(sample(2, 1) + 0.5) 
  if (i > 30){if(sum(button[c((i - 30)):i] == "error!") >= 10){break}}}
  
  remDr$findElements(using = "xpath", value = "//*[contains(@class, 't-mr1')]") -> id
  remDr$findElements(using = "xpath", value = "//*[contains(@class, 'state')]") -> status
  remDr$findElements(using = "xpath", value = "//*[contains(@class, 'domain')]") -> label
  tibble(id = id, status = status, label = label, source = h, leader) %>% 
    mutate(id = map_chr(id, prase),
           status = map_chr(status, prase),
           label = map_chr(label, prase)) -> container
  bind_rows(containers, container) -> containers
}
bind_rows(containers, shanghai_id) -> shanghai_id
}

dir.create("D:\\OneDrive - HKUST Connect\\haibin\\?????????")
setwd("D:\\OneDrive - HKUST Connect\\haibin\\?????????")

saveRDS(shanghai_id, file = "??????_id.Rdata", compress = F)

shanghai_id %>% 
  distinct(id, .keep_all = T) -> shanghai_id
  
remDr$getWindowHandles() -> handles
remDr$switchToWindow(handles[[1]])


shanghai_id %>% 
  mutate(link = map_chr(id, ~ str_extract(., "\\d+")) |> sprintf(fmt = "http://liuyan.people.com.cn/threads/content?tid=%s")) -> shanghai_id

input <- shanghai_id$link

get_contents <- function(x) {
  
  remDr$navigate(x)
  
  Sys.sleep(sample(2,1) + 1.5)
  
  remDr$getPageSource()[[1]] -> web 
  
  remDr$getCurrentUrl() %>% 
    unlist() -> link
  
  tibble(web, link) -> contents
  
  return(contents)
}

remDr$navigate("http://liuyan.people.com.cn/threads/content?tid=9659442")


contents <- parsel::parscrape(scrape_fun = get_contents,
                              scrape_input = input,
                              cores = 6,
                              packages = c("RSelenium","rvest", "tidyverse"),
                              browser = "firefox",
                              scrape_tries = 3)

saveRDS(contents, file = "??????_contents.Rdata", compress = F)

contents$scraped_results %>% 
  bind_rows() -> contents



parse <- function(x){

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'replyObject')]") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[1]/div/div[1]/div[2]/h1") %>% 
  html_text(trim = T) -> ??????

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'typeName')]") %>% 
  html_text(trim = T) -> ??????

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'domain')]") %>% 
  html_text(trim = T) -> ??????

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'state')]") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[1]/div/div[1]/ul/li[2]/span[2]") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'replyContent')]") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[2]/div/div[1]/div/div[2]/div/h4") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'handleContent')]") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[2]/div/div[1]/div/div[2]/div/div") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'satisfied')]") %>% 
  html_text(trim = T) -> ????????????

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'evaluateContent')]") %>% 
  html_text(trim = T) -> ????????????


x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[2]/div/div[2]/div/div/div[1]/h3/div/p") %>% 
  html_text(trim = T) -> ????????????

tibble(????????????, ??????, ??????, ????????????, ??????, ????????????, ????????????, ????????????, ????????????, ????????????, ????????????, ????????????, ????????????) -> board

print(board)
}

plan(multisession, workers = 16)
future:::ClusterRegistry("stop")

tic()
contents %>% 
  mutate(???????????? = future_map(.progress = T, web, parse)) -> contents
toc()

contents %>% 
  unnest(????????????) %>% 
  dplyr::select(-web) %>% 
  mutate(???????????? = map_chr(????????????, ~ str_remove_all(., "???????????????"))) %>% 
  rename(?????? = link) -> share

rio::export(share, file = "????????????????????????.xlsx")

