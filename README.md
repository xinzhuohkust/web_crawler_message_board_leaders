```{r message=FALSE, warning=FALSE}

require("pacman") # package management

p_load(tidyverse, Rselenium, rvest, parsel, rio) 

# ID 

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

shanghai_id <- tibble() # get id of leaders in shanghai

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

shanghai_id %>% 
  distinct(id, .keep_all = T) -> shanghai_id # remove duplicates

# Contents 

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
} # multisessions

contents <- parsel::parscrape(scrape_fun = get_contents,
                              scrape_input = input,
                              cores = 16, # using 16 cpu cores
                              packages = c("RSelenium","rvest", "tidyverse"),
                              browser = "firefox",
                              scrape_tries = 3)
                              
contents$scraped_results %>% 
  bind_rows() -> contents

# Parse 

parse <- function(x){

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'replyObject')]") %>% 
  html_text(trim = T) -> 留言对象

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[1]/div/div[1]/div[2]/h1") %>% 
  html_text(trim = T) -> 标题

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'typeName')]") %>% 
  html_text(trim = T) -> 类别

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'domain')]") %>% 
  html_text(trim = T) -> 主题

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'state')]") %>% 
  html_text(trim = T) -> 回复状态

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[1]/div/div[1]/ul/li[2]/span[2]") %>% 
  html_text(trim = T) -> 提问时间

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'replyContent')]") %>% 
  html_text(trim = T) -> 提问内容

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[2]/div/div[1]/div/div[2]/div/h4") %>% 
  html_text(trim = T) -> 回复部门

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'handleContent')]") %>% 
  html_text(trim = T) -> 回复内容

x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[2]/div/div[1]/div/div[2]/div/div") %>% 
  html_text(trim = T) -> 回复时间

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'satisfied')]") %>% 
  html_text(trim = T) -> 用户评价

x %>% 
  read_html() %>% 
  html_element(xpath = "//*[contains(@class, 'evaluateContent')]") %>% 
  html_text(trim = T) -> 评价内容


x %>% 
  read_html() %>% 
  html_element(xpath = "/html/body/div[1]/div[2]/main/div/div/div[2]/div/div[2]/div/div[2]/div/div/div[1]/h3/div/p") %>% 
  html_text(trim = T) -> 评价时间

tibble(回复状态, 主题, 类别, 留言对象, 标题, 提问内容, 提问时间, 回复部门, 回复时间, 回复内容, 用户评价, 评价内容, 评价时间) -> board

print(board)
}

plan(multisession, workers = 16)
future:::ClusterRegistry("stop")

tic()
contents %>% 
  mutate(具体内容 = future_map(.progress = T, web, parse)) -> contents
toc()

contents %>% 
  unnest(具体内容) %>% 
  dplyr::select(-web) %>% 
  mutate(留言对象 = map_chr(留言对象, ~ str_remove_all(., "留言对象："))) %>% 
  rename(链接 = link) -> share

rio::export(share, file = "人民网留言板上海.xlsx")
```
