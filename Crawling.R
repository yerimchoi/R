# Test commit
# Load library
pkgs <- c("rvest", "dplyr", "httr", "RSelenium", "XML")
sapply(pkgs, require, character.only = T)

# Login
# 아래 local chrome 코드에서 에러가 뜨면:
# 1. 다음 사이트 참조하여 chromedocker, geckodocker 및 selenium 설치, 한 폴더 안에 생성
#    https://sancj.tistory.com/62

# 2. cmd 관리자 모드로 실행, 다음 코드 입력 (selenium-server-standalone 버전 확인 필요)
#    cd C:\r_selenium
#    java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445

# 3. cmd 창을 아래로 내려놓고 다시 코드 실행

remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L,
                      browserName = "chrome")
Sys.sleep(5)
remDr$open()
remDr$navigate('http://www.kpi.or.kr/www/price/detail.asp?CATE_CD=10307514')

# ID와 패스워드 입력 
txt_id <- remDr$findElement(using="name", value="user_id") 
txt_pw <- remDr$findElement(using="name", value="user_pw")
login_btn <- remDr$findElement(using="xpath", value="//*[@id=\"mem_info_out\"]/input[3]")

txt_id$setElementAttribute("value", "***")
txt_pw$setElementAttribute("value", "***")
login_btn$clickElement()

# 한글 페이지 오류 -> 로케일 언어를 영어로 설정
Sys.setlocale('LC_ALL', 'English')

# 데이터 불러오기
webpage <- remDr$getPageSource()[[1]]
webpage <- read_html(webpage)

web_table <- rvest::html_nodes(x=webpage, xpath='//*[@id="table_all"]/table[3]/tbody/tr[5]/td/table')

table <- rvest::html_table(web_table)[[1]]
View(table)
