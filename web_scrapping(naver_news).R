library(rvest)

# popular news head ####################################
url_news <- "http://news.naver.com/main/ranking/popularDay.nhn?mid=etc&sid1=111&date=20170412"   # 웹 스크래핑을 할 url을 입력합니다.
news_data <- read_html(url_news)   # 입력된 url에서 html을 읽어옵니다.

popular_new_head <- data.frame(Policy=c(1:5), Economy=NA, Society=NA, Life=NA, World=NA, IT=NA, TV_Ent=NA, Sports=NA)   # 헤드라인 5개를 각 카테고리 별로 입력하는 데이터 프레임을 만듭니다.

for(i in 1:(nrow(popular_new_head))){   # 1~5면의 헤드에 대한 for문입니다.
  find_num <- paste0('.num',i)   # 헤드라인 마다 번호가 다르므로 번호를 바꿀 수 있도록 합니다. 
  num_head <- html_nodes(news_data, find_num)   # i가 1일 경우, num1에 해당하는 노드를 찾아냅니다. 
  
  for(j in 1:ncol(popular_new_head)){   # 정치 ~ 스포츠까지의 카테고리에 대한 for문입니다.
    head_text <- gsub("\t","",num_head[j] %>% html_text())   # j 번째 노드에서 text를 추출한 후에 탭(\t)을 제거합니다.
    head_text <- strsplit(head_text, "\r\n")[[1]]   # text를 "\r\n"으로 나눠줍니다. list로 반환됩니다. vector로 바꾸기 위해 [[1]]을 뒤에 붙여줍니다.
    if(i==1){
      popular_new_head[i,j] <- head_text[12]    # 첫 번재 카테고리인 정치의 경우는 12번째에 헤드라인이 들어있습니다. 이를 앞서 설정한 데이터 프레임에 넣어줍니다.
    }else{
      popular_new_head[i,j] <- head_text[5]   # 나머지 카테고리들은 5번째에 헤드라인이 들어있습니다. 이를 앞서 설정한 데이터 프레임에 넣어줍니다.
    }
  }
}

write.csv(popular_new_head,"popular_news_head.csv")   # csv 파일로 저장합니다. 디렉토리 설정을 안하면 문서 폴더 내에 저장됩니다.


# paper news 1page top head ####################################
paper_oid <- read.csv("paper_oid.csv", header = TRUE, colClasses = c("character", "character", "character"))   # 저장해놓은 oid가 포함된 csv를 읽어들입니다. oid를 숫자로 읽으면 앞에 0이 사라지므로 colClasses를 통해 column의 타입을 character로 읽어들입니다.
news_paper_top <- data.frame(Paper=paper_oid$Paper, A1_Top=NA)   # 언론사별로 1면의 헤드라인을 저장할 데이터 프레임을 만듭니다.

for(i in 1:nrow(news_paper_top)){
  url_news <- paste0("http://news.naver.com/main/list.nhn?oid=",paper_oid$oid[i],"&listType=paper&mid=sec&mode=LPOD&date=20170412")   # 웹 스크래핑을 할 url을 입력합니다.
  news_data <- read_html(url_news)   # 입력된 url에서 html을 읽어옵니다.
  A1_nodes <- html_nodes(news_data, '.type13.firstlist')   # "type13 firstlist"에 해당하는 노드를 찾아냅니다. 
  A1_1st_node <- A1_nodes[1] %>% html_text()   # 첫 번째 노드에서 text를 추출합니다.
  A1_1st_node <- gsub("\t","",A1_1st_node)   # text의 탭(\t)을 제거합니다.
  A1_1st_node <- gsub(" ","",A1_1st_node)   # text의 공란을 제거합니다.
  A1_head_list <- strsplit(A1_1st_node, "\n")[[1]]   # text를 엔터(\n)로 잘라냅니다. list로 반환됩니다. vector로 바꾸기 위해 [[1]]을 뒤에 붙여줍니다.
  A1_head_list <- A1_head_list[-which(A1_head_list=="")]   # 공란으로 된 character를 제외한 나머지를 추출합니다.
  news_paper_top[i,'A1_Top'] <- A1_head_list[1]   # vector의 첫 번째에 헤드라인이 들어있습니다. 이를 앞서 설정한 데이터 프레임에 넣어줍니다.
}

write.csv(news_paper_top,"news_paper_1page_top.csv")   # csv 파일로 저장합니다. 디렉토리 설정을 안하면 문서 폴더 내에 저장됩니다.


# advanced example ####################################
library(lubridate)

Date <- ymd("2017-04-12") - days(0:365)   # 2017-04-12로부터 1년 동안의 날짜를 모두 나타냅니다.
df_2016 <- data.frame(Date)   # 데이터 프레임으로 변환합니다.
df_2016_format <- format(df_2016$Date,"%Y%m%d")   #yyyy-mm-dd의 구조를 url에 적용할 수 있도록 yyyymmdd의 형태로 바꿔줍니다.

for (d in 1:length(df_2016_format)) {
  url_news <- paste0("http://news.naver.com/main/ranking/popularDay.nhn?mid=etc&sid1=111&date=",df_2016_format[d])   # 웹 스크래핑을 할 url을 입력합니다.
  news_data <- read_html(url_news)   # 입력된 url에서 html을 읽어옵니다.
  
  section_name <- c("Policy","Economy","Society","Life","World","IT","TV_Ent","Sports")   # 카테고리 명칭 작성
  popular_new_head <- data.frame(head_1=c(1:8), head_2=NA, head_3=NA, head_4=NA, head_5=NA)   # 헤드라인 5개를 각 카테고리 별로 입력하는 데이터 프레임
  rownames(popular_new_head) <- section_name   # row name을 카테고리 명칭으로 변경 (추출하는 csv에서 첫 번째 열에 나타납니다.)
  
  for(i in 1:(ncol(popular_new_head))){   # 1~5면의 헤드에 대한 for문입니다.
    find_num <- paste0('.num',i)   # 헤드라인 마다 번호가 다르므로 번호를 바꿀 수 있도록 합니다. 
    num_head <- html_nodes(news_data, find_num)   # i가 1일 경우, num1에 해당하는 노드를 찾아냅니다. 
    
    for(j in 1:nrow(popular_new_head)){   # 정치 ~ 스포츠까지의 카테고리에 대한 for문입니다.
      head_text <- gsub("\t","",num_head[j] %>% html_text())   # j 번째 노드에서 text를 추출한 후에 탭(\t)을 제거합니다.
      head_text <- strsplit(head_text, "\r\n")[[1]]   # text를 "\r\n"으로 나눠줍니다. list로 반환됩니다. vector로 바꾸기 위해 [[1]]을 뒤에 붙여줍니다.
      if(i==1){
        popular_new_head[j,i] <- head_text[12]    # 첫 번재 카테고리인 정치의 경우는 12번째에 헤드라인이 들어있습니다. 이를 앞서 설정한 데이터 프레임에 넣어줍니다.
      }else{
        popular_new_head[j,i] <- head_text[5]   # 나머지 카테고리들은 5번째에 헤드라인이 들어있습니다. 이를 앞서 설정한 데이터 프레임에 넣어줍니다.
      }
    }
  }
  write.csv(popular_new_head,paste0(df_2016_format[d],"_popular_news_head.csv"))   # 날짜별로 csv 파일로 저장합니다. 디렉토리 설정을 안하면 문서 폴더 내에 저장됩니다.
}

