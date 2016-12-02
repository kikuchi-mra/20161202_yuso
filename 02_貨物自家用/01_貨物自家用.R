
rm(list=ls(all=TRUE))
invisible(replicate(4,gc()))

wd <- "//vanilla/16-51100999_自動車輸送/07_PJ実行/02_貨物自家用/01_R"
setwd(wd)
source("rvlookup.R")

#読込
inp1 <- "inp/01 【本調査】貨物自家用_調査票_その１(H24.6-H27.3).csv"
inp2 <- "inp/01 【本調査】貨物自家用_調査票_その２(H24.6-H27.3).csv"
dat1 <- read.csv(inp1,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
dat2 <- read.csv(inp2,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr1  <- nrow(dat1)
Nr2  <- nrow(dat2)

cname1 <- c("調査票種別コード","車種番号","運輸支局コード","サンプル番号","調査年","調査月",
            "調査開始月","調査開始日","調査終了月","調査終了日","調査日数",
            "調査する自動車１","調査する自動車２","調査する自動車３","調査する自動車４",
            "最大積載量","種類","レコードヘッダ","調査開始時メーター値","調査終了時メーター値",
            "調査期間中の走行距離","休車日数","分割番号","調査票ステータス","調査票入力ステータス",
            "初度登録年月","登録年月日","受験年月日","走行距離","旧受験年月日","旧走行距離")
colnames(dat1) <- cname1

cname2 <- c("調査票種別コード","車種番号","運輸支局コード","サンプル番号","調査年","調査月",
            "レコードヘッダ","トリップ番号","積込月","積込日","積込場所_都道府県コード","取卸月","取卸日",
            "取卸場所_都道府県コード","走行距離","輸送貨物品目コード","重量","キロ表記不可_品名_数量_単位",
            "輸送回数","備考","分割番号","データステータス","調査票入力ステータス")
colnames(dat2) <- cname2

######################################################################################
#対象データの抽出
tar1a <- (dat1$調査票ステータス==0)
tar1b <- (dat1$調査票入力ステータス==0)|(dat1$調査票入力ステータス==1)
tar1  <- tar1a & tar1b
dat1 　<- subset(dat1,tar1)
Nr1-nrow(dat1)

tar2a <- (dat2$キロ表記不可_品名_数量_単位!="1")
tar2b <- (dat2$備考=="0")|(dat2$備考=="")
tar2c <- (dat2$データステータス=="0")|(dat2$データステータス=="1")|(dat2$データステータス=="5")
tar2d <- (dat2$調査票入力ステータス=="0")|(dat2$調査票入力ステータス=="1")
tar2e <- as.numeric(dat2$走行距離)>0         #走行距離>0
tar2f <- as.numeric(dat2$重量)>0            #重量>0
tar2  <- tar2a & tar2b & tar2c & tar2d     ##& tar2e & tar2f
dat2 　<- subset(dat2,tar2)
Nr2-nrow(dat2)

# cbind(tar2a,tar2b,tar2c,tar2d,tar2e,tar2f)[!tar2e,]

######################################################################################

#key作成用フィールド
col1 <- c("車種番号","運輸支局コード","サンプル番号","調査年","調査月")
col2 <- c("車種番号","運輸支局コード","サンプル番号","調査年","調査月")

#文字数チェック
# apply(dat1[,col1],2,function(x) unique(nchar(x)))
# apply(dat2[,col2],2,function(x) unique(nchar(x)))

#フォーマット修正
dat1$車種番号   <- formatC(as.numeric(dat1$車種番号),width=2,flag="0")
dat1$サンプル番号 <- formatC(as.numeric(dat1$サンプル番号),width=3,flag="0")
dat1$調査月     <- formatC(as.numeric(dat1$調査月),width=2,flag="0")

######################################################################################
#key作成
key1 <- apply(dat1[,col1],1,paste,collapse="")
key2 <- apply(dat2[,col2],1,paste,collapse="")
# nrow(dat1) - sum(is.element(key1,key2))
# nrow(dat2) - sum(is.element(key2,key1))
# unique(key1[!is.element(key1,key2)])
# unique(key2[!is.element(key2,key1)])
# dat1[which(key1==key2[!tar2e]),]
# dat2[which(key2==key2[!tar2e]),]

#自家用車種(03:普通,04:ダンプ,05:小型,09:特種)
type1 <- dat1$車種番号
type2 <- dat2$車種番号

#調査月
year   <- dat2$調査年
month  <- dat2$調査月
yymm   <- paste(year,month,sep="")

#走行距離,重量,輸送回数
dist <- as.numeric(dat2$走行距離)
weig <- as.numeric(dat2$重量)
freq <- as.numeric(dat2$輸送回数)
tk   <- weig*dist
diff <- tk-weig

######################################################################################

#key,車種,調査月ごとに集計
tmp <- aggregate(cbind(dist,weig,tk,diff),by=list(key=key2,type=type2,year=year,month=month,yymm=yymm),sum,na.rm=T)

#車両（事業者）ごとの調査日数をマッチング
term.dat <- data.frame(key1,dat1$調査日数,stringsAsFactors=F)
term     <- as.numeric(rvlookup(tmp$key,term.dat,2))
tmp$term <- term

#調査月ごとの総日数
get.date <- function(x){
  year   <- x$year
  month  <- x$month
  date   <- ifelse((month=="01")|(month=="03")|(month=="05")|(month=="07")|(month=="08")|(month=="10")|(month=="12"),31,NA)
  date   <- ifelse((month=="04")|(month=="06")|(month=="09")|(month=="11"),30,date)
  date   <- ifelse((month=="02")&(year!="2012"),28,date)
  date   <- ifelse((month=="02")&(year=="2012"),29,date) #うるう年
  return(date)
}
date     <- get.date(tmp)
tmp$date <- date

#自動車票１にひも付け
dat <- rvlookup(key1,tmp,2:ncol(tmp))
if((nrow(dat)-nrow(dat1))!=0) stop("KEYに重複あり")

dat$type  <- type1
dat$year  <- dat1$調査年
dat$month <- dat1$調査月
dat$yymm  <- paste(dat$year,dat$month,sep="")
dat$term  <- as.numeric(dat1$調査日数)
dat$date  <- get.date(dat)  #調査月の日数
dat$rest  <- as.numeric(dat1$休車日数)

dat$dist[is.na(dat$dist)] <- 0
dat$weig[is.na(dat$weig)] <- 0
dat$tk[is.na(dat$tk)]     <- 0
dat$diff[is.na(dat$diff)] <- 0

###############################################################################
#標本標準偏差
sd.rev <- function(x){
  #x[x==0] <- NA
  n <- sum(!is.na(x))
  y <- sqrt( var(x,na.rm=T)*(n-1)/n )
  return(y)
}
mean.rev <- function(x){
  #x[x==0] <- NA
  y <- mean(x,na.rm=T)
  return(y)
}

#集計
get.R <- function(tmp,t){
  flg  <- tmp$type==t
  tmp  <- subset(tmp,flg)
  yymm <- tmp$yymm
  dist <- tmp$dist
  weig <- tmp$weig
  tk   <- tmp$tk
  diff <- tmp$diff

  term <- tmp$term
  date <- tmp$date
  rest <- tmp$rest
  wd   <- term-rest
  if(any(wd<0)) cat(" Warning : 実働日数がゼロより小さい車両がある")

  N    <- tapply(rep(1,nrow(tmp)),factor(yymm),sum,na.rm=T)
  x    <- tk*(date/term)/1000     #tk＝Σ(weig*dist)≠Σweig*Σdist
  y    <- weig*(date/term)/1000   #kg→トン換算
  w    <- wd*(date/term)

  sx   <- tapply(x,   factor(yymm),sum,na.rm=T)
  sy   <- tapply(y,   factor(yymm),sum,na.rm=T)
  sw   <- tapply(w,   factor(yymm),sum,na.rm=T)

  avx  <- tapply(x,   factor(yymm),mean)    #平均
  avy  <- tapply(y,   factor(yymm),mean)
  avw  <- tapply(w,   factor(yymm),mean)

  sdx  <- tapply(x,   factor(yymm),sd)      #不偏標準偏差
  sdy  <- tapply(y,   factor(yymm),sd)
  sdw  <- tapply(w,   factor(yymm),sd)

  res1 <- data.frame(N=N,
                     X=sx,avX=avx,sdX=sdx,Y=sy,avY=avy,sdY=sdy,
                     stringsAsFactors=F,check.names=F)
  res2 <- data.frame(N=N,
                     X=sx,avX=avx,sdX=sdx,W=sw,avW=avw,sdW=sdw,
                     stringsAsFactors=F,check.names=F)

  return(list(res1=res1,res2=res2))
}

#1トンあたり平均輸送キロ
options(scipen=100,digits=2)
R03 <- get.R(dat,t="03")
R04 <- get.R(dat,t="04")
R05 <- get.R(dat,t="05")
R09 <- get.R(dat,t="09")

write.table(R03$res1,"out/1_03普通車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R04$res1,"out/2_04ダンプ車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R05$res1,"out/3_05小型車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R09$res1,"out/4_09特種車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)

write.table(R03$res2,"out/1_03普通車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R04$res2,"out/2_04ダンプ車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R05$res2,"out/3_05小型車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R09$res2,"out/4_09特種車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)





