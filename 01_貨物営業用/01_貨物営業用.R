
rm(list=ls(all=TRUE))
invisible(replicate(4,gc()))

wd <- "//vanilla/16-51100999_自動車輸送/07_PJ実行/01_貨物営業用/01_R"
setwd(wd)
source("rvlookup.R")

#読込
inp1   <- "inp/05 【本調査（現行）】貨物営業用_自動車票_その１(H24.4～H27.3).csv"
inp2   <- "inp/05 【本調査（現行）】貨物営業用_自動車票_その２(H24.4～H27.3).csv"
dat1   <- read.csv(inp1,sep=",",header=T,check.names=F,stringsAsFactors=F,colClasses="character")
dat2   <- read.csv(inp2,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr1    <- nrow(dat1)
Nr2    <- nrow(dat2)
cname1 <- cbind(colnames(dat1))
cname2 <- c("調査票種別コード","層区分コード","運輸支局コード","サンプル番号","調査年","調査月",
            "調査する自動車１","調査する自動車２","調査する自動車３","調査する自動車４",
            "レコードヘッダ","トリップ番号","積込月","積込日","積込場所_都道府県コード",
            "取卸月","取卸日","取卸場所_都道府県コード","走行距離","輸送貨物品目コード","重量",
            "キロ表記不可_品名_数量_単位","輸送回数","備考","分割番号","データステータス","調査票入力ステータス")
colnames(dat2) <- cname2

######################################################################################
#対象データの抽出

tar1a <- (dat1$調査票ステータス==0)
tar1b <- (dat1$調査票入力ステータス==0)|(dat1$調査票入力ステータス==1)
tar1  <- tar1a & tar1b
dat1 　<- subset(dat1,tar1)
Nr1-nrow(dat1)

tar2a <- (dat2[,22]!="1")
tar2b <- (dat2[,24]=="0")|(dat2[,24]=="")
tar2c <- (dat2[,26]=="0")|(dat2[,26]=="1")    #|(dat2[,26]=="5")
tar2d <- (dat2[,27]=="0")|(dat2[,27]=="1")
tar2e <- as.numeric(dat2[,19])>0 #走行距離>0
tar2f <- as.numeric(dat2[,21])>0 #重量>0
tar2  <- tar2a & tar2b & tar2c & tar2d & tar2e & tar2f
dat2 　<- subset(dat2,tar2)
Nr2-nrow(dat2)

######################################################################################

#key作成用フィールド
col1a <- c("層区分コード","運輸支局コード","サンプル番号","事業所管理番号","営業所番号","調査年","調査月")
col1b <- c("層区分コード","運輸支局コード","サンプル番号","調査年","調査月",
           "調査する自動車1","調査する自動車2","調査する自動車3","調査する自動車4")
col2  <- c(2:10)

#文字数チェック
# apply(dat1[,col1a],2,function(x) unique(nchar(x)))
# apply(dat1[,col1b],2,function(x) unique(nchar(x)))
# apply(dat2[,col2], 2,function(x) unique(nchar(x)))

#フォーマット修正
dat1$営業所番号     <- formatC(as.numeric(dat1$営業所番号),width=4,flag="0")
dat1$調査する自動車2 <- formatC(as.numeric(dat1$調査する自動車2),width=3,flag="0")
dat2[,8]          <- formatC(as.numeric(dat2[,8]),width=3,flag="0") #調査する自動車2

######################################################################################

#key作成
key1a <- apply(dat1[,col1a],1,paste,collapse="")
key1b <- apply(dat1[,col1b],1,paste,collapse="")
key2  <- apply(dat2[,col2],1,paste,collapse="")
# nrow(dat1) - sum(is.element(key1b,key2))
# nrow(dat2) - sum(is.element(key2,key1b))
# unique(key1b[!is.element(key1b,key2)])
# unique(key2[!is.element(key2,key1b)])

#車種(01:普通,02:小型,08:特種,11:軽)
type1 <- substr(dat1$調査する自動車4,1,2)
type2 <- substr(dat2[,10],1,2)

#調査月
year   <- dat2[,5]
month  <- dat2[,6]
yymm   <- paste(year,month,sep="")
# sum((year=="2012")&(as.numeric(month)>=4))

#走行距離,重量,輸送回数
dist <- as.numeric(dat2[,19])
weig <- as.numeric(dat2[,21])
freq <- as.numeric(dat2[,23])
tk   <- weig*dist
diff <- tk-weig

#自家用貨物
#普通（ダンプあり、なし）
#小型
#特殊
#ダンプ

######################################################################################

#key,車種,調査月ごとに集計
tmp <- aggregate(cbind(dist,weig,tk,diff),by=list(key=key2,type=type2,year=year,month=month,yymm=yymm),sum,na.rm=T)

#車両（事業者）ごとの調査日数をマッチング
term.dat <- data.frame(key1b,dat1$調査日数,stringsAsFactors=F)
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
dat <- rvlookup(key1b,tmp,2:ncol(tmp))
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
R01 <- get.R(dat,t="01")
R02 <- get.R(dat,t="02")
R08 <- get.R(dat,t="08")
R11 <- get.R(dat,t="11")

write.table(R01$res1,"out/1_01普通車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R02$res1,"out/2_02小型車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R08$res1,"out/3_08特種車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R11$res1,"out/4_11軽自動車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)

write.table(R01$res2,"out/1_01普通車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R02$res2,"out/2_02小型車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R08$res2,"out/3_08特種車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R11$res2,"out/4_11軽自動車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)





