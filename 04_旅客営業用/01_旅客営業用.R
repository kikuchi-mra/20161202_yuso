
rm(list=ls(all=TRUE))
invisible(replicate(4,gc()))

wd <- "//vanilla/16-51100999_自動車輸送/07_PJ実行/04_旅客営業用/01_R"
setwd(wd)
source("rvlookup.R")

###############################################################
#母集団車両数
inp0 <- "inp/02 【本調査】旅客営業用乗用_母集団補助変量(H24.6-H27.3).csv"
dat0 <- read.csv(inp0,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")

cname0 <- c("調査票種別コード","車種コード","ブロック15コード","調査年","調査月",
            "補助変量_走行距離","補助変量_走行距離_乗算_最大積載量",
            "件数","登録者ID","登録日時","調査年月日")
colnames(dat0) <- cname0

#15ブロック：9運輸局の対応テーブル
code.list <- read.csv("inp/15ブロック別地域コード表.csv",sep=",",header=T,check.names=F,stringsAsFactors=F,colClasses="character")
dat0$運輸局コード <- rvlookup(dat0$ブロック15コード,code.list,2)

pop.dat <- aggregate(cbind(保有車両数=as.numeric(dat0$件数)),
                     by=list(車種番号=dat0$車種コード,調査年=dat0$調査年,調査月=dat0$調査月,運輸局コード=dat0$運輸局コード),sum,na.rm=T)
pop.dat <- pop.dat[order(pop.dat$運輸局コード),]
pop.dat <- pop.dat[order(pop.dat$調査月),]
pop.dat <- pop.dat[order(pop.dat$調査年),]

write.table(pop.dat,"out/02_調査月別・運輸局別・保有車両数.csv",sep=",",quote=F,na="",row.names=F,col.names=T)


###############################################################

#読込
inp1 <- "inp/02 【本調査】旅客営業用乗用_調査票_その１(H24.6-H27.3).csv"
inp2 <- "inp/02 【本調査】旅客営業用乗用_調査票_その２(H24.6-H27.3).csv"
dat1 <- read.csv(inp1,sep=",",header=T,check.names=F,stringsAsFactors=F,colClasses="character")
dat2 <- read.csv(inp2,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr1  <- nrow(dat1)
Nr2  <- nrow(dat2)

cname1 <- colnames(dat1)
cname2 <- c("調査票種別コード","車種番号","運輸支局コード","サンプル番号","調査年","調査月",
            "レコードヘッダ","トリップ番号","乗車月","乗車日","乗車場所_都道府県コード","降車月","降車日",
            "降車場所_都道府県コード","走行距離","輸送人員","輸送回数","備考","分割番号","データステータス","調査票入力ステータス")
colnames(dat2) <- cname2

######################################################################################
#対象データの抽出
tar1a <- (dat1$調査票ステータス==0)
tar1b <- (dat1$調査票入力ステータス==0)|(dat1$調査票入力ステータス==1)
tar1  <- tar1a & tar1b
dat1 　<- subset(dat1,tar1)
Nr1-nrow(dat1)

tar2b <- (dat2$備考=="0")|(dat2$備考=="")
tar2c <- (dat2$データステータス=="0")|(dat2$データステータス=="5")
tar2d <- (dat2$調査票入力ステータス=="0")|(dat2$調査票入力ステータス=="1")
tar2e <- as.numeric(dat2$走行距離)>0         #走行距離>0
tar2f <- as.numeric(dat2$重量)>0            #重量>0
tar2  <- tar2b & tar2c & tar2d     ##& tar2e & tar2f
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
#dat1$車種番号   <- formatC(as.numeric(dat1$車種番号),width=2,flag="0")

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

#車種(06:タクシー)
type1 <- dat1$車種番号
type2 <- dat2$車種番号

#調査月
year   <- dat2$調査年
month  <- dat2$調査月
yymm   <- paste(year,month,sep="")

#走行距離,重量,輸送回数
dist <- as.numeric(dat2$走行距離)
weig <- as.numeric(dat2$輸送人員)
freq <- as.numeric(dat2$輸送回数)
tk   <- weig*dist

######################################################################################

#key,車種,調査月ごとに集計
tmp <- aggregate(cbind(dist,weig,tk),by=list(key=key2,type=type2,year=year,month=month,yymm=yymm),sum,na.rm=T)

#車両（事業者）ごとの調査日数をマッチング
term.dat <- data.frame(key1,dat1[,c("調査日数","休車日数")],stringsAsFactors=F)
term     <- as.numeric(rvlookup(tmp$key,term.dat,2))
rest     <- as.numeric(rvlookup(tmp$key,term.dat,3))
tmp$term <- term
tmp$rest <- rest

#調査月ごとの総日数
get.date <- function(year,month){
  date   <- ifelse((month=="01")|(month=="03")|(month=="05")|(month=="07")|(month=="08")|(month=="10")|(month=="12"),31,NA)
  date   <- ifelse((month=="04")|(month=="06")|(month=="09")|(month=="11"),30,date)
  date   <- ifelse((month=="02")&(year!="2012"),28,date)
  date   <- ifelse((month=="02")&(year=="2012"),29,date) #うるう年
  return(date)
}
date     <- get.date(tmp$year,tmp$month)
tmp$date <- date

#################################################
#1車ごとの原単位の平均値
unit1 <- tmp$dist/tmp$weig                        #1トン当たり平均輸送キロ
unit2 <- tmp$tk/(tmp$term-tmp$rest)  #実働1日1車当たり輸送トンキロ
unit3 <- tmp$weig/(tmp$term-tmp$rest)             #実働1日1車当たり輸送トン数
unit4 <- tmp$dist/(tmp$term-tmp$rest)             #実働1日1車当たり実車キロ

tmp2 <- aggregate(cbind("1トン当たり平均輸送キロ"=unit1,
                        "実働1日1車当たり輸送トンキロ"=unit2,
                        "実働1日1車当たり輸送トン数"=unit3,
                        "実働1日1車当たり実車キロ"=unit4),
                  by=list(type=tmp$type,year=tmp$year,month=tmp$month,yymm=tmp$yymm),mean,na.rm=T)
write.table(tmp2,"out/3_06タクシー原単位_1車単位.csv",sep=",",quote=F,row.names=F,col.names=T)



#自動車票１にひも付け
dat <- rvlookup(key1,tmp,2:ncol(tmp))
dat$dist[is.na(dat$dist)] <- 0
dat$weig[is.na(dat$weig)] <- 0
dat$tk[is.na(dat$tk)]     <- 0
if((nrow(dat)-nrow(dat1))!=0) stop("KEYに重複あり")

dat$type  <- type1
dat$year  <- dat1$調査年
dat$month <- dat1$調査月
dat$yymm  <- paste(dat$year,dat$month,sep="")


#調査日数・休車日数・月実働日数
dat$term  <- as.numeric(dat1$調査日数)
dat$date  <- get.date(dat$year,dat$month)  #調査月の日数
dat$rest  <- as.numeric(dat1$休車日数)
dat$mwd   <- (dat$term-dat$rest)*(dat$date/dat$term)  #1ヶ月の延実働日車数
#月換算
dat$mweig <- dat$weig*(dat$date/dat$term)
dat$mtk   <- dat$tk*(dat$date/dat$term)

#平均値（共分散用）
tmp2 <- aggregate(cbind(avw=dat$mweig,avtk=dat$mtk,avwd=dat$mwd),by=list(yymm=dat$yymm,type=dat$type),mean,na.rm=T)
dat  <- data.frame(dat,vlookup(dat,tmp2,c("yymm","type"),c("avw","avtk","avwd")),stringsAsFactors=F)

#共分散
dat$cvtk <- (dat$mtk-dat$avtk)*(dat$mweig-dat$avw)
dat$cvwd <- (dat$mtk-dat$avtk)*(dat$mwd-dat$avwd)


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

  N    <- tapply(rep(1,nrow(tmp)),factor(yymm),sum,na.rm=T)
  x    <- tmp$mtk   #月拡大輸送人キロ
  y    <- tmp$mweig #月拡大輸送人数
  w    <- tmp$mwd   #月延実働日車数

  sx   <- tapply(x,   factor(yymm),sum,na.rm=T)
  sy   <- tapply(y,   factor(yymm),sum,na.rm=T)
  sw   <- tapply(w,   factor(yymm),sum,na.rm=T)

  avx  <- tapply(x,   factor(yymm),mean)    #平均
  avy  <- tapply(y,   factor(yymm),mean)
  avw  <- tapply(w,   factor(yymm),mean)

  sdx  <- tapply(x,   factor(yymm),sd)      #不偏標準偏差
  sdy  <- tapply(y,   factor(yymm),sd)
  sdw  <- tapply(w,   factor(yymm),sd)

  #共分散
  cvy  <- tapply(tmp$cvtk,factor(yymm),sum,na.rm=T)
  cvw  <- tapply(tmp$cvwd,factor(yymm),sum,na.rm=T)

  res1 <- data.frame(N=N,
                     X=sx,avX=avx,sdX=sdx,Y=sy,avY=avy,sdY=sdy,cvY=cvy,
                     stringsAsFactors=F,check.names=F)
  res2 <- data.frame(N=N,
                     X=sx,avX=avx,sdX=sdx,W=sw,avW=avw,sdW=sdw,cvW=cvw,
                     stringsAsFactors=F,check.names=F)
  return(list(res1=res1,res2=res2))
}

#1トンあたり平均輸送キロ
options(scipen=100,digits=2)
R06 <- get.R(dat,t="06")

write.table(R06$res1,"out/01_06タクシー_1人当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R06$res2,"out/01_06タクシー_実働1日1車当たり輸送人キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)





