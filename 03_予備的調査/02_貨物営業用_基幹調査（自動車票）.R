
rm(list=ls(all=TRUE))
invisible(replicate(4,gc()))

wd <- "//vanilla/16-51100999_自動車輸送/07_PJ実行/03_予備的調査"
setwd(wd)
source("rvlookup.R")

#読込
inp1   <- "inp/06 【本調査（現行）】貨物営業用_自動車票_その１(201509-11).csv"
inp2   <- "inp/06 【本調査（現行）】貨物営業用_自動車票_その２(201509-11).csv"
dat1   <- read.csv(inp1,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character",na.string=c("","NULL"))
dat2   <- read.csv(inp2,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character",na.string=c("","NULL"))
Nr1    <- nrow(dat1)
Nr2    <- nrow(dat2)

cname1 <- c("調査票種別コード","層区分コード","運輸支局コード","サンプル番号",
            "事業所管理番号","管轄支局コード","営業所番号","調査年","調査月",
            "調査開始月","調査開始日","調査終了月","調査終了日","調査日数",
            "調査する自動車1","調査する自動車2","調査する自動車3","調査する自動車4",
            "最大積載量","レコードヘッダ","主な用途","調査開始時メーター値","調査終了時メーター値",
            "調査期間中の走行距離","休車日数","分割番号","調査票ステータス","調査票入力ステータス")

cname2 <- c("調査票種別コード","層区分コード","運輸支局コード","サンプル番号","調査年","調査月",
            "調査する自動車1","調査する自動車2","調査する自動車3","調査する自動車4",
            "レコードヘッダ","トリップ番号","積込月","積込日","積込場所_都道府県コード",
            "取卸月","取卸日","取卸場所_都道府県コード","走行距離","輸送貨物品目コード","重量",
            "キロ表記不可_品名_数量_単位","輸送回数","備考","分割番号","データステータス","調査票入力ステータス")

colnames(dat1) <- cname1
colnames(dat2) <- cname2

######################################################################################
#対象データの抽出
tar1a <- (dat1$調査票ステータス=="0")
tar1b <- (dat1$調査票入力ステータス=="0")|(dat1$調査票入力ステータス=="1")
tar1  <- tar1a & tar1b
dat1 　<- subset(dat1,tar1)
Nr1-nrow(dat1)

tar2a <- (dat2[,22]!="1")
tar2b <- (dat2[,24]=="0")|(dat2[,24]=="")
tar2c <- (dat2[,26]=="0")|(dat2[,26]=="1")|(dat2[,26]=="5")
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
col2  <- colnames(dat2)[2:10]

#文字数チェック
# apply(dat1[,col1a],2,function(x) unique(nchar(x)))
# apply(dat1[,col1b],2,function(x) unique(nchar(x)))
# apply(dat2[,col2], 2,function(x) unique(nchar(x)))

#フォーマット修正
dat1$サンプル番号     <- formatC(as.numeric(dat1$サンプル番号),width=3,flag="0",digits=50)
dat1$事業所管理番号  <- formatC(as.numeric(dat1$事業所管理番号),width=12,flag="0",digits=50)
dat1$営業所番号     <- formatC(as.numeric(dat1$営業所番号),width=4,flag="0",digits=50)
dat1$調査月        <- formatC(as.numeric(dat1$調査月),width=2,flag="0",digits=50)
dat1$調査する自動車2 <- formatC(as.numeric(dat1$調査する自動車2),width=3,flag="0",digits=50)
dat1$調査する自動車4 <- formatC(as.numeric(dat1$調査する自動車4),width=4,flag="0",digits=50)

dat2$サンプル番号     <- formatC(as.numeric(dat2$サンプル番号),width=3,flag="0",digits=50)
dat2$調査月        <- formatC(as.numeric(dat2$調査月),width=2,flag="0",digits=50)
dat2$調査する自動車2 <- formatC(as.numeric(dat2$調査する自動車2),width=3,flag="0",digits=50)
dat2$調査する自動車4 <- formatC(as.numeric(dat2$調査する自動車4),width=4,flag="0",digits=50)

#最大積載量
max.weig <- round(as.numeric(dat1$最大積載量)/1000,3)

#最大積載量クラス
rnk <- ifelse(max.weig <3,"01",NA)
rnk <- ifelse((max.weig >=3)  &(max.weig <6.5),"02",rnk)
rnk <- ifelse((max.weig >=6.5)&(max.weig <11) ,"03",rnk)
rnk <- ifelse((max.weig >=11) &(max.weig <16) ,"04",rnk)
rnk <- ifelse((max.weig >=16),"05",rnk)
if(any(is.na(rnk))) stop("最大積載量にNAあり")
dat1$積載量クラス <- rnk


######################################################################################

#key作成
key1a <- apply(dat1[,col1a],1,paste,collapse="")
key1b <- apply(dat1[,col1b],1,paste,collapse="")
key2  <- apply(dat2[,col2],1,paste,collapse="")
# nrow(dat1) - sum(is.element(key1b,key2))
# nrow(dat2) - sum(is.element(key2,key1b))
# sort(unique(key1b[!is.element(key1b,key2)]))
# sort(unique(key2[!is.element(key2,key1b)]))

#車種(01:普通,02:小型,08:特種,11:軽)
type1 <- substr(dat1$調査する自動車4,1,2)
type2 <- substr(dat2$調査する自動車4,1,2)

#調査月
year   <- dat2$調査年
month  <- dat2$調査月
yymm   <- paste(year,month,sep="")

#走行距離,重量,輸送回数,トンキロ,実働日数
dist <- as.numeric(dat2$走行距離)    #km
weig <- as.numeric(dat2$重量)/1000  #kg→トン
freq <- as.numeric(dat2$輸送回数)
tk   <- weig*dist                  #トンキロ

#積載量クラス
rnk.dat <- data.frame(key1b,dat1$積載量クラス,stringsAsFactors=F)
rnk2 <- rvlookup(key2,rnk.dat,2)

#自家用貨物
#普通（ダンプあり、なし）
#小型
#特殊
#ダンプ

#####################################################################################

#key,車種,調査月ごとに集計
tmp <- aggregate(cbind(dist,weig,tk),by=list(key=key2,type=type2,class=rnk2,year=year,month=month,yymm=yymm),sum,na.rm=T)
if( any(duplicated(tmp$key)) ) stop("tmpに重複keyが発生")

#車両（事業者）ごとの調査日数をマッチング
term.dat <- data.frame(key1b,dat1[,c("調査日数","休車日数")],stringsAsFactors=F)
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
unit1 <- tmp$dist/tmp$weig             #1トン当たり平均輸送キロ
unit2 <- tmp$tk/(tmp$term-tmp$rest)    #実働1日1車当たり輸送トンキロ
unit3 <- tmp$weig/(tmp$term-tmp$rest)  #実働1日1車当たり輸送トン数
unit4 <- tmp$dist/(tmp$term-tmp$rest)  #実働1日1車当たり実車キロ

tmp2 <- aggregate(cbind("1トン当たり平均輸送キロ"=unit1,
                        "実働1日1車当たり輸送トンキロ"=unit2,
                        "実働1日1車当たり輸送トン数"=unit3,
                        "実働1日1車当たり実車キロ"=unit4),
                  by=list(type=tmp$type,class=tmp$class,year=tmp$year,month=tmp$month,yymm=tmp$yymm),mean,na.rm=T)
tmp2 <- subset(tmp2,tmp2$type=="01")
tmp2 <- tmp2[order(tmp2$class),]

tmp3 <- aggregate(cbind("1トン当たり平均輸送キロ"=unit1,
                        "実働1日1車当たり輸送トンキロ"=unit2,
                        "実働1日1車当たり輸送トン数"=unit3,
                        "実働1日1車当たり実車キロ"=unit4),
                  by=list(type=tmp$type,year=tmp$year,month=tmp$month,yymm=tmp$yymm),mean,na.rm=T)
tmp3 <- subset(tmp3,tmp3$type=="01")

write.table(tmp3,"out2/6-0_01普通車_原単位_1車単位.csv",sep=",",quote=F,row.names=F,col.names=T)
write.table(tmp2,"out2/6-1_01普通車_積載量別・原単位_1車単位.csv",sep=",",quote=F,row.names=F,col.names=T)

###################################################

#自動車票１にひも付け
dat <- rvlookup(key1b,tmp,2:ncol(tmp))
dat$dist[is.na(dat$dist)] <- 0
dat$weig[is.na(dat$weig)] <- 0
dat$tk[is.na(dat$tk)]     <- 0
if((nrow(dat)-nrow(dat1))!=0) stop("KEYに重複あり")

dat$type  <- type1
dat$year  <- dat1$調査年
dat$month <- dat1$調査月
dat$yymm  <- paste(dat$year,dat$month,sep="")

#最大積載量
dat$max <- round(as.numeric(dat1$最大積載量)/1000,3)

#最大積載量クラス
rnk <- ifelse(dat$max<3,"01",NA)
rnk <- ifelse((dat$max>=3)&(dat$max<6.5),"02",rnk)
rnk <- ifelse((dat$max>=6.5)&(dat$max<11),"03",rnk)
rnk <- ifelse((dat$max>=11)&(dat$max<16),"04",rnk)
rnk <- ifelse((dat$max>=16),"05",rnk)
if(any(is.na(rnk))) stop("最大積載量にNAあり")
dat$class <- rnk

#調査日数・休車日数・月実働日数
dat$term  <- as.numeric(dat1$調査日数)
dat$date  <- get.date(dat$year,dat$month)  #調査月の日数
dat$rest  <- as.numeric(dat1$休車日数)

#月換算
dat$mwd   <- (dat$term-dat$rest)*(dat$date/dat$term)  #1ヶ月の延実働日車数
dat$mweig <- dat$weig*(dat$date/dat$term)
dat$mtk   <- dat$tk*(dat$date/dat$term)

#平均値（共分散用）
tmp2 <- aggregate(cbind(avw=dat$mweig,avtk=dat$mtk,avwd=dat$mwd),by=list(yymm=dat$yymm,type=dat$type,class=dat$class),mean,na.rm=T)
dat  <- data.frame(dat,vlookup(dat,tmp2,c("yymm","type","class"),c("avw","avtk","avwd")),stringsAsFactors=F)

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
get.R <- function(tmp,t,cl=NULL){
  if(!is.null(cl)){
    flg  <- (tmp$type==t)&(tmp$class==cl)
  }else{
    flg  <- (tmp$type==t)
  }
  tmp  <- subset(tmp,flg)
  yymm <- tmp$yymm

  N    <- tapply(rep(1,nrow(tmp)),factor(yymm),sum,na.rm=T)
  x    <- tmp$mtk
  y    <- tmp$mweig
  w    <- tmp$mwd

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
R00 <- get.R(dat,t="01")
R01 <- get.R(dat,t="01",cl="01")
R02 <- get.R(dat,t="01",cl="02")
R03 <- get.R(dat,t="01",cl="03")
R04 <- get.R(dat,t="01",cl="04")
R05 <- get.R(dat,t="01",cl="05")

write.table(R00$res1,"out2/0_01普通車_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R01$res1,"out2/1_01普通車3トン未満_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R02$res1,"out2/2_02普通車3～6.5トン未満_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R03$res1,"out2/3_03普通車6.5～11トン未満_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R04$res1,"out2/4_04普通車11～16トン未満_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R05$res1,"out2/5_05普通車16トン以上_1トン当たり平均輸送キロ.csv",sep=",",quote=F,row.names=T,col.names=NA)

write.table(R00$res2,"out2/0_01普通車_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R01$res2,"out2/1_01普通車3トン未満_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R02$res2,"out2/2_02普通車3～6.5トン未満_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R03$res2,"out2/3_03普通車6.5～11トン未満_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R04$res2,"out2/4_04普通車11～16トン未満_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)
write.table(R05$res2,"out2/5_05普通車16トン以上_実働1日1車当たり輸送トンキロ.csv",sep=",",quote=F,row.names=T,col.names=NA)





