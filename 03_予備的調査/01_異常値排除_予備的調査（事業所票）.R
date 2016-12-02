
rm(list=ls(all=TRUE))
invisible(replicate(4,gc()))

wd <- "//vanilla/16-51100999_自動車輸送/07_PJ実行/03_予備的調査"
setwd(wd)
source("rvlookup.R")

#NA補完用
na.fill <- function(x){
  x[is.na(x)] <- 0
  return(x)
}

#文字数確認用
unchar <- function(x) unique(nchar(x))

#普通車積載量
type <- c("普通車（３トン未満）","普通車（３トン以上６．５トン未満）","普通車（６．５トン以上１１トン未満）",
          "普通車（１１トン以上１６トン未満）","普通車（１６トン以上）")

#積載量別拡大推計用保有車両数
car.dat   <- read.csv("inp/貨物営業用（保有車両数）H27年12月末.csv",sep=",",header=T,check.names=F,stringsAsFactors=F)
code.list <- unique( car.dat[,c("支局コード","運輸局コード","地方運輸局名")] )  #支局：運輸局対応表
car.dat   <- aggregate(car.dat[,2:5],by=list(運輸局コード=car.dat$運輸局コード),sum,na.rm=T)

#母集団車両数
pop.dat1 <- read.csv("inp/【貨物営業用】母集団（比推定用）データ201509.csv",sep=",",header=F,check.names=F,stringsAsFactors=F)
pop.dat2 <- read.csv("inp/【貨物営業用】母集団（比推定用）データ201510.csv",sep=",",header=F,check.names=F,stringsAsFactors=F)
pop.dat3 <- read.csv("inp/【貨物営業用】母集団（比推定用）データ201511.csv",sep=",",header=F,check.names=F,stringsAsFactors=F)
pop.dat  <- rbind(pop.dat1,pop.dat2,pop.dat3)
colnames(pop.dat) <- c("調査年","調査月","支局コード","層コード","母集団")

pop.dat$調査月 <- formatC(pop.dat$調査月,width=2,flag="0")
pop.dat$運輸局コード <- rvlookup(pop.dat$支局コード,code.list,"運輸局コード")
pop.dat <- aggregate(母集団~調査年+調査月+運輸局コード+層コード,sum,na.rm=T,data=pop.dat)
pop.dat <- pop.dat[order(pop.dat$運輸局コード),]
pop.dat <- pop.dat[order(pop.dat$層コード),]
pop.dat <- pop.dat[order(pop.dat$調査月),]
pop.dat <- pop.dat[order(pop.dat$調査年),]


######################################################################################
#調査票A

inp1 <- "inp/06 【本調査（現行）】貨物営業用_事業所票_その１(201509-11).csv"   #基幹統計
#inp2 <- "inp/01 【予備的調査】貨物営業用_調査票A(201509-11).csv"         #予備的調査
inp2 <- "inp/01 【予備的調査】貨物営業用_調査票A(201509-11)_誤記修正.csv"   #予備的調査(輸送トンの誤記修正版)

inp3 <- "dat/Header_基幹.csv"
inp4 <- "dat/Header_A.csv"

dat1 <- read.csv(inp1,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character",na.strings=c("NULL",""))
dat2 <- read.csv(inp2,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr1  <- nrow(dat1)
Nr2  <- nrow(dat2)

#ヘッダー
cname1 <- unlist(read.csv(inp3,sep=",",header=T,stringsAsFactors=F))
cname2 <- unlist(read.csv(inp4,sep=",",header=T,stringsAsFactors=F))
colnames(dat1) <- cname1
colnames(dat2) <- cname2

#文字数修正
dat1$営業所番号 <- formatC(as.numeric(dat1$営業所番号),width=3,flag="0")
dat2[,"コード（サンプル番号）"]         <- formatC(as.numeric(dat2[,"コード（サンプル番号）"]),width=3,flag="0")
dat2[,"事業所コード（事業者管理番号）"] <- formatC(as.numeric(dat2[,"事業所コード（事業者管理番号）"]),width=12,flag="0",digits=50)
dat2[,"事業所コード（営業所番号）"]    <- formatC(as.numeric(dat2[,"事業所コード（営業所番号）"]),width=3,flag="0")
dat2[,"調査年月（月分）"]           <- formatC(as.numeric(dat2[,"調査年月（月分）"]),width=2,flag="0")

#共通キー作成
key1 <- apply(dat1[,c("層区分コード","運輸支局コード","サンプル番号","事業者管理番号","営業所番号","調査年","調査月")],1,paste,collapse="_")
key2 <- apply(dat2[,c("コード（層区分コード）","コード（支局コード）","コード（サンプル番号）","事業所コード（事業者管理番号）","事業所コード（営業所番号）","調査年月（年）","調査年月（月分）")],1,paste,collapse="_")
unchar(key1); unchar(key2)
# table(is.element(key2,key1))

#基幹統計の保有台数
tmp.dat <- data.frame(key1,dat1[,c("普通車（調査開始日現在の保有車両数）","普通車（輸送トン数）","普通車（走行距離）","普通車（実車距離）","データステータス"),drop=F],stringsAsFactors=F,check.names=F)
base.num <- rvlookup(key2,tmp.dat,2:6)

#予備的調査票Aの保有台数
col <- grepl("（調査開始日現在の保有車両数）",colnames(dat2))
A.num <- rowSums(apply(dat2[,col],c(1,2),as.numeric),na.rm=T)

err1 <- is.na(base.num[,"データステータス"]) #基幹統計に存在しないもの
err2 <- (base.num[,"データステータス"]=="1") #基幹統計のデータが不良のもの
err2[is.na(err2)] <- F
err3 <- is.na(base.num[,1])            #基幹統計の台数が欠損
err3[(err1|err2)] <- F
err4 <- (base.num[,1]!=A.num)
err4[(err1|err2|err3)] <- F            #基幹統計と調査票Aの台数が不一致
# colSums(cbind(err1,err2,err3,err4))

tmp1 <- (!err1)&(!err2)&(!err3)&(!err4)
tmp2 <- (!err3)&(!err4)

dat2$基幹保有台数 <- as.numeric(base.num[,1])
dat2$基幹保有台数[is.na(dat2$基幹保有台数)] <- 0
dat2$基幹輸送トン数 <- as.numeric(base.num[,2])
dat2$基幹走行距離 <- as.numeric(base.num[,3])
dat2$基幹実車距離 <- as.numeric(base.num[,4])
dat2$予備保有台数 <- A.num

#異常票
dat3 <- subset(dat2,!tmp2)
chk1 <- (dat3$基幹保有台数>dat3$予備保有台数)
 
#正常票（暫定）
dat4 <- subset(dat2,tmp1)
col1 <- grepl("（輸送トン数）",colnames(dat4))&!grepl("トン数で表せないもの",colnames(dat4))
col2 <- grepl("（走行距離）",colnames(dat4))
col3 <- grepl("（実車距離）",colnames(dat4))

dat4$予備輸送トン数 <- rowSums(apply(dat4[,col1],c(1,2),as.numeric),na.rm=T)
dat4$予備走行距離 <- rowSums(apply(dat4[,col2],c(1,2),as.numeric),na.rm=T)
dat4$予備実車距離 <- rowSums(apply(dat4[,col3],c(1,2),as.numeric),na.rm=T)

#基幹調査と予備調査の値をチェック
get.err <- function(x,y,name){
  ratio <- x/y
  err   <- (ratio<0.99)|(ratio>1.01)
  err[x==0] <- F  #基幹側ゼロは無視
  z <- round(ratio,2)
  chk <- cbind(基幹調査=x,予備調査=y,比=z,無効票=err)
  #print(chk)
  write.table(chk,paste("out/Check_無効票_",name,".csv",sep=""),
              sep=",",quote=F,na="",row.names=F,col.names=T)
  return(err)
}

err1 <- get.err(dat4$基幹輸送トン数,dat4$予備輸送トン数,"輸送トン数")
err2 <- get.err(dat4$基幹走行距離, dat4$予備走行距離,"走行距離")
err3 <- get.err(dat4$基幹実車距離, dat4$予備実車距離,"実車距離")
err  <- (err1|err2|err3)
# table(err)

#調査票Aの有効票
datA <- subset(dat4,!err)
# nrow(datA)

######################################################################################
#調査票B
#読込
inp1 <- "inp/02 【予備的調査】貨物営業用_調査票B_その１(201509-11).csv"  #ベース
inp2 <- "inp/02 【予備的調査】貨物営業用_調査票B_その２(201509-11).csv"  #品目1~3位別
inp3 <- "dat/Header_B1.csv"
inp4 <- "dat/Header_B2.csv"

datB <- read.csv(inp1,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr1  <- nrow(datB)

#ヘッダー
cname1 <- unlist(read.csv(inp3,sep=",",header=T,stringsAsFactors=F))
colnames(datB) <- cname1

#####################################################################

#調査票B＋調査票A有効票(普通車)
dat  <- rbind(datB[,1:50],datA[,1:50])
cn   <- colnames(dat)
nr   <- nrow(dat)
cnt  <- rep(1,nr)
type <- c("3トン未満","3~6.5トン未満","6.5~11トン未満","11~16トン未満","16トン以上")

#フォーマット修正・運輸局追加
dat[,"調査年月（月分）"] <- formatC(as.numeric(dat[,"調査年月（月分）"]),width=2,flag="0")
dat[,"コード（運輸局コード）"] <- rvlookup(dat[,"コード（支局コード）"],code.list,"運輸局コード")

#月別・層別標本数の確認
chk <- aggregate(rep(1,nrow(dat)),by=list(dat[,"調査年月（年）"],dat[,"調査年月（月分）"],dat[,"コード（層区分コード）"]),sum)
chk <- chk[order(chk[,2]),]

#データセット（ゼロサンプルは輸送トン数ゼロの１サンプルと扱う）
date.col <- "調査期間（日間）"
date     <- as.numeric(dat[,date.col])

num.col <- grepl("（調査開始日現在の保有車両数）",cn)
num     <- na.fill(apply(dat[,num.col],c(1,2),as.numeric))
colnames(num) <- type

day.col <- grepl("（１ヶ月間の延実働日車）",cn)
day     <- na.fill(apply(dat[,day.col],c(1,2),as.numeric))
colnames(day) <- type

ton.col <- grepl("（輸送トン数）",cn) & !grepl("トン数で表せないもの",cn)
ton     <- na.fill(apply(dat[,ton.col],c(1,2),as.numeric))
colnames(ton) <- type

dist.col <- grepl("（実車距離）",cn)
dist     <- na.fill(apply(dat[,dist.col],c(1,2),as.numeric))
colnames(dist) <- type

#実働1日1車あたり輸送トン数（異常値仮排除用）
ton.day <- ton/day
ton.day[is.na(ton.day)] <- 0

  ratio  <- 20
  border <- matrix(c(3,6.5,11,16,30)*ratio,
                   nrow=nrow(ton.day),ncol=ncol(ton.day),byrow=T)
  err.ton <- rowSums(ton.day>border)>0
  err.dat <- data.frame(dat[1:15],ton,day,ton.day,エラー=err.ton*1,stringsAsFactors=F,check.names=F)
  write.table(err.dat,"out/check_error.csv",sep=",",quote=F,na="",row.names=F,col.names=T)
  sum(err.ton)

  #異常値サンプル数の確認
  chk <- aggregate(cbind(標本数=cnt,仮排除=err.ton),
                   by=list(調査年=dat[,"調査年月（年）"],調査月=dat[,"調査年月（月分）"],
                           層コード=dat[,"コード（層区分コード）"],運輸局コード=dat[,"コード（運輸局コード）"]),sum,na.rm=T)
  chk <- chk[order(chk$運輸局コード),]
  chk <- chk[order(chk$層コード),]
  chk <- chk[order(chk$調査月),]
  chk <- chk[order(chk$調査年),]

##################################################

#異常値レコードを除く
dat <- subset(dat,!err.ton)
nr  <- nrow(dat)
cnt <- rep(1,nr)
write.table(dat,"result/予備的調査_異常値排除後.csv",sep=",",quote=F,na="",row.names=F,col.names=T)

#保有車両数
num.col <- grepl("（調査開始日現在の保有車両数）",cn)
num     <- na.fill(apply(dat[,num.col],c(1,2),as.numeric))
colnames(num) <- type

#輸送トン数
ton.col <- grepl("（輸送トン数）",cn) & !grepl("トン数で表せないもの",cn)
ton     <- na.fill(apply(dat[,ton.col],c(1,2),as.numeric))
colnames(ton) <- type

#総トン数(普通車・拡大前)
rton <- rowSums(ton,na.rm=T)

#実車距離
dist.col <- grepl("（実車距離）",cn)
dist     <- na.fill(apply(dat[,dist.col],c(1,2),as.numeric))
colnames(dist) <- type

#輸送トンキロ(距離は恐らく[m]→[km]に変換)
tk  <- (dist/1000)*ton
rtk <- rowSums(tk,na.rm=T)

#延実働日車数
day.col <- grepl("（１ヶ月間の延実働日車）",cn)
day     <- na.fill(apply(dat[,day.col],c(1,2),as.numeric))
colnames(day) <- type
rday    <- rowSums(day,na.rm=T)

##################################################

#運輸局別・最大積載量別の保有車両数を推計
num.count <- aggregate(num,by=list(運輸局コード=dat[,"コード（運輸局コード）"]),sum,na.rm=T)
num.share <- num.count[,-1]/rowSums(num.count[,-1],na.rm=T)
# apply(num.share,c(1,2),function(x) round(x*100,1)

#積載量別車両数の按分推計
car.kaku <- data.frame(運輸局コード=c(1:9),car.dat$普通貨物車*num.share,check.names=F,stringsAsFactors=F)
write.table(car.kaku,"result/03-1_運輸局別・最大積載量別保有車両数.csv",
            sep=",",quote=F,na="",row.names=F,col.names=T)

#####################################################################

get.chk2 <- function(rton,rtk,rday,cnt,dat,pop.dat,unit){

  #月別・層別輸送トン数(千トン)
  chk <- aggregate(cbind(総トン数=rton/unit,総トンキロ=rtk/unit,延実働日車数=rday,標本数=cnt),
                   by=list(調査年=dat[,"調査年月（年）"],調査月=dat[,"調査年月（月分）"],
                           運輸局コード=dat[,"コード（運輸局コード）"],層コード=dat[,"コード（層区分コード）"]),sum,na.rm=T)
  chk <- chk[order(chk$層コード),]
  chk <- chk[order(chk$運輸局コード),]
  chk <- chk[order(chk$調査月),]
  chk <- chk[order(chk$調査年),]

  #母集団のマッチング
  pop <- vlookup(chk,pop.dat,c("調査年","調査月","運輸局コード","層コード"),ncol(pop.dat))
  chk$母集団 <- pop
  chk$拡大総トン数 <- (chk$母集団/chk$標本数)*chk$総トン数
  chk$拡大総トンキロ <- (chk$母集団/chk$標本数)*chk$総トンキロ
  chk$拡大延実働日車数 <- (chk$母集団/chk$標本数)*chk$延実働日車数

  ###################################################################
  #標準誤差率
  chk3 <- aggregate(cbind(標本分散=rton/unit),
                    by=list(調査年=dat[,"調査年月（年）"],調査月=dat[,"調査年月（月分）"],
                           　運輸局コード=dat[,"コード（運輸局コード）"],層コード=dat[,"コード（層区分コード）"]),var,na.rm=T)
  chk3 <- chk3[order(chk3$層コード),]
  chk3 <- chk3[order(chk3$運輸局コード),]
  chk3 <- chk3[order(chk3$調査月),]
  chk3 <- chk3[order(chk3$調査年),]

  #標本数1の場合、分散は月別値で補完
  chk4 <- aggregate(cbind(標本分散=rton/unit),
                    by=list(調査年=dat[,"調査年月（年）"],調査月=dat[,"調査年月（月分）"]),var,na.rm=T)
  chk3$月別標本分散 <- vlookup(chk3,chk4,c("調査年","調査月"),"標本分散")
  chk3$標本分散 <- ifelse(is.na(chk3$標本分散),chk3$月別標本分散,chk3$標本分散)

  chk$標本分散 <- vlookup(chk,chk3,c("調査年","調査月","運輸局コード","層コード"),"標本分散")
  chk$標準誤差用 <- chk$母集団*(chk$母集団-chk$標本数)*(chk$標本分散/chk$標本数)

  #★層合計の場合、コメントアウトを解除
  #chk$層コード <- 1

  #運輸局を集約
  chk2 <- aggregate(chk[,c("延実働日車数","総トンキロ","総トン数","標本数","母集団","拡大延実働日車数","拡大総トンキロ","拡大総トン数")],
                    by=list(調査年=chk$調査年,調査月=chk$調査月,層コード=chk$層コード),sum,na.rm=T)
  chk2 <- chk2[order(chk2$層コード),]
  chk2 <- chk2[order(chk2$調査月),]
  chk2 <- chk2[order(chk2$調査年),]

  #層別に標準誤差を求める
  sqrt.sum <- function(x) sqrt(sum(x,na.rm=T))
  chk5 <- aggregate(cbind("標準誤差"=chk$標準誤差用),
                    by=list(調査年=chk$調査年,調査月=chk$調査月,層コード=chk$層コード),sqrt.sum)

  #標準誤差率を求める
  chk2$標準誤差 <- vlookup(chk2,chk5,c("調査年","調査月","層コード"),"標準誤差")
  chk2$標準誤差率 <- chk2$標準誤差/chk2$拡大総トン数*100
 
  return(list(chk=chk,chk2=chk2))
}
res  <- get.chk2(rton,rtk,rday,cnt,dat,pop.dat,1)
chk  <- res$chk
chk2 <- res$chk2
#cbind(トン数=round(res$chk2$拡大総トン数/1000,0),誤差率=round(res$chk2$標準誤差率,2))

write.table(chk, "result/01-1_運輸局別・層別輸送トン数.csv",sep=",",quote=F,na="",row.names=F,col.names=T)
write.table(chk2,"result/01-2_層別輸送トン数.csv",sep=",",quote=F,na="",row.names=F,col.names=T)

###################################################################
#普通車積載量別

#再現用(ゼロサンプルを輸送トン数ゼロの1サンプルとカウント)
res1  <- get.chk2(ton[,1],tk[,1],day[,1],cnt,dat,pop.dat,1)
res2  <- get.chk2(ton[,2],tk[,2],day[,2],cnt,dat,pop.dat,1)
res3  <- get.chk2(ton[,3],tk[,3],day[,3],cnt,dat,pop.dat,1)
res4  <- get.chk2(ton[,4],tk[,4],day[,4],cnt,dat,pop.dat,1)
res5  <- get.chk2(ton[,5],tk[,5],day[,5],cnt,dat,pop.dat,1)
# cbind(トン数=round(res5$chk2$拡大総トン数,0),誤差率=round(res5$chk2$標準誤差率,2))

write.table(res1$chk2,"result/02-1_層別輸送トン数_3トン未満.csv",sep=",",quote=F,na="",row.names=F,col.names=T)
write.table(res2$chk2,"result/02-2_層別輸送トン数_3～6.5トン未満.csv",sep=",",quote=F,na="",row.names=F,col.names=T)
write.table(res3$chk2,"result/02-3_層別輸送トン数_6.5～11トン未満.csv",sep=",",quote=F,na="",row.names=F,col.names=T)
write.table(res4$chk2,"result/02-4_層別輸送トン数_11～16トン未満.csv",sep=",",quote=F,na="",row.names=F,col.names=T)
write.table(res5$chk2,"result/02-5_層別輸送トン数_16トン以上.csv",sep=",",quote=F,na="",row.names=F,col.names=T)

###################################################################





