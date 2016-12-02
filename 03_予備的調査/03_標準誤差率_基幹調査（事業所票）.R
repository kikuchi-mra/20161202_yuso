
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
#調査票B
#読込
inp1 <- "inp/06 【本調査（現行）】貨物営業用_事業所票_その１(201509-11).csv"  #ベース
inp3 <- "dat/Header_基幹.csv"

dat <- read.csv(inp1,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character",na.strings=c("","NULL"))
Nr1  <- nrow(dat)

#ヘッダー
cname1 <- unlist(read.csv(inp3,sep=",",header=T,stringsAsFactors=F))
colnames(dat) <- cname1

######################################################################################
#対象データの抽出
tar1a <- (dat$データステータス=="0") #正常
tar1b <- (dat$調査票入力ステータス=="0")|(dat$調査票入力ステータス=="1")
tar1  <- tar1a & tar1b
dat 　<- subset(dat,tar1)
Nr1-nrow(dat)

#####################################################################

cn   <- colnames(dat)
nr   <- nrow(dat)
cnt  <- rep(1,nr)

#フォーマット修正・運輸局追加
dat$運輸局コード <- rvlookup(dat$運輸支局コード,code.list,"運輸局コード")
sum(is.na(dat$運輸局コード))

#月別・層別標本数の確認
chk <- aggregate(rep(1,nrow(dat)),by=list(dat$調査年,dat$調査月,dat$層区分コード),sum)
chk <- chk[order(chk[,2]),]
# dat[(dat$層区分コード=="20")&(dat$調査年=="2015")&(dat$調査月=="09"),]


#データセット（ゼロサンプルは輸送トン数ゼロの１サンプルと扱う）
date.col <- "調査日数"
date     <- as.numeric(dat[,date.col])

num.col <- grepl("普通車",cn) & grepl("（調査開始日現在の保有車両数）",cn)
num     <- na.fill(as.numeric(dat[,num.col]))

day.col <- grepl("普通車",cn) & grepl("（１ヶ月間の延実働日車）",cn)
day     <- na.fill(as.numeric(dat[,day.col]))

ton.col <- grepl("普通車",cn) & grepl("（輸送トン数）",cn) & !grepl("トン数で表せないもの",cn)
ton     <- na.fill(as.numeric(dat[,ton.col]))

dist.col <- grepl("普通車",cn) & grepl("（実車距離）",cn)
dist     <- na.fill(as.numeric(dat[,dist.col]))

##################################################

#総トン数(普通車・拡大前)
rton <- ton

#輸送トンキロ(距離は恐らく[m]→[km]に変換)
tk  <- (dist/1000)*ton
rtk <- tk

#延実働日車数
rday <- day

##################################################

get.chk2 <- function(rton,rtk,rday,cnt,dat,pop.dat,unit){

  #月別・層別輸送トン数(千トン)
  chk <- aggregate(cbind(総トン数=rton/unit,総トンキロ=rtk/unit,延実働日車数=rday,標本数=cnt),
                   by=list(調査年=dat$調査年,調査月=dat$調査月,
                           運輸局コード=dat$運輸局コード,層コード=dat$層区分コード),sum,na.rm=T)
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
                    by=list(調査年=dat$調査年,調査月=dat$調査月,
                            運輸局コード=dat$運輸局コード,層コード=dat$層区分コード),var,na.rm=T)
  chk3 <- chk3[order(chk3$層コード),]
  chk3 <- chk3[order(chk3$運輸局コード),]
  chk3 <- chk3[order(chk3$調査月),]
  chk3 <- chk3[order(chk3$調査年),]

  #標本数1の場合、分散は月別値で補完
  chk4 <- aggregate(cbind(標本分散=rton/unit),
                    by=list(調査年=dat$調査年,調査月=dat$調査月),var,na.rm=T)
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

write.table(chk, "result2/01-1_運輸局別・層別輸送トン数.csv",sep=",",quote=F,na="",row.names=F,col.names=T)
write.table(chk2,"result2/01-2_層別輸送トン数.csv",sep=",",quote=F,na="",row.names=F,col.names=T)





