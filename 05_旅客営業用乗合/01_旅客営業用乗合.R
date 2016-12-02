
rm(list=ls(all=TRUE))
invisible(replicate(4,gc()))

wd <- "//vanilla/16-51100999_自動車輸送/07_PJ実行/05_旅客営業用乗合/"
setwd(wd)
source("rvlookup.R")

###############################################################
#母集団数
inp0 <- "inp/【予備的調査】調査票C（旅客営業用乗合）母集団名簿.csv"
dat0 <- read.csv(inp0,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")

cname0 <- c("調査票コード","ヘッダ","支局コード","サンプル番号","事業所コード","営業所コード",
            "調査年","調査月","企業コード","住所1","住所2","事業者名","電話番号")
colnames(dat0) <- cname0
dat0$事業者数 <- 1

#支局：運輸局の対応テーブル
code.list <- read.csv("inp/支局コード.csv",sep=",",header=T,check.names=F,
                      stringsAsFactors=F,colClasses="character")
dat0$運輸局コード <- rvlookup(dat0$支局コード,code.list,"運輸局コード")
dat0$ブロックコード  <- rvlookup(dat0$支局コード,code.list,"15ブロックコード")

pop.dat <- aggregate(cbind(母集団数=as.numeric(dat0$事業者数)),
                     by=list(調査年=dat0[,7],調査月=dat0[,8],ブロックコード=dat0$ブロックコード),sum,na.rm=T)
pop.dat <- pop.dat[order(pop.dat$ブロックコード),]
pop.dat <- pop.dat[order(pop.dat$調査月),]
pop.dat <- pop.dat[order(pop.dat$調査年),]
write.table(pop.dat,"out/03_調査月別・15ブロック別・母集団数.csv",sep=",",quote=F,na="",row.names=F,col.names=T)

###############################################################

#読込
inp1 <- "inp/03 【予備的調査】旅客営業用乗合_調査票C(201510).csv"
inp2 <- "inp/04 【予備的調査】旅客営業用乗合_調査票D_その１(201510).csv"
inp3 <- "inp/04 【予備的調査】旅客営業用乗合_調査票D_その２(201510).csv"

dat1 <- read.csv(inp1,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr1  <- nrow(dat1)

dat2 <- read.csv(inp2,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr2  <- nrow(dat2)

dat3 <- read.csv(inp3,sep=",",header=F,check.names=F,stringsAsFactors=F,colClasses="character")
Nr3  <- nrow(dat3)

cname1 <- c("コード（調査票種類）","コード（層区分コード）","コード（支局コード）","コード（サンプル番号）",
            "事業所コード（事業者管理番号）","事業所コード（管轄支局コード）","調査年月（年）","調査年月（月分）",
            "一般路線バス（輸送人員（人））","一般路線バス（走行キロ（km））","一般路線バス（うち実車キロ（km））",
            "一般路線バス（延運行回数（回））","一般路線バス（今月末実在車両数（両））","高速路線バス（輸送人員（人））",
            "高速路線バス（走行キロ（km））","高速路線バス（うち実車キロ（km））","高速路線バス（延運行回数（回））",
            "高速路線バス（今月末実在車両数（両））")

cname2 <- c("コード（調査票種類）","コード（車種番号）","コード（支局コード）","コード（サンプル番号）",
            "事業所コード（事業者管理番号）","事業所コード（管轄支局コード）","調査年月（年）","調査年月（月分）",
            "調査期間（開始月）","調査期間（開始日）","調査期間（終了月）","調査期間（終了日）","調査期間（日間）",
            "調査する自動車（１）","調査する自動車（２）","調査する自動車（３）","調査する自動車（４）","乗車定員",
            "レコードヘッダ","主な用途","休車日数（調査期間中のうち、自動車を使用しなかった日数）")

cname3 <- c("コード（調査票種類）","コード（車種番号）","コード（支局コード）","コード（サンプル番号）",
            "事業所コード（事業者管理番号）","事業所コード（管轄支局コード）","調査年月（年）","調査年月（月分）",
            "調査期間（開始月）","調査期間（開始日）","調査期間（終了月）","調査期間（終了日）","調査期間（日間）",
            "調査する自動車（１）","調査する自動車（２）","調査する自動車（３）","調査する自動車（４）","乗車定員",
            "レコードヘッダ","月","日","運行系統（一般路線・高速路線別）","この運行系統の距離",
            "一日の延輸送人員","一日の運行回数","一人平均乗車キロ")

colnames(dat1) <- cname1
colnames(dat2) <- cname2
colnames(dat3) <- cname3

#運輸局コード
dat1$運輸局コード <- rvlookup(dat1[,"コード（支局コード）"],code.list,"運輸局コード")
dat2$運輸局コード <- rvlookup(dat2[,"コード（支局コード）"],code.list,"運輸局コード")
dat3$運輸局コード <- rvlookup(dat3[,"コード（支局コード）"],code.list,"運輸局コード")
if(any(is.na(dat1$運輸局コード))) stop("運輸局コードにNAあり")
if(any(is.na(dat2$運輸局コード))) stop("運輸局コードにNAあり")
if(any(is.na(dat3$運輸局コード))) stop("運輸局コードにNAあり")

#ブロックコード
dat1$ブロックコード <- rvlookup(dat1[,"コード（支局コード）"],code.list,"15ブロックコード")
dat2$ブロックコード <- rvlookup(dat2[,"コード（支局コード）"],code.list,"15ブロックコード")
dat3$ブロックコード <- rvlookup(dat3[,"コード（支局コード）"],code.list,"15ブロックコード")
if(any(is.na(dat1$ブロックコード))) stop("ブロックコードにNAあり")
if(any(is.na(dat2$ブロックコード))) stop("ブロックコードにNAあり")
if(any(is.na(dat3$ブロックコード))) stop("ブロックコードにNAあり")

######################################################################################
#調査票C：車両数の拡大推計
num1 <- as.numeric(dat1[,"一般路線バス（今月末実在車両数（両））"])
num2 <- as.numeric(dat1[,"高速路線バス（今月末実在車両数（両））"])

dem1 <- as.numeric(dat1[,"一般路線バス（輸送人員（人））"])
dem2 <- as.numeric(dat1[,"高速路線バス（輸送人員（人））"])

dist1 <- as.numeric(dat1[,"一般路線バス（うち実車キロ（km））"])
dist2 <- as.numeric(dat1[,"高速路線バス（うち実車キロ（km））"])


chk <- aggregate(cbind(一般車両数=num1,一般輸送人員=dem1,一般実車キロ=dist1,
                       高速車両数=num2,高速輸送人員=dem2,高速実車キロ=dist2,事業者数=1),
                       by=list(調査年=dat1[,"調査年月（年）"],調査月=dat1[,"調査年月（月分）"],ブロックコード=dat1$ブロックコード),sum,na.rm=T)
pop <- rvlookup(chk$ブロックコード,pop.dat[,3:4],"母集団数")
chk$母集団数 <- pop
kaku <- (pop/chk$事業者数)
chk$一般車両数拡大  <- chk$一般車両数*kaku
chk$一般輸送人員拡大 <- chk$一般輸送人員*kaku
chk$一般実車キロ拡大  <- chk$一般実車キロ*kaku

chk$高速車両数拡大   <- chk$高速車両数*kaku
chk$高速輸送人員拡大 <- chk$高速輸送人員*kaku
chk$高速実車キロ拡大  <- chk$高速実車キロ*kaku

######################################################################################
#調査票D：その①(車両1台ごと）
cnt2 <- rep(1,Nr2)
chk2 <- aggregate(cbind(車両数=cnt2),
                  list(調査年=dat2[,"調査年月（年）"],調査月=dat2[,"調査年月（月分）"],ブロックコード=dat2$ブロックコード),
                  sum,na.rm=T)

######################################################################################
#調査票D：その②（その①の車両のうち、2車両7日間分）

keito <- dat3[,"運行系統（一般路線・高速路線別）"]
nin1  <- as.numeric(dat3[,"一日の延輸送人員"])*(keito=="1")
nin2  <- as.numeric(dat3[,"一日の延輸送人員"])*(keito=="2")
kilo  <- as.numeric(dat3$一人平均乗車キロ)

chk3 <- aggregate(cbind(一般輸送人員=nin1,一般輸送人キロ=nin1*kilo,
                        高速輸送人員=nin2,高速輸送人キロ=nin2*kilo),
                  list(調査年=dat3[,"調査年月（年）"],調査月=dat3[,"調査年月（月分）"],ブロックコード=dat3$ブロックコード),
                  sum,na.rm=T)

#標本車両数をマッチング
chk3$標本車両数 <- vlookup(chk3,chk2,c("調査年","調査月","ブロックコード"),c("車両数"))

#母集団車両数
pop <- vlookup(chk3,chk,c("調査年","調査月","ブロックコード"),c("一般車両数拡大","高速車両数拡大"))

chk3 <- cbind(chk3,pop)
chk3$合計車両数拡大 <- rowSums(pop)

#拡大係数
kaku <- rowSums(pop)/chk3$標本車両数

chk3$一般輸送人員拡大  <- chk3$一般輸送人員*kaku
chk3$一般輸送人キロ拡大 <- chk3$一般輸送人キロ*kaku
chk3$高速輸送人員拡大  <- chk3$高速輸送人員*kaku
chk3$高速輸送人キロ拡大 <- chk3$高速輸送人キロ*kaku

write.table(chk3,"out/04_旅客営業用乗合_拡大推計（D票）.csv",sep=",",quote=F,na="",row.names=F,col.names=T)


