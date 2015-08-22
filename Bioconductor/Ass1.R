#Q1
table(expr.meta$GENDER)[["2-Female"]]

#Q2
median(expr.meta$pkyrs)

library(BSgenome.Hsapiens.UCSC.hg19)

#A subsequence starting at base 10^6 and width 25
subseq(chr11seq,start=10^6,width=25)

chr11seq <- BSgenome.Hsapiens.UCSC.hg19[["chr11"]]
#1.2.1
countPattern("TAA", chr11seq)

chr7seq <- BSgenome.Hsapiens.UCSC.hg19[["chr7"]]
#1.2.2
alphabetFrequency(chr7seq, as.prob = TRUE)["C"]

#1.2.3
library(SNPlocs.Hsapiens.dbSNP.20120608)
s17 = getSNPlocs("ch17")
s17$loc[s17$RefSNP_id == '73971683']

#1.3.1
boxplot(split(e["209169_at"], tissue) )

#1.3.2
IDs = c("201884_at", "209169_at", "206269_at", "207437_at", "219832_s_at", "212827_at")
