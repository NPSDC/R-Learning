library(COPDSexualDimorphism.data)
data(lgrc.expr.meta)
#Q1
table(expr.meta$GENDER)[["2-Female"]]

#Q2
median(expr.meta$pkyrs)

library(BSgenome.Hsapiens.UCSC.hg19)

#A subsequence starting at base 10^6 and width 25
chr11seq <- BSgenome.Hsapiens.UCSC.hg19[["chr11"]]
subseq(chr11seq,start=10^6,width=25)


#1.2.1
ans = sapply(c("ATG", "TGA", "TAA",  "TAG"), function(x)
{
  
  countPattern(x, chr11seq)
})
chr7seq <- BSgenome.Hsapiens.UCSC.hg19[["chr7"]]
#1.2.2
alphabetFrequency(chr7seq, as.prob = TRUE)["C"]

#1.2.3
library(SNPlocs.Hsapiens.dbSNP.20120608)
s17 = getSNPlocs("ch17")
s17$loc[s17$RefSNP_id == '73971683']

library(devtools)
install_github("genomicsclass/tissuesGeneExpression")
library(tissuesGeneExpression)
data(tissuesGeneExpression)
#1.3.1
boxplot(split(e["209169_at",], factor(tissue) ))

#1.3.2
IDs = c("201884_at", "209169_at", "206269_at", "207437_at", "219832_s_at", "212827_at")

ans = c()
for (i in 1:length(IDs))
{
  a = tapply(e[IDs[i], ], tissue, mean)
  if(max(a) == a["placenta"])
    ans = c(ans, i)
}

q = 1
as = sapply(IDs, function(x)
{
 data_set = tapply(e[x, ], factor(tissue), mean)
 ans =c(ans, max(data_set) == data_set["placenta"])
 #if(max(data_set) == data_set["placenta"])
  #ans = c(ans, x)
   
})

#1.3.4
#Another powerful aspect of Bioconductor is that it provides object classes specifically 
#designed to keep high throughput data organized.
#the three tables that are needed to conduct data analysis are available from Bioconductor data objects
install_github("genomicsclass/GSE5859")
library(GSE5859)
data(GSE5859)
dat = exprs(e)
dim(dat)
#for gene expression we can use the ExpressionSet object

#pData is use as shorthand for phenotype data. 
sampleInfo = pData(e)
dim(sampleInfo)
head(sampleInfo)
?
#1.5
biocLite("Homo.sapiens")
library(Homo.sapiens)
class(Homo.sapiens)
#Annotation is the process by which pertinent information about these raw DNA sequences is added to the genome databases.
# This involves describing different regions of the code and identifying which regions can be called genes. 
#The OrganismDB object Homo.sapien contain annotation information about human genes, 
#in particular a series of relations between identifiers about human genes
#Some of the identifiers can be provided as "keys", that is, as values to look up in the database, in order to find pieces of information which match that key. 
 keytypes(Homo.sapiens)
 columns(Homo.sapiens)
 #1.5.1
length(unique(keys(Homo.sapiens, keytype="ENTREZID")))

#1.5.4
tab = select(Homo.sapiens, key="circadian rhythm", keytype="TERM", columns=c("ENTREZID"))
length(unique(tab$ENTREZID))
#expressionSet class
#class provides a container for data that greatly helps with reproducibility. Specifically, we can create one object that 
#contains measurements in the form of a matrix, a table providing information on the samples represented by the 
#columns of this matrix, a table with information on the  features represented by the rows of the
#matrix (or an ID that permits us to find such table in the annotation packages)

data("sample.ExpressionSet")
samp = sample.ExpressionSet
exprs(samp)[1:5, 1:10]
pData(samp) #Access the phenotypic data and the meta data associated with the experiment
experimentData(samp)
abstract(samp)
samp[1:4]
samp[1:4,3:20]
samp[, samp$sex == "Male"]

#1.6
#1.6.1
samp_new = samp[,samp$sex == "Female"]
sum(exprs(samp_new[1,])[1,])
#1.6.2
experimentData(samp)
#1.6.3
annotation(samp)
#1.6.3
cor(exprs(samp["31489_at", ])[1,] ,pData(samp)$score)


 
 