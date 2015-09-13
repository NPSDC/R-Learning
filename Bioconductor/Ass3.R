#Theory
#Both microarray and next generation Sequence are counting the number of DNA/RNA molecules.

#ExpressionSet and SummarisedExperiment
#ExpressionSet used for array based Experiments and SummarizedExperiment used for sequence based
#ExpressionSet lives in Biobase library
library(Biobase)
library(GEOquery) #Lets us pull ExpressionSets by name
geoq <- getGEO("GSE9514") #Downloads the expressionset of microarray data
e = geoq
exprs(e)[1,]
#pData gives us the dataframe which is info about columns of Expression Set
head(pData(e))
pData(e)[1:3,1:6]
dim(pData(e))
names(pData(e))
pData(e)$characteristics_ch1

fData(e)  #Features and this is info about rows which are genes or probe sets
dim(fData(e))
names(fData(e))
head(fData(e)$"Gene Symbol")
head(rownames(e))
experimentData(e)
annotation(e)

##Summarized Experiment
biocLite('parathyroidSE')
library(parathyroiddSE)
data("parathyroidGenesSE")
se <- parathyroidGenesSE # us a SummarizedExperiment object with gene level counts for each samples
dim(se)
assay(se)[1:3,1:3] #Similar to expression set
colData(se)[1:3,1:6] #similar to pData
dim(colData(se))

rownames(se)

#3.3.1
library(GSE5859Subset)
data(GSE5859Subset)
dim(geneExpression)
dim(sampleInfo)
dim(geneAnnotation)

#Before creating this object we must make sure that the rows of sampleInfo match the columns of geneExpression
#and that the rows of geneAnnotation match the rows of geneExpression
#ExpressionSet object we assure this connection is established by forcing the rownames of the assayData
#to match the rownames of featureData and the rownames of phenoData to match the colnames of assayData

#3.3.3
pd <-AnnotatedDataFrame(as.data.frame(geneExpression))
pData(pd) <- sampleInfo
rownames(pd) <- pData(pd)$filename
pData(pd)["GSM136530.CEL.gz","date"]

#3.3.4
varLabels(pd)[1]

#3.3.5
fd <-AnnotatedDataFrame(as.data.frame(geneExpression))
pData(fd) <- geneAnnotation
rownames(fd) <- geneAnnotation$PROBEID
pData(fd)["204810_s_at","CHR"]

#3.3.6
eset <- ExpressionSet(geneExpression, pd, fd)
ind1 = which( featureData(eset)$CHR=="chrY" )
ind2 =  pData(eset)$group==1
femaleY = colMeans(exprs(eset)[ind1, ind2]) 
maleY = colMeans(exprs(eset)[ind1, !ind2]) 
boxplot(maleY,femaleY)
median(maleY)-median(femaleY)

#3.3.8
library(Homo.sapiens)
genes = genes(Homo.sapiens)
library(hgfocus.db)
##INCOMPLETE



#READING FORM MICROARRAY
library(affy)
basedir <- "/home/noor/R-Training/Bioconductor/rawdata-master/celfiles"
tab <- read.delim(file.path(basedir, "sampleinfo.txt"), check.names = T, as.is = T)
fns <- list.celfiles(basedir)
fns%in%tab[,1] #checks all files of the directory in that table
ab <- ReadAffy(filenames = file.path(basedir, tab[,1]), phenoData = tab)

dim(pm(ab)) #returns the perfect match probe level intensities
dim(pData(ab))
annotation(ab)
#When we want to get information about genes its not stored here.
#Instead we keep a character that tells platform this technology used.
#We match gene ids to useful terms 

e <- rma(ab) #What we have is probe level info and we want it in gene level which is done by it

e <- justRMA(filenames = list.celfiles(basedir), phenoData = tab) #Without storing the probe level data

##Agilent for 2 color arrays
library(limma)
basedir <- "/home/noor/R-Training/Bioconductor/rawdata-master/agilent"
target <- readTargets(file.path(basedir, "TargetBeta7.txt"))
RG <- read.maimages(target$FileNames, source = "genepix", path= basedir) #source stands for imaging software
MA <- MA.RG(RG, bc.method = 'none') #instead of storing it as red-green it stores it as log-ratios thats M and 
#average of logs
dim(RG$R)
dim(RG$G)
dim(MA$M)
dim(MA$A)
plot(MA$A[,1], MA$M[,1])
imageplot(MA$M[,2], RG$printer, zlim = c(-3,3))
dev.off()

##3.4
library(BiocInstaller)
library(hgu95acdf)
#3.4.1
tab <- read.delim(file.path( "sampleinfo.txt"), check.names = T , as.is = T)
rownames(pData(e))<- tab$filenames
pData(e)["1521a99hpp_av06.CEL.gz", "36311_at"]

#3.4.2
ab <- ReadAffy(filenames = list.celfiles(), phenoData = tab)
length(which(probeNames(ab) == "36311_at"))

#3.4.3
mat = pm(ab)[which(probeNames(ab) == "36085_at"),]
conc = pData(ab)[c('1532a99hpp_av04.CEL.gz', '1532b99hpp_av04.CEL.gz'), 'X36085_at']
xMat = t(sapply(conc, function(x)
  {
  rep(x, length(rownames(mat)))
}))
 #3.4.4
e <- rma(ab)
g = factor(pData(ab)[,2])
library(genefilter)
#Null Hypothesis
#t-test p-value