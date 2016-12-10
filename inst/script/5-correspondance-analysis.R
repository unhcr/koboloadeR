####################################################################################
#### Apply MCA (Multiple Correspondance Analysis) to Categorical data
####################################################################################
## http://rpubs.com/gaston/MCA

names(data)

data$total2 <- as.factor(data$total)

## We need to generate sample in order to create the MCA object
data.sample <- data[sample(1:nrow(data), 5000,replace=FALSE),
                                    c("Marital_status",
                                      "Ethnicity",
                                      "Gender_of_Head_of_HH" ,
                                      "total2",
                                      "Religion",
                                      "Governorate_origin")
                                    ]

data.sample <- droplevels(data.sample)

str(data.sample)

#### Loading the required R module FactoMineR


data.mca <- MCA(data.sample)
summary(data.mca)

print(data.mca, nb.dec = 3, nbelements=10, ncp = 3, align.names=TRUE, file = "out/summary-mca.txt")


#########
# Plotting the MCA with better readibility
#########
plot.MCA(data.mca, axes=c(1, 2), col.ind="black", col.ind.sup="blue", col.var="darkred", col.quali.sup="darkgreen", label=c("ind", "ind.sup", "quali.sup", "var", "quanti.sup"), invisible=c("none", new.plot=TRUE))
dev.off()
plot.MCA(data.mca, axes=c(1, 2), choix="var", col.var="darkred", col.quali.sup="darkgreen", label=c("var", "quali.sup"), invisible=c("none", new.plot=TRUE))
dev.off()
plot.MCA(data.mca, axes=c(1, 2), choix="quanti.sup", col.quanti.sup="blue", label=c("quanti.sup", new.plot=TRUE))

#plotellipses(data.mca)


### Describing axis
round(data.mca$eig,2)
dimdesc(data.mca,axes=1:2)
dimdesc(data.mca,axes=1:2,proba=0.05)
dimdesc(data.mca,axes=1:2,proba=0.20)
dimdesc(data.mca,axes=1:2,proba=0.30)
dimdesc(data.mca,axes=1:2,proba=0.50)

### Category description
res.catdes <- catdes(data.mca, num.var=2, proba = 0.05)
plot(res.catdes)


## Hierarchical clustering

data.hcpc<-HCPC(data.mca ,nb.clust=-1,consol=TRUE,min=3,max=6,graph=TRUE)
data.hcpc$desc.var
data.hcpc$desc.axes
data.hcpc$desc.ind

print (data.hcpc, "out/descriptioncluster.txt")
dataclust <- data.hcpc$data.clust

dev.off()
plot(data.hcpc)

dev.off()
plot(data.hcpc, choice="map")


###################
#  clear graphics settings so that it works with multiple windows
dev.off()
Sys.setenv("DISPLAY"=":0.0")

capabilities()
sessionInfo()
options(device="X11")