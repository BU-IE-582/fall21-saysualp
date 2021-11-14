options(scipen=999)

require(data.table)

# assuming you have the homework data in your working directory in the following format:
# 'working_directory/'
current_folder = getwd()

data=read.csv('IE582_Fall21_HW2_q1_data.csv', header=T)
class=as.numeric(data[,3])
plot(data[,1], data[,2], col=class, pch=class, xlab=names(data)[1], ylab=names(data)[2])
legend("topleft", paste("Class", levels(data[,3])), col=unique(class), pch=unique(class))

summary(data)
# since the features are in the same scale, there is no need to scale data

pca = princomp(data[c(1,2)], cor=T)
summary(pca, loadings=T)

plot(pca$scores[,1], col=data$class, pch=as.numeric(data$class), xlab="Observation Number", ylab="PCA Score")
legend("topleft", paste("Class", levels(data[,3])), col=unique(class), pch=unique(class))

# compute distance matrix
d <- dist(data, method="euclidean")  

# perform MDS
mds_1 <- cmdscale(d, k=1)
plot(mds_1, col=data$class, pch=as.numeric(data$class), xlab="Observation Number", ylab="MDS", main="MDS with Euclidean Distance")
legend("topleft", paste("Class", levels(data[,3])), col=unique(class), pch=unique(class))

# compute distance matrix
d <- dist(data, method="manhattan")  

# perform MDS
mds_1 <- cmdscale(d, k=1)
plot(mds_1, col=data$class, pch=as.numeric(data$class), xlab="Observation Number", ylab="MDS", main="MDS with Manhattan Distance")
legend("topleft", paste("Class", levels(data[,3])), col=unique(class), pch=unique(class))

data$X12 <- data$X1^2
data$X22 <- data$X2^2
data$X1X2 <- data$X1*data$X2

pca = princomp(data[c(1,2,4,5,6)], cor=T)
summary(pca, loadings=T)

require(readxl)

distance_matrix = as.matrix(read_excel("ilmesafe.xls", skip=2))
distance_matrix = distance_matrix[,-1]
row.names(distance_matrix) = distance_matrix[, 1]
distance_matrix = distance_matrix[, -1]
distance_matrix =`class<-`(distance_matrix, 'numeric')
head(distance_matrix)

# make symetric
distance_matrix[lower.tri(distance_matrix)] = t(distance_matrix)[lower.tri(distance_matrix)]
head(distance_matrix)

# perform MDS
distance_matrix[is.na(distance_matrix)] = 0
d <- cmdscale(distance_matrix, eig = TRUE, k = 2)

Plot_ConvexHull<-function(xcoord, ycoord, lcolor){
hpts = chull(x = xcoord, y = ycoord)
hpts = c(hpts, hpts[1])
lines(xcoord[hpts], ycoord[hpts], col = lcolor)
}

xrange = range(c(d$points[, 1]))
yrange = range(c(d$points[, 2]))

par(tck = 0.02, mgp = c(1.7, 0.3, 0))

# draw locations
plot(d$points[, 1], d$points[, 2], type = "p", pch = 19, col = "black", xaxt = "n", yaxt = "n", xlab = "", ylab = "", xlim = c(xrange), ylim = c(yrange))
city.names = colnames(distance_matrix)
text(x=d$points[, 1], y=d$points[, 2], pos = 3, labels = city.names, cex=0.5)

# draw borders
Plot_ConvexHull(xcoord = d$points[, 1], ycoord = d$points[, 2], lcolor = "blue")

require(data.table)

# assuming you have the XYZ cordinates in your working directory in the following format:
# 'working_directory/'
current_folder = getwd()

file_names = list.files(pattern = "uWaveGestureLibrary")
list_files = lapply(file_names, read.table)

list_files = lapply(seq_along(list_files), function(x) {
  list_files[[x]]$V0 = 1:nrow(list_files[[x]]) # column VO is created to specify the sample instance
  list_files[[x]]
})

x = as.data.table(list_files[1])
y = as.data.table(list_files[2])
z = as.data.table(list_files[3])

    
# melt X cordinates for long format
setnames(x,"V1","class")
setnames(x,"V0","instance")
x = melt(x,id.vars=c('class','instance'))
x[,time:=as.numeric(gsub("\\D", "", variable))-1]
x = x[,list(class,instance, time, value)]
x = x[order(instance, class,time)]
setnames(x, "value", "x_cordinate")


# melt Y cordinates for long format
setnames(y,"V1","class")
setnames(y,"V0","instance")
y = melt(y,id.vars=c('class','instance'))
y[,time:=as.numeric(gsub("\\D", "", variable))-1]
y = y[,list(class,instance, time, value)]
y = y[order(instance, class, time)]
setnames(y, "value", "y_cordinate")

# melt Z cordinates  for long format
setnames(z,"V1","class")
setnames(z,"V0","instance")
z = melt(z,id.vars=c('class','instance'))
z[,time:=as.numeric(gsub("\\D", "", variable))-1]
z = z[,list(class, instance, time, value)]
z = z[order(instance, class, time)]
setnames(z, "value", "z_cordinate")


xy = merge(x,
            y,
            by=c("class","instance","time"))
            

xyz = merge(xy,
            z,
            by=c("class","instance","time"))

# shows x,y,z cordinates of one instance belongs to class 1
head(xyz)

require(lattice)

# instances belong to class 1, 2, 3, 4, 5, 6, 7, 8 respectively
samples <- xyz[instance %in% c(11, 15, 4, 5, 2, 1, 7, 6),]

velocity_samples = samples[, list(x_cordinate=cumsum(x_cordinate),
                                  y_cordinate=cumsum(y_cordinate),
                                  z_cordinate=cumsum(z_cordinate)), list(class, instance)]

cloud(z_cordinate~x_cordinate*y_cordinate|as.factor(class), data=velocity_samples, xlab="x", ylab="y", zlab="z")

x = as.data.table(list_files[1])
x = x[,-317]
colnames(x) = paste0("X", as.numeric(gsub("V", "", colnames(x)))-1)
setnames(x, "X0", "class")

y = as.data.table(list_files[2])
y = y[,-317]
colnames(y) = paste0("Y", as.numeric(gsub("V", "", colnames(y)))-1)
y = y[,-1]

z = as.data.table(list_files[3])
z = z[,-317]
colnames(z) = paste0("Z", as.numeric(gsub("V", "", colnames(z)))-1)
z = z[,-1]

xyz = cbind(x, y, z)

head(xyz)

pca1 = prcomp(xyz[class==1,-c(1, 946)])
summary(pca1)

pca2 = prcomp(xyz[class==2,-c(1, 946)])
summary(pca2)

pca3 = prcomp(xyz[class==3,-c(1, 946)])
summary(pca3)

pca4 = prcomp(xyz[class==4,-c(1, 946)])
summary(pca4)

pca5 = prcomp(xyz[class==5,-c(1, 946)])
summary(pca5)

pca6 = prcomp(xyz[class==6,-c(1, 946)])
summary(pca6)

pca7 = prcomp(xyz[class==7,-c(1, 946)])
summary(pca7)

pca8 = prcomp(xyz[class==8,-c(1, 946)])
summary(pca8)

pca1_rot=as.data.table(t(pca1$rotation[,c(1,2)]))
pca1_rot[,eigenvector:=c("PC1", "PC2")]
pca1_rot[,class:=1]

pca2_rot=as.data.table(t(pca2$rotation[,c(1,2)]))
pca2_rot[,eigenvector:=c("PC1", "PC2")]
pca2_rot[,class:=2]

pca3_rot=as.data.table(t(pca3$rotation[,c(1,2)]))
pca3_rot[,eigenvector:=c("PC1", "PC2")]
pca3_rot[,class:=3]

pca4_rot=as.data.table(t(pca4$rotation[,c(1,2)]))
pca4_rot[,eigenvector:=c("PC1", "PC2")]
pca4_rot[,class:=4]

pca5_rot=as.data.table(t(pca5$rotation[,c(1,2)]))
pca5_rot[,eigenvector:=c("PC1", "PC2")]
pca5_rot[,class:=5]

pca6_rot=as.data.table(t(pca6$rotation[,c(1,2)]))
pca6_rot[,eigenvector:=c("PC1", "PC2")]
pca6_rot[,class:=6]

pca7_rot=as.data.table(t(pca7$rotation[,c(1,2)]))
pca7_rot[,eigenvector:=c("PC1", "PC2")]
pca7_rot[,class:=7]

pca8_rot=as.data.table(t(pca8$rotation[,c(1,2)]))
pca8_rot[,eigenvector:=c("PC1", "PC2")]
pca8_rot[,class:=8]

rotations = rbind(pca1_rot, pca2_rot, pca3_rot, pca4_rot, pca5_rot, pca6_rot, pca7_rot, pca8_rot)

rotations_melt = melt(rotations, id.vars=c('class','eigenvector'))
rotations_melt = rotations_melt[,list(time=1:.N,
                                      value=value), list(class, eigenvector)]
rotations_melt = rotations_melt[order(class,eigenvector, time)]

head(rotations_melt[class==1&eigenvector=="PC1", ])

require(ggplot2)
require(repr)

options(repr.plot.width=15, repr.plot.height=8)

ggplot(rotations_melt, aes(x=time, y=value, color=eigenvector)) +  geom_line(size=1) + facet_wrap(~class) + ylab("principal component")


