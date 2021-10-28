options(scipen=999)

number_of_points <- 1000
D <- 1:15

set.seed(2021)

fraction_table <- function(D, number_of_points) {
    
    # create matrix that will store the data
    data <- matrix(runif(number_of_points * D, min=-1, max=1), nrow = number_of_points, ncol = D)
    origin <-  matrix(0, nrow = number_of_points, ncol = D)
    
    # calculate distance of each intances from the origin
    distances <- as.matrix(sqrt(rowSums((data - origin)^2)))
    
    # find the fraction of matrix less than 1
    fraction <- sum(distances < 1)
    
    # construct table
    data.frame(D, fraction)
    
}
 
result_table = lapply(D,
                      number_of_points,
                      FUN = fraction_table)

final_table <- do.call(rbind, result_table)

plot(final_table, type="b", col="blue", lwd=2, xlab="# of Dimensions", ylab="Fraction < 1", main="Curse of Dimensionality")

plot(x, y, xlim = c(-2,2), ylim=c(-2,2), type='n', asp = 1)
x2 <- c(seq(-2, 2, 0.01), seq(-2, 2, 0.01))
y2 <- c((  1 * (4 - x2[1:401]^2)^0.5 ), ( -1 * (4 - x2[402:802]^2)^0.5 ))
rect(-2,-2,2,2, col = 'blue')
polygon(x2,y2, col = 'green', border = NA)

final_table[D==1|D==2, ]

4*786/1000 

final_table[D==1|D==3, ]

6*533/1000 

set.seed(2021)

datalist = list()
D <- 2:3

for (number_of_points in c(5000, 10000, 25000, 50000, 100000)) {

    result_data = lapply(D, 
                        number_of_points,
                        FUN = fraction_table)
    
        final_table <- do.call(rbind, result_data)
        final_table$number_of_points <- number_of_points  
        datalist[[number_of_points]] <- final_table 
}

all_fractions = do.call(rbind, datalist)

all_fractions

all_fractions$pi_estimation <- ifelse(all_fractions$D == 2, 
                                      4*all_fractions$fraction/all_fractions$number_of_points, 
                                      6*all_fractions$fraction/all_fractions$number_of_points)

plot(all_fractions[D==2,c("number_of_points", "pi_estimation")], type="b", col="blue", 
     lwd=2, xlab="Sample Size", ylab="Pi", yaxt="n", ylim=c(2.9,3.4), main="Pi Estimate")
abline(h = pi, col="red", lwd=3, lty=2)
axis(2, at = seq(2, 4, by = 0.2), las=2)

plot(all_fractions[D==3,c("number_of_points", "pi_estimation")], type="b", col="blue", 
     lwd=2, xlab="Sample Size", ylab="Pi", yaxt="n", ylim=c(2.9,3.4), main="Pi Estimate")
abline(h = pi, col="red", lwd=3, lty=2)
axis(2, at = seq(2, 4, by = 0.2), las=2)

number_of_test_instances <- 100

avg_distance_table <- function(D, number_of_points, number_of_test_instances) {
    
    # create matrix that will store the data
    # dimensions added as row so that apply function works properly
    data <- matrix(runif(number_of_points * D, min=-1, max=1), nrow = D, ncol = number_of_points)
    test_instances <-  matrix(runif(number_of_test_instances * D, min=-1, max=1), nrow = D, ncol = number_of_test_instances)

    # calculate the distance to each test instance’s
    distances <- apply(test_instances, 2, function(test_instance) {
         colSums((data - test_instance)^2)
    })    
    
    # take column-wise min calculate the distance to each test instance’s nearest neighbor
    nearest_neighbor <- apply(distances, 2, FUN = min)
    
    # average distance from the test instances to their nearest neighbors
    avg_distances <- mean(nearest_neighbor)
    
    # construct table
    data.frame(D, avg_distances)
    
}
 
result_table = lapply(D,
                      number_of_points,
                      number_of_test_instances,
                      FUN = avg_distance_table)

final_table <- do.call(rbind, result_table)

plot(final_table, type="b", col="blue", lwd=2, xlab="# of Dimensions", ylab="Average Distances")

D = 2
number_of_points = 5
number_of_test_instances = 2

data <- matrix(runif(number_of_points * D, min=-1, max=1), nrow = D, ncol = number_of_points)
test_instances <-  matrix(runif(number_of_test_instances * D, min=-1, max=1), nrow = D, ncol = number_of_test_instances)

# each column represents an instance 
# (-0.04466295, -0.94327471), (0.9390605, 0.5575227)
data

test_instances

distances <- apply(test_instances, 2, function(test_instance) {
             colSums((data - test_instance)^2)
    })

# distance between (-0.04466295, -0.94327471) and first test instance (0.4770919, 0.6790647) = 2.9042133
# distance between (0.9390605, 0.5575227) and first test instance (0.4770919, 0.6790647) = 0.2281875
# distance between (-0.3520766, 0.3268797) and second test instance (-0.2874879, -0.5719368) = 0.8120427
distances

nearest_neighbor <- apply(distances, 2, FUN = min)

# first element shows min distance to first instance and second shows min distance to second instance
nearest_neighbor

avg_distances <- mean(nearest_neighbor)

# average of distance to nearest neigbour
# (0.228187518851622+0.121412017850196)/2 
avg_distances

install.packages("jpeg")

# assuming you have the photo in your working directory in the following format:
# 'working_directory/'
current_folder = getwd()
photo_name = "photo"
photo_path = sprintf('%s/%s.jpg', current_folder, photo_name)

require("jpeg")
photo <- readJPEG(photo_path, native = FALSE)

str(photo)

dim(photo)

plot(c(100, 100), c(100, 100), type = "n", xlab = "", ylab = "")
rasterImage(photo, xleft = 75, xright = 125, ybottom = 75, ytop = 125)

par(mfrow=c(1,3))

for(i in c(1,2,3)){
    
    plot(c(100, 100), c(100, 100), type = "n", xlab = "", ylab = "")
    rasterImage(photo[,,i], xleft = 75, xright = 125, ybottom = 75, ytop = 125)

}

# create df that stores the information about column averages of channels 
data <- data.frame(matrix(1:512, ncol = 1, nrow = 512))
colnames(data) <- c("index")

for(i in c(1,2,3)){
    
    ch <- as.data.frame(colMeans(photo[,,i]))
    colnames(ch) <- c(paste("ch",i, sep="_"))    
    data <- cbind(data, ch)
    
}

plot(data$index, data$ch_1, type="l", col="red", lwd=2, xlab="Index", ylab="Column Averages", ylim = c(0, 1))
lines(data$index, data$ch_2, col="green")
lines(data$index, data$ch_3, col="blue")
labels <- c("ch1", "ch2", "ch3")
legend("topright", legend=labels, cex=0.8, inset=0.05, col=c("red", "blue", "green"), lty=1)

photo_upd <- photo

for(i in c(1,2,3)){
    
    photo_upd[,1:256,i] <- photo_upd[,1:256,i]-photo_upd[,257:512,i]
    
}

photo_upd[photo_upd < 0] <- 0

plot(c(100, 100), c(100, 100), type = "n", xlab = "", ylab = "")
rasterImage(photo_upd, xleft = 75, xright = 125, ybottom = 75, ytop = 125)

par(mfrow=c(1,3))


for(i in c(1,2,3)){
    
    plot(c(100, 100), c(100, 100), type = "n", xlab = "", ylab = "")
    rasterImage(photo_upd[,,i], xleft = 75, xright = 125, ybottom = 75, ytop = 125)

}

photo_noise <- photo


for(i in c(1,2,3)){
    
    noise <- matrix(runif(nrow(photo[,,i])* ncol(photo[,,i]), min=0, max=0.1*max(photo_noise[,,i])), 
                nrow = nrow(photo[,,i]), ncol = ncol(photo[,,i]))
    
    photo_noise[,,i] <- photo_noise[,,i]+noise
    
}

photo_noise[photo_noise > 1] <- 1

plot(c(100, 100), c(100, 100), type = "n", xlab = "", ylab = "")
rasterImage(photo_noise, xleft = 75, xright = 125, ybottom = 75, ytop = 125)

par(mfrow=c(1,3))

for(i in c(1,2,3)){
    
    plot(c(100, 100), c(100, 100), type = "n", xlab = "", ylab = "")
    rasterImage(photo_noise[,,i], xleft = 75, xright = 125, ybottom = 75, ytop = 125)


}
