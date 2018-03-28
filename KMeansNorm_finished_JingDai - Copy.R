# KMeansNorm.R

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    #observations <- sampleObservations
    #clusterCenters <- centersGuess
    #clusterCenters[,1]
    meanoffirstdim<-mean(observations[,1])
    meanoffirstdim
    sdoffirstdim<-sd(observations[,1])
    sdoffirstdim
    # normalize 1st dimension of observations
    observations[,1]<-(observations[,1]-meanoffirstdim)/sdoffirstdim
    observations[,1]
    # normalize 1st dimension of clusterCenters
    # normalize 1st dimension of clusterCenters shall use mean/sd of first dim of observation, not clustercenter
    #meanoffirstcenters<-mean(clusterCenters[,1])
    #meanoffirstcenters
    #sdoffirstcenters<-sd(clusterCenters[,1])
    #sdoffirstcenters
    clusterCenters[,1]<-(clusterCenters[,1]-meanoffirstdim)/sdoffirstdim
    normfirstcenters<-clusterCenters[,1]
    normfirstcenters
    
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    meanofseconddim<-mean(observations[,2])
    meanofseconddim
    sdofseconddim<-sd(observations[,2])
    sdofseconddim
    # normalize 2nd dimension of observations
    observations[,2]<-(observations[,2]-meanofseconddim)/sdofseconddim
    # normalize 2nd dimension of clusterCenters
    #meanofsecondcenters<-mean(clusterCenters[,2])
    #meanofsecondcenters
    #sdofsecondcenters<-sd(clusterCenters[,2])
    #sdofsecondcenters
    clusterCenters[,2]<-(clusterCenters[,2]-meanofseconddim)/sdofseconddim
    normsecondcenters<-clusterCenters[,2]
  }
  clusterCenters <- KMeans(observations, clusterCenters)
  if (normD1)
  {
    # denormalize in first dimension
    clusterCenters[,1]<-normfirstcenters*sdoffirstdim+meanoffirstdim
    clusterCenters[,1]
  } 
  if (normD2)
  {
    # denormalize in second dimension
    clusterCenters[,2]<-normsecondcenters*sdofseconddim+meanofseconddim
    clusterCenters[,2]
  } 
  return(clusterCenters)
}

# 3.a What is the single most obvious difference between these two distributions?
# Answer: These two distributions are on different scale
# 3.b Test 1 Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# Answer: yes, clustering occurs along the second dimension. In this case, either of the dimension is normalized, and because second dimension is on a much larger
#scale than first dimension, hence clustering occurs along second dimension.
# 3.c Test 2 Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# Answer: yes, clustering occurs along the second dimension. In this case, only the  first dimension is normalized, and because second dimension is on a much larger
#scale than first dimension, hence clustering occurs along second dimension.
# 3.d Test 3 Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# Answer: yes, clustering occur along first dimensiion. In this case, only second dimension is normalized, and because first dimension is on a larger scale, so 
#clustering occurs along first dimension.
# 3.e Test 4 Does clustering occur along one or two dimensions?  Which dimensions?  Why?
# Answer: No. clustering did not occur along either of the dimension. Because both of the dimensions are normalized, they are on the same scale.
# 4. why is normalization important for kmeans clustering?
# Answer: because normalization put all dimensions on the same scale, clustering will not occur along any of the dimensions that has large scale.
# 5. How to encode categorical data in kmeans clustering?
# Answer: to binarize the categorical data into dimensions with values either 0 or 1.
# 6. Why is kmeans unsupervised learning?
# Answer: because with kmeans clustering, we do not have outcomes set by expert.
