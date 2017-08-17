#' Assigns binary values to a vector
#'
#' This function will assign 0 if value < seuil, 1 otherwise.
#'
#' @param vecteur A numeric vector to transform.
#' @param seuil A numeric indicating the decision threshold. 0.5 is the default value.
#' @return A vector with 0 and 1 values.
#' @examples
#' vector <- c(0.2,0.6,0.75,0.05)
#' vector <- vec_assign(vector,0.6)
#' print(vector)
#' ## [1] 0 1 1 0
#' @export

vec_assign <- function(vecteur, seuil=0.5){
  sapply(1:length(vecteur), FUN = function(x){
    if (vecteur[x] < seuil){
      vecteur[x] <- 0
    } else {
      vecteur[x] <- 1
    }
  })
}

#' Displays error rates for predictions
#'
#' This function will help you when you have to choose a decision threshold to turn probabilities into binary values. Depending on your scoring problem, the false negatives and false positives haven't got the same level of importance so you can't keep a 0.5 decision threshold.
#'
#' @param obs_test A numeric or factor vector containing the observations from the test sample you try to predict.
#' @param pred_test A numeric vector inheriting from a \code{predict} object.
#' @param seuils A numeric vector with the threshold values you want to try.
#' @return Plots the graph with the false positive and false negative rates for each threshold, and displays the table with the global error rate as well as false negative and false positive rates, for each threshold. Also indicates the optimal threshold
#' @examples
#' ## Let's take an example with the SAheart dataset
#' ## You can run the following code :
#' 
#' data("SAheart")
#' set.seed(234)
#' indapp <- sample(1:nrow(SAheart),nrow(SAheart)*(2/3),replace=FALSE)
#' dapp <- SAheart[indapp,]
#' dtest <- SAheart[-indapp,]
#' model_complet <- glm(chd~., data = dapp, family = binomial)
#' predictions <- predict(model_complet, newdata = dtest, type = "response")
#' seuils <- seq(0.4,0.9,by=0.05)
#' choix_seuil(dtest$chd,predictions,seuils)
#' 
#' ## "Seuil minimisant le taux d'erreur global : 0.55"
#' @note A dataframe with the results as well as a graph of the errors will be returned in addition.
#' @export

choix_seuil <- function(obs_test,pred_test,seuils){
  error_rate_test <- c()
  false_negative_rate_test <- c()
  false_positive_rate_test <- c()
  
  # On construit un dataframe contenant les prévisions de test pour les différents seuils choisis
  
  previsions2 <- data.frame(obs_test)
  for (i in seuils){
    assign(paste0("prev_seuil_",i),data.frame(pred_test))
    class(get(paste0("prev_seuil_",i)))
    assign("col",data.frame(vec_assign(get(paste0("prev_seuil_",i))[,1],i)))
    previsions2 <- data.frame(previsions2,col[,1])
    names(previsions2)[length(previsions2)] <- paste0("prev_seuil_",i)
  }
  
  # On compare maintenant les erreurs pour chaque seuil
  
  colonnes <- 2:length(names(previsions2))
  for(j in colonnes){
    m <- SDMTools::confusion.matrix(previsions2[,1],previsions2[,j])
    error_rate_test <- c(error_rate_test,mean(previsions2[,j] != previsions2[,1]))
    false_negative_rate_test <- c(false_negative_rate_test,round((m[1,2]/(m[1,2]+m[2,2])),2))
    false_positive_rate_test <- c(false_positive_rate_test,round((m[2,1]/(m[1,1]+m[2,1])),2))
  }
  
  resultats <- data.frame(seuil = seuils,error_rate_test, false_negative_rate_test, false_positive_rate_test)
  
  
  # On affiche la table des résultats
  
  View(resultats)
  
  # On affiche les seuils qui minimise le critère
  
  resultats <- as.data.table(resultats)
  seuil_opti <- resultats[error_rate_test == min(error_rate_test), .(seuil, false_positive_rate_test)]
  seuil_opti <- seuil_opti[false_positive_rate_test == min(false_positive_rate_test)]$seuil
  print(paste("Seuil minimisant le taux d'erreur :",seuil_opti))
  
  # On trace le graphe des taux de faux positifs et faux négatifs
  
  plot(x = seuils, y = false_negative_rate_test, main = "Taux d'erreur en fonction du seuil",
       xlab = "seuil", ylab = "Taux d'erreur", type = "b",col="red", xlim = c(min(seuils),max(seuils)),
       ylim = c(min(false_positive_rate_test,false_negative_rate_test),max(false_positive_rate_test,false_negative_rate_test)+0.1))
  
  lines(x = seuils, y = false_positive_rate_test, type = "b",col="blue", lty=3)
  legend("topright",c("Faux négatifs","Faux positifs"),col=c("red","blue"),lty=c(1,3))
  
}
