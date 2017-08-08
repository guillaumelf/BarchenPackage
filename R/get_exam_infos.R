#' Adds blank exams informations
#'
#' This function will the number of attempts, of questions attempted, and scores for each indivual accross all phases.
#'
#' @param list_tab A list of tables.
#' @return A data.table object with the informations wanted.
#' @examples
#' tab <- get_exam_infos(liste_qr)
#' @export

get_exam_infos <- function(list_tab){
  
  tranches <- length(list_tab)
  liste_id <- c()
  for(i in 1:tranches){
    assign(paste0("tab_qr",i), list_tab[[i]])
    liste_id <- c(liste_id, get(paste0("tab_qr",i))$userid)
  }
  
  id <- data.table(userid = liste_id)
  
  for (i in 1:tranches){
    tab <- get(paste0("tab_qr", i))[, length(unique(attempt)), by= .(userid, exam)][, tentatives := V1][, V1 := NULL]
    assign(paste0("recap", i), tab)
    remove(tab)
  }
  
  # On conserve les examens blancs

  for (i in 1:tranches){
    tab <- get(paste0("recap", i))[exam == "exam blanc"]
    assign(paste0("nb_examblanc", i), tab)
    remove(tab)
    assign(paste0("nb_examblanc", i), get(paste0("nb_examblanc", i))[, .(userid, tentatives)])
    l <- length(names(get(paste0("nb_examblanc", i))))
    new_names <- c("userid", paste0("nb_examblanc_attempt_t",i))
    noms <- names(get(paste0("nb_examblanc", i)))
    for (j in 1:l){
      assign(paste0("nb_examblanc", i),setnames(get(paste0("nb_examblanc", i)),noms[j],new_names[j]) )
    }
  }
  
  # On rassemble les infos dans une même table. On récupère d'abord les identifiants de ceux qui ont tenté les questionnaires
  
  for (i in 1:tranches){
    assign(paste0("res",i),id)
  }
  
  # On va faire un "left join" car il peut y avoir des valeurs manquantes (que l'on va conserver)
  
  for (i in 1:tranches){
    setkey(get(paste0("res", i)),userid)
  }
  
  for (i in 1:tranches){
    setkey(get(paste0("nb_examblanc", i)),userid)
    assign(paste0("res",i), merge(get(paste0("res", i)),get(paste0("nb_examblanc", i)), all.x = TRUE))
  }
  
  # On va ajouter les scores désormais au cours de chaque période
  
  # On commence par stocker tous ces résultats dans une table intermédiaire
  
  for (i in 1:tranches){
    assign(paste0("inter",i),get(paste0("tab_qr", i))[,.(resultat=round(sum(grade)/length(grade),2),
                                                         nb_questions = length(grade)), by = .(userid,niveau,attempt,exam)])
  }
  
  # On commence par stocker tous ces résultats dans une table intermédiaire
  
  for (i in 1:tranches){
    assign(paste0("inter",i),get(paste0("tab_qr", i))[,.(resultat=round(sum(grade)/length(grade),2),
                                                         nb_questions = length(grade)), by = .(userid,niveau,attempt,exam)])
  }
  
  # On ne garde que les résultats concernant les examens blancs
  
  for (i in 1:tranches){
    assign(paste0("exam_blanc",i),get(paste0("inter", i))[exam == "exam blanc"])
  }
  
  # On récupère maintenant les résultats pour les questionnaires de type A
  
  for (i in 1:tranches){
    assign(paste0("res_A",i),get(paste0("exam_blanc", i))[niveau == "A"])
    assign(paste0("res_A",i),get(paste0("res_A", i))[, .(mean(resultat),
                                                         mean(nb_questions)), by = userid])
    l <- length(names(get(paste0("res_A", i))))
    new_names <- c("userid",paste0("score_moy_examblanc_A_t",i),paste0("nb_moy_questions_examblanc_A_t",i))
    noms <- names(get(paste0("res_A", i)))
    for (j in 1:l){
      assign(paste0("res_A", i),setnames(get(paste0("res_A", i)),noms[j],new_names[j]) )
    }
    
    get(paste0("res_A",i))[, c(paste0("score_moy_examblanc_A_t",i),
                               paste0("nb_moy_questions_examblanc_A_t",i)) := list(round(get(paste0("score_moy_examblanc_A_t",i)),2),
                                                                                   round(get(paste0("nb_moy_questions_examblanc_A_t",i))))]
  }
  
  
  # De même pour les questionnaires de type C
  
  for (i in 1:tranches){
    assign(paste0("res_C",i),get(paste0("exam_blanc", i))[niveau == "C"])
    assign(paste0("res_C",i),get(paste0("res_C", i))[, .(mean(resultat),
                                                         mean(nb_questions)), by = userid])
    l <- length(names(get(paste0("res_C", i))))
    new_names <- c("userid",paste0("score_moy_examblanc_C_t",i),paste0("nb_moy_questions_examblanc_C_t",i))
    noms <- names(get(paste0("res_C", i)))
    for (j in 1:l){
      assign(paste0("res_C", i),setnames(get(paste0("res_C", i)),noms[j],new_names[j]) )
    }
    
    get(paste0("res_C",i))[, c(paste0("score_moy_examblanc_C_t",i),
                               paste0("nb_moy_questions_examblanc_C_t",i)) := list(round(get(paste0("score_moy_examblanc_C_t",i)),2),
                                                                                   round(get(paste0("nb_moy_questions_examblanc_C_t",i))))]
  }
  
  # On ajoute ces variables à la table res construite précédemment et on fait le ménage dans la mémoire de R
  
  for (i in 1:tranches){
    setkey(get(paste0("res_A", i)),userid)
    setkey(get(paste0("res_C", i)),userid)
    assign(paste0("res", i), merge(get(paste0("res", i)),get(paste0("res_A", i)), all.x = TRUE))
    assign(paste0("res", i),unique(get(paste0("res", i))))
    assign(paste0("res", i), merge(get(paste0("res", i)),get(paste0("res_C", i)), all.x = TRUE))
    assign(paste0("res", i),unique(get(paste0("res", i))))
  }
  
  res <- id
  for(i in 1:tranches){
    assign("res",merge(res,get(paste0("res", i)), all.x = TRUE))
  }
  
  res <- unique(res)
  res <- na_replace(res)
  return(res)
}