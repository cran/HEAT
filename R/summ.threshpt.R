summ.threshpt <-
function(obj){

      summary_results<-list()
      thresh_best<-obj$best_fit
      dev<-thresh_best[7]
      threshold<-thresh_best[8]
      parm_coef=obj$parm_coef
      myform<-obj$formula


      Best_fit<-matrix(0, 2,3)
      rownames(Best_fit)<-c("<Threshold", ">=Threshold")
      colnames(Best_fit)<-c("beta", "s.e","p")

      Best_fit[1,1]<-thresh_best[1]
      Best_fit[1,2]<-thresh_best[2]
      Best_fit[1,3]<-thresh_best[3]
      Best_fit[2,1]<-thresh_best[4]
      Best_fit[2,2]<-thresh_best[5]
      Best_fit[2,3]<-thresh_best[6]


      summary_results[[1]]<-myform
      summary_results[[2]]<-dev
      summary_results[[3]]<-threshold
      summary_results[[4]]<-rbind(Best_fit,parm_coef)
      #summary_results[[4]]<-summary(obj$glm_obj)$coef

      names(summary_results)<- c("Formula", "Deviance", "Threshold", "Best fit")

      return(summary_results)
      }
