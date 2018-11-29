library(tidyverse)

get_expected_runs_matrix=function(base_cd, outs, runs_rest_of_inn){
  ER=data_frame(base_cd,outs,runs_rest_of_inn)%>%
    group_by(base_cd,outs)%>%
    summarize(ERV=round(mean(runs_rest_of_inn),3))%>%
    ungroup()%>%
    mutate(state=paste(base_cd, outs, sep=' '))%>%
    arrange(outs)
  
  ER=matrix(ER$ERV, ncol=3, byrow=TRUE )
  rownames(ER)=c('_ _ _','X _ _','_ X _ ','X X _','_ _ X','X _ X','_ X X','X X X')
  colnames(ER)=c('0','1','2')
  
  
  return(ER)
}
