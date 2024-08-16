#' Helpers for the `get_table_rapid()` function
#'
#' @param data data from get_table_rapid()
#' @param time_break number of years definedy by user
#' @param matrix_obj a matrix like object
#'
matrix_cells_vec <- function(data, time_break, matrix_obj){

  ##update progress bar

  # Count the number of person-years
  cmin <- min(as.numeric(colnames(matrix_obj)))
  rmin <- min(as.numeric(rownames(matrix_obj)))

  data$s1 = floor((data$start1 - cmin)/time_break)
  data$s2 = floor((data$start2 - rmin)/time_break)
  data$l1 = ceiling((data$stop1 - cmin)/time_break)
  data$l2 = ceiling((data$stop2 - rmin)/time_break)
  data$count0start = pmin(data$s1,data$s2)
  data$count0last = pmin(ncol(matrix_obj) -data$l1,
                         nrow(matrix_obj) -data$l2)

  data$ov = ifelse(data$start1 - floor(data$start1/time_break)*time_break>
                     data$start2 - floor(data$start2/time_break)*time_break, -1, 1)
  data$mv = ifelse(data$s1 < data$s2, -1, 1)
  data$ev = ifelse(data$s2==0, -1, 0)
  data$even_last = !(data$l1 -data$s1 == data$l2 -data$s2)
  data$count0lastodd = ifelse(data$even_last,
                              pmin(ncol(matrix_obj) -data$l1 - (data$ov-1)/2,
                                   nrow(matrix_obj) -data$l2 + (data$ov+1)/2),
                              pmin(ncol(matrix_obj) -data$l1,
                                   nrow(matrix_obj) -data$l2))
  data$count0lasteven = ifelse(data$even_last,
                               pmin(ncol(matrix_obj) -data$l1,
                                    nrow(matrix_obj) -data$l2),
                               pmin(ncol(matrix_obj) -data$l1 + (data$ov+1)/2,
                                    nrow(matrix_obj) -data$l2 - (data$ov-1)/2))
  data$count0starteven <- ifelse(data$ov ==-1,
                                 pmin(data$s1+1,data$s2),
                                 pmin(data$s1,data$s2+1))

  data$oos1 = ifelse(data$mv==-1,
                     data$mv*data$s1,
                     data$mv*data$s2)
  data$oos2 = ifelse(data$mv==-1,
                     data$mv*data$s2,
                     data$mv*data$s1)
  data$eos1 = ifelse(data$mv==-1,
                     data$mv*data$s1+ data$ov,
                     data$mv*data$s2)
  data$eos2 = ifelse(data$mv==-1, data$mv*data$s2,
                     data$mv*data$s1+ (data$ov)*-1)

  obj_list <- with(data, mapply(list_helper, s1,s2,l1,l2,
                                first, odd, even, last, even_last,
                                count0start,count0starteven,count0lasteven,count0lastodd,
                                ov, ev,start1, stop1))
  odd_list <- unlist(obj_list[1,], recursive = F)
  even_list <- unlist(obj_list[2,], recursive = F)

  matrix_out <- mapply(function(a,b,c,d,e,f){matrix_helper(a,b,c,d,e,f, matrix_obj)},
                       data$eos1,data$eos2, data$oos1, data$oos2,
                       even_list, odd_list)

  matrix_out <- Reduce("+", matrix_out)
  return(matrix_out)
}

#' list_helper
#'
#' @param s1
#' @param s2
#' @param l1
#' @param l2
#' @param first
#' @param odd
#' @param even
#' @param last
#' @param even_last
#' @param count0start
#' @param count0starteven
#' @param count0lasteven
#' @param count0lastodd
#' @param ov
#' @param ev
#' @param start1
#' @param stop1
#'
#'
list_helper <- function(s1,s2,l1,l2, first, odd, even, last, even_last,
                        count0start,count0starteven,
                        count0lasteven,count0lastodd,
                        ov, ev, start1, stop1){
  # Assign the observed person years to the strata
  if(l1-s1==1 & l2 -s2 ==1){
    odd_list <- c(rep(0,count0start),
                  stop1 - start1 + (1/365.25),
                  rep(0,count0lastodd))
    even_list <- c(rep(0,count0starteven),
                   rep(0,count0lasteven))
  }else{
    odd_list <- c(rep(0,count0start),
                  first,
                  rep(odd,max(min(l1 - s1, l2 - s2)-1- !even_last,0)),
                  last[!even_last],
                  rep(0,max(count0lastodd,0)))
    even_list <- c(rep(0,max(count0starteven,0)) ,
                   rep(even, max(min(l1 - s1, l2 - s2)-1,0)),
                   last[even_last],
                   rep(0,max(count0lasteven,0)))}
  return(list(list(odd_list),list(even_list)))
}



#' matrix_counts_vec
#'
#' @param data
#' @param time_break
#' @param matrix_obj
#'
matrix_counts_vec <- function(data, time_break, matrix_obj){

  # Count the number of observed cases
  cmin <- min(as.numeric(colnames(matrix_obj)))
  rmin <- min(as.numeric(rownames(matrix_obj)))

  data$s1 = floor((data$start1 - cmin)/time_break)
  data$s2 = floor((data$start2 - rmin)/time_break)
  data$l1 = ceiling((data$stop1 - cmin)/time_break)
  data$l2 = ceiling((data$stop2 - rmin)/time_break)
  data$count0start = pmin(data$s1,data$s2)

  data$ov = ifelse(data$start1 - floor(data$start1/time_break)*time_break>
                     data$start2 - floor(data$start2/time_break)*time_break, -1, 1)
  data$mv = ifelse(data$s1 < data$s2, -1, 1)
  data$ev = ifelse(data$s2==0, -1, 0)
  data$even_last = !(data$l1 -data$s1 == data$l2 -data$s2)
  data$count0lastodd = ifelse(data$even_last,
                              pmin(ncol(matrix_obj) -data$l1 - (data$ov-1)/2,
                                   nrow(matrix_obj) -data$l2 + (data$ov+1)/2),
                              pmin(ncol(matrix_obj) -data$l1,
                                   nrow(matrix_obj) -data$l2))
  data$count0lasteven = ifelse(data$even_last,
                               pmin(ncol(matrix_obj) -data$l1,
                                    nrow(matrix_obj) -data$l2),
                               pmin(ncol(matrix_obj) -data$l1 + (data$ov+1)/2,
                                    nrow(matrix_obj) -data$l2 - (data$ov-1)/2))
  data$count0starteven <- ifelse(data$ov ==-1,
                                 pmin(data$s1+1,data$s2),
                                 pmin(data$s1,data$s2+1))

  data$oos1 = ifelse(data$mv==-1,
                     data$mv*data$s1,
                     data$mv*data$s2)
  data$oos2 = ifelse(data$mv==-1,
                     data$mv*data$s2,
                     data$mv*data$s1)
  data$eos1 = ifelse(data$mv==-1,
                     data$mv*data$s1+ data$ov,
                     data$mv*data$s2)
  data$eos2 = ifelse(data$mv==-1, data$mv*data$s2,
                     data$mv*data$s1+ (data$ov)*-1)

  data$first=0
  data$even=0
  data$odd=0
  data$last=1

  obj_list <- with(data, mapply(list_count_helper, s1,s2,l1,l2,
                                first, odd, even, last, even_last,
                                count0start,count0starteven,count0lasteven,count0lastodd,
                                ov, ev,start1, stop1))
  odd_list <- unlist(obj_list[1,], recursive = F)
  even_list <- unlist(obj_list[2,], recursive = F)

  matrix_out <- mapply(matrix_helper,
                       data$eos1,data$eos2, data$oos1, data$oos2,
                       even_list, odd_list, list(matrix_obj))
  matrix_out <- Reduce("+", matrix_out)
  return(matrix_out)
}


#' list_count_helper
#'
#' @param s1
#' @param s2
#' @param l1
#' @param l2
#' @param first
#' @param odd
#' @param even
#' @param last
#' @param even_last
#' @param count0start
#' @param count0starteven
#' @param count0lasteven
#' @param count0lastodd
#' @param ov
#' @param ev
#' @param start1
#' @param stop1
#'
list_count_helper <- function(s1,s2,l1,l2, first, odd, even, last, even_last,
                              count0start,count0starteven,
                              count0lasteven,count0lastodd,
                              ov, ev, start1, stop1){
  ## Assign the observed cases per strata

  if(l1-s1==1 & l2 -s2 ==1){
    odd_list <- c(rep(0,count0start),
                  last,
                  rep(0,count0lastodd))
    even_list <- c(rep(0,count0starteven),
                   rep(0,count0lasteven))
  }else{
    odd_list <- c(rep(0,count0start),
                  first,
                  rep(odd,max(min(l1 - s1, l2 - s2)-1- !even_last,0)),
                  last[!even_last],
                  rep(0,max(count0lastodd,0)))
    even_list <- c(rep(0,max(count0starteven,0)) ,
                   rep(even, max(min(l1 - s1, l2 - s2)-1,0)),
                   last[even_last],
                   rep(0,count0lasteven))}
  return(list(list(odd_list),list(even_list)))
}


#' matrix_helper
#'
#' @param eos1
#' @param eos2
#' @param oos1
#' @param oos2
#' @param even_list
#' @param odd_list
#' @param matrix_obj
#'
matrix_helper <- function(eos1, eos2,oos1, oos2,even_list, odd_list, matrix_obj){
  matrix_obj[row(matrix_obj) +eos2 == col(matrix_obj) + eos1] <-
    matrix_obj[row(matrix_obj) +eos2 == col(matrix_obj) + eos1] + even_list
  matrix_obj[row(matrix_obj) +oos2 == col(matrix_obj) + oos1] <-
    matrix_obj[row(matrix_obj) +oos2 == col(matrix_obj) + oos1] + odd_list
  return(list(matrix_obj))

}


#' combine_matrix
#'
#' @param data
#'
combine_matrix <- function(data){
  # Convert matrix to a long-form data table
  data_dt <- cbind(
    as.data.table(expand.grid(rownames(data), colnames(data))
    ), value = as.vector(data))

  return(data_dt)
}
