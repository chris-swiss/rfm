#' caculateRFM
#'
#Description
#' Calculates recency, frequency and monetary values for every customer.
#'
#Arguments
#' @param transactions data table which contains the transactions
#'
#' @details
#' \code{transactions} has to contain the columns \code{PurchAmount}, \code{TransDate}.
#'
#' @return RFM table

calculateRFM <- function(transactions) {
  transactions[,TransDate:=dmy(TransDate, tz="UTC")]
  max.Date <- max(transactions$TransDate)
  result <- transactions[,list(
    recency = as.numeric(max.Date - max(TransDate)), #recency = difference between latest transaction and "today"
    frequency = .N, #frequency = number of transactions
    monetary = mean(PurchAmount)), #monetary = amount spent per transaction
    by="Customer"
    ]
  return(result)
}
