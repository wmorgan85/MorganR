require(Rblpapi)


fetch_all_ca_flavours <- function(security, field, start, end, ...){
  
  raw_opt <- c(
    "adjustmentFollowDPDF"="false",
    "adjustmentSplit"="false",
    "adjustmentAbnormal"="false",
    "adjustmentNormal"="false"
  )
  spt_opt <- c(
    "adjustmentFollowDPDF"="false",
    "adjustmentSplit"="true",
    "adjustmentAbnormal"="false",
    "adjustmentNormal"="false"
  )
  ful_opt <- c(
    "adjustmentFollowDPDF"="false",
    "adjustmentSplit"="true",
    "adjustmentAbnormal"="true",
    "adjustmentNormal"="true"
  )
  
  raw <- bdh(security, field, start, end, options=raw_opt)
  spt <- bdh(security, field, start, end, options=spt_opt)
  ful <- bdh(security, field, start, end, options=ful_opt)
  
  names(raw) <- c("date", "raw")
  raw <- cbind(raw, split=spt[,field])
  raw <- cbind(raw, full=ful[,field])
  
  return(raw)
}