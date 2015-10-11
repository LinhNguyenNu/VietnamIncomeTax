## this function takes monthly income in VND million 
## it returns tax amount due and effective tax rate for such input income 

tax.rate = function(income){
  # progressive tax rate : 
  # 0 - 9 mil - 0%
  # >9 - 14 mil (next 5mil) - 5%
  # >14 - 19 mil (next 5mil) - 10%
  # >19 - 27 mil (next 8mil) - 15%
  # >27 - 31 mil (next 14mil) - 20%
  # >31 - 51 mil (next 20mil) - 25%
  # >51 - 79 mil (next 28mil) - 30%
  # >79 mil (remaining income amount) - 35%
  
  tax = 0
  
  # calculate income for tax purpose
  income4tax = income - 9 # first 9mil is tax exempt
  
  # calculating tax amount on 5% rate
  if (income4tax > 0) {
    tax = tax + 0.05*min(c(income4tax, 5))
    income4tax = income4tax - 5 # 5% rate for next 5mil
  }else {
    # return tax and rate = 0 for income < 9mil or income4tax < 0
    return(
      data.frame(income_mil = income, 
                 tax = tax, 
                 effective_rate = paste0(round(tax/income*100, 2), "%")
      )
    ) 
  }
  
  # calculating tax amount on 10% rate 
  if (income4tax > 0) {
    tax = tax + 0.1*min(c(income4tax, 5))
    income4tax = income4tax - 5 # 10% rate for next 5mil
  }else {
    # return tax and rate if income4tax < 0 after applying 5% rate
    return(
      data.frame(income_mil = income, 
                 tax = tax, 
                 effective_rate = paste0(round(tax/income*100, 2), "%")
      )
    ) 
  }
  
  # calculating tax amount on 15% rate for remaining income after applying 10% rate
  if (income4tax > 0) {
    tax = tax + 0.15*min(c(income4tax, 8))
    income4tax = income4tax - 8 # 15% rate for next 8mil
  }else {
    # returning tax and rate if income4tax < 0 after applying 10% rate
    return(
      data.frame(income_mil = income, 
                 tax = tax, 
                 effective_rate = paste0(round(tax/income*100, 2), "%")
      )
    ) 
  }
  
  # calculating tax amount on 20% rate
  if (income4tax > 0) {
    tax = tax + 0.2*min(c(income4tax, 14))
    income4tax = income4tax - 14 # 20% rate for next 14mil
  }else {
    # returning tax and rate if income4tax < 0 after applying 15% rate
    return(
      data.frame(income_mil = income, 
                 tax = tax, 
                 effective_rate = paste0(round(tax/income*100, 2), "%")
      )
    ) 
  }
  
  # calculating tax amount on 25% rate for remaining income after applying 20% rate
  if (income4tax > 0) {
    tax = tax + 0.25*min(c(income4tax, 20)) # 25% rate for next 20mil
    income4tax = income4tax - 20
  }else{
    # returning tax and rate if income4tax < 0 after applying 20% rate
    return(
      data.frame(income_mil = income, 
                 tax = tax, 
                 effective_rate = paste0(round(tax/income*100, 2), "%")
      )
    ) 
  }
  
  # calculating tax amount on 30% rate
  if (income4tax > 0) {
    tax = tax + 0.2*min(c(income4tax, 28))
    income4tax = income4tax - 28 # 30% rate for next 28mil
  }else {
    # returning tax and rate if income4tax < 0 after applying 25% rate
    return(
      data.frame(income_mil = income, 
                 tax = tax, 
                 effective_rate = paste0(round(tax/income*100, 2), "%")
                 )
  ) 
}
  # calculating tax amount on 35% rate for remaining income after applying 30% rate
  if (income4tax > 0) {
    tax = tax + 0.25*income4tax # 35% rate for remaining income4tax (if any)
  }
  
  # returning tax and rate 
  return(
    data.frame(income_mil = income,
               tax = tax, 
               effective_rate = paste0(round(tax/income*100, 2), "%"))
  )
}


## checking function tax.rate
income = 100
tax.rate(income)


