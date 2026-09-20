calculate_purchase <- function(amount) {

  if (amount >= 1000) {
    discount_percent <- 25
    discount <- amount * discount_percent / 100
    total <- amount - discount

    cat(
      "Итоговая сумма:", total,
      "$; скидка:", discount_percent, "%\n"
    )

  } else if (amount >= 100) {
    discount_percent <- 12.5
    discount <- amount * discount_percent / 100
    total <- amount - discount

    cat(
      "Итоговая сумма:", total,
      "$; скидка:", discount_percent, "%\n"
    )

  } else {
    cat(
      "Итоговая сумма:", amount,
      "$; Nodiscounts\n"
    )
  }
}

calculate_purchase(50)    # скидки нет
calculate_purchase(200)   # скидка 12.5
calculate_purchase(1200)  # скидка 25
