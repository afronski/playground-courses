package week6

object maps {
  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Poland" -> "Warsaw")

  romanNumerals.->("I")

  capitalOfCountry("Poland")
  capitalOfCountry get "Germany"
}