revwords <- function(){
  c(
    tm::stopwords(),
    # add words for numbers
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve", "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen",
    "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety",
    "hundred", "thousand", "million", "billion", "trillion",
    # add different endings
    "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth",
    "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth",
    "sixteenth", "seventeenth", "eighteenth", "nineteenth",
    "twentieth", "thirtieth", "fortieth", "fiftieth", "sixtieth",
    "seventieth", "eightieth", "ninetieth",
    "hundredth", "thousandth", "millionth", "billionth"
  )
}