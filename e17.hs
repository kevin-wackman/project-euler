singleDigits = 0+3+3+5+4+4+3+5+5+4
tens         = 3+6+6+8+8+7+7+9+8+8
doubleDigits = singleDigits*8 + 6*4*10 + 5*3*10 + 7*10
hundreds     = (singleDigits*100) + 10*99*9 + 7*9 + (singleDigits + tens + doubleDigits)*9

main = do
    print $ singleDigits + tens + doubleDigits + hundreds + 11