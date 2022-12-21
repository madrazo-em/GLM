# Question 1
message("Sorry we cannot understand")

# Question 2 
answer <- readline("Please enter either yes or no:")

# Question 3
# a)
procTrackingCode <- function() {
   code <- as.numeric(
            readline("Please provide the tracking code of your package: ")
            )
    if (is.na(code)) {
        message("Sorry we cannot help without a valid tracking code.")
    }
    else { 
        message("Your package is on its way to you.")
    }
}

# b)
procPayment <- function() { 
    paymentDecision <- readline("Would you like to make the payment (y/n)? ")
    
    if (paymentDecision == "y") {
        message("Thank you for your payment!")
    }
    
    else if (paymentDecision == "n") {
       message("We are sorry that we will not be able to deliver your package.")
    }
    else {
       message("Sorry we cannot understand. Goodbye.")
    }
}

# c)
procWeight <- function() {
    weightCategory <- readline("Does you package weigh more than 2 kg (y/n)? ")

    if (weightCategory == "y") {
        message("The delivery will cost £50.")
        procPayment()
    }
    else if (weightCategory == "n") {
        message("The delivery will cost £10.")
        procPayment()
    }
    else {
       message("Sorry we cannot understand. Goodbye.")
    }
}

procService <- function() {
    message("Hello, how can we help?")
    serviceType <- as.numeric(
                    readline("Please Enter 1 for sending a package, 2 for tracking a package, and 3 for other: ") # nolint
                    )
    if (is.na(serviceType)) {
        message("Sorry we cannot understand. Goodbye.")
    }
    else if (serviceType == 1) {
       procWeight()
    }
    else if (serviceType == 2) {
       procTrackingCode()
    }
    else if (serviceType == 3) {
       readline("Please enter your phone number and
                one of our staff members will contact you later: ")
    }
    else {
       message("Sorry we cannot understand. Goodbye.")
    }

}

procService()
