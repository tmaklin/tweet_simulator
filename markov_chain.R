InitializeChain <- function(data, order) {
    # Initializes a Markov Chain of the argument order from the given data.
    #
    # Args:
    #   data: A vector of observed values from which the chain is to be initialized.
    #   order: A positive integer defining the order of the Markov Chain.
    #
    # Returns:
    #   A vector of length order containing the initial values.

    chain <- vector(length = order)
    chain[1] <- sample(unique(data), 1)

    if (order == 0) {
        chain
    } else {
      for (i in 2:max(2, order)) {
          logical.vector <- CreateLogicVector(data, chain[1:(i - 1)], i - 1)
          next.values <- data[logical.vector]
          chain[i] <- sample(next.values, 1)
      }

      chain
    }
}

CreateLogicVector <- function(data, values, order) {
    # Finds the positions of the next possible states of a Markov chain of argument order.
    #
    # Args:
    #   data: A vector of observed values from which the chain is simulated.
    #   values: The precedeing #order values of the chain.
    #   order: A positive integer defining the order of the Markov Chain.
    #
    # Returns:
    #   A logical vector containing the positions of the next possible states.

    logical.vector <- rep(TRUE, length(data) + order - 1)

    if (order == 0) {
        logical.vector
    } else {
      for (i in 1:order) {
          logical.vector <- logical.vector & c(rep(FALSE, order - i), data == values[i], rep(FALSE, i - 1))
      }

      c(FALSE, logical.vector)
    }
}

SimulateMarkovChain <- function(data, length, burn.length, order) {
    # Simulates a sample of argument length from a Markov Chain of argument order.
    #
    # Args:
    #   data: A vector of observed values from which a new sequence is simulated.
    #   length: Desired length of the output sequence.
    #   burn.length: Amount of samples to burn from the start of the simulated sequence.
    #   order: Order of the simulated Markov Chain.
    #
    # Returns:
    #   A vector of argument length containing simulated values.

    chain <- vector(length = length + burn.length)
    chain[1:order] <- InitializeChain(data, order)

    for (i in (order + 1):(length + burn.length)) {
        logical.vector <- CreateLogicVector(data, chain[(i - order):(i - 1)], order)
        next.letters <- data[logical.vector]
        chain[i] <- sample(next.letters, 1)
    }

    chain
  }