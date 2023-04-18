# Define the transition function for the Turing machine
def transition(state, symbol):
    if state == 'q0':
        if symbol == '0':
            return ('q1', '0', 'R')
        elif symbol == '1':
            return ('q1', '1', 'R')
    elif state == 'q1':
        if symbol == '0':
            return ('q0', '0', 'R')
        elif symbol == '1':
            return ('q0', '1', 'R')
    else:
        return ('halt', '', '')
# Define the Turing machine
def turing_machine(input_string):
    # Initialize the tape
    tape = ['B'] + list(input_string) + ['B']
    # Initialize the head position and the current state
    pos = 1
    state = 'q0'
    # Run the machine until it halts
    while True:
        # Get the current symbol under the head
        symbol = tape[pos]
        # Get the transition function for the current state and symbol
        new_state, new_symbol, move = transition(state, symbol)
        # Update the tape with the new symbol
        tape[pos] = new_symbol
        # Move the head
        if move == 'R':
            pos += 1
        elif move == 'L':
            pos -= 1
        # Update the state
        state = new_state
        # Check if the machine has halted
        if state == 'halt':
            break
    # Return whether the input was odd or even
    return tape[pos] == '0'

# Test the Turing machine with some input strings
print(turing_machine('1010')) # should output True (even)
# print(turing_machine('1011')) # should output False (odd)

