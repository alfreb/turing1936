def transition(state, symbol):
    if state == 'start':
        if symbol == '0':
            return ('start', '0', 'R')
        elif symbol == '1':
            return ('odd', '1', 'R')
    elif state == 'odd':
        if symbol == '0':
            return ('odd', '0', 'R')
        elif symbol == '1':
            return ('odd', '1', 'R')
        elif symbol == '_':
            return ('reject', '_', 'R')
    elif state == 'even':
        if symbol == '0':
            return ('even', '0', 'R')
        elif symbol == '1':
            return ('even', '1', 'R')
        elif symbol == '_':
            return ('accept', '_', 'R')
    else:
        return None

def turing_machine(input_string):
    tape = list(input_string)
    tape.append('_')
    state = 'start'
    index = 0

    while True:
        symbol = tape[index]
        transition_result = transition(state, symbol)
        if transition_result is None:
            return 'Error: no transition defined for state {} and symbol {}'.format(state, symbol)
        new_state, new_symbol, move = transition_result
        tape[index] = new_symbol
        state = new_state

        if move == 'L':
            if index == 0:
                tape.insert(0, '_')
            else:
                index -= 1
        elif move == 'R':
            index += 1
            if index == len(tape):
                tape.append('_')
        else:
            return 'Error: invalid move specified'

        if state == 'accept':
            return 'Input {} is even'.format(input_string)
        elif state == 'reject':
            return 'Input {} is odd'.format(input_string)



print(turing_machine('1010')) # should output True (even)
print(turing_machine('1011')) # should output False (odd)
        
