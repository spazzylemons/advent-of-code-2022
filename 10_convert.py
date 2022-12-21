with open('input', 'r') as infile, open('10_input.bin', 'wb') as outfile:
    for line in infile.read().splitlines():
        if line == 'noop':
            outfile.write(bytes([0]))
        elif line.startswith('addx'):
            amt = int(line[5:]) % 256
            if amt == 0:
                # just in case add amount is zero, replace with two nops
                outfile.write(bytes([0]))
                outfile.write(bytes([0]))
            else:
                outfile.write(bytes([amt]))
