
def compute_frequency(input_fn):
    with open(input_fn, 'r') as fd:
        current_frequency = sum(int(freq) for freq in fd)
    return current_frequency

def find_repeats(input_fn):
    with open(input_fn, 'r') as fd:
        freqs = [int(freq) for freq in fd]

    seen = {0}
    current = 0

    while True:
        for freq in freqs:
            current += freq
            if current in seen:
                return current
            seen.add(current)

    return

def main():
    #print(compute_frequency('input.txt'))
    print(find_repeats('input.txt'))
    
if __name__ == '__main__':
    main()
    
