import sys

file_input = open(sys.argv[1], 'r')

demand = []
ondemand = []
reserve = []
total_cost = 0

while True:

    line = file_input.readline()

    if not line:
        break

    line = line.split('\n')[0]


    if line == '$instsTotal':
        line = file_input.readline()
        while True:

            line = file_input.readline()
            line = line.split('\n')[0]
            if line == '':
                break

            line = line.split()
            demand.append(int(line[2]))

    elif line == '$instsODM.opt':
        line = file_input.readline()
        while True:

            line = file_input.readline()
            line = line.split('\n')[0]
            if line == '':
                break

            line = line.split()
            ondemand.append(int(line[2]))
    elif line == '$instsPU1.opt':
        line = file_input.readline()
        reserved = 0
        while True:

            line = file_input.readline()
            line = line.split('\n')[0]
            if line == '':
                break

            line = line.split()
            reserved += int(line[2])
            reserve.append(reserved)
    elif line == '$cost.opt':
        line = file_input.readline()
        while True:

            line = file_input.readline()
            line = line.split('\n')[0]
            if line == '':
                break

            line = line.split()
            total_cost += float(line[2])

print(total_cost)