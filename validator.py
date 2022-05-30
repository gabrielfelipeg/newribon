import sys

def read_market(file):
    ondemand = []
    noupfront = []
    parcialupfront = []
    total_cost = 0
    while True:

        line = file.readline()

        if not line:
            break

        line = line.split('\n')[0]


        if line == '$instsODM.opt':
            line = file.readline()
            while True:

                line = file.readline()
                line = line.split('\n')[0]
                if line == '':
                    break

                line = line.split()
                ondemand.append(int(line[2]))
        elif line == '$instsPU1.opt':
            line = file.readline()
            reserved = 0
            while True:

                line = file.readline()
                line = line.split('\n')[0]
                if line == '':
                    break

                line = line.split()
                reserved += int(line[2])
                parcialupfront.append(reserved)
        elif line == '$cost.NU1.opt':
            line = file.readline()
            reserved = 0
            while True:

                line = file.readline()
                line = line.split('\n')[0]
                if line == '':
                    break

                line = line.split()
                reserved += int(line[2])
                noupfront.append(reserved)

        elif line == '$cost.opt':
            line = file.readline()
            while True:

                line = file.readline()
                line = line.split('\n')[0]
                if line == '':
                    break

                line = line.split()
                total_cost += float(line[2])
    return [ondemand, noupfront, parcialupfront, total_cost]


previous_input = open(sys.argv[1], 'r')
current_input = open(sys.argv[2], 'r')

previous_input = read_market(previous_input)
current_input = read_market(current_input)

label = ['OnDemand', 'NoUp', 'ParcialUp']

for i in range(len(label)):
    output_file = open('{}.txt'.format(label[i]), 'w')
    diff = 0
    for j in range(len(previous_input[i])):
        diff += abs(previous_input[i][j] - current_input[i][j])
        output_file.write('{} {} = {} {}\n'.format(previous_input[i][j], current_input[i][j], abs(previous_input[i][j] - current_input[i][j]), diff))

print('A:{}\nC:{}'.format(previous_input[3], current_input[3]))