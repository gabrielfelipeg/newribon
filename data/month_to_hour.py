import sys

month_days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

def main(month_file):
    month_file = open(month_file, 'r')
    header = month_file.readline()
    new_file = open('output.csv', 'w')
    new_file.write(','.join(header.split(',')[:2]) + '\n')
    year = 1
    hour = 0
    for line in month_file:
        line = line.split(',')
        month = line[0][-2:]
        for day in range(month_days[int(month) - 1]):
            for i in range(24):
                line[0] = str(year) + str(hour).zfill(4)
                new_file.write(','.join(line[:2]) + '\n')
                hour += 1
                if hour == 8784:
                    hour = 0
                    year += 1
    new_file.close()
    month_file.close()

if __name__ == '__main__':
    main(sys.argv[1])