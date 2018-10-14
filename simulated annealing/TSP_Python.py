import math
from collections import namedtuple
import numpy as np

Point = namedtuple("Point", ['x', 'y'])


input_data = "tsp_51_1"
# input_data = "./data/tsp_51_1"
f = open(input_data)
input_data = f.read()
lines = input_data.split('\n')

nodeCount = int(lines[0])

points = []

for i in range(1, nodeCount + 1):
    line = lines[i]
    parts = line.split()
    points.append(Point(float(parts[0]), float(parts[1])))


def length(point1, point2):
    return math.sqrt((point1.x - point2.x)**2 + (point1.y - point2.y)**2)

def obj_eval(solution):
    obj = length(points[solution[-1]], points[solution[0]])
    for index in range(0, nodeCount - 1):
        obj += length(points[solution[index]], points[solution[index + 1]])
    return obj


def cal_distance_matrix(data):
    data_matrix = np.zeros((nodeCount,nodeCount),dtype=np.float)
    for i in range(nodeCount):
        for j in range(nodeCount):
            data_matrix[i][j] = np.sqrt((points[i][0]-points[j][0]) **2 + (points[i][1]-points[j][1]) ** 2 )
    return data_matrix


data_matrix=cal_distance_matrix(points)


def object_eval(data_matrix, solution):
  tour_distance =0;
  for i in range(nodeCount-1):
    tour_distance = tour_distance + data_matrix[solution[i],solution[i+1]]
  tour_distance = tour_distance + data_matrix[solution[nodeCount-1],solution[1]]
  return(np.around(tour_distance,decimals=2))

cursolution = np.random.permutation(nodeCount)

object_eval(data_matrix,cursolution)

def circular_before(pos, nTotal):
    if pos == 0:
        # prev = nTotal
        prev = (nTotal-1)
    else:
        prev = pos-1
    return prev


def circular_after(pos, nTotal):
    # if pos==nTotal:
    if pos==(nTotal-1):
        after = 0
    else:
        after = pos +1
    return after


def TSP_SA(temperature,t_min,coolingRate):


    cursolution = np.random.permutation(nodeCount)
    f1 = object_eval(data_matrix, cursolution)
    best_solution = cursolution
    best_solution_obj = f1

    while temperature > t_min:
        equiv_number = np.int(nodeCount*(nodeCount-1) * 0.5)

        for i in range(equiv_number):
            if f1 <= best_solution_obj:
                best_solution_obj = f1
                best_solution = cursolution

            newsolution = cursolution

            rand_pos1 = np.random.randint(nodeCount, size=1)
            rand_pos2 = np.random.randint(nodeCount, size=1)

            newsolution[rand_pos1], newsolution[rand_pos2] = newsolution[rand_pos2], newsolution[rand_pos1]

            rp1_p = circular_before(rand_pos1, nodeCount)  #-1
            rp2_p = circular_before(rand_pos2, nodeCount)

            rp1_n = circular_after(rand_pos1, nodeCount)
            rp2_n = circular_after(rand_pos2, nodeCount)

            f2 = f1 - data_matrix[cursolution[rp1_p],cursolution[rand_pos1]]    \
                        -data_matrix[cursolution[rand_pos1],cursolution[(rp1_n)]]   \
                        -data_matrix[cursolution[rp2_p],cursolution[rand_pos2]] \
                        -data_matrix[cursolution[rand_pos2], cursolution[rp2_n]]  \
                        +data_matrix[newsolution[rp2_p], newsolution[rand_pos2]] \
                        +data_matrix[newsolution[rand_pos2], newsolution[rp2_n]]   \
                        +data_matrix[newsolution[rp1_p], newsolution[rand_pos1]] \
                        +data_matrix[newsolution[rand_pos1], newsolution[rp1_n]]
            # f2 = object_eval(data_matrix, newsolution)

            dE = f2 - f1
            if dE <0:
                cursolution = newsolution
                f1 = f2
            else:
                if np.exp((-1) * dE/temperature) > np.random.random_sample():
                    cursolution = newsolution
                    f1 = f2
        temperature = coolingRate * temperature
        print("tempeture",temperature)
        print(best_solution_obj)
        print(best_solution)
    return(best_solution)

test = TSP_SA(5, 0.0001, 0.90)
print(test)
print(object_eval(data_matrix,test))

# a=[14,36, 5,  9 ,47  ,4 ,28 ,46 ,10 ,11 ,29  ,3  ,6 ,48 ,27  ,7 ,37 ,21 ,26  ,2 ,23 ,32 ,22 ,38 ,31 ,13 ,24 ,35 ,25 ,42 ,3,4  ,1 ,33,
# 49 ,18 ,50 ,40 ,51 ,39, 16 ,15 ,45 ,17 ,30 ,44 ,43 ,12 ,19 ,41 ,20  ,8] - [1]
#
# b=np.subtract(a,1)
# object_eval(data_matrix,b)
