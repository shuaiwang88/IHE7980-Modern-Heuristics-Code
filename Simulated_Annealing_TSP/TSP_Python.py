##############################################################################
# Use Simulated Annealing to solve TSP
# Author: Shuai Wang 
# Usage: Open Source 
#
#
################################################################################
#
# import math
from collections import namedtuple
import numpy as np


# initialize the data ---------------------------------------------------------
Point = namedtuple("Point", ['x', 'y'])
input_data = "26city.txt"

f = open(input_data)
input_data = f.read()
lines = input_data.split('\n')

nodeCount = int(lines[0])

points = []

for i in range(1, nodeCount + 1):
    line = lines[i]
    parts = line.split()
    points.append(Point(float(parts[0]), float(parts[1])))

# generate distance matrix ---------------------------------------------------
# euclidean distance 
def length(point1, point2):
    return np.sqrt((point1.x - point2.x)**2 + (point1.y - point2.y)**2)

# def obj_eval(solution):
#     obj = length(points[solution[-1]], points[solution[0]])
#     for index in range(0, nodeCount - 1):
#         obj += length(points[solution[index]], points[solution[index + 1]])
#     return obj

# euclidean distance 
# def cal_distance_matrix(data):
#     data_matrix = np.zeros((nodeCount,nodeCount),dtype=np.float)
#     for i in range(nodeCount):
#         for j in range(nodeCount):
#             data_matrix[i][j] = np.sqrt((points[i][0]-points[j][0]) **2 +
#                                         (points[i][1]-points[j][1]) ** 2 )
#     return data_matrix

up_  = 60  # fake
low_ = 1 # fake

# manhantan like distance
def cal_distance_matrix(data):
    data_matrix = np.zeros((nodeCount,nodeCount),dtype=np.float)
    for i in range(nodeCount):
        for j in range(nodeCount):
            data_matrix[i][j] = min(np.array( np.abs(points[i][0]- up_) + np.abs(points[i][0]- points[j][0]) + np.abs(points[j][0] - up_)) ,
                   np.array(np.abs(points[i][0] - low_) + np.abs(points[i][0] - points[j][0]) + np.abs(points[j][0] - low_)))
    return data_matrix

data_matrix=cal_distance_matrix(points)

# evaluate the objective ----------------------------------------------------
def object_eval(data_matrix, solution):
  tour_distance =0;
  for i in range(nodeCount-1):
    tour_distance = tour_distance + data_matrix[solution[i],solution[i+1]]
  tour_distance = tour_distance + data_matrix[solution[nodeCount-1],solution[0]]
  return(np.around(tour_distance,decimals=2))



# define position's  previous and after location-------------------------------
def circular_before(pos, nTotal):
    if pos == 0:
        # prev = nTotal
        prev = (nTotal-1)
    else:
        prev = pos-1
    return prev

# circular_after(0, nodeCount)
def circular_after(pos, nTotal):
    # if pos==nTotal:
    if pos==(nTotal-1):
        after = 0
    else:
        after = pos +1
    return after




# main ------------------------------------------------------------------------
def TSP_SA(temperature,t_min,coolingRate):

    cursolution = np.random.permutation(nodeCount)
    f1 = object_eval(data_matrix, cursolution)
    best_solution = np.copy(cursolution)
    best_solution_obj = f1

    while temperature > t_min:
        equiv_number = np.int(nodeCount*(nodeCount-1) * 0.5)

        for i in range(equiv_number):
            if f1 <= best_solution_obj:
                best_solution_obj = f1
                best_solution = np.copy(cursolution)

            newsolution = np.copy(cursolution)

            rand_pos1 = np.random.randint(nodeCount, size=1)
            rand_pos2 = np.random.randint(nodeCount, size=1)

            newsolution[rand_pos1], newsolution[rand_pos2] = newsolution[rand_pos2],\
                                                             newsolution[rand_pos1]

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
#             f2 = object_eval(data_matrix, newsolution)

            dE = f2 - f1
            if dE <0:
                cursolution = np.copy(newsolution)
                f1 = f2
            elif np.exp((-1) * dE/temperature) > np.random.random_sample():
                    cursolution = np.copy(newsolution)
                    f1 = f2
        temperature = coolingRate * temperature
        # print("tempeture",temperature)
        print(best_solution_obj)
        print(best_solution)
    return best_solution

from datetime import datetime
start_time = datetime.now()
test = TSP_SA(500, 0.0001, 0.99)
time_elapsed = datetime.now() - start_time
time_elapsed
print(test)
print(object_eval(data_matrix,test))




if __name__ == '__main__':
    main()






# r_best = np.array((38,22,44,51,40,50,18,33,49,23,32,2,26,21,37,7,27,48,34,1,6,3,29,11,10,46,4,28,13,31,24,35,25,42,47\
# ,9,5,36,14,8,20,41,19,17,45,15,16,39,30,43,12) ) -1

# object_eval(data_matrix, r_best)
