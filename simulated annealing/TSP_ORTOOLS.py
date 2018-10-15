#!/usr/bin/env python
# This Python file uses the following encoding: utf-8
# Copyright 2015 Tin Arm Engineering AB
# Copyright 2018 Google LLC
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# https://github.com/google/or-tools/blob/master/ortools/constraint_solver/routing_flags.cc
# https://stackoverflow.com/questions/39328358/or-tools-consistently-returns-very-sub-optimal-tsp-solution

from __future__ import print_function
from six.moves import xrange
from ortools.constraint_solver import pywrapcp
from ortools.constraint_solver import routing_enums_pb2

###########################
# Problem Data Definition #
###########################
# def create_data_model():
#   """Stores the data for the problem"""
#   data = {}
#   # Locations in block unit
#   _locations = \
#           [(4, 4), # depot
#            (2, 0), (8, 0), # locations to visit
#            (0, 1), (1, 1),
#            (5, 2), (7, 2),
#            (3, 3), (6, 3),
#            (5, 5), (8, 5),
#            (1, 6), (2, 6),
#            (3, 7), (6, 7),
#            (0, 8), (7, 8)]
 
# #   data["locations"] = [(l[0] * 114, l[1] * 80) for l in _locations]
#   data["locations"] = [(l[0] , l[1] ) for l in _locations]

#   data["num_locations"] = len(data["locations"])
#   data["num_vehicles"] = 1
#   data["depot"] = 0
#   return datadef create_data_model():



  """Stores the data for the problem"""
  data = {}
  # Locations in block unit
  _locations = \
         [(35, 35 ),
        (41,  49),  (35,  17), 
        (55,  45),  (55,  20), 
        (15,  30),  (25,  30), 
        (20,  50),  (10,  43), 
        (55,  60),  (30,  60), 
        (20,  65),  (50,  35), 
        (30,  25),  (15,  10), 
        (30,   5),  (10,  20), 
        (5  ,30 ), (20,  40), 
        (15,  60),  (45,  65), 
        (45,  20),  (45,  10), 
        (55,   5),  (65,  35), 
        (65,  20)]

  data["locations"] = [(l[0] , l[1] ) for l in _locations]

  data["num_locations"] = len(data["locations"])
  data["num_vehicles"] = 1
  data["depot"] = 0
  return data

def manhattan_distance(position_1, position_2):
  """Computes the Manhattan distance between two points"""
  return (
      abs(position_1[0] - position_2[0]) + abs(position_1[1] - position_2[1]))

def create_distance_evaluator(data):
  """Creates callback to return distance between points."""
  _distances = {}
  # precompute distance between location to have distance callback in O(1)
  for from_node in xrange(data["num_locations"]):
    _distances[from_node] = {}
    for to_node in xrange(data["num_locations"]):
      if from_node == to_node:
        _distances[from_node][to_node] = 0
      else:
        _distances[from_node][to_node] = (
            manhattan_distance(data["locations"][from_node],
                               data["locations"][to_node]))

  def distance_evaluator(from_node, to_node):
    """Returns the manhattan distance between the two nodes"""
    return _distances[from_node][to_node]

  return distance_evaluator
  
  
  ###########
# Printer #
###########
def print_solution(routing, assignment):
  """Prints assignment on console"""
  print('Objective: {}'.format(assignment.ObjectiveValue()))
  index = routing.Start(0)
  plan_output = 'Route:\n'
  distance = 0
  while not routing.IsEnd(index):
    plan_output += ' {} ->'.format(routing.IndexToNode(index))
    previous_index = index
    index = assignment.Value(routing.NextVar(index))
    distance += routing.GetArcCostForVehicle(previous_index, index, 0)
  plan_output += ' {}\n'.format(routing.IndexToNode(index))
  plan_output += 'Distance of the route: {}m\n'.format(distance)
  print(plan_output)



########
# Main #
########
def main():
  """Entry point of the program"""
  # Instantiate the data problem.
  data = create_data_model()

  # Create Routing Model
  routing = pywrapcp.RoutingModel(
      data["num_locations"],
      data["num_vehicles"],
      data["depot"])
  # Define weight of each edge
  distance_evaluator = create_distance_evaluator(data)
  routing.SetArcCostEvaluatorOfAllVehicles(distance_evaluator)

  # Setting first solution heuristic (cheapest addition).
  search_parameters = pywrapcp.RoutingModel.DefaultSearchParameters()
  search_parameters.first_solution_strategy = (
      routing_enums_pb2.FirstSolutionStrategy.PATH_CHEAPEST_ARC)

  search_parameters.local_search_metaheuristic = routing_enums_pb2.LocalSearchMetaheuristic.GUIDED_LOCAL_SEARCH
  # OBJECTIVE_TABU_SEARCH
  search_parameters.time_limit_ms = 200000
# Solve the problem.
  assignment = routing.SolveWithParameters(search_parameters)
  print_solution(routing, assignment)

if __name__ == '__main__':
  main()














def create_data_model():
#   """Stores the data for the problem"""
#   data = {}
#   # Locations in block unit
#   _locations = \
#          [(3    ,  1817),
# (1782 ,   995),
# (3896 ,   742),
# (1829 ,   812),
# (1286 ,   550),
# (3017 ,   108),
# (2132 ,   1432),
# (2000 ,   1110),
# (3317 ,   1966),
# (1729 ,   1498),
# (2408 ,   1747),
# (3292 ,   152),
# (193  ,   1210),
# (782  ,   1462),
# (2503 ,   352),
# (1697 ,   1924),
# (3821 ,   147),
# (3370 ,   791),
# (3162 ,   367),
# (3938 ,   516),
# (2741 ,   1583),
# (2330 ,   741),
# (3918 ,   1088),
# (1794 ,   1589),
# (2929 ,   485),
# (3453 ,   1998),
# (896  ,   705),
# (399  ,   850),
# (2614 ,   195),
# (2800 ,   653),
# (2630 ,   20),
# (563  ,   1513),
# (1090 ,   1652),
# (2009 ,   1163),
# (3876 ,   1165),
# (3084 ,   774),
# (1526 ,   1612),
# (1612 ,   328),
# (1423 ,   1322),
# (3058 ,   1276),
# (3782 ,   1865),
# (347  ,   252),
# (3904 ,   1444),
# (2191 ,   1579),
# (3220 ,   1454),
# (468  ,   319),
# (3611 ,   1968),
# (3114 ,   1629),
# (3515 ,   1892),
# (3060 ,   155),
# (3140 ,   1401),
# (556  ,   1056),
# (3675 ,   1522),
# (1182 ,   1853),
# (3595 ,   111),
# (962  ,   1895),
# (2030 ,   1186),
# (3507 ,   1851),
# (2642 ,   1269),
# (3438 ,   901),
# (3858 ,   1472),
# (2937 ,   1568),
# (376  ,   1018),
# (839  ,   1355),
# (706  ,   1925),
# (749  ,   920),
# (298  ,   615),
# (694  ,   552),
# (387  ,   190),
# (2801 ,   695),
# (3133 ,   1143),
# (1517 ,   266),
# (1538 ,   224),
# (844  ,   520),
# (2639 ,   1239),
# (3123 ,   217),
# (2489 ,   1520),
# (3834 ,   1827),
# (3417 ,   1808),
# (2938 ,   543),
# (71   ,   1323),
# (3245 ,   1828),
# (731  ,   1741),
# (2312 ,   1270),
# (2426 ,   1851),
# (380  ,   478),
# (2310 ,   635),
# (2830 ,   775),
# (3829 ,   513),
# (3684 ,   445),
# (171  ,   514),
# (627  ,   1261),
# (1490 ,   1123),
# (61   ,   81),
# (422  ,   542),
# (2698 ,   1221),
# (2372 ,   127),
# (177  ,   1390),
# (3084 ,   748),
# (1213 ,   910)]

#   data["locations"] = [(l[0] , l[1] ) for l in _locations]

#   data["num_locations"] = len(data["locations"])
#   data["num_vehicles"] = 1
#   data["depot"] = 0
#   return data
