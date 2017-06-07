########
#### Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########

import os
import matplotlib.pyplot as plt
import sys

test = "_build/examples/benchmark.native "
test_old = "_build/examples/benchmark_old.native "

nb_thread = 8
nb_task_init = 2
nb_task_end = 64
nb_item = 10000

f_test = "output/work_stealing_" + str(nb_thread) + "_" + str(nb_task_init) + "_" + str(nb_task_end) + "_" + str(nb_item) + ".txt"
f_test_old = "output/work_sharing_" + str(nb_thread) + "_" + str(nb_task_init) + "_" + str(nb_task_end) + "_" + str(nb_item) + ".txt"


f_name1 = sys.argv[1]
f_name2 = sys.argv[2]
cmd1 = "echo \"" + str(f_name1) + " \"" + " >> " + f_test
print(cmd1)
os.system(cmd1)
cmd2 = "echo \"" + str(f_name2) + " \"" + " >> " + f_test_old
print(cmd2)
os.system(cmd2)

for nb_task in range(nb_task_init, nb_task_end+1, 2):
  cmd_x = "echo \"" + str(nb_task) + " \"" + " >> " + f_test
  cmd = test + str(nb_thread) + " " + str(nb_task) + " " + str(nb_item) + ' >> ' + f_test
  cmd_x_old = "echo \"" + str(nb_task) + " \"" + " >> " + f_test_old
  cmd_old = test_old + str(nb_thread) + " " + str(nb_task) + " " + str(nb_item) + ' >> ' + f_test_old
  print(cmd_x)
  os.system(cmd_x)
  print(cmd)
  tmp = os.system(cmd)
  #print(tmp)
  if tmp == 0:
    print(cmd_x_old)
    os.system(cmd_x_old)
    print(cmd_old)
    os.system(cmd_old)
