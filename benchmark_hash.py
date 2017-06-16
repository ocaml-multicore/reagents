########
#### Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########

import os
import sys

test = "_build/examples/hash_test.native "

nb_thread = 16
n_init = 0
step = 10000
n_end = 300000

f_test = "output_hash/std_hash" + str(nb_thread) + "_" +  str(n_init) + "_" + str(step) + "_" + str(n_end) + ".txt"

f_name1 = sys.argv[1]
cmd1 = "echo \"" + str(f_name1) + " \"" + " >> " + f_test
print(cmd1)
os.system(cmd1)

for n in range(n_init, n_end+1, step):
  cmd_x = "echo \"" + str(n) + " \"" + " >> " + f_test
  cmd = test + str(nb_thread) + " " + str(n) + ' >> ' + f_test
  print(cmd_x)
  os.system(cmd_x)
  print(cmd)
  tmp = os.system(cmd)
