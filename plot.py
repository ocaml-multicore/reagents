########
#### Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########

import sys
import matplotlib.pyplot as plt

l_f = sys.argv[1:]

x_l = []
y_l = []
title_l = []

for f_name in l_f:
  f = open(f_name, "r")
  content = f.read()
  print(content)
  f.close()

  x = []
  y = []

  lines = content.split('\n')
  print(lines)
  for i in range(1, len(lines), 2):
    if lines[i] != '':
      x.append(lines[i+1])
      y.append(lines[i])

  title_l.append(lines[0])

  x_l.append(x)
  y_l.append(y)

print(x_l)
print(y_l)

plt.title('Scheduler benchmark (MSQueue 10000 produce and consume)')
plt.xlabel('Number of tasks')
plt.ylabel('execution time (in seconds)')

for i in range(len(l_f)):
  plt.plot(x_l[i], y_l[i], label=(title_l[i]).title())

plt.legend()
plt.show()
