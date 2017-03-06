import sys
sys.path.append("./cobe/")
from cobe.brain import Brain

print "Learning from all"

text = []

for line in sys.stdin:
    text.append(line)

print "Starting"

b = Brain("cobe.brain")
map(lambda x: b.learn(x), text)

print "Done"
