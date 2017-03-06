import markovify

import sys
reload(sys)
sys.setdefaultencoding('utf8')

name = sys.argv[1]

text = ""

for line in sys.stdin:
    if line.find("markov") == -1:
        text += line + " "

# Build the model.
text_model = markovify.Text(text, state_size = 1)

on = text_model.to_json()

f = open(name + "_markov", "w")
f.write(on)
f.close()
print "Done"
