# -*- coding: utf-8 -*-
import sys
sys.path.append("./cobe/")
from cobe.brain import Brain

text = ""

for line in sys.stdin:
    text += line + " "

b = Brain("cobe.brain")
print b.reply(text, loop_ms = 2500)
