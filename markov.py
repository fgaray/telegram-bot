import markovify

import sys
reload(sys)
sys.setdefaultencoding('utf8')

name = sys.argv[1]

try:
    f = open(name + "_markov")
    json = f.read()
    reconstituted_model = markovify.Text.from_json(json)

    print name + " dice:"
    print reconstituted_model.make_short_sentence(140, tries=1000)
except:
    print name + " not trained"

