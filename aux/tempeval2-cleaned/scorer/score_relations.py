"""

== score_relations.py

Simple scorer script for Tempeval relations. It works on one relation task at a time. It
will count how often the system response agrees with the relation type in the gold
standard and calculate the score as follows:

    correct relations / ( correct relations + incorrect relations )

There are actually two versions of this. They are only different in how the value 'NONE'
in the response is dealt with. The first version will skip key-response pairs where the
response in 'NONE'. In the second version, 'NONE' is counted as incorrect. These two
versions essentially are precision and recall scores. The precision will be used as the
primary score for comparisons.

The result is printed to the standard output.


== usage

% python score_relations.py <relations_key> <relations_response>

   <relations_key>       file with the relations gold standard annotations
   <relations_response>  file with the system output

   The two files have to be exactly the same, except that they can have different relation
   types in the fourth column of each line. The script will break otherwise.


"""


import sys


def score_relations(key_relations, response_relations):

    fh1 = open(key_relations)
    fh2 = open(response_relations)

    scores = Scores()
    
    for line in fh1:
        try:
            file, sid, tid, rel = line.strip("\n").split("\t")
            scores.add_key_relation_data(file, sid, tid, rel)
        except ValueError:
            pass

    for line in fh2:
        try:
            file, sid, tid, rel = line.strip("\n").split("\t")
            scores.add_response_relation_data(file, sid, tid, rel)
        except ValueError:
            pass

    scores.calculate_relation_scores()
    #scores.pp_data()
    #scores.pp_counts()
    #scores.pp_score()
    return scores


class Scores:

    def __init__(self):
        self.data = {}
        self.correct = 0.0
        self.incorrect = 0.0
        self.skipped = 0.0

    def add_key_relation_data(self, file, sid, tid, rel):
        position = "%s-%s-%s" % (file, sid, tid)
        self.data[position] = [rel, None]

    def add_response_relation_data(self, file, sid, tid, rel):
        position = "%s-%s-%s" % (file, sid, tid)
        self.data[position][1] = rel
                
    def calculate_relation_scores(self):
        # collect counts...
        for k, r in self.data.values():
            #print
            #print "<<<<< %s >>>>>" % k
            #print "[[[[[ %s ]]]]]" % r
            if k.strip()==r.strip():
                self.correct += 1
            elif r.strip() == 'NONE':
                self.skipped += 1
            else:
                self.incorrect += 1
        # and calculate score
        self.score = self.correct / (self.correct + self.incorrect)
        self.recall = self.correct / (self.correct + self.incorrect + self.skipped)

    def pp_data(self):
        for position in self.data.keys():
            k, r = self.data[position]
            print "%-30s %s %s" % (position, k, r)
        print

    def pp_counts(self):
        print "correct    %4s" % int(self.correct)
        print "incorrect  %4s" % int(self.incorrect)
        print "skipped    %4s" % int(self.skipped)

    def pp_score(self):
        print "score      %.2f" % self.score
        print "recall     %.2f" % self.recall
        
        
if __name__ == '__main__':

    key_relations, response_relations = sys.argv[1:3]
    score_relations(key_relations, response_relations)
