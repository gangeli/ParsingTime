#!/usr/bin/python

"""

== score_entities.py

Script to calculate scores for events and timexes, both for the extents and the
attributes. Prints the counts and the calculated statistics to the standard output.


== usage

    % python score_extents.py tokens extent_key extent_res attr_key attribute_res

       tokens      the base-segmentation.tab file
       extent_key  the annotation gold standard with event or timex extents
       extent_res  the response file from the system with event or timex extents
       attr_key    the annotation gold standard with event or timex attributes
       attr_res    the response file from the system with event or timex attributes

    The response files need to have the sdame format as the key files. Some hickups, like
    empty lines will not break the script, but may influence the scores.

    
== extents

The script counts include true positives (tp), true negatives (tn), false positives (fp)
and false negatives (fn). Counts are on a token by token basis. That is, if the key
contains a timex 'Sunday morning', and the response has 'morning', then there will be one
true positive and one false negative (the latter because 'Sunday' was not recognized as
part of the timex).

The statistics calculated are:

    precision  = tp / (tp + fp)
    recall     = tp / (tp + fn)
    accuracy   = (tp + tn) / (tp + tn + fp + fn)
    f1-measure = 2 * (precision * recall) / (precision + recall)

The accuracy is of limited use due to the typically large number of true negatives.


== attributes

Attribtues are compared only for those events and timexes where the key and the response
are identical, that is, systems are not penalized for their attribute scores if the
extents do not match up with the gold standard.

The scores are calculated for each attribute by counting correct and incorrect values and
simply dividing the correct values by the total values. The score is between 0 and 1.

Systems are not penalized for adding attributes that are not in the gold standard.

"""


import sys


def score_entities(tokens, key_extents, response_extents, key_attrs, response_attrs):

    fh1 = open(tokens)
    fh2 = open(key_extents)
    fh3 = open(response_extents)
    fh4 = open(key_attrs)
    fh5 = open(response_attrs)
    

    scores = Scores()
    
    for line in fh1:
        try:
            file, sid, tid = line.strip("\n").split("\t")[0:3]
            scores.initialize(file, sid, tid)
        except ValueError:
            pass
        
    for line in fh2:
        try:
            file, sid, tid, timex, tmid = line.strip("\n").split("\t")[0:5]
            scores.add_key_extent_data(file, sid, tid)
            scores.add_key_mention_data(file, sid, tid, tmid)
        except ValueError:
            pass

    for line in fh3:
        try:
            file, sid, tid, timex, tmid = line.strip("\n").split("\t")[0:5]
            scores.add_response_extent_data(file, sid, tid)
            scores.add_response_mention_data(file, sid, tid, tmid)
        except ValueError:
            pass

    for line in fh4:
        try:
            fields = line.strip("\n").split("\t")
            file, sid, tid = fields[0:3]
            attr, val = fields[6:8]
            scores.add_key_attribute_data(file, sid, tid, attr, val)
        except ValueError:
            pass

    for line in fh5:
        try:
            fields = line.strip("\n").split("\t")
            file, sid, tid = fields[0:3]
            attr, val = fields[6:8]
            scores.add_response_attribute_data(file, sid, tid, attr, val)
        except ValueError:
            pass
			
    scores.calculate_mention_extents()
    scores.calculate_extent_scores()
    scores.calculate_attribute_scores()
    scores.calculate_mention_attribute_scores()
    
    #print
    #scores.pp_data()
    scores.pp_counts()
    scores.pp_stats()
    return scores


class Scores:

    def __init__(self):
        self.data = {}
        self.tp, self.fp, self.tn, self.fn = 0.0, 0.0, 0.0, 0.0
        self.precision, self.recall, self.fmeasure, self.accuracy = None, None, None, None
        self.attribute_counts = {}
        self.attribute_scores = {}
        self.mention_attribute_counts = {}
        self.mention_attribute_scores = {}
        self.attribute_extended_scores = {}
        self.mentions = {} 
        self.mentions[0] = {} 
        self.mentions[1] = {} 		
        self.mentions[2] = {} 		
        self.mentions[3] = {} 		

    def initialize(self, file, sid, tid):
        position = "%s-%s-%s" % (file, sid, tid)
        self.data[position] = [0,0,{},{}]

    def add_key_extent_data(self, file, sid, tid):
        position = "%s-%s-%s" % (file, sid, tid)
        self.data[position][0] = 1
                
    def add_response_extent_data(self, file, sid, tid):
        position = "%s-%s-%s" % (file, sid, tid)
        try:
            self.data[position][1] = 1
        except KeyError:
            pass
            #print "Warning: tried adding response extent to non-existing token %s" % position
               
    def add_key_attribute_data(self, file, sid, tid, attr, val):
        position = "%s-%s-%s" % (file, sid, tid)
        normalized_attr = attr.lower().strip()
        normalized_val = val.lower().strip()
        #if normalized_attr == 'type':
        #    print "[%s:%s]" % (normalized_attr,normalized_val)
        self.data[position][2][normalized_attr] = normalized_val
                
    def add_response_attribute_data(self, file, sid, tid, attr, val):
        position = "%s-%s-%s" % (file, sid, tid)
        normalized_attr = attr.lower().strip()
        normalized_val = val.lower().strip()
        #if normalized_attr == 'type':
        #    print "<%s:%s>" % (normalized_attr,normalized_val)
        try:
            self.data[position][3][normalized_attr] = normalized_val
        except KeyError:
            pass
            #print "Warning: tried adding response attribute to non-existing token %s" % position
    
    def add_key_mention_data(self, file, sid, tid, tmid):
        utmid = "%s %s %s" % (file, sid, tmid)
        tid = int(tid)
        if not self.mentions[2].has_key(utmid):
            self.mentions[2][utmid] = [ tid ]
        else: self.mentions[2][utmid].append(tid)
                
    def add_response_mention_data(self, file, sid, tid, tmid):
        utmid = "%s %s %s" % (file, sid, tmid)
        tid = int(tid)
        if not self.mentions[3].has_key(utmid):
            self.mentions[3][utmid] = [ tid ]
        else: self.mentions[3][utmid].append(tid)
                
    def calculate_mention_extents(self):
        # 0th element is gold key, 1st element is system response
        # each is array of start/end positions
        n = -1		
        for utmid, tids in self.mentions[2].items():
            ptid = -1
            file, sid, tmid = utmid.split(" ")[0:3]		
            for tid in sorted(tids): 
                position = "%s-%s-%s" % (file, sid, tid)
                if ((ptid >= 0) and (tid == ptid + 1)):
                    self.mentions[0][n].append(position)      
                else: 
                    n = n+1
                    self.mentions[0][n] = [position]
                ptid = tid				
        self.nmentions_key = len(self.mentions[0])
        print "gold key mentions: %s" % self.nmentions_key
        n = -1		
        for utmid, tids in self.mentions[3].items():
            ptid = -1
            file, sid, tmid = utmid.split(" ")[0:3]		
            for tid in sorted(tids): 
                position = "%s-%s-%s" % (file, sid, tid)
                if ((ptid >= 0) and (tid == ptid + 1)):
                    self.mentions[1][n].append(position)      
                else: 
                    n = n+1
                    self.mentions[1][n] = [position]
                ptid = tid				
        self.nmentions_resp = len(self.mentions[1])
        print "system response mentions: %s" % self.nmentions_resp
        print
		
    def calculate_mention_attribute_scores(self):
        key_attr_lookup = {}
        for mention_positions in self.mentions[0].values():
            found = 0		
            for position in mention_positions:
                r = self.data[position][0]
                a1 = self.data[position][2]				
                if ((r == 1) and (len(a1) > 0)):
                    found = position
                    break
            if found != 0:
                for position in mention_positions:
                    key_attr_lookup[position] = found
            else:
                for position in mention_positions:
                    key_attr_lookup[position] = position
        for mention_positions in self.mentions[1].values():
            found = 0		
            for position in mention_positions:
                r = self.data[position][0]
                if (r == 1):
                    kp = key_attr_lookup[position]
                    if kp != position:
                        print "map %s to %s" % (position,  kp)
                    a1 = self.data[kp][2]				
                    a2 = self.data[position][3]				
                    if (len(a1) > 0):
                        found = 1
                        for a,v in a1.items():
                            if not self.mention_attribute_counts.has_key(a):
                                self.mention_attribute_counts[a] = {'correct': 0.0, 'incorrect': 0.0}
                            if a2.get(a) == v:
                                self.mention_attribute_counts[a]['correct'] += 1
                            elif a2.get(a) == '' and v == 'none' and a == 'modality':
                                self.mention_attribute_counts[a]['correct'] += 1
                            else:
                                self.mention_attribute_counts[a]['incorrect'] += 1
                        break
#            if found == 0:
#                self.mention_attribute_counts[a]['incorrect'] += 1					
        for attr, counts in self.mention_attribute_counts.items():
            correct = counts['correct']
            incorrect = counts['incorrect']
            self.mention_attribute_scores[attr] = {}
            self.mention_attribute_scores[attr]['m'] = correct / (correct + incorrect) 
            self.mention_attribute_scores[attr]['p'] = correct / (self.nmentions_resp) 
            self.mention_attribute_scores[attr]['r'] = correct / (self.nmentions_key) 
            try:
                p = self.mention_attribute_scores[attr]['p']
                r = self.mention_attribute_scores[attr]['r']
                self.mention_attribute_scores[attr]['f1'] = 2 * (p*r) / (p+r)
            except ZeroDivisionError:
                self.mention_attribute_scores[attr]['f1'] = 0.0

    def calculate_extent_scores(self):
        # collect counts...
        # data is array with each element corresponding to a token
        # each token is array of 4 elements: 
        #  element 0 - 1/0 indicating if in gold key, 
        #  element 1 - 1/0 indicating if in system response,
        #  element 2 - map of attribute to attribute value (from gold key)
        #  element 3 - map of attribute to attribute value (from system response)
        for k, r, a1, a2 in self.data.values():
            if k==1 and r==1: self.tp += 1
            if k==1 and r==0: self.fn += 1
            if k==0 and r==1: self.fp += 1
            if k==0 and r==0: self.tn += 1
        # and calculate
        self.precision = self.tp / (self.tp + self.fp)
        self.recall = self.tp / (self.tp + self.fn)
        try:
            self.fmeasure = 2 * (self.precision * self.recall) / (self.precision + self.recall)
        except ZeroDivisionError:
            self.fmeasure = 0.0
        self.accuracy = (self.tp + self.tn) / (self.tp + self.tn + self.fp + self.fn)

    def calculate_attribute_scores(self):
        # collect counts...
        for k, r, a1, a2 in self.data.values():
            #print k,r,a1,a2
            if k==1 and r==1:
                # Note: key only has one entry per mention (so the overall measure is per temporal mention)
                # Better for system response to include entries for all tokens, because don't know which one the
                # gold key will be for...
                for a,v in a1.items():
                    #if a in ['type', 'value','modality']:
                    #    print a,v, a2.get(a)
                    if not self.attribute_counts.has_key(a):
                        self.attribute_counts[a] = {'correct': 0.0, 'incorrect': 0.0}
                    if a2.get(a) == v:
                        self.attribute_counts[a]['correct'] += 1
                    elif a2.get(a) == '' and v == 'none' and a == 'modality':
                        self.attribute_counts[a]['correct'] += 1
                    else:
                        self.attribute_counts[a]['incorrect'] += 1
        # and calculate
        for attr, counts in self.attribute_counts.items():
            correct = counts['correct']
            incorrect = counts['incorrect']
            self.attribute_scores[attr] = correct / (correct + incorrect) 
            # Extended scorers
            self.attribute_extended_scores[attr] = {}
            # % correct out of all our guesses
            self.attribute_extended_scores[attr]['p'] = correct / (self.nmentions_resp) 
            # % correct out of total number of gold mentions (same denominator for all systems)
            self.attribute_extended_scores[attr]['r'] = correct / (self.nmentions_key) 
            try:
                p = self.attribute_extended_scores[attr]['p']
                r = self.attribute_extended_scores[attr]['r']
                self.attribute_extended_scores[attr]['f1'] = 2 * (p*r) / (p+r)
            except ZeroDivisionError:
                self.attribute_extended_scores[attr]['f1'] = 0.0
        
    def pp_data(self):
        positions =  self.data.keys()
        positions.sort()
        for position in positions:
            k, r, ka, ra = self.data[position]
            print "%-30s %s %s  %s %s" % (position, k, r, ka ,ra)
        print
        
    def pp_counts(self):
        print "true positives:   %s" % int(self.tp)
        print "true negatives:   %s" % int(self.tn)
        print "false positives:  %s" % int(self.fp)
        print "false negatives:  %s" % int(self.fn)
        print
        for attr, counts in self.attribute_counts.items():
            print "attribute %s: +%s -%s" % (attr, counts['correct'], counts['incorrect'])
        print
        for attr, counts in self.mention_attribute_counts.items():
            print "mention attribute %s: +%s -%s" % (attr, counts['correct'], counts['incorrect'])
        print
        
    def pp_stats(self):
        print "precision   %.3f" % self.precision
        print "recall      %.3f" % self.recall
        print "f1-measure  %.3f" % self.fmeasure
        print "accuracy    %.3f" % self.accuracy        
        print
        for attr, score in self.attribute_scores.items():
            extended_scores = self.attribute_extended_scores[attr]
            print "attribute %-10s %.3f | %.3f %.3f %.3f" % (attr, score, extended_scores['p'], extended_scores['r'], extended_scores['f1'])
        print
        for attr, scores in self.mention_attribute_scores.items():
            print "mention attribute %-10s %.3f | %.3f %.3f %.3f" % (attr, scores['m'], scores['p'], scores['r'], scores['f1'])
        print


        
if __name__ == '__main__':

    tokens, key_extents, response_extents, key_attrs, response_attrs = sys.argv[1:6]
    score_entities(tokens, key_extents, response_extents, key_attrs, response_attrs)
