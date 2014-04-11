# Python prog to convert .lin BBO files to .pbn files
#
# Paul Bethe
#
#
#Copyright (c) 2006, Paul Bethe
#       All rights reserved.
#
# Redistribution and use in source and binary forms,
# with or without modification, are permitted provided
# that the following conditions are met:
#
# 	1. Redistributions of source code must retain the
#	   above copyright notice, this list of conditions
#	   and the following disclaimer. 
#
#	2. Redistributions in binary form must reproduce the
#       above copyright notice, this list of conditions
#       and the following disclaimer in the documentation
#       and/or other materials provided with the
#       distribution. 
#
#	3. Neither the name of Paul Bethe or the
#       names of its contributors may be used to endorse
#       or promote products derived from this software
#       without specific prior written permission. 
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
# THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
# USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
# IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
# USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.


import sys, string, time


#### global defs

suits = ( "S", "H", "D", "C" )

vuln = ("None", "NS", "EW", "All")
# starts at 0 == 16 which is "EW"
vulh = [2,0,1,2,3,1,2,3,0,2,3,0,1,3,0,1,2]
vul_lookup = [ vuln[x] for x in vulh ]

ident_fields=("Event", "Site", "Date", "Board", "West", "North", "East", "South", "Dealer",
             "Vulnerable", "Deal", "Scoring", "Declarer", "Contract", "Result",
              "HomeTeam", "Room", "Round", "Score", "Stage", "VisitTeam")
scoring_methods=("MP", "MatchPoints", "IMP", "Cavendish", "Chicago", "Rubber", "BAM", "Instant")

room_map = { 'o' : "Open", 'c' : "Closed" }

# since N is board one, W is board '0'
dealer_array = ['W', 'N', 'E', 'S']

db_under_tricks = { False : (100,300,500,800,1100,1400,1700,2000),
                    True : (200,500,800,1100,1400,1700,2000) }

under_tricks = { False : 50, True : 100}

game_bonus =  { False : 300, True : 500}
slam_bonus =  [{ False : 800, True : 1250}, { False : 1300, True : 2000} ]

oneSuit = ['A','K','Q','J','T','9','8','7','6','5','4','3','2']

####
# helper functions
###

def cdCmp (left, right):
    if oneSuit.index(left) < oneSuit.index(right):
        return True
    return False

def suitSort(suit):
    if len(suit) <= 1:
        return suit
    p = len(suit)/2
    left = "".join ([ x for x in suit[:p] + suit[p+1:] if cdCmp(x, suit[p]) ])
    right = "".join ([ x for x in suit[:p] + suit[p+1:] if cdCmp (suit[p], x) ])
    return suitSort(left) + suit[p] + suitSort(right)

def cardSort (hand):
    return ".".join (map(suitSort, hand.split(".")))

def lClone (list):
    return [] + list

def getDeck ():
    return lClone(oneSuit), lClone(oneSuit), lClone(oneSuit), lClone(oneSuit)

def writeToken (id, val, handle=sys.stdout):
    handle.write ("[" + id + " \"" + str(val) + "\"]\n")
    
def writeData (id, data, handle=sys.stdout):
    if data.has_key (id):
        writeToken (id, data[id], handle)
        

def write4 (list, handle=sys.stdout):
    if len(list) < 1:
        return
    else:
        handle.write (string.join (list [:4]) + "\n")
        write4 (list[4:], handle)

# return True if the 'cardNext' will beat the 'cardCurrent' to the
# current trick, given a trump suit of 'trumps'
#
def br_beats (cardNext, cardCurrent, trumps):
    # if same suit...
    if cardNext[0] == cardCurrent[0]:
        # and next card is closer to Ace than current
        if oneSuit.index(cardNext[1]) < oneSuit.index(cardCurrent[1]):
            #print cardNext + " beats the " + cardCurrent
            return True
        else:
            return False
    elif cardNext[0].upper() == trumps.upper():
        #print cardNext + " trumps the " + cardCurrent
        return True
    else:
        return False


###
# one result
###


class PBNResult:
    def __init__ (self, istr):
        if istr == "PASS" or istr =="P":
            self.passout = True
            self.contract = "PASS"
            self.declarer = "N"
            self.tricks = 0
        else:
            self.passout = False
            self.contract = istr[:2]
            self.declarer = istr[2]
            istr = istr[3:]
            if istr[0:2].upper() == "XX":
                self.contract = self.contract + "XX"
                istr = istr[2:]
            elif istr[0].upper() == "X":
                self.contract = self.contract + "X"
                istr = istr[1:]
            elif istr[0].upper() == "R":
                self.contract = self.contract + "XX"
                istr = istr[1:]
            if self.contract[1] == "N":
                self.contract = self.contract[0] + "NT" + self.contract[2:]
            self.tricks = 6 + int(self.contract[0])
            self.cont_tricks = self.tricks
            if istr != "=":
                self.tricks = self.tricks + int(istr)
            self.tricks = self.tricks

    
    def calcScore (self, Vul):
        if self.passout:
            return "+0"
        if self.tricks < self.cont_tricks:
            if self.contract[-1:] == "X":
                min = db_under_tricks[Vul][self.cont_tricks - self.tricks - 1]
            else:
                min = (self.cont_tricks - self.tricks) * under_tricks[Vul]
            if self.contract[-2:] == "XX":
                min = 2 * min
            return "-" + str(min)
        else:
            bid_tricks = self.cont_tricks - 6
            db_bonus = 0
            if self.contract[1] in ("D", "C"):
                bid_score = 20*bid_tricks
                over_tricks = 20 * (self.tricks  - self.cont_tricks)
            else:
                bid_score = 30*bid_tricks
                over_tricks = 30 * (self.tricks  - self.cont_tricks)
            if self.contract[1] == "N":
                bid_score = bid_score + 10

            if self.contract[-2:] == "XX":
                bid_score = bid_score * 4
                db_bonus = 100
                over_tricks = 4 * under_tricks[Vul] * (self.tricks  - self.cont_tricks)
            elif self.contract[-1:] == "X":
                bid_score = bid_score * 2
                db_bonus = 50
                over_tricks = 2 * under_tricks[Vul] * (self.tricks  - self.cont_tricks)

            if bid_tricks >= 6:
                score = bid_score + over_tricks + db_bonus + slam_bonus[bid_tricks-6][Vul]
            elif bid_score >= 100:
                score = bid_score + over_tricks + db_bonus + game_bonus[Vul]
            else:
                score = bid_score + over_tricks + db_bonus + 50
            return "+" + str(score)
            
###
# one deal
###

class PBNDeal:
    def __init__(self, id, players=[], linFile=None):
        self.id = id
        self.data = {}
        self.auction=[]
        self.auctionNotes=[]
        self.play=[]
        self.playNotes=[]
        self.last_e = False
        self.setBoard (id[1:])
        if linFile: self.readFromLIN(linFile)

    def writePlay(self, handle=sys.stdout):        
        write4 (self.play, handle)

    def write(self, handle=sys.stdout):
        for id in ident_fields: writeData(id, self.data, handle)
        writeData ("Auction", self.data, handle)
        write4 (self.auction, handle)
        
        for val in self.auctionNotes: writeToken("Note", val, handle)

        writeData ("Play", self.data, handle)
        self.writePlay (handle)
        if self.data.has_key("Play"):
            handle.write ("*\n")
        for val in self.playNotes: writeToken("Note", val, handle) 
        handle.write ("\n");

    def setPlayers (self, glob_pl):
        for dir, pl in zip (("South", "West", "North", "East"), glob_pl):
            self.data[dir] = pl

    def addBid (self, bid):
        bmap = { 'p': "Pass", 'd' : "X", 'r' : "XX" , 'p!': "Pass!", 'd!' : "X!", 'r!' : "XX!" }
        if bmap.has_key (bid):
            bid = bmap[bid]
        if bid[1:2] == "N" and bid[1:3] != "NT":
            bid = bid[0] + "NT" + bid[2:]
        self.auction.append(bid)
        if bid[-1] == "!":
            self.last_e = False
            self.auction[-1] = self.auction[-1][:-1]
            self.addNote("alerted")
            self.last_e = True
        else:
            self.last_e = False
        
    def addNote (self, note):
        # replace 'artificial' with actual note
        if self.last_e:
            num = len(self.auctionNotes)
            #print ("Note %d changed to : " + note) % num
            self.auctionNotes[-1] = str(num)+ ":" + note
        else:
            num = 1+ len(self.auctionNotes)
            self.auctionNotes.append (str(num)+ ":" + note)
            self.auction[-1] = self.auction[-1] + " =" + str(num) + "="
        


    def setDeal(self, deal):
        for s in suits:
            deal = deal.replace (s, ".")
        ha = string.split (deal, ",")
        south = ha[0][2:]
        west = ha[1][1:]
        north = ha[2][1:]

        s,h,d,c = getDeck()
        for play in (south, west, north):
            for tmp_cards, this_suit in zip (play.split("."), (s,h,d,c) ):
                for card in tmp_cards:
                    # print play + "//" + card
                    this_suit.remove(card)
        east = ".".join (map (lambda x: "".join(x), (s,h,d,c)))
        south, west, north = map (cardSort, (south, west, north))
        
        dealStr = "S:" + " ".join ((south, west, north, east))
        self.data['Deal'] = dealStr
        self.south = south

    def setBoard(self,b):
        self.data['Board'] = int(b)
        self.data['Vulnerable'] = vul_lookup[self.data['Board'] % 16]

    # if no 'result' available - determine from the data at hand (tedious)
    def intuitResult(self):
        pass

    def process (self, glob_data, id, players, result=None):
        for k,v  in glob_data.iteritems():  self.data[k] = v
        self.data['Room'] = room_map[id[0]]
        if not self.data.has_key ('South'):
            self.setPlayers (players)
        if not self.data.has_key ('Board'):
            self.data['Board'] = int(id[1:])
        if result:
            r = PBNResult (result)
            self.data['Contract'] = r.contract
            self.data['Result'] = r.tricks
            self.data['Declarer'] = r.declarer
            declVul = self.data["Vulnerable"] <> "None" and r.declarer in self.data["Vulnerable"] or self.data["Vulnerable"] == "All"
            self.data['Score'] = r.calcScore(declVul)
        else:
            self.intuitResult ()
        dealer = dealer_array[self.data['Board'] % 4]
        self.data['Dealer'] = dealer
        if len(self.auction) > 0:
            self.data['Auction'] = dealer
        if len(self.play) > 0:
            self.data['Play'] = dealer_array[(dealer_array.index(self.data['Declarer']) + 1) % 4]
        while (len(self.play) % 4) != 0:
            self.play.append ("-")

        # need to figure out who won the last trick,
        # and re-orient this play to 'Play' first
        # but LIN order is the winner of the last trick...
        #s, h, d, c = self.south.split (".")
        trumps = self.data['Contract'][1]
        wonby = [0] * 13
        for trick in range (len(self.play) / 4):
            for player in (1,2,3):
                if br_beats (self.play[trick*4 + player], self.play[trick*4 + wonby[trick]],
                             trumps):
                    wonby[trick] = player

        oldplay = self.play
        self.play = [] + oldplay[:4]
        for trick in range(1, len(oldplay) / 4):
            index2 = 4 * trick
            fsize = 4 - wonby[trick-1]
            index1 = index2 + fsize
            self.play = self.play  + oldplay[index1:index2 + 4] + oldplay[index2:index1]
            wonby[trick] = (wonby[trick] - fsize) % 4
            
###
# set of deals
###


class PBNDealSet:
    def __init__ (self):
        self.openPlayers=[]
        self.closedPlayers=[]
        self.deals={}
        self.data = {}
        self.results=[]
        self.high_deal = 0
        self.first_board = 1
        self.boards = ""
        self.lastdeal = None

    def setPlayers (self, pl):
        tok = map (string.strip, string.split (pl, ","))
        #if len(self.openPlayers) < 4: self.openPlayers = tok[:4]
        # maybe a LastName, F
        # so check for 1 character names that are 1st names
        index = 0
        while len (tok) > 8 and index < len(tok):
            if len(tok[index]) == 1:
                tok[index - 1] = tok[index - 1] + ", " + tok[index]
                del(tok[index])
            index = index + 1
        
        if len(tok) > 3:
            self.openPlayers = tok[:4]
        if len(tok) > 7: # and len(self.closedPlayers) < 4:
            self.closedPlayers=tok[4:]
        if self.lastdeal:
            self.lastdeal.setPlayers(self.whichPlayers(self.lastdeal.id))        

    def getDeal (self, id):
        if not self.deals.has_key  (id):
            self.deals[id] = PBNDeal(id, self.whichPlayers(id))
            if self.high_deal < int(id[1:]): self.high_deal = int(id[1:])
        self.lastdeal = self.deals[id]
        return self.deals[id]

    def whichPlayers(self,id):
        if id[0] == 'o':
            return self.openPlayers
        else:
            return self.closedPlayers

    def getResult (self, id):
        if id[0] == 'o':
            offset = 2 * -int(self.first_board)
        else:
            offset = 2 * -int(self.first_board) + 1
        index = int(id[1:]) * 2 + offset
        #print "id = " + id + " offset = %d" % offset
        #print "size = %d ; index = %d" % (len(self.results), index)
        return self.results[index]

    def process(self):
        if self.boards <> "":
            btok = self.boards.split (",")
            for i, bn in enumerate (btok):
                for room in ('c', 'o'):
                    key = room + str(i+1)
                    if self.deals.has_key(key):
                        self.deals[key].setBoard (bn)

        # add deals w/ results, but no play info
        if len(self.closedPlayers) > 0:
            for bn in range(int(self.first_board), 1+ int(self.num_boards)):
                ckey = 'c' + str(bn)
                okey = 'o' + str(bn)
                for a, b in ((ckey, okey), (okey, ckey)) :
                    if not self.deals.has_key(a) and self.deals.has_key(b) and self.getResult(a) <> "":
                        cdeal = self.getDeal (a)
                        odeal = self.getDeal (b)
                        if odeal.data.has_key("Deal"):
                            cdeal.data["Deal"] = odeal.data["Deal"]
                        print "Created for missing " + a + " using " + b
                    elif not self.deals.has_key(a) and not self.deals.has_key(b) and self.getResult(a) <> "":
                        # a result exists, but neither room has info
                        cdeal = self.getDeal (a)
                    
        
        for id, deal in self.deals.iteritems():
            deal.process(self.data, id, self.whichPlayers(id), self.getResult(id))

        

                
    def __getitem__(self, i):
        self.lastdeal = self.deals['o'+str(i+1)]
        return self.lastdeal

    def write(self, handle=sys.stdout):
        handle.write ("% PBN 2.0\n% EXPORT\n%\n")        
        i=1
        while i <= self.high_deal:
            for room in ('c', 'o'):
                key = room + str(i)
                if self.deals.has_key(key):
                    self.deals[key].write(handle)
            i = i + 1
        
        

### body of code ##

linVul = { '0' : 'None', 'o' : 'None', 'n' : 'NS', 'e' : 'EW', 'b' : 'All' }
linScore = { 'I': scoring_methods[2] , 'P' : scoring_methods[0] , 'B' : scoring_methods[6]}

def createPBNFromLIN(datestr, filename, sitestr="BridgeBase"):
    f = open (filename,"r")
    lines=f.readlines()
    f.close()

    allDeals = PBNDealSet()
    vgData = allDeals.data
    vgData['Date'] = datestr
    vgData['Site'] = sitestr

    for line in lines:
        tok = string.split (line,"|")
        while len(tok) > 0:
            if tok[0] == "qx":
                currentDeal=allDeals.getDeal(string.split(tok[1], ",")[0])
            elif tok[0] == "vg":
                vgData['Event'], vgData['Stage'], scoring, allDeals.first_board, allDeals.num_boards, vgData['HomeTeam'], \
                                 hscore, vgData['VisitTeam'], vscore = string.split(tok[1], ",")[:9]
                vgData['Scoring'] = linScore[scoring]
            elif tok[0] == "bn":
                allDeals.boards = tok[1]
            elif tok[0] == "rs":
                allDeals.results = string.split(tok[1], ",")
            elif tok[0] == "pn":
                allDeals.setPlayers (tok[1])
            elif tok[0] == "sv":
                currentDeal.data["Vulnerable"] = linVul[tok[1]]
            elif tok[0] == "mc":
                currentDeal.data['Result'] = tok[1]
            elif tok[0] == "md":
                currentDeal.setDeal (tok[1])
            elif tok[0] == "mb":
                currentDeal.addBid(tok[1])
            elif tok[0] == "pc":
                currentDeal.play.append(tok[1].upper())
            elif tok[0] == "an":
                currentDeal.addNote (tok[1])
            else:
                pass

            tok = tok[2:]
            
    # basic data was read
    # now post process and fill-out
    allDeals.process()
            
    return allDeals


###  running the program ###

def main():
    ourdir = ""
    docdir = ""
    inputdir=""
    outdir = "pbns/"
    web=False
    import optparse
    parser = optparse.OptionParser()
    parser.add_option("-d", "--date", dest="date",
                      default=None, help="event date", metavar="Date")
    parser.add_option("-s", "--site", dest="site",
                      default="BridgeBase", help="Site where event was played", metavar="Site")
    (options, args) = parser.parse_args()

    if len (sys.argv) > 1:        
        if not options.date or len(args) < 1:
            parser.print_usage()
            sys.exit(1)

        outdir = ""
        datestr = options.date
        # make sure is right
        time.strptime (datestr, "%Y.%m.%d") 
        filename = args[0]
        if len (args) > 1:
            outfile = args[1]
        else:
            if filename[-4:] == ".lin":
                outfile = filename[:-4] + ".pbn"
            else:
                outfile = filename + ".pbn"
    else:
        try:
            import cgi
            print "getting fields"
            The_Form = cgi.FieldStorage()
            inputname = string.strip(The_Form["name"].value)  #required field is filename
            print "input file name is ",inputname
            ourdir = "lins/"
            if The_Form.has_key('stage'):
                newstage = string.strip(The_Form["stage"].value)
            print "stage is ",newstage
            if The_Form.has_key('doc'):
                docdir = string.strip(The_Form["doc"].value)
            if The_Form.has_key('date'):
                datestr =  string.strip(The_Form["date"].value)
                print "daTE is ",datestr

            print "url file name is ",docname
            filename = docdir + ourdir + filename
            web=True
            print "Content-type: text/html"
            print
            print "<pre>"

        except:
            try:
                import win32ui
                d=win32ui.CreateFileDialog(1)
                d.DoModal()
                inputpath=d.GetPathName()
                inputsplit=string.split(inputpath,"\\")
                inputdir = string.join(inputsplit[:-1],"\\") + "\\"
                filename=d.GetFileName()
                outfile = filename.replace(".lin",".pbn")
                docdir = ""
                outdir = inputdir.replace("lin","pbn")
                print inputdir,filename
            except:
                parser.print_usage()
                print "AND win32 GUI is not supported by this OS"
                sys.exit(1)
            datestr = string.strip(raw_input("Enter date of event"))
            if not datestr:
                datestr = time.strftime("%Y.%m.%d")
    print "Starting PBN2htm conversion of ",filename
    d = createPBNFromLIN (datestr, inputdir + filename, options.site)
    outhandle = open (docdir + outdir + outfile, "w")    
    d.write(outhandle)
    outhandle.close()
    print "Conversion of ",filename," to ",docdir,outdir,outfile," is complete"
    if web:
        print "<br><br>"
        print '<a href=/cgi-bin/PBN2html.py?name="',outfile,"?doc=",docdir,'">',
        print "Click here to continue and make the HTML pages</a>"

if __name__ == "__main__":
    main()
