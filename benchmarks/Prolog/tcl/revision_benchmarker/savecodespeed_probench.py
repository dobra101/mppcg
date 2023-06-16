# -*- coding: utf-8 -*-
####################################################
# save probench result data to codespeed (server must be running) #
####################################################
from datetime import datetime
import urllib, urllib2, sys

temp = datetime.today()

def setdata():
  if sys.argv[8] == "0":
    data = {
      'commitid': sys.argv[3],
      'project': sys.argv[2],
      'revision_date': datetime.today(), # Optional. Default is taken
                          # either from VCS integration or from current date
      'executable': sys.argv[6],
      'benchmark': sys.argv[4],
      'environment': sys.argv[7], # environment must always exist
      'result_value': sys.argv[5],
      'result_date': datetime.today(), # use current date instead of revision date
      'std_dev': sys.argv[10], # Optional. Default is blank
      'max': sys.argv[11], # Optional. Default is blank
      'min': sys.argv[12], # Optional. Default is blank
    }
  else:
    revdate = "%s %s" % (sys.argv[8], sys.argv[9])
    data = {
      'commitid': sys.argv[3],
      'project': sys.argv[2],
      'revision_date': revdate,
      'executable': sys.argv[6],
      'benchmark': sys.argv[4],
      'environment': sys.argv[7], # environment must always exist
      'result_value': sys.argv[5],
      'result_date': revdate,
      'std_dev': sys.argv[10], # Optional. Default is blank
      'max': sys.argv[11], # Optional. Default is blank
      'min': sys.argv[12], # Optional. Default is blank
    }
  return data

def add(data):
    params = urllib.urlencode(data)
    response = "None"
    print "Executable %s, revision %s, benchmark %s" % (data['executable'], data['commitid'], data['benchmark'])
    f = urllib2.urlopen(codespeed_url + 'result/add/', params)
    response = f.read()
    f.close()
    print "Server (%s) response: %s\n" % (codespeed_url, response)

if __name__ == "__main__":
    codespeed_url = 'http://cobra.cs.uni-duesseldorf.de:8000/'
    if len(sys.argv)<=9:
        print "ERROR: too few parameters."
        print "used codespeed server: %s \n" % (codespeed_url)
        #print " needed: codespeed server URL, project, revision(id), benchmark name, result value, interpreter(executable), environment(host), date(or 0), time(or 0)\n"
        print " If you want to perform ProBench autorun use probench_nongui.tcl -autobench\n"
    else:
        data = setdata()
        add(data)
