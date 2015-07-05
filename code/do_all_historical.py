# Grab historical data or not (should only need to do once)
old = False

#Import necessary libraries
import mechanize
import cookielib
from BeautifulSoup import BeautifulSoup
import html2text
import pandas as pd
import subprocess
import os
#os.environ['R_HOME']='/usr/lib/R'
import rpy2
import rpy2.robjects as robjects
import re
import time
import platform
import shutil

# Get today's date
today = time.strftime("%Y-%m-%d")

# Get yesterday's date
from datetime import datetime, timedelta
yesterday = (datetime.today() - timedelta(days=1)).strftime("%b %d %Y")

# Adjust for oddity
#today = (datetime.today() - timedelta(days=1))#.strftime("%b %d %Y")
#yesterday = (datetime.today() - timedelta(days=2)).strftime("%b %d %Y")
#today = today.strftime("%Y-%m-%d")

print 'today is ' + today
print 'yesterday is ' + yesterday
plat = platform.uname()
public = '/home/joebrew/Documents/surv_florida'
private = public + '/data'
private_historical = public + '/data/historical'

private_today = private + '/' + today

# If necessary, create today's directory
# and set working directory there
#if not os.path.exists(private_today):
#  os.makedirs(private_today)

# Get a vector of all the counties in the state
os.chdir(public)
counties = pd.read_csv(public + '/counties.csv')


#####################
# Get historical data only if necessary
######################################

# Cd into historical data folder
os.chdir(private_historical)

# Loop through each county, getting the data for the last week
for i in range(0, len(counties), 1):

	# Extract just the county of interest
	county = counties['x'][i]

	# cd into directory for that county
	os.chdir(private_historical)
	os.chdir(county)

	# Only continue if the roi2 file doesn't exist
	if os.path.isfile(os.getcwd() + '/roi2.txt'):
		print 'skipping ' + county + ' because already done'
	else:
		# Give an update
		print 'working on ' + county

		# Browser
		br = mechanize.Browser()
		print '---a'

		# Cookie Jar
		cj = cookielib.LWPCookieJar()
		print '---b'
		br.set_cookiejar(cj)
		print '---c'

		# Browser options
		br.set_handle_equiv(True)
		br.set_handle_gzip(True)
		br.set_handle_redirect(True)
		br.set_handle_referer(True)
		br.set_handle_robots(False)
		br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

		print '---d'
		br.addheaders = [('User-agent', 'Chrome')]
		print '---e'
		# The site we will navigate into, handling it's session
		br.open('https://www.essencefl.com/florida_5_1_14/servlet/Login')
		print '---f'
		#br.open('https://github.com/login')

		# View available forms
		#for f in br.forms():
		#    print f

		# Select the second (index one) form (the first form is a search query box)
		br.select_form(nr=0)
		print '---g'

		# Read in credentials
		os.chdir(public)
		pu = pd.read_csv('credentials/pu.csv', dtype = 'string')
		print '---h'
		# Extract ESSENCE username and password from the pu file
		u = list(pu['u'])[0]
		p = list(pu['p'])[0]
		print '---j'
		# User credentials
		br.form['j_username'] = u
		br.form['j_password'] = p

		# Login
		print '---logging in'
		br.submit()
		print '---k'
		# Read in which links I need for today
		os.chdir(private_historical)
		os.chdir(county)
		print '---getting links'
		historical_links = pd.read_csv('historical_links.csv')
		print '---l'
		# Loop through each link, download the data for that link, and write that data to a file
		#os.chdir(private_historical + '/' + county)
		for j in range(0,9,1):
		    my_file = br.open(historical_links[0:]['link'][j])
		    print '---getting data for ' + county + ' ' + historical_links['file'][j]
		    # Write a text file
		    f = open(historical_links[0:]['file'][j], 'w')
		    f.write(my_file.read())
		    f.close()
		    #print '------' + [j+1] ' of 10'
		os.chdir(private_historical)
		print '---m'

		# Give an update
		print '---done with ' + county

		# Sleep
		print '---n'
		print 'sleeping for 240 seconds'
		time.sleep(240)
		print '---o'

		
