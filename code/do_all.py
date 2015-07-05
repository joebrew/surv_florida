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
# Specify which directories we'll be using (different on Ben vs. Joe-linux vs. Joe-Windows)
plat = platform.uname()
public = '/home/joebrew/Documents/surv_florida'
private = public + '/data'
private_historical = public + '/data/historical'

private_today = private + '/' + today

# If necessary, create today's directory
# and set working directory there
#if not os.path.exists(private_today):
#  os.makedirs(private_today)

# Set working directory to the data directory
os.chdir(private)

# Run the r script to get the dates and URLs for today's data download
os.chdir(public)
os.system('Rscript code/00_get_links.R')
#robjects.r['source']("code/00_get_links.R")

# Browser
br = mechanize.Browser()

# Cookie Jar
cj = cookielib.LWPCookieJar()
br.set_cookiejar(cj)

# Browser options
br.set_handle_equiv(True)
br.set_handle_gzip(True)
br.set_handle_redirect(True)
br.set_handle_referer(True)
br.set_handle_robots(False)
br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=1)

br.addheaders = [('User-agent', 'Chrome')]

# The site we will navigate into, handling it's session
br.open('https://www.essencefl.com/florida_5_1_14/servlet/Login')
#br.open('https://github.com/login')

# View available forms
#for f in br.forms():
#    print f

# Select the second (index one) form (the first form is a search query box)
br.select_form(nr=0)

# Read in credentials
os.chdir(public)
pu = pd.read_csv('credentials/pu.csv', dtype = 'string')

# Extract ESSENCE username and password from the pu file
u = list(pu['u'])[0]
p = list(pu['p'])[0]

# User credentials
br.form['j_username'] = u
br.form['j_password'] = p

# Login
br.submit()

# Get a vector of all the counties in the state
counties = pd.read_csv(public + '/counties.csv')


############
# Cd into today's data folder
os.chdir(private_today)


# Loop through each county, getting the data for the last week
for i in range(0, len(counties), 1):

	# Extract just the county of interest
	county = counties['x'][i]

	# Give an update
	print 'working on ' + county

	# cd into directory for that county
	os.chdir(county)

	# Read in which links I need for today
	todays_links = pd.read_csv('todays_links.csv')

	# Check to see if Alachua has reported yet
	#reported_yet = br.open('https://www.essencefl.com/florida_5_1_14/servlet/HomePageServlet')
	#reported_text = reported_yet.read()
	#cleaned_yesterday = re.sub(' 0', '  ', yesterday)
	#if  county.capitalize() + r'              reporting (2\/2) hospitals for ' + cleaned_yesterday in reported_text:
	#    print 'Good to go - both hospitals are reporting'
	#else:
	#    print 'Stop here - not all hospitals have reported yet for today'

	# Loop through each link, download the data for that link, and write that data to a file
	os.chdir(private_today + '/' + county)
	for i in range(0,9,1):
	    my_file = br.open(todays_links[0:]['link'][i])
	    # Write a text file
	    f = open(todays_links[0:]['file'][i], 'w')
	    f.write(my_file.read())
	    f.close()
	os.chdir(private_today)

	# Give an update
	print 'done with ' + county

######################################
# Get historical data only if necessary
######################################
if old:

	# Cd into historical data folder
	os.chdir(private_historical)

	# Loop through each county, getting the data for the last week
	for i in range(16, len(counties), 1):

		# Extract just the county of interest
		county = counties['x'][i]

		# Give an update
		print 'working on ' + county

		# cd into directory for that county
		os.chdir(county)

		# Read in which links I need for today
		historical_links = pd.read_csv('historical_links.csv')

		# Loop through each link, download the data for that link, and write that data to a file
		#os.chdir(private_historical + '/' + county)
		for j in range(0,9,1):
		    my_file = br.open(historical_links[0:]['link'][j])
		    print 'getting data for ' + county + ' ' + historical_links['file'][j]
		    # Write a text file
		    f = open(historical_links[0:]['file'][j], 'w')
		    f.write(my_file.read())
		    f.close()
		os.chdir(private_historical)

		# Give an update
		print 'done with ' + county



# If zap files are needed, copy and paste them into the new folder
# def copy_zap(file_name):
#    if not file_name in os.listdir(private_today):
#     shutil.copyfile(src = public + '/code/' + file_name, 
#                     dst = private_today + '/' + file_name) 

# copy_zap('zap.R')
# copy_zap('zap.Rnw')
# copy_zap('doh.png')
# copy_zap('zap_compile.R')
# copy_zap('sweave_it.R')


# # Run the zap file (daily surveillance)
# os.chdir(private_today)
# os.system('Rscript zap.R')
# #os.system('R CMD BATCH --no-save --no-restore zap.R')
# #robjects.r['source']("zap.R")

# # Compile the pdf
# os.chdir(private_today)
# os.system('Rscript sweave_it.R')

# #os.chdir(private_today)
# #os.system('ls -l -h')
# #os.system('R CMD Sweave --pdf zap.Rnw')

# os.system('gnome-open zap.pdf')
