from selenium import webdriver
from selenium.webdriver.common.keys import Keys

driver = webdriver.Firefox('/Users/yanivbronshtein/Coding/Rutgers/DataWrangling_FinalProject/Python_Selenium/')
driver.get("http://www.python.org")
driver.close()