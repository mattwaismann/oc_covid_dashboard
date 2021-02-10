"""
Program to extract COVID data from ArcGIS Server used by OCHCA

Service Directory link (for meta data): https://services2.arcgis.com/LORzk2hk9xzHouw9/ArcGIS/rest/services

"""

import requests
import json
import schedule
import time

EXTRACT_URL_GENERIC = 'https://services2.arcgis.com/LORzk2hk9xzHouw9/ArcGIS/rest/services'
EXTRACT_URL_LOCAL = ("{}/{}/FeatureServer/0/query?where=0%3D0"
                 "&objectIds=&time=&resultType=none&outFields=*"
                 "&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false"
                 "&returnDistinctValues=false&cacheHint=false&orderByFields=date&"
                 "groupByFieldsForStatistics=&outStatistics=&having=&resultOffset="
                 "&resultRecordCount=&sqlFormat=none&f=pjson&token=")

EXTRACT_URL_LOCAL = ("{}/{}/FeatureServer/0/query?where=0%3D0"
				"&objectIds=&time=&resultType=none&outFields=*"
				"&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false"
				"&returnDistinctValues=false&cacheHint=false&orderByFields=date&"
				"groupByFieldsForStatistics=&outStatistics=&having=&resultOffset="
				"&resultRecordCount=&sqlFormat=none&f=pjson&token=")


class CovidServerPull:
	"""
	Class used to pull data from ArcGIS Server used by OCHA
	"""
	def write_to_json(self,path,file_name,data):
		filePathName = './' + path + '/' + file_name + '.json'
		with open(filePathName, 'w') as fp:
			json.dump(data,fp)

	def get_json(self,csv):
		url = EXTRACT_URL_LOCAL.format(EXTRACT_URL_GENERIC,csv)
		response = requests.get(url)
		json_data = response.json()
		self.write_to_json(path = '',file_name = "covid_data"+csv, data = json_data)

class Util:
	"""
	Class used for miscallenous functions
	"""
	def get_all_json():
		#instantiate a covid server pull
	    schedule.every(10).seconds.do
	    covid_server_pull = CovidServerPull()

	    #call get_json method to write json file to the active directory
	    covid_server_pull.get_json(csv = 'occovid_case_csv')
	    covid_server_pull.get_json(csv = 'occovid_death_csv')
	    covid_server_pull.get_json(csv = 'occovid_blueprint_csv')
	    covid_server_pull.get_json(csv = 'occovid_main_csv')
	    covid_server_pull.get_json(csv = 'occovid_hospicu_csv')
	    covid_server_pull.get_json(csv = 'occovid_pcr_csv')

if __name__ == "__main__":
	schedule.every(10).seconds.do(Util.get_all_json)
	while True:
		schedule.run_pending()
		time.sleep(1)


