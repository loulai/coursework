import json
import requests

parameters = {"lat": 40.71, "lon": -74}

response = requests.get("http://api.open-notify.org/iss-pass.json", params=parameters)
print(response.content)

response = requests.get("http://api.open-notify.org/iss-pass.json?lat=40.71&lon=-74")
print(response.content)

#dumps = Python -> string
#loads = Json string -> Python
