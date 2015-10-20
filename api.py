
from flask import Flask, request, redirect, render_template, escape

from flask.ext.cors import CORS

import json
import random

from couchbase.bucket import Bucket
from couchbase.exceptions import CouchbaseError, KeyExistsError, NotFoundError
from couchbase.views.params import Query
from couchbase.views.iterator import RowProcessor

app = Flask(__name__, static_url_path='')
app.config.from_object(__name__)
CORS(app)

c = Bucket("couchbase://localhost/dashboard")

@app.route('/symptom/search/', methods=['GET'])
def searchSymptom():
  if request.args.get('search'): 
    key = request.args.get('search').lower()
  else:
    return "No search string provided", 404
  q = Query()
  rows = c.query("dev_symptoms", "all")
  
  result = list()
  
  for row in rows:
    found = False
    for v in row.value.values():
      if key.lower() in v.lower():
        result.append(row.value)
        found = True
    if found:
      continue     
    
  resultContainer = dict()
  resultContainer["results"] = result
  
  return json.dumps(resultContainer)
  
@app.route('/symptom/keys/', methods=['POST'])
def getSymptoms():
  target = json.loads(request.data)
  rows = c.query("dev_symptoms", "all")
  result = list()
  for row in rows:
    if row.value["key"] in str(target):
      result.append(row.value)

  return json.dumps(result)


@app.route('/symptom/all/', methods=['GET'])
def allSymptoms():
  q = Query()
  rows = c.query("dev_symptoms", "all")
  
  result = list()
  for row in rows:
    rowvalue = dict()
    for key in row.value.keys():
      rowvalue[key] = row.value[key]
    result.append(rowvalue)
    
  return json.dumps(result)

@app.route('/symptom/add/', methods=['POST'])
def addsymptom():
  resp = dict()
  required = ["name", "desc", "class", "rep_window", "severity"]
  try :
    data = json.loads(request.data)
  except ValueError:
    resp["info"] = "Malformed JSON"
    return json.dumps(resp)
  
  for key in required:
    if not key in data.keys():
      resp["info"] = "Symptom missing key: " + key
      return json.dumps(resp)
    if len(data[key]) < 1:
      resp["info"] = "Symptom missing value for: " + key
      return json.dumps(resp)
  
  data["type"] = "symptom"
  data["key"] = "symptom_" + str(data.get("name").replace(" ", "").lower())
  
  # check if same key already exists
  q = Query()
  rows = c.query("dev_symptoms", "all")
  for row in rows:
    if data["key"] == row.value.get("key"):
      resp["info"] = "Symptom already exists."
      return json.dumps(resp)
  
  add(data)

  obj = c.get(data["key"])
  if not obj:
    resp["info"] = "Added symptom not in bucket"
    return json.dumps(resp)

  resp["key"] = data["key"]
  resp["info"] = "Added new symptom"

  return json.dumps(resp)

@app.route('/factor/add/', methods=['POST'])
def addfactor():
  resp = dict()
  required = ["name", "desc", "rep_window", "input"]
  # jos input on multiple niin values pakolline
  try:
    data = json.loads(request.data)
  except ValueError:
    resp["info"] = "Malformed JSON"
    return json.dumps(resp)

  for key in required:
    if not key in data.keys():
      resp["info"] = "Factor missing key: " + key
      return json.dumps(resp)
    if len(data[key]) < 1:
      resp["info"] = "Factor missing value for: " + key
      return json.dumps(resp)
  if (data["input"] == "multiple") and not data["values"]:
    resp["info"] = "Factor with 'multiple' input type missing 'values'"
    return json.dumps(resp)

  
  data["type"] = "factor"
  data["key"] = "factor_" + str(data["name"].replace(" ", "").lower())
  
  # check if same key already exists
  q = Query()
  rows = c.query("dev_factors", "all")
  for row in rows:
    if row.value and data["key"] == row.value.get("key"):
      resp["info"] = "Factor already exists."
      return json.dumps(resp)  

  add(data)
  obj = c.get(data["key"])
  if not obj:
    resp["info"] = "Added factor not in bucket"
    return json.dumps(resp)

  resp["key"] = data["key"]
  resp["info"] = "Added new factor"

  print json.dumps(resp)
  return json.dumps(resp)

@app.route('/factor/search/', methods=['GET'])
def searchFactor():
  if request.args.get('search'): 
    key = request.args.get('search').lower()
  else:
    return "No search string provided", 404
  q = Query()
  rows = c.query("dev_factors", "all")
  
  result = list()
  
  print "searching factor: " + str(key)

  for row in rows:
    found = False
    for v in row.key.values():
      try:
        if key.lower() in v.lower():
          result.append(row.key)
          found = True
      except AttributeError:
        if key.lower == v:
          result.append(row.key)
          found = True
   
    if found:
        continue     

  resultContainer = dict()
  resultContainer["results"] = result
  
  return json.dumps(resultContainer)

@app.route('/factor/keys/', methods=['POST'])
def getFactors():
  target = json.loads(request.data)
  rows = c.query("dev_factors", "all")
  result = list()
  for row in rows:
    if row.key["key"] in str(target):
      result.append(row.key)
      
  return json.dumps(result)


@app.route('/schema/add/', methods=['POST'])
def addschema():
  required = ["title", "author", "desc", "schema_type", "symptoms", "factors"]
  resp = dict()
  print escape(request.data)
  try :
    data = json.loads(request.data)
  except ValueError:
    resp["info"] = "Malformed JSON"
    return json.dumps(resp)

  print "adding new schema: " + str(data)

  for key in required:
    if not key in data.keys():
      resp["info"] = "Schema missing key: " + key
      return json.dumps(resp)
    if len(data[key]) < 1:
      resp["info"] =  "Schema missing value for: " + key
      return json.dumps(resp)
  
  data["type"] = "schema"
  data["key"] = "schema_" + str(data["title"].replace(" ", "").lower())
  data["db_name"] = str(data["title"].replace(" ", "").lower())
  
  add(data)
  resp["info"] = "Added new schema"
  resp["key"] = data["key"]

  print resp

  return json.dumps(resp)

@app.route('/schema/all/', methods=['GET'])
def getSchemas():
  print "uliuli?"
  q = Query()
  rows = c.query("dev_schemas", "all")
  
  result = list()
  for row in rows:
    result.append(row.key)
  
  print "all schemas"
  print result
  return json.dumps(result)

def add(data):
  print ("adding now, key: " + str(data["key"]))
  try:
    c.add(data["key"], data)
  except KeyExistsError:
    return "Key already exists!"


if __name__ == "__main__":
  app.run(debug=True, host='0.0.0.0')
