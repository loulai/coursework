import json

best_food_chains = ["Taco Bell", "Shake Shack", "Chipotle"]

print(type(best_food_chains)) #>> list

#using .dumps() to convert list -> string
best_food_chains_string = json.dumps(best_food_chains)
print(type(best_food_chains_string)) #>> str


#using .loads() to convert str -> list
print(type(json.loads(best_food_chains_string))) #>>list

fast_food_franchise ={
  "Subway" : 24722,
  "McDonalds": 14098,
  "Starbucks": 10821,
  "Pizza Hut": 7600
}

#dump a dictionary to a string and load it
fast_food_franchise_string= json.dumps(fast_food_franchise)
print(fast_food_franchise_string)
print(json.loads(fast_food_franchise_string))
print(type(fast_food_franchise_string)) #>> str
 

print(type(json.loads(fast_food_franchise_string))) #>> dict
