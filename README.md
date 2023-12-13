# ![](logo.png) Pivot
## A Concise, functional, relational data manipulation language

Pivot is an experiment on a language and a runtime to make structured data manipulation easy, concise and principled.
It is based on relational algebra and logic, aiming for a way less verbose result than SQL.

```
-- Declaring a new empty relationship, with schema and constraints
rel Employee { 
  name: str, 
  age: int [. > 0],
  salary: int       
}

-- Relation are immutable. Here we use the set union operator (+)
-- to populate it with some records
Employees := Employee
  + {'John Does',     30, 60000}
  + {'Intell IJ',     35, 80000}
  + {'Alice Chains',  25, 80000}
  + {'I robot',      100,     0}
  
-- then we can query the data
Employees[name, age, age < 50, age ^][# < 2] > stdout

-- The corresponding SQL query is something like
-- SELECT name, age FROM Employees WHERE age < 50 ORDER BY age ASC LIMIT 2
```

