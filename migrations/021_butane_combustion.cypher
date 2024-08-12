CALL {
  WITH '021' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (butaneCombustion:Reaction { id: 20 }),
  
  (butane:Molecule { id: 9 }),
  (water:Molecule { id: 1 }),
  (carbonDioxide:Molecule { id: 3 }),
  (oxygen:Molecule { id: 5 }),
  
  (platinum:Catalyst { id: 1 }),
  (palladium:Catalyst { id: 3 }),
  (supportedCuOSiO2:Catalyst { id: 21 })
  
  CREATE
  (butaneCombustion)<-[:REAGENT_IN { amount: 1.0 }]-(butane),
  (butaneCombustion)<-[:REAGENT_IN { amount: 6.5 }]-(oxygen),
  
  (butaneCombustion)-[:PRODUCT_FROM { amount: 4.0 }]->(water),
  (butaneCombustion)-[:PRODUCT_FROM { amount: 2.0 }]->(carbonDioxide),
  
  (butaneCombustion)<-[:ACCELERATE { temperature: [473.15, 773.15], pressure: [101.325, 1013.25] }]-(platinum),
  (butaneCombustion)<-[:ACCELERATE { temperature: [523.15, 773.15], pressure: [101.325, 1013.25] }]-(palladium),
  (butaneCombustion)<-[:ACCELERATE { temperature: [573.15, 873.15], pressure: [101.325, 1013.25] }]-(supportedCuOSiO2)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
