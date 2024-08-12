CALL {
  WITH '004' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (methaneCombustion:Reaction { id: 3 }),
  
  (water:Molecule { id: 1 }),
  (carbonDioxide:Molecule { id: 3 }),
  (methane:Molecule { id: 6 }),
  (oxygen:Molecule { id: 5 }),
  
  (palladium:Catalyst { id: 3 }),
  
  (platinum:Catalyst { id: 1 })
  
  CREATE
  (methaneCombustion)<-[:REAGENT_IN { amount: 1.0 }]-(methane),
  (methaneCombustion)<-[:REAGENT_IN { amount: 2.0 }]-(oxygen),
  
  (methaneCombustion)-[:PRODUCT_FROM { amount: 2.0 }]->(water),
  (methaneCombustion)-[:PRODUCT_FROM { amount: 1.0 }]->(carbonDioxide),
  
  (methaneCombustion)<-[:ACCELERATE { temperature: [723.15, 973.15], pressure: [101.325, 1013.25] }]-(palladium),
  (methaneCombustion)<-[:ACCELERATE { temperature: [773.15, 1073.15], pressure: [101.325, 1013.25] }]-(platinum)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
