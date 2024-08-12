CALL {
  WITH '007' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (butaneCracking:Reaction { id: 6 }),
  
  (butane:Molecule { id: 9 }),
  (ethene:Molecule { id: 7 }),
  (methane:Molecule { id: 6 }),
  (hydrogen:Molecule { id: 4 }),
  
  (platinum:Catalyst { id: 1 }),
  (nickel:Catalyst { id: 6 }),
  (cobalt:Catalyst { id: 10 }),
  (zeolite:Catalyst { id: 25 }),
  (alumina:Catalyst { id: 26 })
  
  CREATE
  (butaneCracking)<-[:REAGENT_IN { amount: 1.0 }]-(butane),
  
  (butaneCracking)-[:PRODUCT_FROM { amount: 1.0 }]->(ethene),
  (butaneCracking)-[:PRODUCT_FROM { amount: 1.0 }]->(methane),
  (butaneCracking)-[:PRODUCT_FROM { amount: 0.5 }]->(hydrogen),
  
  (butaneCracking)<-[:ACCELERATE { temperature: [673.15, 823.15], pressure: [101.325, 1013.25] }]-(platinum),
  (butaneCracking)<-[:ACCELERATE { temperature: [673.15, 873.15], pressure: [101.325, 1013.25] }]-(nickel),
  (butaneCracking)<-[:ACCELERATE { temperature: [723.15, 873.15], pressure: [101.325, 1013.25] }]-(cobalt),
  (butaneCracking)<-[:ACCELERATE { temperature: [773.15, 923.15], pressure: [101.325, 2026.5] }]-(alumina),
  (butaneCracking)<-[:ACCELERATE { temperature: [823.15, 973.15], pressure: [101.325, 1013.25] }]-(zeolite)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
