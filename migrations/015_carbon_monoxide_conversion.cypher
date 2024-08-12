CALL {
  WITH '015' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (carbonMonoxideConversion:Reaction { id: 14 }),
  
  (carbonMonoxide:Molecule { id: 17 }),
  (carbonDioxide:Molecule { id: 3 }),
  (hydrogen:Molecule { id: 4 }),
  
  (platinum:Catalyst { id: 1 }),
  (nickel:Catalyst { id: 6 }),
  (molybdenum:Catalyst { id: 8 })
  
  CREATE
  (carbonMonoxideConversion)<-[:REAGENT_IN { amount: 1.0 }]-(carbonMonoxide),
  
  (carbonMonoxideConversion)-[:PRODUCT_FROM { amount: 1.0 }]->(carbonDioxide),
  (carbonMonoxideConversion)-[:PRODUCT_FROM { amount: 1.0 }]->(hydrogen),
  
  (carbonMonoxideConversion)<-[:ACCELERATE { temperature: [473.15, 573.15], pressure: [1013.25, 2026.5] }]-(platinum),
  (carbonMonoxideConversion)<-[:ACCELERATE { temperature: [473.15, 573.15], pressure: [1013.25, 2026.5] }]-(nickel),
  (carbonMonoxideConversion)<-[:ACCELERATE { temperature: [473.15, 573.15], pressure: [1013.25, 2026.5] }]-(molybdenum)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
