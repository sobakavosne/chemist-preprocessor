CALL {
  WITH '008' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (acetoneOxidation:Reaction { id: 7 }),
  
  (acetone:Molecule { id: 10 }),
  (carbonDioxide:Molecule { id: 3 }),
  (water:Molecule { id: 1 }),
  (oxygen:Molecule { id: 5 }),
  
  (platinum:Catalyst { id: 1 }),
  (copper:Catalyst { id: 5 }),
  (molybdenum:Catalyst { id: 8 }),
  (manganeseDioxide:Catalyst { id: 19 }),
  (ruthenium:Catalyst { id: 11 })
  
  CREATE
  (acetoneOxidation)<-[:REAGENT_IN { amount: 1.0 }]-(acetone),
  (acetoneOxidation)<-[:REAGENT_IN { amount: 2.0 }]-(oxygen),
  
  (acetoneOxidation)-[:PRODUCT_FROM { amount: 2.0 }]->(carbonDioxide),
  (acetoneOxidation)-[:PRODUCT_FROM { amount: 3.0 }]->(water),
  
  (acetoneOxidation)<-[:ACCELERATE { temperature: [573.15, 673.15], pressure: [101.325, 2026.5] }]-(platinum),
  (acetoneOxidation)<-[:ACCELERATE { temperature: [623.15, 723.15], pressure: [101.325, 1013.25] }]-(copper),
  (acetoneOxidation)<-[:ACCELERATE { temperature: [673.15, 773.15], pressure: [101.325, 1013.25] }]-(molybdenum),
  (acetoneOxidation)<-[:ACCELERATE { temperature: [723.15, 823.15], pressure: [101.325, 1013.25] }]-(manganeseDioxide),
  (acetoneOxidation)<-[:ACCELERATE { temperature: [773.15, 873.15], pressure: [101.325, 2026.5] }]-(ruthenium)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
