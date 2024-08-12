CALL {
  WITH '013' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (sulfurDioxideOxidation:Reaction { id: 12 }),
  
  (sulfurDioxide:Molecule { id: 15 }),
  (oxygen:Molecule { id: 5 }),
  (sulfurTrioxide:Molecule { id: 26 }),
  
  (platinum:Catalyst { id: 1 }),
  (vanadiumPentoxide:Catalyst { id: 28 })
  
  CREATE
  (sulfurDioxideOxidation)-[:PRODUCT_FROM { amount: 1.0 }]->(sulfurTrioxide),
  
  (sulfurDioxideOxidation)<-[:REAGENT_IN { amount: 2.0 }]-(sulfurDioxide),
  (sulfurDioxideOxidation)<-[:REAGENT_IN { amount: 1.0 }]-(oxygen),
  
  (sulfurDioxideOxidation)<-[:ACCELERATE { temperature: [673.15, 873.15], pressure: [1013.25, 3039.75] }]-(platinum),
  (sulfurDioxideOxidation)<-[:ACCELERATE { temperature: [673.15, 773.15], pressure: [1013.25, 3039.75] }]-(vanadiumPentoxide)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
