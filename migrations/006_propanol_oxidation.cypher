CALL {
  WITH '006' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (propanolOxidation:Reaction { id: 5 }),
  
  (water:Molecule { id: 1 }),
  (oxygen:Molecule { id: 5 }),
  (propanol:Molecule { id: 8 }),
  (propanoicAcid:Molecule { id: 23 }),
  
  (palladium:Catalyst { id: 3 }),
  (copper:Catalyst { id: 5 }),
  (manganeseDioxide:Catalyst { id: 19 })
  
  CREATE
  (propanolOxidation)<-[:REAGENT_IN { amount: 1.0 }]-(propanol),
  (propanolOxidation)<-[:REAGENT_IN { amount: 1.0 }]-(oxygen),
  
  (propanolOxidation)-[:PRODUCT_FROM { amount: 1.0 }]->(propanoicAcid),
  (propanolOxidation)-[:PRODUCT_FROM { amount: 1.0 }]->(water),
  
  (propanolOxidation)<-[:ACCELERATE { temperature: [298.15, 373.15], pressure: [101.325] }]-(copper),  
  (propanolOxidation)<-[:ACCELERATE { temperature: [298.15, 373.15], pressure: [101.325] }]-(manganeseDioxide)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
