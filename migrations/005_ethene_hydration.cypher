CALL {
  WITH '005' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (etheneHydration:Reaction { id: 4 }),
  
  (ethanol:Molecule { id: 2 }),
  (ethene:Molecule { id: 7 }),
  (water:Molecule { id: 1 }),
  
  (phosphoricAcid:Catalyst { id: 23 }),
  (sulfuricAcid:Catalyst { id: 24 }),
  (zeolite:Catalyst { id: 25 })
  
  CREATE
  (etheneHydration)<-[:REAGENT_IN { amount: 1.0 }]-(ethene),
  (etheneHydration)<-[:REAGENT_IN { amount: 1.0 }]-(water),
  
  (etheneHydration)-[:PRODUCT_FROM { amount: 1.0 }]->(ethanol),
  
  (etheneHydration)<-[:ACCELERATE { temperature: [573.15], pressure: [101.325] }]-(phosphoricAcid),
  (etheneHydration)<-[:ACCELERATE { temperature: [573.15], pressure: [101.325] }]-(sulfuricAcid),
  (etheneHydration)<-[:ACCELERATE { temperature: [573.15], pressure: [101.325] }]-(zeolite)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
