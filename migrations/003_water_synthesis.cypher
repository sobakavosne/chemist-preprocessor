CALL {
  WITH '003' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (waterSynthesis:Reaction { id: 2 }),
  
  (water:Molecule { id: 1 }),
  (hydrogen:Molecule { id: 4 }),
  (oxygen:Molecule { id: 5 }),
  
  (platinum:Catalyst { id: 1 }),
  (palladium:Catalyst { id: 3 }),
  (rhodium:Catalyst { id: 4 }),
  (nickel:Catalyst { id: 6 }),
  (ruthenium:Catalyst { id: 11 })
  
  CREATE
  (waterSynthesis)<-[:REAGENT_IN { amount: 2.0 }]-(hydrogen),
  (waterSynthesis)<-[:REAGENT_IN { amount: 1.0 }]-(oxygen),
  
  (waterSynthesis)-[:PRODUCT_FROM { amount: 2.0 }]->(water),
  
  (waterSynthesis)<-[:ACCELERATE { temperature: [298.15, 473.15], pressure: [101.325, 1013.25] }]-(platinum),
  (waterSynthesis)<-[:ACCELERATE { temperature: [373.15, 573.15], pressure: [101.325, 1013.25] }]-(palladium),
  (waterSynthesis)<-[:ACCELERATE { temperature: [473.15, 673.15], pressure: [101.325, 3039.75] }]-(nickel),
  (waterSynthesis)<-[:ACCELERATE { temperature: [473.15, 773.15], pressure: [101.325, 1013.25] }]-(rhodium),
  (waterSynthesis)<-[:ACCELERATE { temperature: [473.15, 773.15], pressure: [101.325, 1013.25] }]-(ruthenium)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
