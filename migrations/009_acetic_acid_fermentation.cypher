CALL {
  WITH '009' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (aceticAcidFermentation:Reaction { id: 8 }),
  
  (ethanol:Molecule { id: 2 }),
  (oxygen:Molecule { id: 5 }),
  (aceticAcid:Molecule { id: 11 }),
  (water:Molecule { id: 1 }),
  
  (acetobacter:Catalyst { id: 27 })
  
  CREATE
  (aceticAcidFermentation)<-[:REAGENT_IN { amount: 1.0 }]-(ethanol),
  (aceticAcidFermentation)<-[:REAGENT_IN { amount: 1.0 }]-(oxygen),
  
  (aceticAcidFermentation)-[:PRODUCT_FROM { amount: 1.0 }]->(aceticAcid),
  (aceticAcidFermentation)-[:PRODUCT_FROM { amount: 1.0 }]->(water),
  
  (aceticAcidFermentation)<-[:ACCELERATE { temperature: [293.15, 303.15], pressure: [101.325, 1013.25] }]-(acetobacter)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
