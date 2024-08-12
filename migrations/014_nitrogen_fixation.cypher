CALL {
  WITH '014' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (nitrogenFixation:Reaction { id: 13 }),
  
  (nitrogen:Molecule { id: 16 }),
  (ammonia:Molecule { id: 14 }),
  (hydrogen:Molecule { id: 4 }),
  
  (iron:Catalyst { id: 2 }),
  (molybdenum:Catalyst { id: 8 }),
  (vanadiumPentoxide:Catalyst { id: 28 })
  
  CREATE
  (nitrogenFixation)<-[:REAGENT_IN { amount: 1.0 }]-(nitrogen),
  (nitrogenFixation)<-[:REAGENT_IN { amount: 3.0 }]-(hydrogen),
  
  (nitrogenFixation)-[:PRODUCT_FROM { amount: 2.0 }]->(ammonia),
  
  (nitrogenFixation)<-[:ACCELERATE { temperature: [673.15, 873.15], pressure: [1013.25, 3039.75] }]-(iron),
  (nitrogenFixation)<-[:ACCELERATE { temperature: [673.15, 773.15], pressure: [1013.25, 3039.75] }]-(molybdenum),
  (nitrogenFixation)<-[:ACCELERATE { temperature: [673.15, 773.15], pressure: [1013.25, 3039.75] }]-(vanadiumPentoxide)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
