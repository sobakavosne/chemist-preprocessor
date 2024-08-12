CALL {
  WITH '012' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (ammoniaSynthesis:Reaction { id: 11 }),
  
  (hydrogen:Molecule { id: 4 }),
  (nitrogen:Molecule { id: 16 }),
  (ammonia:Molecule { id: 14 }),
  
  (iron:Catalyst { id: 2 }),
  (ruthenium:Catalyst { id: 11 }),
  (molybdenum:Catalyst { id: 8 })
  
  CREATE
  (ammoniaSynthesis)<-[:REAGENT_IN { amount: 3.0 }]-(hydrogen),
  (ammoniaSynthesis)<-[:REAGENT_IN { amount: 1.0 }]-(nitrogen),
  
  (ammoniaSynthesis)-[:PRODUCT_FROM { amount: 2.0 }]->(ammonia),
  
  (ammoniaSynthesis)<-[:ACCELERATE { temperature: [673.15, 873.15], pressure: [1013.25, 20265] }]-(iron),
  (ammoniaSynthesis)<-[:ACCELERATE { temperature: [673.15, 873.15], pressure: [1013.25, 20265] }]-(ruthenium),
  (ammoniaSynthesis)<-[:ACCELERATE { temperature: [673.15, 873.15], pressure: [1013.25, 20265] }]-(molybdenum)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
