CALL {
  WITH '018' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (glycineSynthesis:Reaction { id: 17 }),
  
  (glyoxal:Molecule { id: 29 }),
  (ammonia:Molecule { id: 14 }),
  (glycine:Molecule { id: 20 }),
  
  (nickel:Catalyst { id: 6 }),
  (copper:Catalyst { id: 5 }),
  (iron:Catalyst { id: 2 })
  
  CREATE
  (glycineSynthesis)<-[:REAGENT_IN { amount: 1.0 }]-(glyoxal),
  (glycineSynthesis)<-[:REAGENT_IN { amount: 1.0 }]-(ammonia),
  
  (glycineSynthesis)-[:PRODUCT_FROM { amount: 1.0 }]->(glycine),
  
  (glycineSynthesis)<-[:ACCELERATE { temperature: [323.15, 373.15], pressure: [101.325, 202.65] }]-(nickel),
  (glycineSynthesis)<-[:ACCELERATE { temperature: [373.15, 423.15], pressure: [202.65, 303.975] }]-(copper),
  (glycineSynthesis)<-[:ACCELERATE { temperature: [423.15, 473.15], pressure: [303.975, 405.3] }]-(iron)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
