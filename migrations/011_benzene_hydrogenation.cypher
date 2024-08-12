CALL {
  WITH '011' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (benzeneHydrogenation:Reaction { id: 10 }),
  
  (benzene:Molecule { id: 13 }),
  (hydrogen:Molecule { id: 4 }),
  (cyclohexane:Molecule { id: 25 }),
  
  (platinum:Catalyst { id: 1 }),
  (palladium:Catalyst { id: 3 }),
  (nickel:Catalyst { id: 6 })
  
  CREATE
  (benzeneHydrogenation)<-[:REAGENT_IN { amount: 1.0 }]-(benzene),
  (benzeneHydrogenation)<-[:REAGENT_IN { amount: 3.0 }]-(hydrogen),
  
  (benzeneHydrogenation)-[:PRODUCT_FROM { amount: 1.0 }]->(cyclohexane),
  
  (benzeneHydrogenation)<-[:ACCELERATE { temperature: [298.15, 573.15], pressure: [101.325, 1013.25] }]-(platinum),
  (benzeneHydrogenation)<-[:ACCELERATE { temperature: [298.15, 573.15], pressure: [101.325, 1013.25] }]-(palladium),
  (benzeneHydrogenation)<-[:ACCELERATE { temperature: [373.15, 673.15], pressure: [101.325, 3039.75] }]-(nickel)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
