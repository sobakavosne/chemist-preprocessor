CALL {
  WITH '019' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (methanolSynthesis:Reaction { id: 18 }),
  
  (carbonDioxide:Molecule { id: 3 }),
  (hydrogen:Molecule { id: 4 }),
  (methanol:Molecule { id: 30 }),
  
  (copper:Catalyst { id: 5 }),
  (zinc:Catalyst { id: 7 }),
  (aluminum:Catalyst { id: 26 }),
  (chromium:Catalyst { id: 29 }),
  (alumina:Catalyst { id: 30 }),
  (zincOxide:Catalyst { id: 31 })
  
  CREATE
  (methanolSynthesis)<-[:REAGENT_IN { amount: 1.0 }]-(carbonDioxide),
  (methanolSynthesis)<-[:REAGENT_IN { amount: 2.0 }]-(hydrogen),
  
  (methanolSynthesis)-[:PRODUCT_FROM { amount: 1.0 }]->(methanol),
  
  (methanolSynthesis)<-[:ACCELERATE { temperature: [473.15, 523.15], pressure: [505.325, 707.65] }]-(copper),
  (methanolSynthesis)<-[:ACCELERATE { temperature: [473.15, 523.15], pressure: [505.325, 707.65] }]-(zinc),
  (methanolSynthesis)<-[:ACCELERATE { temperature: [473.15, 523.15], pressure: [505.325, 707.65] }]-(aluminum),
  (methanolSynthesis)<-[:ACCELERATE { temperature: [473.15, 523.15], pressure: [505.325, 707.65] }]-(chromium),
  (methanolSynthesis)<-[:ACCELERATE { temperature: [473.15, 523.15], pressure: [505.325, 707.65] }]-(alumina),
  (methanolSynthesis)<-[:ACCELERATE { temperature: [473.15, 523.15], pressure: [505.325, 707.65] }]-(zincOxide)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
