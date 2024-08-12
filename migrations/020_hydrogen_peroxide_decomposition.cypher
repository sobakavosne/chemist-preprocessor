CALL {
  WITH '020' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (hydrogenPeroxideDecomposition:Reaction { id: 19 }),
  
  (hydrogenPeroxide:Molecule { id: 21 }),
  (water:Molecule { id: 1 }),
  (oxygen:Molecule { id: 2 }),
  
  (platinum:Catalyst { id: 1 }),
  (manganeseDioxide:Catalyst { id: 19 })
  
  CREATE
  (hydrogenPeroxideDecomposition)<-[:REAGENT_IN { amount: 2.0 }]-(hydrogenPeroxide),
  
  (hydrogenPeroxideDecomposition)-[:PRODUCT_FROM { amount: 2.0 }]->(water),
  (hydrogenPeroxideDecomposition)-[:PRODUCT_FROM { amount: 1.0 }]->(oxygen),
  
  (hydrogenPeroxideDecomposition)<-[:ACCELERATE { temperature: [298.15, 373.15] }]-(manganeseDioxide),
  (hydrogenPeroxideDecomposition)<-[:ACCELERATE { temperature: [298.15, 373.15] }]-(platinum)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
