CALL {
  WITH '017' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (ethanoicAcidFormation:Reaction { id: 16 }),
  
  (ethane:Molecule { id: 28 }),
  (oxygen:Molecule { id: 5 }),
  (ethanoicAcid:Molecule { id: 19 }),
  
  (platinum:Catalyst { id: 1 }),
  (nickel:Catalyst { id: 6 }),
  (copper:Catalyst { id: 5 })
  
  CREATE
  (ethanoicAcidFormation)<-[:REAGENT_IN { amount: 1.0 }]-(ethane),
  (ethanoicAcidFormation)<-[:REAGENT_IN { amount: 2.0 }]-(oxygen),
  
  (ethanoicAcidFormation)-[:PRODUCT_FROM { amount: 1.0 }]->(ethanoicAcid),
  
  (ethanoicAcidFormation)<-[:ACCELERATE { temperature: [573.15, 623.15], pressure: [101.325, 202.65] }]-(platinum),
  (ethanoicAcidFormation)<-[:ACCELERATE { temperature: [623.15, 673.15], pressure: [202.65, 303.975] }]-(nickel),
  (ethanoicAcidFormation)<-[:ACCELERATE { temperature: [673.15, 723.15], pressure: [303.975, 405.3] }]-(copper)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
