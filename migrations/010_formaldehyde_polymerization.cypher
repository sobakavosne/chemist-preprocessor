CALL {
  WITH '010' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (formaldehydePolymerization:Reaction { id: 9 }),
  
  (formaldehyde:Molecule { id: 12 }),
  (polyformaldehyde:Molecule { id: 24 }),
  
  (sulfuricAcid:Catalyst { id: 24 })
  
  CREATE
  (formaldehydePolymerization)<-[:REAGENT_IN { amount: 1.0 }]-(formaldehyde),
  
  (formaldehydePolymerization)-[:PRODUCT_FROM { amount: 1.0 }]->(polyformaldehyde),
  
  (formaldehydePolymerization)<-[:ACCELERATE { temperature: [298.15, 373.15], pressure: [101.325] }]-(sulfuricAcid)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
