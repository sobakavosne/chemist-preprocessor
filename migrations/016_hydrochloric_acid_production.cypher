CALL {
  WITH '016' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (hydrochloricAcidProduction:Reaction { id: 15 }),
  
  (chlorine:Molecule { id: 27 }),
  (hydrogen:Molecule { id: 4 }),
  (hydrochloricAcid:Molecule { id: 18 }),
  
  (iron:Catalyst { id: 2 }),
  (platinum:Catalyst { id: 1 }),
  (aluminum:Catalyst { id: 26 })
  
  CREATE
  (hydrochloricAcidProduction)<-[:REAGENT_IN { amount: 1.0 }]-(hydrogen),
  (hydrochloricAcidProduction)<-[:REAGENT_IN { amount: 1.0 }]-(chlorine),
  
  (hydrochloricAcidProduction)-[:PRODUCT_FROM { amount: 2.0 }]->(hydrochloricAcid),
  
  (hydrochloricAcidProduction)<-[:ACCELERATE { temperature: [298.15, 373.15], pressure: [101.325, 1013.25] }]-(iron),
  (hydrochloricAcidProduction)<-[:ACCELERATE { temperature: [298.15, 373.15], pressure: [101.325, 1013.25] }]-(platinum),
  (hydrochloricAcidProduction)<-[:ACCELERATE { temperature: [298.15, 373.15], pressure: [101.325, 1013.25] }]-(aluminum)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
