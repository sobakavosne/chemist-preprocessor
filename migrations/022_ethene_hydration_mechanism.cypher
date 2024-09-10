CALL {
  WITH "022" AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (etheneHydration:Reaction { id: 4 }),
  
  (ethene:Molecule { id: 7 }),
  (ethanol:Molecule { id: 2 }),
  (water:Molecule { id: 1 }),
  
  (phosphoricAcid:Catalyst { id: 23 }),
  (sulfuricAcid:Catalyst { id: 24 }),
  (zeolite:Catalyst { id: 25 })
  
  CREATE (mechanism:Mechanism {
           id: 4,
           name: "Electrophilic Addition",
           type: "AdE",
           activationEnergy: 75.0
         })
  
  CREATE (etheneHydration)-[:FOLLOW { description: "Electrophilic addition of water to ethene" }]->(mechanism)
  
  CREATE (stage1:Stage {
           order: 1,
           name: "Protonation of ethene",
           description: "Ethene is protonated, forming a carbocation",
           products: ["Carbocation"] // → :Ion { id: 1, iupacName: "ethan-1-olium", smiles: "CC[+]" } →
         }),
         (stage2:Stage {
           order: 2,
           name: "Nucleophilic attack by water",
           description: "Water molecule attacks the carbocation",
           products: ["Ethanolium"]  // → :Ion { id: 2, iupacName: "Ethan-1-olium", smiles: "CCO+[H]" } →
         }),
         (stage3:Stage {
           order: 3,
           name: "Deprotonation",
           description: "Excess proton is removed, forming ethanol",
           products: ["Ethanol" ]    // → :Molecule { id: 2 }
         })
  
  CREATE (mechanism)-[:HAS_STAGE]->(stage1),
         (mechanism)-[:HAS_STAGE]->(stage2),
         (mechanism)-[:HAS_STAGE]->(stage3)
  
  CREATE (stage1)-[:NEXT_STAGE]->(stage2),
         (stage2)-[:NEXT_STAGE]->(stage3)
  
  CREATE (stage1)<-[:INCLUDE]-(ethene),
         (stage1)<-[:INCLUDE]-(phosphoricAcid),
         (stage1)<-[:INCLUDE]-(sulfuricAcid),
         (stage1)<-[:INCLUDE]-(zeolite)
  
  CREATE (stage2)<-[:INCLUDE]-(water),
         (stage2)<-[:INCLUDE]-(ethanol)
  
  CREATE (stage3)<-[:INCLUDE]-(ethanol)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
