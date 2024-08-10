CALL {
  WITH '002' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  MATCH
  (ethanolCombustion:Reaction { id: 1 }),
  
  (water:Molecule { id: 1 }),
  (carbonDioxide:Molecule { id: 3 }),
  (ethanol:Molecule { id: 2 }),
  (oxygen:Molecule { id: 5 }),
  
  (platinum:Catalyst { id: 1 }),
  
  (palladium:Catalyst { id: 3 }),
  
  (iron:Catalyst { id: 2 }),
  
  (rhodium:Catalyst { id: 4 }),
  
  (monolithicCuOCr2O3:Catalyst { id: 19 }),
  (supportedCuOSiO2:Catalyst { id: 20 }),
  (supportedCuOAl2O3:Catalyst { id: 21 }),
  (ultrasonicNiOSiO2:Catalyst { id: 22 })
  
  CREATE
  (ethanolCombustion)-[:PRODUCT_FROM { amount: 3.0 }]->(water),
  (ethanolCombustion)-[:PRODUCT_FROM { amount: 2.0 }]->(carbonDioxide),
  (ethanolCombustion)<-[:REAGENT_IN { amount: 1.0 }]-(ethanol),
  (ethanolCombustion)<-[:REAGENT_IN { amount: 3.0 }]-(oxygen),
  
  (ethanolCombustion)<-[:ACCELERATE { temperature: 300.0, pressure: 1.0 }]-(platinum),
  
  (ethanolCombustion)<-[:ACCELERATE { temperature: 350.0, pressure: 1.2 }]-(palladium),
  
  (ethanolCombustion)<-[:ACCELERATE { temperature: 400.0, pressure: 1.5 }]-(iron),
  
  (ethanolCombustion)<-[:ACCELERATE { temperature: 450.0, pressure: 1.8 }]-(rhodium),
  
  (ethanolCombustion)<-[:ACCELERATE { temperature: 500.0, pressure: 1.0 }]-(monolithicCuOCr2O3),
  (ethanolCombustion)<-[:ACCELERATE { temperature: 450.0, pressure: 1.1 }]-(supportedCuOSiO2),
  (ethanolCombustion)<-[:ACCELERATE { temperature: 470.0, pressure: 1.2 }]-(supportedCuOAl2O3),
  (ethanolCombustion)<-[:ACCELERATE { temperature: 480.0, pressure: 1.3 }]-(ultrasonicNiOSiO2)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
