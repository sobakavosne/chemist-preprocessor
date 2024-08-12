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
  (ethanolCombustion)<-[:REAGENT_IN { amount: 1.0 }]-(ethanol),
  (ethanolCombustion)<-[:REAGENT_IN { amount: 3.0 }]-(oxygen),
  
  (ethanolCombustion)-[:PRODUCT_FROM { amount: 3.0 }]->(water),
  (ethanolCombustion)-[:PRODUCT_FROM { amount: 2.0 }]->(carbonDioxide),
  
  (ethanolCombustion)<-[:ACCELERATE { temperature: [573.15], pressure: [101.325] }]-(platinum),
  (ethanolCombustion)<-[:ACCELERATE { temperature: [623.15, 673.15], pressure: [121.59, 151.9875] }]-(palladium),
  (ethanolCombustion)<-[:ACCELERATE { temperature: [673.15, 723.15], pressure: [151.9875, 182.655] }]-(iron),
  (ethanolCombustion)<-[:ACCELERATE { temperature: [723.15, 773.15], pressure: [182.655, 202.65] }]-(rhodium),
  (ethanolCombustion)<-[:ACCELERATE { temperature: [773.15, 823.15], pressure: [101.325] }]-(monolithicCuOCr2O3),
  (ethanolCombustion)<-[:ACCELERATE { temperature: [723.15, 753.15], pressure: [111.4575, 131.7225] }]-(supportedCuOSiO2),
  (ethanolCombustion)<-[:ACCELERATE { temperature: [743.15, 773.15], pressure: [121.59, 141.855] }]-(supportedCuOAl2O3),
  (ethanolCombustion)<-[:ACCELERATE { temperature: [753.15, 793.15], pressure: [131.7225, 151.9875] }]-(ultrasonicNiOSiO2)
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
