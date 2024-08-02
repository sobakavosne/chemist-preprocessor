CREATE
(water:Molecule { id: 1, smiles: "O", iupacName: "Oxidane" }),
(ethanol:Molecule { id: 2, smiles: "CCO", iupacName: "Ethyl alcohol" }),

(combustion:Reaction { id: 1, name: "Ethanol Combustion" }),
(synthesis:Reaction { id: 2, name: "Water Synthesis" }),

(platinum:Catalyst { id: 1, smiles: "[Pt]", name: "Platinum" }),
(iron:Catalyst { id: 2, smiles: "[Fe]", name: null }),

(combustion)-[:PRODUCT_FROM { amount: 1.0 }]->(water),
(synthesis)-[:PRODUCT_FROM { amount: 2.0 }]->(water),
(synthesis)-[:PRODUCT_FROM { amount: 1.0 }]->(ethanol),

(combustion)-[:ACCELERATE { temperature: 300.0, pressure: 1.0 }]->(platinum),
(synthesis)-[:ACCELERATE { temperature: 400.0, pressure: 1.5 }]->(iron);
