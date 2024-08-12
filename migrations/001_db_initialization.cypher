CREATE INDEX reaction_id_index IF NOT EXISTS FOR (n:Reaction) ON (n.id);

CREATE INDEX molecule_id_index IF NOT EXISTS FOR (n:Molecule) ON (n.id);

CREATE INDEX catalyst_id_index IF NOT EXISTS FOR (n:Catalyst) ON (n.id);

CALL db.indexes()
YIELD id, name, properties, type, state
MERGE (index:Index { id: id, name: name, properties: properties, type: type, state: state })
RETURN index;

CALL {
  WITH '001' AS version
  
  OPTIONAL MATCH (existedMigration:Migration { version: version })
  WITH version, existedMigration
  WHERE existedMigration IS null
  UNWIND [1] AS dummy
  
  CREATE
  (water:Molecule { id: 1, smiles: "O", iupacName: "Oxidane" }),
  (ethanol:Molecule { id: 2, smiles: "CCO", iupacName: "Ethyl alcohol" }),
  (carbonDioxide:Molecule { id: 3, smiles: "O=C=O", iupacName: "Carbon dioxide" }),
  (hydrogen:Molecule { id: 4, smiles: "[H][H]", iupacName: "Hydrogen" }),
  (oxygen:Molecule { id: 5, smiles: "O=O", iupacName: "Oxygen" }),
  (methane:Molecule { id: 6, smiles: "CH4", iupacName: "Methane" }),
  (ethene:Molecule { id: 7, smiles: "C=C", iupacName: "Ethene" }),
  (propanol:Molecule { id: 8, smiles: "CCCO", iupacName: "Propanol" }),
  (butane:Molecule { id: 9, smiles: "CCCC", iupacName: "Butane" }),
  (acetone:Molecule { id: 10, smiles: "CC(C)=O", iupacName: "Propan-2-one" }),
  (aceticAcid:Molecule { id: 11, smiles: "CC(O)=O", iupacName: "Ethanoic acid" }),
  (formaldehyde:Molecule { id: 12, smiles: "C=O", iupacName: "Methanal" }),
  (benzene:Molecule { id: 13, smiles: "C1=CC=CC=C1", iupacName: "Benzene" }),
  (ammonia:Molecule { id: 14, smiles: "N", iupacName: "Azane" }),
  (sulfurDioxide:Molecule { id: 15, smiles: "O=S=O", iupacName: "Sulfur dioxide" }),
  (nitrogen:Molecule { id: 16, smiles: "N#N", iupacName: "Dinitrogen" }),
  (carbonMonoxide:Molecule { id: 17, smiles: "C#O", iupacName: "Carbon monoxide" }),
  (hydrochloricAcid:Molecule { id: 18, smiles: "Cl", iupacName: "Hydrochloric acid" }),
  (ethanoicAcid:Molecule { id: 19, smiles: "CC(=O)O", iupacName: "Acetic acid" }),
  (glycine:Molecule { id: 20, smiles: "NCC(C(=O)O)C", iupacName: "2-Aminoacetic acid" }),
  (hydrogenPeroxide:Molecule { id: 21, smiles: "OO", iupacName: "Hydrogen Peroxide" }),
  (propanal:Molecule { id: 22, smiles: "CCC=O", iupacName: "Propanal" }),
  (propanoicAcid:Molecule { id: 23, smiles: "CCC(=O)O", iupacName: "Propanoic Acid" }),
  (polyformaldehyde:Molecule { id: 24, smiles: "(C=O)n", iupacName: "Polyformaldehyde" }),
  (cyclohexane:Molecule { id: 25, smiles: "C1CCCCC1", iupacName: "Cyclohexane" }),
  (sulfurTrioxide:Molecule { id: 26, smiles: "O=S(=O)=O", iupacName: "Sulfur trioxide" }),
  (chlorine:Molecule { id: 27, smiles: "Cl", iupacName: "Chlorine" }),
  (ethane:Molecule { id: 28, smiles: "CC", iupacName: "Ethane" }),
  (glyoxal:Molecule { id: 29, smiles: "O=CC=O", iupacName: "Glyoxal" }),
  (methanol:Molecule { id: 30, smiles: "CO", iupacName: "Methanol" })
  
  CREATE
  (ethanolCombustion:Reaction { id: 1, name: "Ethanol Combustion" }),
  (waterSynthesis:Reaction { id: 2, name: "Water Synthesis" }),
  (methaneCombustion:Reaction { id: 3, name: "Methane Combustion" }),
  (etheneHydration:Reaction { id: 4, name: "Ethene Hydration" }),
  (propanolOxidation:Reaction { id: 5, name: "Propanol Oxidation" }),
  (butaneCracking:Reaction { id: 6, name: "Butane Cracking" }),
  (acetoneOxidation:Reaction { id: 7, name: "Acetone Oxidation" }),
  (aceticAcidFermentation:Reaction { id: 8, name: "Acetic Acid Fermentation" }),
  (formaldehydePolymerization:Reaction { id: 9, name: "Formaldehyde Polymerization" }),
  (benzeneHydrogenation:Reaction { id: 10, name: "Benzene Hydrogenation" }),
  (ammoniaSynthesis:Reaction { id: 11, name: "Ammonia Synthesis" }),
  (sulfurDioxideOxidation:Reaction { id: 12, name: "Sulfur Dioxide Oxidation" }),
  (nitrogenFixation:Reaction { id: 13, name: "Nitrogen Fixation" }),
  (carbonMonoxideConversion:Reaction { id: 14, name: "Carbon Monoxide Conversion" }),
  (hydrochloricAcidProduction:Reaction { id: 15, name: "Hydrochloric Acid Production" }),
  (ethanoicAcidFormation:Reaction { id: 16, name: "Ethanoic Acid Formation" }),
  (glycineSynthesis:Reaction { id: 17, name: "Glycine Synthesis" }),
  (methanolSynthesis:Reaction { id: 18, name: "Methanol Synthesis" }),
  (hydrogenPeroxideDecomposition:Reaction { id: 19, name: "Hydrogen Peroxide Decomposition" }),
  (butaneCombustion:Reaction { id: 20, name: "Butane Combustion" })
  
  CREATE
  (platinum:Catalyst { id: 1, smiles: "[Pt]", name: "Platinum" }),
  (iron:Catalyst { id: 2, smiles: "[Fe]", name: "Iron" }),
  (palladium:Catalyst { id: 3, smiles: "[Pd]", name: "Palladium" }),
  (rhodium:Catalyst { id: 4, smiles: "[Rh]", name: "Rhodium" }),
  (copper:Catalyst { id: 5, smiles: "[Cu]", name: "Copper" }),
  (nickel:Catalyst { id: 6, smiles: "[Ni]", name: "Nickel" }),
  (zinc:Catalyst { id: 7, smiles: "[Zn]", name: "Zinc" }),
  (molybdenum:Catalyst { id: 8, smiles: "[Mo]", name: "Molybdenum" }),
  (tungsten:Catalyst { id: 9, smiles: "[W]", name: "Tungsten" }),
  (cobalt:Catalyst { id: 10, smiles: "[Co]", name: "Cobalt" }),
  (ruthenium:Catalyst { id: 11, smiles: "[Ru]", name: "Ruthenium" }),
  (silver:Catalyst { id: 12, smiles: "[Ag]", name: "Silver" }),
  (tantalum:Catalyst { id: 13, smiles: "[Ta]", name: "Tantalum" }),
  (mercury:Catalyst { id: 14, smiles: "[Hg]", name: "Mercury" }),
  (lead:Catalyst { id: 15, smiles: "[Pb]", name: "Lead" }),
  (barium:Catalyst { id: 16, smiles: "[Ba]", name: "Barium" }),
  (strontium:Catalyst { id: 17, smiles: "[Sr]", name: "Strontium" }),
  (calcium:Catalyst { id: 18, smiles: "[Ca]", name: "Calcium" }),
  (manganeseDioxide:Catalyst { id: 19, smiles: "[O-][Mn+4][O-]", name: "Manganese Dioxide" }),
  (monolithicCuOCr2O3:Catalyst { id: 20, smiles: "O=[Cu]O.[Cr]=O", name: "Monolithic CuO and Cr2O3" }),
  (supportedCuOSiO2:Catalyst { id: 21, smiles: "O=[Cu].O=[Si](O)O", name: "Supported 25% CuO/SiO2" }),
  (niOSiO2:Catalyst { id: 22, smiles: "O=[Ni].O=[Si](O)O", name: "NiO/SiO2" }),
  (phosphoricAcid:Catalyst { id: 23, smiles: "O=P(O)(O)O", name: "Phosphoric Acid" }),
  (sulfuricAcid:Catalyst { id: 24, smiles: "O=S(=O)(O)O", name: "Sulfuric Acid" }),
  (zeolite:Catalyst { id: 25, smiles: "O=[Si]O[Si](O)O[Si](O)O", name: "Zeolite" }),
  (aluminum:Catalyst { id: 26, smiles: "[Al]", name: "Aluminum" }),
  (acetobacter:Catalyst { id: 27, smiles: "CC(=O)OC[C@H](O)C(O)=O", name: "Acetobacter" }),
  (vanadiumPentoxide:Catalyst { id: 28, smiles: "O=[V](O)O=[V](O)O=[V](O)O=[V](O)", name: "Vanadium pentoxide" }),
  (chromium:Catalyst { id: 29, smiles: "[Cr]", name: "Chromium" }),
  (alumina:Catalyst { id: 30, smiles: "O=[Al]O", name: "Alumina (Al2O3)" }),
  (zincOxide:Catalyst { id: 31, smiles: "O=[Zn]", name: "Zinc Oxide (ZnO)" })
  
  CREATE (createdMigration:Migration { version: version })
  
  RETURN createdMigration
}

RETURN createdMigration;
