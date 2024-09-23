:Chemist Graph DB

   • Create .env file from examples/

     NEO4J_HOST=neo4j     -- launching project via Docker 
     NEO4J_HOST=localhost -- launching project locally

   • For Unix-like systems (Linux, macOS, BSD):

     > chmod +x run.sh
     > ./run.sh

:Issues

   • If you encounter a "permissions" error while Docker is running:
     
     > sudo chown -R $USER:$USER neo4j_db/
     > sudo chmod -R 755 neo4j_db/

   • When changing Neo4j versions, remember to remove the "neo4j_db/plugins" directory to 
     avoid potential issues ("java.lang.UnsupportedClassVersionError: apoc/ApocSignatures 
     has been compiled by a more recent version of the Java Runtime" error)

:Notes

   ! Not all of the reactions are real

   • Reaction example with 3d-metal oxides as catalysts was referenced from an article 
     available on Springer: https://link.springer.com/article/10.1134/S096554412306018X

   • Any reaction has conditions (temperature and pressure) and should be supplied with 
     a catalyst or an empty catalyst node since the "conditions" element is a bond

   • The standard migration batch contains 20 reactions available by IDs [1..20]

   • Currently, only the 4th process is available and contains a mechanism

   • After running the Dockerized application, the Neo4j GUI is available at 
     http://localhost:7474/browser/

:Queries
   
   •  Ethene hydration mechanism is available by ID=4 (the only mechanism added). The full
      process can be queried using the following query:

      MATCH 
             (reaction:Reaction { id: 4 })<-[accelerate:ACCELERATE]-(catalyst:Catalyst),
             (reaction)-[product_from:PRODUCT_FROM]->(product:Molecule),
             (reaction)<-[reagent_in:REAGENT_IN]-(reagent:Molecule),
             (reaction)-[follow:FOLLOW]->(mechanism:Mechanism),
             (mechanism)-[has_stage:HAS_STAGE]->(stage:Stage),
             (stage)<-[include:INCLUDE]-(participant)
      
      RETURN 
             mechanism, reaction, accelerate, catalyst, product, reagent, product_from,
             reagent_in, follow, has_stage, stage, include, participant;

:Next

   • Mechanism should utilize the only relationship for ":Stage" (1st stage) to specify the starting 
     point with ":START_WITH" relationship
   
   • Products should be pointed out with ":Ion" or ":Molecule"
