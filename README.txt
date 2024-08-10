:Chemist Graph DB

   • create .env file from examples/

   • for Unix-like systems (Linux, macOS, BSD):

     > chmod +x run.sh
     > ./run.sh

:Issues

   • if you encounter a "permissions" error while Docker is running:
     
     > sudo chown -R $USER:$USER neo4j_db/
     > sudo chmod -R 755 neo4j_db/

   • when changing Neo4j versions, remember to remove the neo4j_db/plugins directory to 
     avoid potential issues (check logs/Local/error for more details)

:Notes

   • Real reactions were utilized to understand the concept behind the graph database

   • Multicatalytic reaction example was referenced from an article available on
     Springer: https://link.springer.com/article/10.1134/S096554412306018X

:Future

   • For instance, we can create a new relationship to indicate whether or not
     an inorganic catalyst can poison an enzyme. For example, Tetraethyllead (Pb(CH3CH2)4)
     is known to be a potent enzyme inhibitor, particularly for those involved in
     neurotransmitter processes. Additionally, heavy metals like mercury (Hg), lead (Pb),
     and cadmium (Cd) are notorious for their ability to inhibit various enzymatic activities,
     leading to significant toxic effects in biological systems
