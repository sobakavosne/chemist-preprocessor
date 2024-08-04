#!/bin/bash

wait_for_neo4j() {
  until wget -q -O /dev/null http://$NEO4J_HOST:$NEO4J_HTTP_PORT; do
    echo "Waiting for Neo4j to be ready..."
    sleep 2
  done
}

run_migrations() {
  MIGRATIONS_DIR="/migrations"

  for file in $(ls $MIGRATIONS_DIR/*.cypher | sort); do
    echo "Applying migration: $file"
    if ! cat $file | cypher-shell -u $NEO4J_USER -p $NEO4J_PASSWORD -a bolt://$NEO4J_HOST:$NEO4J_BOLT_PORT --format plain; then
      echo "Migration failed: $file"
      exit 1
    fi
  done

  echo "All migrations applied."
}

wait_for_neo4j
run_migrations

touch /migration_done
