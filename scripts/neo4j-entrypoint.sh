#!/bin/bash

# Load environment variables from .env file
if [ -f /app/.env ]; then
  export $(cat /app/.env | grep -v '^#' | xargs)
fi

wait_for_neo4j() {
  until wget -q -O /dev/null http://localhost:$NEO4J_HTTP_PORT; do
    echo "Waiting for Neo4j to be ready..."
    sleep 5
  done
}

run_migrations() {
  MIGRATIONS_DIR="/migrations"

  for file in $(ls $MIGRATIONS_DIR/*.cypher | sort); do
    echo "Applying migration: $file"
    if ! cat $file | cypher-shell -u $NEO4J_USER -p $NEO4J_PASSWORD -a bolt://localhost:$NEO4J_BOLT_PORT --format plain; then
      echo "Migration failed: $file"
      exit 1
    fi
  done

  echo "All migrations applied."
}

wait_for_neo4j
run_migrations
