#!/bin/bash

HOST_DATA_PATH="neo4j_db/data"
HOST_IMPORT_PATH="neo4j_db/import"
HOST_LOGS_PATH="neo4j_db/logs"
HOST_PLUGINS_PATH="neo4j_db/plugins"

if [ ! -d "$HOST_PLUGINS_PATH" ]; then
  echo "Error: Folder $HOST_PLUGINS_PATH does not exist."
  exit 1
fi

export NEO4J_UID=$(id -u)
export NEO4J_GID=$(id -g)

sudo chown $NEO4J_UID:$NEO4J_GID "$HOST_DATA_PATH"
sudo chown $NEO4J_UID:$NEO4J_GID "$HOST_IMPORT_PATH"
sudo chown $NEO4J_UID:$NEO4J_GID "$HOST_LOGS_PATH"
sudo chown $NEO4J_UID:$NEO4J_GID "$HOST_PLUGINS_PATH"

sudo chmod 755 "$HOST_DATA_PATH"
sudo chmod 755 "$HOST_IMPORT_PATH"
sudo chmod 755 "$HOST_LOGS_PATH"
sudo chmod 755 "$HOST_PLUGINS_PATH"

docker compose -f docker-compose.dev.yml -f docker-compose.override.yml up --build
