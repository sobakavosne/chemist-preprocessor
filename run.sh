#!/bin/bash

HOST_DATA_PATH="neo4j_db/data"
HOST_IMPORT_PATH="neo4j_db/import"
HOST_LOGS_PATH="neo4j_db/logs"
HOST_PLUGINS_PATH="neo4j_db/plugins"

create_dir() {
  local dir_path=$1
  if [ ! -d "$dir_path" ]; then
    mkdir -p "$dir_path"
    if [ $? -ne 0 ]; then
      echo "Error: Failed to create directory $dir_path."
      exit 1
    fi
  fi
}

create_dir "neo4j_db"
create_dir "$HOST_DATA_PATH"
create_dir "$HOST_IMPORT_PATH"
create_dir "$HOST_LOGS_PATH"
create_dir "$HOST_PLUGINS_PATH"

for dir in "$HOST_DATA_PATH" "$HOST_IMPORT_PATH" "$HOST_LOGS_PATH" "$HOST_PLUGINS_PATH"; 
do
  if [ ! -d "$dir" ]; then
    echo "Error: Directory $dir does not exist."
    exit 1
  fi
done

NEO4J_UID=$(id -u)
NEO4J_GID=$(id -g)

export NEO4J_UID
export NEO4J_GID

for dir in "$HOST_DATA_PATH" "$HOST_IMPORT_PATH" "$HOST_LOGS_PATH" "$HOST_PLUGINS_PATH"; 
do
  chown "$NEO4J_UID:$NEO4J_GID" "$dir"
  chmod 755 "$dir"
done

docker compose -f docker-compose.yml -f docker-compose.override.yml up --build
