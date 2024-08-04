#!/bin/sh

while [ ! -f /migration_done ]; do
  echo "Waiting for migration to complete..."
  sleep 5
done

# Start the original command
exec "$@"
