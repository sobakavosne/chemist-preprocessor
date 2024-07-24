biocad test task

Prepare environment variables

> export NEO4J_UID=$(id -u)
> export NEO4J_GID=$(id -g)

Docker up

> docker compose -f docker-compose.yml -f docker-compose.override.yml up --build
