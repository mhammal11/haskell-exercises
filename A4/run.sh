# Run the container
docker-compose up --force-recreate
# Stop the container after finishing the test run
docker-compose stop -t 1
# Remove the container
docker rm -f 3fp3_container