A `Dockerfile` and `docker-compose.yml` is included, which
generates image based on `Ubuntu 20.04` with `ghc`/`cabal` installed on it. We also
added all the necessary packages you may need to finish the tasks. If you do not have `ghc`/`cabal` or `stack` installed
in your device, use this docker configuration to compile and test your implementation. A brief instruction about how to
generate the image and how to run the container has been added to the bottom of this file.

## Make sure scripts are executable

It is possible that `run.sh` and `setup.sh` are not executable. Run the following commands to make them executable
scripts and try again.

```shell script
chmod +x setup.sh  
```

```shell script
chmod +x run.sh
```

## Setup
Use `docker-compose.yml` and its configuration file to build the image. Just execute the
following code to generate the image.

```shell script
./setup.sh
```

## Running the tests
The configuration needed for running the tests is inside `docker-compose.yml`. 
Just execute the below script to see the results of the tests.

```shell script
./run.sh
```
