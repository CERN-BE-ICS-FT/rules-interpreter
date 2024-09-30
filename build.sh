#Build a Ubuntu based Docker image and run this container to build the project

# Step 1: Build the Docker image
docker build -t ocaml-builder .

# Step 2: Run the container and execute the commands inside it
docker run -it --rm -v $(pwd):/home/opam/rules-interpreter ocaml-builder /bin/bash -c "
    cd /home/opam/rules-interpreter &&
    dune build
"