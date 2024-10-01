docker build -t ocaml-builder .
docker run -it -v $(pwd):/home/opam/rules-interpreter ocaml-builder /bin/bash

docker build -t ocaml-builder-deb .
docker run -it -v $(pwd):/home/opam/rules-interpreter ocaml-builder-deb /bin/bash

docker build -t plc-agent-builder .
docker run -it -v $(pwd):/home/ubuntu/plc-diagnostics-c plc-agent-builder /bin/bash

docker build -t plc-agent-builder-deb .
docker run -it -v $(pwd):/home/ubuntu/plc-diagnostics-c plc-agent-builder-deb /bin/bash