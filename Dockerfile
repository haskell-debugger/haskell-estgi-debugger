FROM ubuntu:latest
EXPOSE 4711:4711/tcp
ENV DEBIAN_FRONTEND=noninteractive
ENV PATH=/root/.local/bin:$PATH
RUN apt-get -y update; apt-get -y install curl libgmp-dev pkg-config libomp-dev zlib1g-dev libbz2-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack install zip-cmd
COPY ./dap-estgi-server ./dap-estgi-server
COPY ./stack.yaml ./stack.yaml
WORKDIR .
RUN stack install dap-estgi-server
CMD ["dap-estgi"]
