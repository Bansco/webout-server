# Reference:
# https://www.dev-log.me/Deploying_Haskell:_Painless_CICD_with_Travis,_Docker_and_Digital_Ocean_(or_any_linux_VM)/
FROM ubuntu:18.04

RUN mkdir -p /opt/webout-server/

ARG BINARY_PATH

WORKDIR /opt/webout-server

RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  netbase \
  libpq-dev

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

COPY "$BINARY_PATH" /opt/webout-server

EXPOSE 8080

CMD ["/opt/webout-server/webout-server-exe"]

