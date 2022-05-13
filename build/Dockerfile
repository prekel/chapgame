FROM ubuntu:focal as run

RUN apt-get update -y
RUN apt-get install libev-dev libssl-dev -y

COPY ./server_bin.exe /bin/app

EXPOSE 8080
ENTRYPOINT /bin/app
