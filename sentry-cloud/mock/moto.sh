#!/bin/bash
# Mock AWS services (moto) for tests: CloudWatch, IAM, S3, STS
#
#   ./mock/moto.sh start      - start (docker if available, otherwise python venv)
#   ./mock/moto.sh stop
#   ./mock/moto.sh status
#
# Tests are enabled when mock is reachable: AWS_MOCK_URL (default http://localhost:5000)
#   sbt "sentry_cloud/testOnly *CloudAWSMockSpec"
#
# NOT supported by moto: CloudTrail LookupEvents (use real AWS: CloudAWSIntegrationSpec)

PORT=${MOTO_PORT:-5000}
NAME=sentry-cloud-moto
IMAGE=${MOTO_IMAGE:-motoserver/moto:latest}
VENV=${MOTO_VENV:-/tmp/moto-venv}
LOG=${MOTO_LOG:-/tmp/moto.log}

docker_ok() { command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; }

case "$1" in
  start)
    if docker_ok; then
      docker rm -f $NAME >/dev/null 2>&1
      docker run -d --name $NAME -p 127.0.0.1:$PORT:5000 $IMAGE
    else
      >&2 echo "docker is not available: using python venv: $VENV"
      [ -x $VENV/bin/moto_server ] || { python3 -m venv $VENV && $VENV/bin/pip install -q "moto[server]"; } || exit 1
      nohup $VENV/bin/moto_server -H 127.0.0.1 -p $PORT >$LOG 2>&1 &
      echo $! > /tmp/$NAME.pid
    fi
    for i in $(seq 1 30); do
      curl -s -o /dev/null http://localhost:$PORT/moto-api/ && { echo "moto: http://localhost:$PORT"; exit 0; }
      sleep 1
    done
    >&2 echo "moto: failed to start"; exit 1
    ;;
  stop)
    docker_ok && docker rm -f $NAME >/dev/null 2>&1
    [ -f /tmp/$NAME.pid ] && kill $(cat /tmp/$NAME.pid) 2>/dev/null && rm -f /tmp/$NAME.pid
    ;;
  status)
    curl -s -o /dev/null http://localhost:$PORT/moto-api/ && echo "running" || { echo "stopped"; exit 1; }
    ;;
  *)
    echo "usage: $0 start|stop|status"; exit 1
    ;;
esac
