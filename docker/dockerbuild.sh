#!/usr/bin/env bash
echo 'START TO BUILD DOCKER IMAGE AND START THE IMAGE'
echo '==============================================='

# $1 build host, run `docker build` on it
DOCKER_BUILD_HOST=$1
OPTS_SVN_PATH=$2
SOURCE_STABILITY=$3
MARATHON_DEPLOY_ID=$4
#the deployed war file path(absolutely)
WAR_FILE=$WORKSPACE/FatOpenTSDB/target/scala-2.11/fatopentsdb_2.11-0.1.0.4-SNAPSHOT.war
IMAGE_NAME=openview.fatopentsdb.$SOURCE_STABILITY

TMP_DIR_PREFIX=/tmp/fatopentsdb_build_
TMP_DIR_BUILD=$TMP_DIR_PREFIX$BUILD_NUMBER

MARATHON_DEPLOY_TPL=$TMP_DIR_BUILD/marathon.tpl.json
MARATHON_DEPLOY_FILE=$TMP_DIR_BUILD/marathon.json

# STEP1:  clean build tmp dir
echo "clean build tmp dir: $TMP_DIR_PREFIX*"
ssh -nq root@$DOCKER_BUILD_HOST "rm -fr $TMP_DIR_PREFIX*"

# STEP2: create build tmp dir
echo "create build tmp dir: $TMP_DIR_BUILD"
ssh -nq root@$DOCKER_BUILD_HOST "
    mkdir -p $TMP_DIR_BUILD
"

# STEP3: SCP working files to TMP_DIR_BUILD
echo "copy working files from jeknins to hosts:$TMP_DIR_BUILD"
scp $WORKSPACE/docker/* root@$DOCKER_BUILD_HOST:$TMP_DIR_BUILD
scp $WAR_FILE root@$DOCKER_BUILD_HOST:$TMP_DIR_BUILD
ssh -nq root@$DOCKER_BUILD_HOST "
    unzip $TMP_DIR_BUILD/fatopentsdb_2.11-0.1.0.4-SNAPSHOT.war -d $TMP_DIR_BUILD/war
"

# STEP4: checkout latest opts
echo "checkout latest opts..."
ssh -nq root@$DOCKER_BUILD_HOST "svn checkout $OPTS_SVN_PATH $TMP_DIR_BUILD/opts"

DOCKER_IMAGE_NAME=docker.inflab.com/$IMAGE_NAME:build_$BUILD_NUMBER
# STEP5: generate marathon deploy file
echo "use scala scripts to generate marathon deploy file "
ssh -nq root@$DOCKER_BUILD_HOST '
    scala /data/scalascripts/replace.scala '$MARATHON_DEPLOY_TPL' \$id='$MARATHON_DEPLOY_ID' \$image='$DOCKER_IMAGE_NAME' >> '$MARATHON_DEPLOY_FILE'
'
# STEP6: build docker image
echo "build docker image: $DOCKER_IMAGE_NAME"
ssh -nq root@$DOCKER_BUILD_HOST "docker build -t $DOCKER_IMAGE_NAME $TMP_DIR_BUILD"

# STEP7: push docker image
echo "push docker image: $DOCKER_IMAGE_NAME"
ssh -nq root@$DOCKER_BUILD_HOST "docker push $DOCKER_IMAGE_NAME"

# STEP8 invoke marathon api to re-deploy
echo "delete $MARATHON_DEPLOY_ID ... (wait 20s)"
ssh -nq root@$DOCKER_BUILD_HOST "
    curl -X DELETE http://10.65.178.161:8080/v2/apps/$MARATHON_DEPLOY_ID?force=true && sleep 20
"
echo "start deploy $MARATHON_DEPLOY_ID..."
ssh -nq root@$DOCKER_BUILD_HOST "
    curl -X POST -H \"Content-Type: application/json\" -d@$MARATHON_DEPLOY_FILE http://10.65.178.161:8080/v2/apps
"
echo "God bless you!"
