##
## FatOpenTSDB Docker file
##  Dockerfile 文档参考: https://docs.docker.com/engine/reference/builder/
##
##  执行docker build前,需确保配置文件已经checkout到./opts/openview, war包已经解压到./war
##
FROM docker.inflab.com/servicebase
MAINTAINER songzenghui@weibopay.com

COPY ./war /opt/share/tomcat6/webapps/ROOT
COPY ./opts/http-registry /opt/pay/config/basis/inf/http-registry

EXPOSE 8080

