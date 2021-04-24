#!/bin/bash
export PATH=/usr/local/erlang-20.3/bin:${PATH}


DIR_ROOT=$(dirname $(readlink -f $0))/
export HOME=${DIR_ROOT} #开机启动需要设置 HOME 环境变量

BEAM='deps/*/ebin ebin'
KERNEL_ARGS='-kernel inet_dist_listen_min 13001 inet_dist_listen_max 13999'

NODE_SERVER="erlms_server@$(hostname)"
NODE_CLIENT="erlms_client@$(hostname)"
NODE_UP="erlms_up@$(hostname)"
NODE_STOP="erlms_stop@$(hostname)"


fun_start_server()
{
    cd ${DIR_ROOT}

    dirVar='var/server/'
    mkdir -p ${dirVar}

    erl -pa ${BEAM} -name ${NODE_SERVER} -config ./elog +P 1024000 ${KERNEL_ARGS} -hidden -detached -s main start -extra ${dirVar}
}

fun_start_client()
{
    cd ${DIR_ROOT}

    dirVar='var/client/'
    mkdir -p ${dirVar}

    erl -pa ${BEAM} -name ${NODE_CLIENT} -config ./elog +P 1024000 ${KERNEL_ARGS} -hidden -detached -s main start -extra ${dirVar}
}

fun_up_server()
{
	cd ${DIR_ROOT}

    erl -pa ${BEAM} -name ${NODE_UP} -noshell -s main up -extra ${NODE_SERVER}
}

fun_up_client()
{
    cd ${DIR_ROOT}

    erl -pa ${BEAM} -name ${NODE_UP} -noshell -s main up -extra ${NODE_CLIENT}
}

fun_stop_server()
{
    cd ${DIR_ROOT}

    erl -pa ${BEAM} -name ${NODE_STOP} -noshell -s main stop -extra ${NODE_SERVER}
}

fun_stop_client()
{
    cd ${DIR_ROOT}

    erl -pa ${BEAM} -name ${NODE_STOP} -noshell -s main stop -extra ${NODE_CLIENT}
}


fun_help()
{
    echo "$0 start_server          启动服务器"
    echo "$0 start_client          启动客户端"
    echo "$0 up_server             热更服务器"
    echo "$0 up_client             热更客户端"
    echo "$0 stop_server           关闭服务器"
    echo "$0 stop_client           关闭客户端"

    exit 1
}



if [ $# -eq 0 ]
then
	fun_help
else
	fun_$1 $*
fi

exit 0
