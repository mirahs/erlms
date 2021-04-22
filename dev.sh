#!/bin/bash
export PATH=/d/apps/erl9.3/bin:${PATH}


DIR_ROOT=$(dirname $(readlink -f $0))/
DIR_CLIENT=${DIR_ROOT}../erlms_client/

# 发布目录
DIR_REL=/d/cy.worker/release/erlms/

BEAM='deps/*/ebin ebin'
KERNEL_ARGS='-kernel inet_dist_listen_min 13001 inet_dist_listen_max 13999'

NODE_SERVER='erlms_server@127.0.0.1'
NODE_CLIENT='erlms_client@127.0.0.1'
NODE_UP='erlms_up@127.0.0.1'
NODE_STOP='erlms_stop@127.0.0.1'


fun_dev()
{
	cd ${DIR_ROOT}
	rm -rf ebin
	./rebar g-d
	./rebar -D debug co

	echo 编译模板开始
	erl -pa ${BEAM} -noshell -s erlweb_make_dtl -s c q -extra ./src/web/view ./ebin web_erlydtl_tag
	echo 编译模板完成
}

fun_rel()
{
	cd ${DIR_ROOT}
	rm -rf ebin
	./rebar g-d
	./rebar co

	echo 编译模板开始
    erl -pa ${BEAM} -noshell -s erlweb_make_dtl -s c q -extra ./src/web/view ./ebin web_erlydtl_tag
    echo 编译模板完成
}


fun_start_server()
{
    cd ${DIR_ROOT}

    dirVar='var/'
    mkdir -p ${dirVar}

    #erl -pa ${BEAM} -name ${NODE_SERVER} -config ./elog +P 1024000 ${KERNEL_ARGS} -s main start -extra ${dirVar}
    werl -pa ${BEAM} -name ${NODE_SERVER} -config ./elog +P 1024000 ${KERNEL_ARGS} -s main start -extra ${dirVar} &
}

fun_start_client()
{
    copy_to_client
    cd ${DIR_CLIENT}

    dirVar=${DIR_CLIENT}var/
    mkdir -p ${dirVar}

    #erl -pa ${BEAM} -name ${NODE_CLIENT} -config ./elog +P 1024000 ${KERNEL_ARGS} -s main start -extra ${dirVar}
    werl -pa ${BEAM} -name ${NODE_CLIENT} -config ./elog +P 1024000 ${KERNEL_ARGS} -s main start -extra ${dirVar} &
}

fun_up_server()
{
	cd ${DIR_ROOT}

    erl -pa ${BEAM} -name ${NODE_UP} -noshell -s main up -extra ${NODE_SERVER}
}

fun_up_client()
{
	copy_to_client
    cd ${DIR_CLIENT}

    erl -pa ${BEAM} -name ${NODE_UP} -noshell -s main up -extra ${NODE_CLIENT}
}

fun_stop_server()
{
    cd ${DIR_ROOT}

    erl -pa ${BEAM} -name ${NODE_STOP} -noshell -s main stop -extra ${NODE_SERVER}
}

fun_stop_client()
{
    cd ${DIR_CLIENT}

    erl -pa ${BEAM} -name ${NODE_STOP} -noshell -s main stop -extra ${NODE_CLIENT}
}


fun_sync()
{
    cd ${DIR_REL}
    git pull

    cd ${DIR_ROOT}
    fun_rel

    for dirDep in $(ls deps)
    do
        mkdir ${DIR_REL}deps/${dirDep} -p
        \cp deps/${dirDep}/ebin ${DIR_REL}deps/${dirDep}/ -r
    done
    \cp ebin ${DIR_REL} -r
    \cp priv ${DIR_REL} -r
    \cp ctl.sh ${DIR_REL}
    \cp elog.config ${DIR_REL}

    cd ${DIR_REL}
    git add .
    git commit -m 同步版本
    git push
}


copy_to_client()
{
    cd ${DIR_ROOT}

    mkdir -p ${DIR_CLIENT}

    for dirDep in $(ls deps)
    do
        mkdir ${DIR_CLIENT}deps/${dirDep} -p
        \cp deps/${dirDep}/ebin ${DIR_CLIENT}deps/${dirDep}/ -r
    done
    \cp ebin ${DIR_CLIENT} -r
    \cp priv ${DIR_CLIENT} -r
    \cp dev.sh ${DIR_CLIENT}
    \cp elog.config ${DIR_CLIENT}
}


fun_help()
{
    echo "dev                   开发版"
    echo "rel                   正式版"

	echo "start_server          启动服务器"
	echo "start_client          启动客户端"
	echo "up_server             热更服务器"
	echo "up_client             热更客户端"
	echo "stop_server           关闭服务器"
	echo "stop_client           关闭客户端"

	echo "sync                  发布版本"

    exit 1
}



if [ $# -eq 0 ]
then
	fun_help
else
	fun_$1 $*
fi

exit 0
