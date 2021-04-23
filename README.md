# erlms

## 使用erlang开发的服务器管理系统

## 依赖
- [erlang 20.3](https://www.erlang.org/downloads/20.3)
- [erlweb 管理后台模板](https://github.com/mirahs/erlweb)

## Todo
- [x] 根据节点类型(服务端, 客户端)启动程序
- [x] 后台服务端 更新文件、热更新、重启
- [x] 后台添加主机
- [x] 跨服(服务端连接主机)
- [x] 后台执行 主机 系统命令
- [x] 后台内置 更新文件、热更新、重启 命令
- [x] 后台主机添加WebSSH功能, 通过WebSSH维护主机
- [x] 主机信息统计
- [x] 任务调度

## 开发调试

### 配置
- 修改 include/web.hrl 文件 web_port 宏定义，比如 3001
- 修改 include/common.hrl 文件 mysql_username、mysql_password、mysql_database 宏定义
- MySQL 导入 erlms.sql 文件

### 编译及启动
编译(第一次编译要先使用 rel 先编译一次, 不然 deps/ibrowse/src/ibrowse_lib.erl 这个依赖文件会编译不过, 因这 ibrowse 这个库使用了 warnings_as_errors 编译选项, 编译成功以后就可以直接使用 dev 编译了)
```shell
sh dev.sh rel
sh dev.sh dev
```

启动服务端和客户端
```shell
sh dev.sh start_server
sh dev.sh start_client
```

### 测试
浏览器输入 http://localhost:3001/adm/ 并输入账号 admin，密码 admin 登录管理系统

因为是开发调试, 所以主机只能添加 127.0.0.1
