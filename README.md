# erlms

## 使用erlang开发的服务器管理系统

## Thanks JetBrains
[JetBrains IDEA](https://www.jetbrains.com/?from=erlms)

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

## [功能预览](doc/功能预览.md)

## 部署文档
- [部署.开发调试](doc/部署.开发调试.md)(win 10 + git for windows)
- [部署.正式环境](doc/部署.正式环境.md)
- [部署.正式环境-版本更新](doc/部署.正式环境-版本更新.md)(发布版本后, 后台可以更新服务端和客户端, 建议使用这种部署)
