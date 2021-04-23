## 安装依赖
```shell
yum install -y python2-pip

pip install --upgrade "pip < 21.0"
pip install --upgrade setuptools
```

## 安装
```shell
pip install webssh
```

## 启动
```shell
nohup wssh --port=4001 --fbidhttp=false > webssh.log 2>&1 &
```

## 访问
http://ip:4001
