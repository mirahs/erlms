## 安装依赖
```shell
yum install -y gcc gcc-c++ ncurses-devel openssl-devel
```

## 安装
```shell
wget http://erlang.org/download/otp_src_20.3.tar.gz
tar xf otp_src_20.3.tar.gz
cd otp_src_20.3
./configure --prefix=/usr/local/erlang-20.3 --with-ssl && make && make install
```

## 添加环境变量
```shell
echo 'export PATH=/usr/local/erlang-20.3/bin/:${PATH}' >> /etc/profile
source /etc/profile
```
