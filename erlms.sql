-- phpMyAdmin SQL Dump
-- version 5.0.2
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: 2021-03-12 17:51:08
-- 服务器版本： 5.7.14
-- PHP Version: 7.4.9

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `erlms`
--

-- ----------------------------
-- Table structure for adm_user
-- ----------------------------
DROP TABLE IF EXISTS `adm_user`;
CREATE TABLE IF NOT EXISTS `adm_user` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `account` varchar(32) NOT NULL COMMENT '帐号',
  `password` char(32) NOT NULL COMMENT '密码',
  `type` tinyint(3) unsigned NOT NULL COMMENT '类型 10:管理员|20:游客',

  `is_locked` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '是否被锁住 0:否|1:是',
  `remark` text NOT NULL COMMENT '备注',

  `login_times` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '登录次数',
  `login_time` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '登录时间',
  `login_ip` varchar(15) NOT NULL DEFAULT '' COMMENT '登录IP',
  PRIMARY KEY (`id`),
  UNIQUE KEY `account` (`account`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='后台账号';

-- ----------------------------
-- Records of adm_user
-- ----------------------------
INSERT INTO `adm_user` (`id`, `account`, `password`, `type`, `remark`) VALUES
(1, 'admin', 'c3284d0f94606de1fd2af172aba15bf3', 10, 'admin');

-- ----------------------------
-- Table structure for log_adm_user_login
-- ----------------------------
DROP TABLE IF EXISTS `log_adm_user_login`;
CREATE TABLE IF NOT EXISTS `log_adm_user_login` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `account` varchar(32) NOT NULL COMMENT '帐号',
  `time` int(10) unsigned NOT NULL COMMENT '时间',
  `status` tinyint(3) unsigned NOT NULL COMMENT '状态 0:失败|1:成功',
  `ip` varchar(15) NOT NULL COMMENT 'IP',
  `ip_segment` varchar(31) NOT NULL COMMENT 'IP段',
  `address` varchar(128) NOT NULL COMMENT '地址',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='日志-后台账号登录';


-- ----------------------------
-- Table structure for uuid
-- ----------------------------
DROP TABLE IF EXISTS `uuid`;
CREATE TABLE IF NOT EXISTS `uuid` (
  `key` varchar(20) NOT NULL,
  `val` int(10) unsigned NOT NULL,
  PRIMARY KEY (`key`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='唯一ID';


-- ----------------------------
-- Table structure for host
-- ----------------------------
DROP TABLE IF EXISTS `host`;
CREATE TABLE IF NOT EXISTS `host` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `name` varchar(30) NOT NULL COMMENT '主机名(hostname)',
  `ssh_port` smallint(5) unsigned NOT NULL DEFAULT '22' COMMENT 'ssh端口',
  `ssh_username` varchar(30) NOT NULL DEFAULT '' COMMENT 'ssh账号',
  `ssh_password` varchar(50) NOT NULL DEFAULT '' COMMENT 'ssh密码',
  `time` int(10) unsigned NOT NULL COMMENT '添加时间',
  `remark` text NOT NULL COMMENT '备注',
  PRIMARY KEY (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='主机';

-- ----------------------------
-- Table structure for cmd
-- ----------------------------
DROP TABLE IF EXISTS `cmd`;
CREATE TABLE IF NOT EXISTS `cmd` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `name` varchar(20) NOT NULL COMMENT '命令名称',
  `cmd` varchar(100) NOT NULL COMMENT '命令',
  `time` int(10) unsigned NOT NULL COMMENT '添加时间',
  `is_del` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '是否已删除 0:否|1:是',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='命令';

-- ----------------------------
-- Records of cmd
-- ----------------------------
INSERT INTO `cmd` (`id`, `name`, `cmd`, `time`) VALUES
(1, '更新文件', 'git pull', 1617255073),
(2, '热更新(包括更新文件)', 'git pull && sh ctl.sh up_client', 1617255133),
(3, '重启', 'sh ctl.sh stop_client && sh ctl.sh start_client', 1617259320);

-- ----------------------------
-- Table structure for log_cmd_run
-- ----------------------------
DROP TABLE IF EXISTS `log_cmd_run`;
CREATE TABLE IF NOT EXISTS `log_cmd_run` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `cmd_id` int(10) unsigned NOT NULL COMMENT '命令ID',
  `time` int(10) unsigned NOT NULL COMMENT '执行时间',
  `cnt_all` int(10) unsigned NOT NULL COMMENT '执行主机数量',
  `cnt_succ` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '执行主机成功数量',
  PRIMARY KEY (`id`),
  KEY `cmd_id` (`cmd_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='日志-命令执行';

-- ----------------------------
-- Table structure for log_cmd_run_detail
-- ----------------------------
DROP TABLE IF EXISTS `log_cmd_run_detail`;
CREATE TABLE IF NOT EXISTS `log_cmd_run_detail` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `log_id` int(10) unsigned NOT NULL COMMENT '命令日志ID',
  `cmd_id` int(10) unsigned NOT NULL COMMENT '命令ID',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  `time` int(10) unsigned NOT NULL COMMENT '执行时间',
  `result_time` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '执行返回时间',
  `result` text NOT NULL COMMENT '执行返回结果',
  `runtime` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '运行时间(毫秒)',
  `wall_clock` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '运行时间(毫秒)',
  PRIMARY KEY (`id`),
  KEY `log_id` (`log_id`),
  KEY `cmd_id` (`cmd_id`),
  KEY `host_id` (`host_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='日志-命令执行详情';

-- ----------------------------
-- Table structure for cron
-- ----------------------------
DROP TABLE IF EXISTS `cron`;
CREATE TABLE IF NOT EXISTS `cron` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `name` varchar(20) NOT NULL COMMENT '任务名称',
  `expr` varchar(100) NOT NULL COMMENT 'cron表达式',
  `expr_erlang` varchar(100) NOT NULL COMMENT 'cron erlang表达式',
  `cmd` varchar(200) NOT NULL COMMENT '命令',
  `time` int(10) unsigned NOT NULL COMMENT '添加时间',
  `is_del` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '是否已删除 0:否|1:是',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='任务';

-- ----------------------------
-- Table structure for log_cron_sync
-- ----------------------------
DROP TABLE IF EXISTS `log_cron_sync`;
CREATE TABLE IF NOT EXISTS `log_cron_sync` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `cron_id` int(10) unsigned NOT NULL COMMENT '任务ID',
  `is_del` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '是否删除任务 0:否|1:是',
  `time` int(10) unsigned NOT NULL COMMENT '同步时间',
  `cnt_all` int(10) unsigned NOT NULL COMMENT '同步主机数量',
  `cnt_succ` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '同步主机成功数量',
  PRIMARY KEY (`id`),
  KEY `cron_id` (`cron_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='日志-任务同步';

-- ----------------------------
-- Table structure for log_cron_sync_detail
-- ----------------------------
DROP TABLE IF EXISTS `log_cron_sync_detail`;
CREATE TABLE IF NOT EXISTS `log_cron_sync_detail` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `log_id` int(10) unsigned NOT NULL COMMENT '同步日志ID',
  `cron_id` int(10) unsigned NOT NULL COMMENT '任务ID',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  `is_del` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '是否删除任务 0:否|1:是',
  `time` int(10) unsigned NOT NULL COMMENT '同步时间',
  `state` tinyint(3) unsigned NOT NULL DEFAULT '1' COMMENT '状态 1:同步开始|2:同步成功|3:同步失败',
  `time_succ` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '同步成功时间',
  `reason` text NOT NULL COMMENT '同步失败原因',
  PRIMARY KEY (`id`),
  KEY `log_id` (`log_id`),
  KEY `cron_id` (`cron_id`),
  KEY `host_id` (`host_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='日志-任务同步详情';

-- ----------------------------
-- Table structure for log_cron_run
-- ----------------------------
DROP TABLE IF EXISTS `log_cron_run`;
CREATE TABLE IF NOT EXISTS `log_cron_run` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `cron_id` int(10) unsigned NOT NULL COMMENT '任务ID',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  `time` int(10) unsigned NOT NULL COMMENT '执行时间',
  `result` text NOT NULL COMMENT '执行结果',
  `runtime` int(10) unsigned NOT NULL COMMENT '运行时间(毫秒)',
  `wall_clock` int(10) unsigned NOT NULL COMMENT '运行时间(毫秒)',
  PRIMARY KEY (`id`),
  KEY `cron_id` (`cron_id`),
  KEY `host_id` (`host_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='日志-任务执行日志';

-- ----------------------------
-- Table structure for stat_load
-- ----------------------------
DROP TABLE IF EXISTS `stat_load`;
CREATE TABLE IF NOT EXISTS `stat_load` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  `time` int(10) unsigned NOT NULL COMMENT '时间戳',
  `hi_key` char(5) NOT NULL COMMENT '时分key',
  `load1` varchar(10) NOT NULL COMMENT '负载-最近1分钟',
  `load5` varchar(10) NOT NULL COMMENT '负载-最近5分钟',
  `load15` varchar(10) NOT NULL COMMENT '负载-最近15分钟',
  PRIMARY KEY (`id`),
  KEY `host_id` (`host_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='统计-负载';

-- ----------------------------
-- Table structure for stat_cpu_used
-- ----------------------------
DROP TABLE IF EXISTS `stat_cpu_used`;
CREATE TABLE IF NOT EXISTS `stat_cpu_used` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  `time` int(10) unsigned NOT NULL COMMENT '时间戳',
  `hi_key` char(5) NOT NULL COMMENT '时分key',
  `used` float(5,2) NOT NULL COMMENT '使用率',
  PRIMARY KEY (`id`),
  KEY `host_id` (`host_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='统计-cpu使用率';

-- ----------------------------
-- Table structure for stat_mem
-- ----------------------------
DROP TABLE IF EXISTS `stat_mem`;
CREATE TABLE IF NOT EXISTS `stat_mem` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  `time` int(10) unsigned NOT NULL COMMENT '时间',
  `hi_key` char(5) NOT NULL COMMENT '时分key',
  `mem_total` int(10) unsigned NOT NULL COMMENT '总内存(kB)',
  `mem_used` int(10) unsigned NOT NULL COMMENT '已使用内存(kB)',
  PRIMARY KEY (`id`),
  KEY `host_id` (`host_id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='统计-内存';


-- ----------------------------
-- Table structure for game
-- ----------------------------
DROP TABLE IF EXISTS `game`;
CREATE TABLE IF NOT EXISTS `game` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `key` varchar(20) NOT NULL COMMENT '标识',
  `name` varchar(30) NOT NULL COMMENT '名称',
  `secret` char(32) NOT NULL COMMENT '密钥',
  `time` int(10) unsigned NOT NULL COMMENT '添加时间',
  `remark` text NOT NULL COMMENT '备注',
  PRIMARY KEY (`id`),
  UNIQUE KEY `key` (`key`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='游戏';

-- ----------------------------
-- Table structure for game_host
-- ----------------------------
DROP TABLE IF EXISTS `game_host`;
CREATE TABLE IF NOT EXISTS `game_host` (
  `game_key` varchar(20) NOT NULL COMMENT '游戏标识',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  PRIMARY KEY (`game_key`, `host_id`),
  KEY `game_key` (`game_key`),
  KEY `host_id` (`host_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='游戏主机';

-- ----------------------------
-- Table structure for log_game_cmd
-- ----------------------------
DROP TABLE IF EXISTS `log_game_cmd`;
CREATE TABLE IF NOT EXISTS `log_game_cmd` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `game_key` varchar(20) NOT NULL COMMENT '游戏标识',
  `cmd_id` int(10) unsigned NOT NULL COMMENT '命令ID',
  `run_time` int(10) unsigned NOT NULL COMMENT '执行时间',
  `run_cnt_all` int(10) unsigned NOT NULL COMMENT '执行主机数量',
  `run_cnt_succ` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '执行主机成功数量',
  PRIMARY KEY (`id`),
  KEY `game_key` (`game_key`),
  KEY `cmd_id` (`cmd_id`),
  KEY `run_time` (`run_time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='日志-游戏命令执行';

-- ----------------------------
-- Table structure for log_game_cmd_detail
-- ----------------------------
DROP TABLE IF EXISTS `log_game_cmd_detail`;
CREATE TABLE IF NOT EXISTS `log_game_cmd_detail` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `game_key` varchar(20) NOT NULL COMMENT '游戏标识',
  `log_id` int(10) unsigned NOT NULL COMMENT '命令日志ID',
  `cmd_id` int(10) unsigned NOT NULL COMMENT '命令ID',
  `host_id` int(10) unsigned NOT NULL COMMENT '主机ID',
  `run_time` int(10) unsigned NOT NULL COMMENT '执行时间',
  `result_time` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '命令返回时间',
  `result_text` text NOT NULL COMMENT '命令返回结果',
  PRIMARY KEY (`id`),
  KEY `game_key` (`game_key`),
  KEY `log_id` (`log_id`),
  KEY `cmd_id` (`cmd_id`),
  KEY `host_id` (`host_id`),
  KEY `run_time` (`run_time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4 COMMENT='日志-游戏命令执行详情';
