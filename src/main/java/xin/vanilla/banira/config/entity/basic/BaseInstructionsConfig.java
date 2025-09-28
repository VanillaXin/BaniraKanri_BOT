package xin.vanilla.banira.config.entity.basic;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.List;

/**
 * 基础指令配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class BaseInstructionsConfig {

    /**
     * 添加 (如: 加入黑名单)
     */
    private List<String> add;
    /**
     * 删除 (如: 删除群管理)
     */
    private List<String> del;
    /**
     * 查询 (如: 查询黑名单)
     */
    private List<String> list;
    /**
     * 启用 (如: 启用词库)
     */
    private List<String> enable;
    /**
     * 禁用 (如: 禁用词库)
     */
    private List<String> disable;
    /**
     * 全局 (如: 全局词库)
     */
    private List<String> global;
    /**
     * 当前 (如: 当前群)
     */
    private List<String> that;
    /**
     * 艾特全体
     */
    private List<String> atAll;
    /**
     * 状态 (如: 系统状态)
     */
    private List<String> status;
    /**
     * 帮助 (如: 指令帮助)
     */
    private List<String> help;
    /**
     * 刷新
     */
    private List<String> refresh;

    /**
     * /**
     * 例子
     */
    private List<String> example;


    {
        this.add = BaniraUtils.mutableListOf("添加", "add", "put", "insert");
        this.del = BaniraUtils.mutableListOf("删除", "del", "delete", "remove");
        this.list = BaniraUtils.mutableListOf("查询", "list", "ls", "sel", "get");
        this.enable = BaniraUtils.mutableListOf("启用", "enable", "open");
        this.disable = BaniraUtils.mutableListOf("禁用", "disable", "close");
        this.global = BaniraUtils.mutableListOf("全局", "all", "global");
        this.that = BaniraUtils.mutableListOf("当前", "that", "this", "here");
        this.atAll = BaniraUtils.mutableListOf("@全体成员", "@全体", "@所有人", "@all", "@All", "@ALL");
        this.status = BaniraUtils.mutableListOf("统计", "状态", "status", "statistics", "stats");
        this.help = BaniraUtils.mutableListOf("帮助", "指令帮助", "help", "tips");
        this.refresh = BaniraUtils.mutableListOf("刷新", "重载", "reload", "refresh");
        this.example = BaniraUtils.mutableListOf("示例", "例子", "栗子", "🌰", "demo", "example", "case");
    }

}
