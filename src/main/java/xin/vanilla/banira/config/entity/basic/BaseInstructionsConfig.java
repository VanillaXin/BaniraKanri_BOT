package xin.vanilla.banira.config.entity.basic;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.Arrays;
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
     * 例子
     */
    private List<String> example;


    {
        this.add = Arrays.asList("添加", "add", "put", "insert");
        this.del = Arrays.asList("删除", "del", "delete", "remove");
        this.list = Arrays.asList("查询", "list", "ls", "sel", "get");
        this.enable = Arrays.asList("启用", "enable", "open");
        this.disable = Arrays.asList("禁用", "disable", "close");
        this.global = Arrays.asList("全局", "all", "global");
        this.that = Arrays.asList("当前", "that", "this", "here");
        this.atAll = Arrays.asList("@全体成员", "@全体", "@所有人", "@all", "@All", "@ALL");
        this.status = Arrays.asList("统计", "状态", "status", "statistics", "stats");
        this.help = Arrays.asList("帮助", "指令帮助", "help", "tips");
        this.example = Arrays.asList("示例", "例子", "栗子", "🌰", "demo", "example", "case");
    }

}
