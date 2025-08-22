package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;

import java.util.Arrays;
import java.util.List;

/**
 * 基础指令配置
 *
 * @param add     添加 (如: 加入黑名单)
 * @param del     删除 (如: 删除群管理)
 * @param list    查询 (如: 查询黑名单)
 * @param enable  启用 (如: 启用词库)
 * @param disable 禁用 (如: 禁用词库)
 * @param global  全局 (如: 全局词库)
 * @param that    当前 (如: 当前群)
 * @param atAll   艾特全体
 * @param status  状态 (如: 系统状态)
 */
@Accessors(chain = true)
public record BaseInstructionsConfig(
        List<String> add,
        List<String> del,
        List<String> list,
        List<String> enable,
        List<String> disable,
        List<String> global,
        List<String> that,
        List<String> atAll,
        List<String> status,
        List<String> help
) {

    public static BaseInstructionsConfig preset() {
        return new BaseInstructionsConfig(
                Arrays.asList("添加", "add", "put", "insert"),
                Arrays.asList("删除", "del", "delete", "remove"),
                Arrays.asList("查询", "list", "ls", "sel", "get"),
                Arrays.asList("启用", "enable", "open"),
                Arrays.asList("禁用", "disable", "close"),
                Arrays.asList("全局", "all", "global"),
                Arrays.asList("当前", "that", "this", "here"),
                Arrays.asList("@全体成员", "@全体", "@所有人", "@all", "@All", "@ALL"),
                Arrays.asList("统计", "状态", "status", "statistics", "stats"),
                Arrays.asList("帮助", "指令帮助", "help", "tips")
        );
    }

}
