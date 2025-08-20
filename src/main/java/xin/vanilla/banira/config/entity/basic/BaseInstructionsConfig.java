package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

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
        Set<String> add,
        Set<String> del,
        Set<String> list,
        Set<String> enable,
        Set<String> disable,
        Set<String> global,
        Set<String> that,
        Set<String> atAll,
        Set<String> status,
        Set<String> help
) {

    public static BaseInstructionsConfig preset() {
        return new BaseInstructionsConfig(
                BaniraUtils.mutableSetOf("添加", "add", "put", "insert"),
                BaniraUtils.mutableSetOf("删除", "del", "delete", "remove"),
                BaniraUtils.mutableSetOf("查询", "list", "ls", "sel", "get"),
                BaniraUtils.mutableSetOf("启用", "enable", "open"),
                BaniraUtils.mutableSetOf("禁用", "disable", "close"),
                BaniraUtils.mutableSetOf("全局", "all", "global"),
                BaniraUtils.mutableSetOf("当前", "that", "this", "here"),
                BaniraUtils.mutableSetOf("@全体成员", "@全体", "@所有人", "@all", "@All", "@ALL"),
                BaniraUtils.mutableSetOf("统计", "状态", "status", "statistics", "stats"),
                BaniraUtils.mutableSetOf("帮助", "指令帮助", "help", "tips")
        );
    }

}
