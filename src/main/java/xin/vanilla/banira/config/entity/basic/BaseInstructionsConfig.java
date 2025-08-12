package xin.vanilla.banira.config.entity.basic;

import lombok.experimental.Accessors;
import xin.vanilla.banira.util.BaniraUtils;

import java.util.Set;

/**
 * 基础指令配置
 *
 * @param add    添加 (如: 加入黑名单)
 * @param del    删除 (如: 删除群管理)
 * @param list   查询 (如: 查询黑名单)
 * @param global 全局 (如: 全局词库)
 * @param that   当前 (如: 当前群)
 * @param atAll  艾特全体
 */
@Accessors(chain = true)
public record BaseInstructionsConfig(
        Set<String> add,
        Set<String> del,
        Set<String> list,
        Set<String> global,
        Set<String> that,
        Set<String> atAll
) {

    public static BaseInstructionsConfig preset() {
        return new BaseInstructionsConfig(
                BaniraUtils.mutableSetOf("添加", "add", "put", "insert"),
                BaniraUtils.mutableSetOf("删除", "del", "delete"),
                BaniraUtils.mutableSetOf("查询", "list", "ls", "sel"),
                BaniraUtils.mutableSetOf("全局", "all", "global"),
                BaniraUtils.mutableSetOf("当前", "that", "this", "here"),
                BaniraUtils.mutableSetOf("@全体成员", "@全体", "@所有人", "@all", "@All", "@ALL")
        );
    }

}
