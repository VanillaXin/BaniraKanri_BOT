package xin.vanilla.banira.plugin.kanri;

import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.kanri.KanriContext;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Set;

public interface KanriHandler {

    /**
     * 是否有权限执行
     *
     * @param context 上下文
     */
    boolean hasPermission(@Nonnull KanriContext context);

    /**
     * 动作，如 "ban"
     */
    @Nonnull
    Set<String> getAction();

    /**
     * 执行命令
     *
     * @param context 上下文
     * @param args    参数数组（已剔除前缀）
     * @return 命令执行结果
     */
    boolean execute(@Nonnull KanriContext context, @Nonnull String[] args);

    @Nonnull
    default Set<Long> getQQs(String[] args) {
        Set<Long> qqs = BaniraUtils.mutableSetOf();
        for (String arg : args) {
            if (BaniraUtils.hasAt(arg)) {
                qqs.add(BaniraUtils.getAtQQ(arg));
            } else {
                long l = StringUtils.toLong(arg, -1L);
                if (l != -1) qqs.add(l);
            }
        }
        return qqs;
    }

}
