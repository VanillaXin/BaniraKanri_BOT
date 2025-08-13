package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.Set;

public interface KanriHandler {

    int SUCCESS = 1;
    int FAIL = 0;
    int NO_PERMISSION = -1;

    /**
     * 没有权限操作的QQ
     */
    Set<Long> fail = BaniraUtils.mutableSetOf();

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
    int execute(@Nonnull KanriContext context, @Nonnull String[] args);

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

    default void clearFail() {
        fail.clear();
    }

    /**
     * 提示没有权限
     */
    default void executeFail(@Nonnull KanriContext context) {
        if (!fail.isEmpty()) {
            MsgUtils builder = MsgUtils.builder();
            if (context.msgId() > 0) {
                builder.reply(context.msgId());
            } else {
                builder.reply(context.guildMsgId());
            }
            context.bot().sendGroupMsg(context.group()
                    , builder.text(String.format("你没有权限对%s执行该操作", fail)).build()
                    , false
            );
            clearFail();
        }
    }

}
