package xin.vanilla.banira.plugin.kanri;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import jakarta.annotation.Nonnull;
import xin.vanilla.banira.domain.KanriContext;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.List;
import java.util.Set;

public interface KanriHandler {

    int NIL = 0;
    int SUCCESS = 1;
    int FAIL = -1;
    int NO_OP = -2;
    int BOT_NO_OP = -3;

    /**
     * 没有权限操作的QQ
     */
    Set<Long> nop = BaniraUtils.mutableSetOf();

    @Nonnull
    List<String> getHelpInfo(String type);

    /**
     * 机器人是否有权限执行
     *
     * @param context 上下文
     */
    boolean botHasPermission(@Nonnull KanriContext context);

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

    default String getKanriCommand() {
        return BaniraUtils.getInsPrefixWithSpace() + String.join("|", getAction());
    }

    default void clearFail() {
        nop.clear();
    }

    /**
     * 提示没有权限
     */
    default void executeFail(@Nonnull KanriContext context) {
        if (!nop.isEmpty()) {
            MsgUtils builder = MsgUtils.builder();
            if (context.msgId() > 0) {
                builder.reply(context.msgId());
            } else {
                builder.reply(context.guildMsgId());
            }
            context.bot().sendGroupMsg(context.group()
                    , builder.text(String.format("你没有权限对%s执行该操作", nop)).build()
                    , false
            );
            clearFail();
        }
    }

    @Nonnull
    default Set<Long> getQQs(String[] args) {
        Set<Long> qqs = BaniraUtils.mutableSetOf();
        for (String arg : args) {
            if (BaniraUtils.hasAt(arg)) {
                qqs.add(BaniraUtils.getAtQQ(arg));
            } else {
                long l = StringUtils.toLong(arg, -1L);
                if (BaniraUtils.isFriendIdValid(l)) qqs.add(l);
            }
        }
        return qqs;
    }

    @Nonnull
    default Set<Long> getQQsWithoutReplay(@Nonnull KanriContext context, @Nonnull String[] args) {
        Set<Long> result = BaniraUtils.mutableSetOf();
        result.addAll(ShiroUtils.getAtList(context.event().getArrayMsg()));
        result.addAll(getQQs(args));
        return result;
    }

    @Nonnull
    default Set<Long> getQQsWithReplay(@Nonnull KanriContext context, @Nonnull String[] args) {
        Set<Long> result = BaniraUtils.mutableSetOf();
        if (BaniraUtils.hasReply(context.event().getArrayMsg())) {
            result.add(BaniraUtils.getReplyQQ(context.bot(), context.group(), context.event().getArrayMsg()));
        } else if (BaniraUtils.hasAtAll(context.event().getArrayMsg())) {
            result.add(233L);
        }
        result.addAll(getQQsWithoutReplay(context, args));
        return result;
    }

}
