package xin.vanilla.banira.plugin.common;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.context.event.EventListener;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.event.ConfigReloadedEvent;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.RegexpHelper;

import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class BasePlugin {

    @Resource
    protected Supplier<GlobalConfig> globalConfig;
    @Resource
    protected Supplier<GroupConfig> groupConfig;
    @Resource
    protected Supplier<InstructionsConfig> insConfig;

    private Pattern BASE_COMMAND_PATTERN;
    private Pattern KANRI_COMMAND_PATTERN;
    private final Set<Pattern> TIMER_COMMAND_PATTERN = BaniraUtils.mutableSetOf();
    private final Set<Pattern> KEYWORD_COMMAND_PATTERN = BaniraUtils.mutableSetOf();

    public static String replaceReply(String msg) {
        return BaniraUtils.replaceReply(msg);
    }

    // region 基础指令

    private Pattern baseCommand() {
        if (BASE_COMMAND_PATTERN == null) {
            BASE_COMMAND_PATTERN = RegexpHelper.start()
                    .groupByName("prefix", insConfig.get().prefix())
                    .groupIgByName("prefixSpace", RegexpHelper.REG_SEPARATOR)
                    .compile();
        }
        return BASE_COMMAND_PATTERN;
    }

    /**
     * 是否是指令
     */
    public boolean isCommand(String msg) {
        return baseCommand()
                .matcher(replaceReply(msg))
                .find();
    }

    /**
     * 删除指令前缀
     */
    public String replaceCommand(String msg) {
        return baseCommand()
                .matcher(replaceReply(msg))
                .replaceAll("");
    }

    // endregion 基础指令

    // region 群管指令

    private Pattern kanriCommand() {
        if (KANRI_COMMAND_PATTERN == null) {
            RegexpHelper regexpHelper = RegexpHelper.start()
                    .groupByName("prefix", insConfig.get().prefix())
                    .groupIgByName("prefixSpace", RegexpHelper.REG_SEPARATOR);
            if (CollectionUtils.isNotNullOrEmpty(insConfig.get().kanri().prefix())) {
                regexpHelper.groupByName("action", insConfig.get().kanri().prefix())
                        .groupIgByName("actionSpace", RegexpHelper.REG_SEPARATOR);
            }
            KANRI_COMMAND_PATTERN = regexpHelper.compile();
        }
        return KANRI_COMMAND_PATTERN;
    }

    /**
     * 是否是群管指令
     */
    public boolean isKanriCommand(String msg) {
        return kanriCommand().matcher(replaceReply(msg)).find();
    }

    /**
     * 删除群管指令前缀
     */
    public String replaceKanriCommand(String msg) {
        return kanriCommand().matcher(replaceReply(msg)).replaceAll("");
    }


    // endregion 群管指令

    // region 定时器指令

    private Set<Pattern> timerCommand() {
        if (TIMER_COMMAND_PATTERN.isEmpty()) {
            BaseInstructionsConfig baseInsConfig = insConfig.get().base();

            Set<String> timerActions = BaniraUtils.mutableSetOf();
            timerActions.addAll(baseInsConfig.add());
            timerActions.addAll(baseInsConfig.del());
            timerActions.addAll(baseInsConfig.list());

            Set<String> timerTargets = BaniraUtils.mutableSetOf("<\\d{5,10}>");
            timerTargets.addAll(baseInsConfig.that());
            timerTargets.addAll(baseInsConfig.global());

            BaniraUtils.getTimerIns().locator()
                    .forEach(kv -> TIMER_COMMAND_PATTERN.add(
                            RegexpHelper.start()
                                    .groupByName("prefix", insConfig.get().prefix())
                                    .groupIgByName("prefixSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupByName("actionStart", kv.getKey())
                                    .groupIgByName("actionStartSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupByName("timerAction", timerActions)
                                    .groupIgByName("timerActionSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupIgByName("timerTarget", timerTargets).appendIg("?")
                                    .groupIgByName("timerTargetSpace", RegexpHelper.REG_SEPARATOR).appendIg("?")
                                    .groupIgByName("timerKey", "[^\r\n]+")
                                    .groupByName("actionEnd", kv.getValue())
                                    .groupIgByName("actionEndSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupIgByName("timerValue", "[\\s\\S]*")
                                    .end()
                                    .compile()
                    ));
        }
        return TIMER_COMMAND_PATTERN;
    }

    /**
     * 是否是定时器指令
     */
    public boolean isTimerCommand(String msg) {
        return timerCommand().stream()
                .anyMatch(pattern ->
                        pattern.matcher(replaceReply(msg)).find()
                );
    }

    /**
     * 获取定时器指令匹配器
     */
    public Matcher getTimerCommandMatcher(String msg) {
        return timerCommand().stream()
                .filter(pattern ->
                        pattern.matcher(replaceReply(msg)).find()
                )
                .findFirst()
                .map(pattern ->
                        pattern.matcher(replaceReply(msg))
                )
                .orElse(null);
    }

    // endregion 定时器指令

    // region 关键词指令

    private Set<Pattern> keywordCommand() {
        if (KEYWORD_COMMAND_PATTERN.isEmpty()) {
            KeyInstructionsConfig keyInsConfig = BaniraUtils.getKeyIns();
            BaseInstructionsConfig baseInsConfig = insConfig.get().base();

            Set<String> keywordActions = BaniraUtils.mutableSetOf();
            keywordActions.addAll(baseInsConfig.add());
            keywordActions.addAll(baseInsConfig.del());
            keywordActions.addAll(baseInsConfig.list());

            Set<String> keywordTargets = BaniraUtils.mutableSetOf("<\\d{5,10}>");
            keywordTargets.addAll(baseInsConfig.that());
            keywordTargets.addAll(baseInsConfig.global());

            Set<String> keywordTypes = BaniraUtils.mutableSetOf();
            keywordTypes.addAll(keyInsConfig.exactly());
            keywordTypes.addAll(keyInsConfig.contain());
            keywordTypes.addAll(keyInsConfig.pinyin());
            keywordTypes.addAll(keyInsConfig.regex());

            // /bk key add <target> perfect a rep b
            keyInsConfig.locator()
                    .forEach(kv -> KEYWORD_COMMAND_PATTERN.add(
                            RegexpHelper.start()
                                    .groupByName("prefix", insConfig.get().prefix())
                                    .groupIgByName("prefixSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupByName("actionStart", kv.getKey())
                                    .groupIgByName("actionStartSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupByName("keywordAction", keywordActions)
                                    .groupIgByName("keywordActionSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupIgByName("keywordTarget", keywordTargets).appendIg("?")
                                    .groupIgByName("keywordTargetSpace", RegexpHelper.REG_SEPARATOR).appendIg("?")
                                    .groupIgByName("keywordType", keywordTypes)
                                    .groupIgByName("keywordTypeSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupIgByName("keywordKey", "[\\s\\S]*?")
                                    .groupIgByName("keywordKeySpace", RegexpHelper.REG_SEPARATOR)
                                    .groupByName("actionEnd", kv.getValue())
                                    .groupIgByName("actionEndSpace", RegexpHelper.REG_SEPARATOR)
                                    .groupIgByName("keywordValue", "[\\s\\S]*")
                                    .end()
                                    .compile()
                    ));
        }
        return KEYWORD_COMMAND_PATTERN;
    }

    /**
     * 是否是关键词指令
     */
    public boolean isKeywordCommand(String msg) {
        return keywordCommand().stream()
                .anyMatch(pattern ->
                        pattern.matcher(replaceReply(msg)).find()
                );
    }

    /**
     * 获取关键词指令匹配器
     */
    public Matcher getKeywordCommandMatcher(String msg) {
        return keywordCommand().stream()
                .filter(pattern ->
                        pattern.matcher(replaceReply(msg)).find()
                )
                .findFirst()
                .map(pattern ->
                        pattern.matcher(replaceReply(msg))
                )
                .orElse(null);
    }

    // endregion 关键词指令


    /**
     * 监听配置重载事件并刷新指令匹配模式
     */
    @EventListener
    public void onGlobalConfigReloaded(ConfigReloadedEvent<GlobalConfig> event) {
        BASE_COMMAND_PATTERN = null;
        KANRI_COMMAND_PATTERN = null;
        TIMER_COMMAND_PATTERN.clear();
        KEYWORD_COMMAND_PATTERN.clear();
    }

    /**
     * 获取帮助信息
     *
     * @param groupId 群组ID
     * @param types   帮助类型
     */
    @Nonnull
    public abstract List<String> getHelpInfo(@Nullable Long groupId, @Nonnull String... types);

}
