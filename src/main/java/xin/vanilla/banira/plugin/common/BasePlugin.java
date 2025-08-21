package xin.vanilla.banira.plugin.common;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.context.event.EventListener;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.event.ConfigReloadedEvent;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.RegUtils;

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

    private Pattern BASE_COMMAND_PATTERN;
    private Pattern KANRI_COMMAND_PATTERN;
    private final Set<Pattern> TIMER_COMMAND_PATTERN = BaniraUtils.mutableSetOf();
    private final Set<Pattern> KEYWORD_COMMAND_PATTERN = BaniraUtils.mutableSetOf();

    public static String replaceReplay(String msg) {
        return BaniraUtils.replaceReply(msg);
    }

    // region 基础指令

    private Pattern baseCommand() {
        if (BASE_COMMAND_PATTERN == null) {
            BASE_COMMAND_PATTERN = RegUtils.start()
                    .groupByName("prefix", globalConfig.get().instConfig().prefix())
                    .groupIgByName("prefixSpace", RegUtils.REG_SEPARATOR)
                    .compile();
        }
        return BASE_COMMAND_PATTERN;
    }

    /**
     * 是否是指令
     */
    public boolean isCommand(String msg) {
        return baseCommand()
                .matcher(replaceReplay(msg))
                .find();
    }

    /**
     * 删除指令前缀
     */
    public String replaceCommand(String msg) {
        return baseCommand()
                .matcher(replaceReplay(msg))
                .replaceAll("");
    }

    // endregion 基础指令

    // region 群管指令

    private Pattern kanriCommand() {
        if (KANRI_COMMAND_PATTERN == null) {
            RegUtils regUtils = RegUtils.start()
                    .groupByName("prefix", globalConfig.get().instConfig().prefix())
                    .groupIgByName("prefixSpace", RegUtils.REG_SEPARATOR);
            if (CollectionUtils.isNotNullOrEmpty(globalConfig.get().instConfig().kanri().prefix())) {
                regUtils.groupByName("action", globalConfig.get().instConfig().kanri().prefix())
                        .groupIgByName("actionSpace", RegUtils.REG_SEPARATOR);
            }
            KANRI_COMMAND_PATTERN = regUtils.compile();
        }
        return KANRI_COMMAND_PATTERN;
    }

    /**
     * 是否是群管指令
     */
    public boolean isKanriCommand(String msg) {
        return kanriCommand().matcher(replaceReplay(msg)).find();
    }

    /**
     * 删除群管指令前缀
     */
    public String replaceKanriCommand(String msg) {
        return kanriCommand().matcher(replaceReplay(msg)).replaceAll("");
    }


    // endregion 群管指令

    // region 定时器指令

    private Set<Pattern> timerCommand() {
        if (TIMER_COMMAND_PATTERN.isEmpty()) {
            globalConfig.get().instConfig().timer().locator()
                    .forEach(kv -> TIMER_COMMAND_PATTERN.add(
                            RegUtils.start()
                                    .groupByName("prefix", globalConfig.get().instConfig().prefix())
                                    .groupIgByName("prefixSpace", RegUtils.REG_SEPARATOR)
                                    .groupByName("actionStart", kv.getKey())
                                    .groupIgByName("actionStartSpace", RegUtils.REG_SEPARATOR)
                                    .groupIgByName("timerKey", RegUtils.REG_NOT_SEPARATOR)
                                    .groupByName("actionEnd", kv.getKey())
                                    .groupIgByName("actionEndSpace", RegUtils.REG_SEPARATOR)
                                    .groupIgByName("timerValue", ".*")
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
                        pattern.matcher(replaceReplay(msg)).find()
                );
    }

    /**
     * 获取定时器指令匹配器
     */
    public Matcher getTimerCommandMatcher(String msg) {
        return timerCommand().stream()
                .filter(pattern ->
                        pattern.matcher(replaceReplay(msg)).find()
                )
                .findFirst()
                .map(pattern ->
                        pattern.matcher(replaceReplay(msg))
                )
                .orElse(null);
    }

    // endregion 定时器指令

    // region 关键词指令

    private Set<Pattern> keywordCommand() {
        if (KEYWORD_COMMAND_PATTERN.isEmpty()) {
            KeyInstructionsConfig keyInsConfig = globalConfig.get().instConfig().key();
            BaseInstructionsConfig baseInsConfig = globalConfig.get().instConfig().base();

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
                            RegUtils.start()
                                    .groupByName("prefix", globalConfig.get().instConfig().prefix())
                                    .groupIgByName("prefixSpace", RegUtils.REG_SEPARATOR)
                                    .groupByName("actionStart", kv.getKey())
                                    .groupIgByName("actionStartSpace", RegUtils.REG_SEPARATOR)
                                    .groupByName("keywordAction", keywordActions)
                                    .groupIgByName("keywordActionSpace", RegUtils.REG_SEPARATOR)
                                    .groupIgByName("keywordTarget", keywordTargets).appendIg("?")
                                    .groupIgByName("keywordTargetSpace", RegUtils.REG_SEPARATOR).appendIg("?")
                                    .groupIgByName("keywordType", keywordTypes)
                                    .groupIgByName("keywordTypeSpace", RegUtils.REG_SEPARATOR)
                                    .groupIgByName("keywordKey", "[.\\s]*?")
                                    .groupIgByName("keywordKeySpace", RegUtils.REG_SEPARATOR)
                                    .groupByName("actionEnd", kv.getKey())
                                    .groupIgByName("actionEndSpace", RegUtils.REG_SEPARATOR)
                                    .groupIgByName("keywordValue", "[.\\s]*")
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
                        pattern.matcher(replaceReplay(msg)).find()
                );
    }

    /**
     * 获取关键词指令匹配器
     */
    public Matcher getKeywordCommandMatcher(String msg) {
        return keywordCommand().stream()
                .filter(pattern ->
                        pattern.matcher(replaceReplay(msg)).find()
                )
                .findFirst()
                .map(pattern ->
                        pattern.matcher(replaceReplay(msg))
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
     * @param type    帮助类型
     * @param groupId 群组ID
     */
    @Nonnull
    public abstract List<String> getHelpInfo(@Nonnull String type, @Nullable Long groupId);

}
