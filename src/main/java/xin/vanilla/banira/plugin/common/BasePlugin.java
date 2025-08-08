package xin.vanilla.banira.plugin.common;

import jakarta.annotation.Resource;
import xin.vanilla.banira.config.entity.GlobalConfig;
import xin.vanilla.banira.config.entity.GroupConfig;
import xin.vanilla.banira.config.entity.basic.BaseInstructionsConfig;
import xin.vanilla.banira.config.entity.basic.KeyInstructionsConfig;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.RegUtils;

import java.util.Set;
import java.util.function.Supplier;
import java.util.regex.Matcher;

public abstract class BasePlugin {

    @Resource
    protected Supplier<GlobalConfig> globalConfig;
    @Resource
    protected Supplier<GroupConfig> groupConfig;

    public static String replaceReplay(String msg) {
        return BaniraUtils.replaceReplay(msg);
    }

    public boolean isCommond(String msg) {
        return RegUtils.start()
                .groupByName("prefix", globalConfig.get().instConfig().prefix())
                .groupIgByName("prefixSpace", RegUtils.REG_SEPARATOR)
                .compile()
                .matcher(replaceReplay(msg))
                .find();
    }

    private Matcher kanriCommand(String msg) {
        RegUtils regUtils = RegUtils.start()
                .groupByName("prefix", globalConfig.get().instConfig().prefix())
                .groupIgByName("prefixSpace", RegUtils.REG_SEPARATOR);
        if (CollectionUtils.isNotNullOrEmpty(globalConfig.get().instConfig().kanri().prefix())) {
            regUtils.groupByName("action", globalConfig.get().instConfig().kanri().prefix())
                    .groupIgByName("actionSpace", RegUtils.REG_SEPARATOR);
        }
        return regUtils.compile().matcher(replaceReplay(msg));
    }

    public boolean isKanriCommand(String msg) {
        return kanriCommand(replaceReplay(msg)).find();
    }

    public String replaceKanriCommand(String msg) {
        return kanriCommand(replaceReplay(msg)).replaceAll("");
    }

    public boolean isTimerCommand(String msg) {
        return globalConfig.get().instConfig().timer().locator().stream()
                .anyMatch(kv -> RegUtils.start()
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
                        .matcher(replaceReplay(msg))
                        .find()
                );
    }

    public boolean isKeywordCommand(String msg) {
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
        return keyInsConfig.locator().stream()
                .anyMatch(kv -> RegUtils.start()
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
                        .groupIgByName("keywordKey", RegUtils.REG_NOT_SEPARATOR)
                        .groupIgByName("keywordKeySpace", RegUtils.REG_SEPARATOR)
                        .groupByName("actionEnd", kv.getKey())
                        .groupIgByName("actionEndSpace", RegUtils.REG_SEPARATOR)
                        .groupIgByName("keywordValue", ".*")
                        .end()
                        .compile()
                        .matcher(replaceReplay(msg))
                        .find()
                );
    }


}
