package xin.vanilla.banira.plugin.help;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/**
 * 帮助查询与格式化服务
 */
@Service
public class HelpService {

    private static final long PAGE_SIZE = 98L;
    private static final String SENDER_FEATURE_LIST = "功能列表";
    private static final String SENDER_SUB_FEATURE_LIST = "子功能列表";

    @Resource
    private HelpTopicRegistry registry;
    @Resource
    private Supplier<InstructionsConfig> insConfig;

    /**
     * 构建帮助合并转发消息，每条对应合并转发中的一条消息
     *
     * @param groupId      群号
     * @param featureAlias 功能别名，可为空
     * @param subAlias     子功能别名，可为空
     * @param page         页码，从 1 开始
     */
    @Nonnull
    public List<HelpMessage> buildMessages(@Nullable Long groupId
            , @Nullable String featureAlias
            , @Nullable String subAlias
            , long page
    ) {
        if (page <= 0) {
            page = 1;
        }
        List<HelpMessage> messages = new ArrayList<>();
        // 第二条：统一帮助提示，使用机器人默认昵称
        messages.add(HelpMessage.of(buildUsageHeader()));

        if (StringUtils.isNullOrEmptyEx(featureAlias)) {
            appendFeatureListMessages(messages, registry.list(groupId));
            return paginate(messages, page);
        }

        HelpTopic topic = registry.resolve(groupId, featureAlias);
        if (topic == null) {
            messages.add(HelpMessage.of("未找到功能：" + featureAlias));
            return paginate(messages, page);
        }

        messages.add(HelpMessage.of(buildFeatureSection(topic), topic.name()));

        if (topic.sortedChildren().isEmpty()) {
            if (StringUtils.isNotNullOrEmpty(topic.detail())) {
                messages.add(HelpMessage.of(buildDetailSection(topic.name(), topic.detail()), topic.name()));
            }
            return paginate(messages, page);
        }

        appendSubFeatureEntries(messages, topic.sortedChildren());

        if (StringUtils.isNotNullOrEmpty(subAlias)) {
            topic.findChild(subAlias).ifPresent(sub ->
                    messages.add(HelpMessage.of(buildSubFeatureDetail(sub), sub.name()))
            );
        }

        return paginate(messages, page);
    }

    @Nonnull
    private String buildUsageHeader() {
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        String helpIns = HelpTopics.joinAliases(insConfig.get().base().help());
        return "指令帮助\n\n"
                + "用法：\n"
                + prefix + helpIns + " [<页数>]\n"
                + prefix + helpIns + " <功能别名> [<页数>]\n"
                + prefix + helpIns + " <功能别名> <子功能别名> [<页数>]\n\n"
                + "示例：\n"
                + prefix + insConfig.get().base().help().getFirst() + "\n"
                + prefix + insConfig.get().base().help().getFirst() + " keyword\n"
                + prefix + insConfig.get().base().help().getFirst() + " keyword add";
    }

    private void appendFeatureListMessages(@Nonnull List<HelpMessage> messages, @Nonnull List<HelpTopic> topics) {
        if (topics.isEmpty()) {
            messages.add(HelpMessage.of("暂无可用功能。", SENDER_FEATURE_LIST));
            return;
        }
        for (HelpTopic topic : topics) {
            messages.add(HelpMessage.of(formatTopicEntry(topic), SENDER_FEATURE_LIST));
        }
    }

    private void appendSubFeatureEntries(@Nonnull List<HelpMessage> messages, @Nonnull List<HelpTopic> topics) {
        for (HelpTopic topic : topics) {
            messages.add(HelpMessage.of(formatTopicEntry(topic), SENDER_SUB_FEATURE_LIST));
        }
    }

    @Nonnull
    private String formatTopicEntry(@Nonnull HelpTopic topic) {
        StringBuilder sb = new StringBuilder();
        sb.append("功能：").append(topic.name()).append('\n');
        List<String> aliases = topic.displayAliases();
        if (!aliases.isEmpty()) {
            sb.append("别名：").append(HelpTopics.joinAliases(aliases)).append('\n');
        }
        if (StringUtils.isNotNullOrEmpty(topic.description())) {
            sb.append("描述：").append(topic.description());
        }
        return sb.toString().stripTrailing();
    }

    @Nonnull
    private String buildFeatureSection(@Nonnull HelpTopic topic) {
        return formatTopicEntry(topic);
    }

    @Nonnull
    private String buildSubFeatureDetail(@Nonnull HelpTopic sub) {
        StringBuilder sb = new StringBuilder();
        sb.append("子功能：").append(sub.name()).append('\n');
        sb.append("别名：").append(HelpTopics.joinAliases(sub.displayAliases())).append('\n');
        if (StringUtils.isNotNullOrEmpty(sub.description())) {
            sb.append("描述：").append(sub.description()).append('\n');
        }
        if (StringUtils.isNotNullOrEmpty(sub.detail())) {
            sb.append('\n').append(sub.detail());
        } else if (!sub.sortedChildren().isEmpty()) {
            sb.append("\n\n");
            for (HelpTopic child : sub.sortedChildren()) {
                sb.append('【').append(child.name()).append("】\n");
                if (StringUtils.isNotNullOrEmpty(child.detail())) {
                    sb.append(child.detail());
                }
                sb.append("\n\n");
            }
        }
        return sb.toString().stripTrailing();
    }

    @Nonnull
    private String buildDetailSection(@Nonnull String title, @Nonnull String detail) {
        return title + "\n\n" + detail;
    }

    @Nonnull
    private List<HelpMessage> paginate(@Nonnull List<HelpMessage> messages, long page) {
        return messages.stream()
                .skip((page - 1) * PAGE_SIZE)
                .limit(PAGE_SIZE)
                .toList();
    }

}
