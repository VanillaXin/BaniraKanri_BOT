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
import java.util.Optional;
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
     * @param groupId 群号
     * @param path    功能路径（可为空）
     * @param page    页码，从 1 开始
     */
    @Nonnull
    public List<HelpMessage> buildMessages(@Nullable Long groupId
            , @Nonnull List<String> path
            , long page
    ) {
        if (page <= 0) {
            page = 1;
        }
        List<HelpMessage> messages = new ArrayList<>();
        messages.add(HelpMessage.of(buildUsageHeader()));

        if (path.isEmpty()) {
            appendFeatureListMessages(messages, registry.list(groupId));
            return paginate(messages, page);
        }

        HelpTopic root = registry.resolve(groupId, path.getFirst());
        if (root == null) {
            messages.add(HelpMessage.of("未找到功能：" + path.getFirst()));
            return paginate(messages, page);
        }

        messages.add(HelpMessage.of(buildFeatureSection(root), root.name()));

        HelpTopic focus = root;
        for (int i = 1; i < path.size(); i++) {
            Optional<HelpTopic> next = focus.findChild(path.get(i));
            if (next.isEmpty()) {
                messages.add(HelpMessage.of("未找到子功能：" + path.get(i)));
                return paginate(messages, page);
            }
            focus = next.get();
        }

        appendFocusContent(messages, focus);
        return paginate(messages, page);
    }

    /**
     * 仅展示当前聚焦节点的子功能列表或详情，不包含上级节点的同级功能
     */
    private void appendFocusContent(@Nonnull List<HelpMessage> messages, @Nonnull HelpTopic focus) {
        if (!focus.sortedChildren().isEmpty()) {
            appendSubFeatureEntries(messages, focus.sortedChildren());
            return;
        }
        if (StringUtils.isNotNullOrEmpty(focus.detail())) {
            messages.add(HelpMessage.of(buildSubFeatureDetail(focus), focus.name()));
        }
    }

    @Nonnull
    private String buildUsageHeader() {
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        String helpIns = HelpTopics.joinAliases(insConfig.get().base().help());
        return "指令帮助\n\n"
                + "用法：\n"
                + prefix + helpIns + " [<页数>]\n"
                + prefix + helpIns + " <功能别名> [<页数>]\n"
                + prefix + helpIns + " <功能别名> <子功能别名> [<页数>]\n"
                + prefix + helpIns + " <功能别名> <子功能别名> ... [<页数>]\n\n"
                + "示例：\n"
                + prefix + insConfig.get().base().help().getFirst() + "\n"
                + prefix + insConfig.get().base().help().getFirst() + " keyword\n"
                + prefix + insConfig.get().base().help().getFirst() + " mcmod 评论检测\n"
                + prefix + insConfig.get().base().help().getFirst() + " mcmod 评论检测 添加";
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
        }
        return sb.toString().stripTrailing();
    }

    @Nonnull
    private List<HelpMessage> paginate(@Nonnull List<HelpMessage> messages, long page) {
        return messages.stream()
                .skip((page - 1) * PAGE_SIZE)
                .limit(PAGE_SIZE)
                .toList();
    }

}
