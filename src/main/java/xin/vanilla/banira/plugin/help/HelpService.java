package xin.vanilla.banira.plugin.help;

import com.mikuac.shiro.common.utils.ShiroUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import org.springframework.stereotype.Service;
import xin.vanilla.banira.config.entity.InstructionsConfig;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * 帮助查询与格式化服务
 */
@Service
public class HelpService {

    private static final long PAGE_SIZE = 98L;
    private static final String SENDER_FEATURE_LIST = "功能列表";
    private static final String SENDER_SUB_FEATURE_LIST = "子功能列表";
    private static final int MAX_EXTENDED_NEST_DEPTH = 4;

    @Resource
    private HelpTopicRegistry registry;
    @Resource
    private Supplier<InstructionsConfig> insConfig;

    /**
     * 列出顶层功能主题名称与别名（不含详细用法）
     */
    @Nonnull
    public String listTopicNames(@Nullable Long groupId) {
        List<HelpTopic> topics = registry.list(groupId);
        if (topics.isEmpty()) {
            return "暂无可用功能。";
        }
        return topics.stream()
                .map(this::formatTopicEntry)
                .collect(Collectors.joining("\n\n"));
    }

    /**
     * 构建帮助合并转发消息，每条对应合并转发中的一条消息
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
            return paginateMessages(messages, page);
        }

        ResolvedHelp resolved = resolvePath(groupId, path);
        if (!resolved.ok()) {
            messages.add(HelpMessage.of(resolved.error()));
            return paginateMessages(messages, page);
        }

        messages.add(HelpMessage.of(buildFeatureSection(resolved.root()), resolved.root().name()));
        appendFocusContent(messages, resolved.focus());
        return paginateMessages(messages, page);
    }

    /**
     * 构建 -ex 嵌套合并转发节点（不含用户触发消息）
     */
    @Nonnull
    public List<Map<String, Object>> buildExtendedNodes(@Nullable Long groupId
            , @Nonnull List<String> path
            , long page
            , long botId
            , @Nonnull String defaultNickname
    ) {
        if (page <= 0) {
            page = 1;
        }
        List<Map<String, Object>> nodes = new ArrayList<>();
        nodes.add(singleNode(botId, defaultNickname, buildUsageHeader()));

        if (path.isEmpty()) {
            List<HelpTopic> topics = registry.list(groupId);
            if (topics.isEmpty()) {
                nodes.add(singleNode(botId, SENDER_FEATURE_LIST, "暂无可用功能。"));
            } else {
                for (HelpTopic topic : topics) {
                    nodes.add(wrapTopLevelNestedNode(botId, topic));
                }
            }
            return paginateNodes(nodes, page);
        }

        ResolvedHelp resolved = resolvePath(groupId, path);
        if (!resolved.ok()) {
            nodes.add(singleNode(botId, defaultNickname, resolved.error()));
            return paginateNodes(nodes, page);
        }

        nodes.add(singleNode(botId, resolved.root().name(), buildFeatureSection(resolved.root())));
        appendExtendedFocusContent(nodes, botId, resolved.focus(), path.size());
        return paginateNodes(nodes, page);
    }

    /**
     * 路径段数达到上限时，最外层改用简单文本展示
     */
    private static boolean shouldUseSimpleDisplay(int pathDepth) {
        return pathDepth >= MAX_EXTENDED_NEST_DEPTH - 1;
    }

    @Nonnull
    private ResolvedHelp resolvePath(@Nullable Long groupId, @Nonnull List<String> path) {
        HelpTopic root = registry.resolve(groupId, path.getFirst());
        if (root == null) {
            return ResolvedHelp.error("未找到功能：" + path.getFirst());
        }
        HelpTopic focus = root;
        for (int i = 1; i < path.size(); i++) {
            Optional<HelpTopic> next = focus.findChild(path.get(i));
            if (next.isEmpty()) {
                return ResolvedHelp.error("未找到子功能：" + path.get(i));
            }
            focus = next.get();
        }
        return ResolvedHelp.of(root, focus);
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

    private void appendExtendedFocusContent(@Nonnull List<Map<String, Object>> nodes
            , long botId
            , @Nonnull HelpTopic focus
            , int pathDepth
    ) {
        if (shouldUseSimpleDisplay(pathDepth)) {
            appendSimpleFocusContent(nodes, botId, focus);
            return;
        }
        if (!focus.sortedChildren().isEmpty()) {
            for (HelpTopic child : focus.sortedChildren()) {
                nodes.add(wrapNestedTopicNode(botId, child));
            }
            return;
        }
        if (StringUtils.isNotNullOrEmpty(focus.detail()) || StringUtils.isNotNullOrEmpty(focus.description())) {
            nodes.add(wrapNestedTopicNode(botId, focus));
        }
    }

    private void appendSimpleFocusContent(@Nonnull List<Map<String, Object>> nodes, long botId, @Nonnull HelpTopic focus) {
        if (!focus.sortedChildren().isEmpty()) {
            nodes.add(singleNode(botId, focus.name(), formatTopicEntry(focus)));
            nodes.add(singleNode(botId, SENDER_SUB_FEATURE_LIST, "包含子功能："));
            for (HelpTopic child : focus.sortedChildren()) {
                nodes.add(singleNode(botId, child.name(), buildLeafTextContent(child)));
            }
            return;
        }
        String content = StringUtils.isNotNullOrEmpty(focus.detail())
                ? buildSubFeatureDetail(focus)
                : formatTopicEntry(focus);
        nodes.add(singleNode(botId, focus.name(), content));
    }

    @Nonnull
    private Map<String, Object> wrapTopLevelNestedNode(long botId, @Nonnull HelpTopic topic) {
        return wrapNestedTopicNode(botId, topic, 1);
    }

    @Nonnull
    private Map<String, Object> wrapNestedTopicNode(long botId, @Nonnull HelpTopic topic) {
        return wrapNestedTopicNode(botId, topic, 1);
    }

    @Nonnull
    private Map<String, Object> wrapNestedTopicNode(long botId, @Nonnull HelpTopic topic, int nestLevel) {
        return nestedForwardNode(botId, topic.name(), buildNestedInnerForward(botId, topic, nestLevel));
    }

    /**
     * 构建嵌套合并转发的内层节点；有子功能且未达深度上限时，每个子功能独立套一层合并转发
     */
    @Nonnull
    private List<Map<String, Object>> buildNestedInnerForward(long botId, @Nonnull HelpTopic topic, int nestLevel) {
        List<Map<String, Object>> result = new ArrayList<>();
        result.add(singleNode(botId, topic.name(), formatTopicEntry(topic)));

        if (topic.sortedChildren().isEmpty()) {
            if (StringUtils.isNotNullOrEmpty(topic.detail())) {
                result.add(singleNode(botId, topic.name(), topic.detail()));
            }
            return result;
        }

        if (nestLevel >= MAX_EXTENDED_NEST_DEPTH) {
            result.add(singleNode(botId, SENDER_SUB_FEATURE_LIST, "包含子功能："));
            for (HelpTopic child : topic.sortedChildren()) {
                result.add(singleNode(botId, child.name(), buildLeafTextContent(child)));
            }
            return result;
        }

        for (HelpTopic child : topic.sortedChildren()) {
            result.add(wrapNestedTopicNode(botId, child, nestLevel + 1));
        }
        return result;
    }

    @Nonnull
    private String buildLeafTextContent(@Nonnull HelpTopic topic) {
        if (StringUtils.isNotNullOrEmpty(topic.detail())) {
            return buildSubFeatureDetail(topic);
        }
        return formatTopicEntry(topic);
    }

    @Nonnull
    private Map<String, Object> nestedForwardNode(long botId, @Nonnull String senderName, @Nonnull List<Map<String, Object>> innerForward) {
        Map<String, Object> data = new HashMap<>();
        data.put("name", senderName);
        data.put("uin", String.valueOf(botId));
        data.put("content", innerForward);
        Map<String, Object> node = new HashMap<>();
        node.put("type", "node");
        node.put("data", data);
        return node;
    }

    @Nonnull
    private Map<String, Object> singleNode(long botId, @Nonnull String senderName, @Nonnull String content) {
        return ShiroUtils.generateSingleMsg(botId, senderName, content);
    }

    @Nonnull
    private String buildUsageHeader() {
        String prefix = BaniraUtils.getInsPrefixWithSpace();
        String helpIns = HelpTopics.formatAliasChoices(insConfig.get().base().help());
        String cmd = prefix + helpIns;
        return "指令帮助\n\n用法：\n"
                + cmd + " [<页数>]\n"
                + cmd + " <功能> [<子功能> ...] [<页数>]\n"
                + cmd + " -ex <功能> [<子功能> ...] [<页数>]";
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
    private List<HelpMessage> paginateMessages(@Nonnull List<HelpMessage> messages, long page) {
        return messages.stream()
                .skip((page - 1) * PAGE_SIZE)
                .limit(PAGE_SIZE)
                .toList();
    }

    @Nonnull
    private List<Map<String, Object>> paginateNodes(@Nonnull List<Map<String, Object>> nodes, long page) {
        return nodes.stream()
                .skip((page - 1) * PAGE_SIZE)
                .limit(PAGE_SIZE)
                .toList();
    }

    private record ResolvedHelp(@Nullable HelpTopic root, @Nullable HelpTopic focus, @Nullable String error) {

        @Nonnull
        static ResolvedHelp of(@Nonnull HelpTopic root, @Nonnull HelpTopic focus) {
            return new ResolvedHelp(root, focus, null);
        }

        @Nonnull
        static ResolvedHelp error(@Nonnull String error) {
            return new ResolvedHelp(null, null, error);
        }

        boolean ok() {
            return error == null;
        }
    }

}
