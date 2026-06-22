package xin.vanilla.banira.plugin.chat.capability;

import com.mikuac.shiro.common.utils.MessageConverser;
import com.mikuac.shiro.model.ArrayMsg;
import dev.langchain4j.agent.tool.P;
import dev.langchain4j.agent.tool.Tool;
import dev.langchain4j.data.message.*;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.domain.MessageRecord;
import xin.vanilla.banira.enums.EnumMessageType;
import xin.vanilla.banira.mapper.param.MessageRecordQueryParam;
import xin.vanilla.banira.plugin.chat.*;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.plugin.chat.memory.MemoryEmbeddingService;
import xin.vanilla.banira.plugin.chat.memory.MemoryRetriever;
import xin.vanilla.banira.plugin.chat.model.ChatQuotaService;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.service.IMessageRecordManager;
import xin.vanilla.banira.start.SpringContextHolder;
import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.StringUtils;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 单次 Agent 请求的工具桥接，委托给能力注册中心与记忆模块
 */
@Slf4j
public class AiToolBridge {

    private static final Pattern FIRST_NUMBER = Pattern.compile("\\d+(?:\\.\\d+)?");
    private static final Pattern CQ_REPLY_ID = Pattern.compile("\\[CQ:reply,id=(\\d+)]");

    private final AgentContext ctx;
    private final ChatConfig chatConfig;
    private final AiCapabilityRegistry capabilityRegistry;
    private final MemoryRetriever memoryRetriever;
    private final IAiMemoryManager aiMemoryManager;
    private final ChatQuotaService chatQuotaService;
    private final MemoryEmbeddingService memoryEmbeddingService;
    private final IMessageRecordManager messageRecordManager;
    private final int memoryRetrieveLimit;
    private final List<String> toolReferences;
    private final List<ChatMessage> pendingMediaMessages = new ArrayList<>();

    public AiToolBridge(@Nonnull AgentContext ctx
            , @Nonnull ChatConfig chatConfig
            , @Nonnull AiCapabilityRegistry capabilityRegistry
            , @Nonnull MemoryRetriever memoryRetriever
            , @Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatQuotaService chatQuotaService
            , int memoryRetrieveLimit
    ) {
        this(ctx, chatConfig, capabilityRegistry, memoryRetriever, aiMemoryManager, chatQuotaService, memoryRetrieveLimit, new ArrayList<>(), null);
    }

    public AiToolBridge(@Nonnull AgentContext ctx
            , @Nonnull ChatConfig chatConfig
            , @Nonnull AiCapabilityRegistry capabilityRegistry
            , @Nonnull MemoryRetriever memoryRetriever
            , @Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatQuotaService chatQuotaService
            , int memoryRetrieveLimit
            , @Nonnull List<String> toolReferences
    ) {
        this(ctx, chatConfig, capabilityRegistry, memoryRetriever, aiMemoryManager, chatQuotaService, memoryRetrieveLimit, toolReferences, null);
    }

    public AiToolBridge(@Nonnull AgentContext ctx
            , @Nonnull ChatConfig chatConfig
            , @Nonnull AiCapabilityRegistry capabilityRegistry
            , @Nonnull MemoryRetriever memoryRetriever
            , @Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatQuotaService chatQuotaService
            , int memoryRetrieveLimit
            , @Nonnull List<String> toolReferences
            , @Nullable MemoryEmbeddingService memoryEmbeddingService
    ) {
        this(ctx, chatConfig, capabilityRegistry, memoryRetriever, aiMemoryManager, chatQuotaService, memoryRetrieveLimit, toolReferences, memoryEmbeddingService, null);
    }

    public AiToolBridge(@Nonnull AgentContext ctx
            , @Nonnull ChatConfig chatConfig
            , @Nonnull AiCapabilityRegistry capabilityRegistry
            , @Nonnull MemoryRetriever memoryRetriever
            , @Nonnull IAiMemoryManager aiMemoryManager
            , @Nonnull ChatQuotaService chatQuotaService
            , int memoryRetrieveLimit
            , @Nonnull List<String> toolReferences
            , @Nullable MemoryEmbeddingService memoryEmbeddingService
            , @Nullable IMessageRecordManager messageRecordManager
    ) {
        this.ctx = ctx;
        this.chatConfig = chatConfig;
        this.capabilityRegistry = capabilityRegistry;
        this.memoryRetriever = memoryRetriever;
        this.aiMemoryManager = aiMemoryManager;
        this.chatQuotaService = chatQuotaService;
        this.memoryRetrieveLimit = memoryRetrieveLimit;
        this.toolReferences = toolReferences;
        this.memoryEmbeddingService = memoryEmbeddingService;
        this.messageRecordManager = messageRecordManager;
    }

    @Tool("列出当前可用的插件能力名称与说明")
    public String listCapabilities() {
        return capabilityRegistry.describeAvailable(ctx, chatConfig);
    }

    @Tool("查询当前 QQ 群真实群主。用户问群主是谁、让你自己查群主时使用；不要把配置文件主人当成群主。")
    public String getGroupOwner() {
        return executeWithPolicy("get_group_owner", Map.of(), false);
    }

    @Tool("查询当前 QQ 群基础信息。用户问群名、群号、群主、管理员数量、成员数量等常见群信息时使用。")
    public String getGroupSummary() {
        return executeWithPolicy("get_group_summary", Map.of(), false);
    }

    @Tool("搜索公开网页资料。用户要查人物、学历、新闻、百科外信息，且没有更专用插件能力时使用；query 写完整搜索关键词。")
    public String webSearch(@P("搜索关键词") String query) {
        String finalQuery = normalizeWebSearchQuery(query);
        return executeWithPolicy("web_search", Map.of(
                "query", finalQuery
        ), false);
    }

    @Tool("解析短追问里的真实搜索目标。用户说“你再搜搜/搜搜看/再查查”或引用你自己“没搜到/不知道”的回复时，先调用它拿原始关键词，再调用 webSearch。")
    public String resolveFollowUpSearchTarget(@P("当前可见线索，可空") String hint) {
        String target = resolveFollowUpSearchTargetInternal(hint);
        if (StringUtils.isNullOrEmptyEx(target)) {
            return "未能从最近上下文解析出明确搜索关键词；如果用户没有给新关键词，就简短说明没找到原始话题。";
        }
        return "建议搜索关键词：" + target;
    }

    @Tool("查询城市天气。用户问今天/明天某地天气、温度、冷不冷、热不热、下不下雨时优先使用，不要用网页搜索代替。")
    public String getWeather(@P("城市或地点名称，例如 成都") String location) {
        return executeWithPolicy("get_weather", Map.of(
                "location", StringUtils.isNotNullOrEmpty(location) ? location : ""
        ), false);
    }

    @Tool("调用插件能力。name 为能力名，args 为 key=value 参数，多个用逗号分隔，例如 host=127.0.0.1,port=25565")
    public String invokeCapability(@P("能力名称") String name, @P("参数字符串") String args) {
        Map<String, String> parsedArgs = AiCapabilityArgs.parse(args);
        LOGGER.debug("AI invoke capability name={} argKeys={}", name, parsedArgs.keySet());
        return executeWithPolicy(name, parsedArgs, true);
    }

    @Tool("把长内容加入本次回复的合并转发资料。用户要求输出普通长文、列表、资料整理时使用；完整代码文件或很长代码应使用 uploadCodeTextFile。调用后最终回复只保留一句短台词。")
    public String addForwardReference(@P("合并转发标题或简短说明") String title, @P("完整长内容，可包含代码块") String content) {
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "合并转发内容为空。";
        }
        String cleanedContent = xin.vanilla.banira.plugin.chat.StructuredReplyPipeline.cleanReferenceText(content);
        if (xin.vanilla.banira.plugin.chat.StructuredReplyPipeline.isUnsuitableForwardReferenceText(cleanedContent)) {
            LOGGER.debug("AI ignored unsuitable forward reference title={} chars={}", title, content.length());
            return "这段不是可发送资料，已忽略。";
        }
        if (shouldUploadAsCodeFile(title, cleanedContent)) {
            LOGGER.debug("AI forward reference converted to code file upload title={} chars={}",
                    title, cleanedContent.length());
            return uploadCodeTextFile(suggestCodeFileName(title, cleanedContent), cleanedContent);
        }
        String refTitle = StringUtils.isNotNullOrEmpty(title) ? title.trim() : "完整内容";
        String ref = refTitle + "\n\n" + AiTextLimits.truncate(
                cleanedContent,
                AiTextLimits.MAX_TOOL_RESULT
        );
        toolReferences.add(ref);
        LOGGER.debug("AI added forward reference title={} chars={}", refTitle, content.length());
        return "已加入合并转发。最终回复用一句短话说明即可，不要再把完整内容发在普通消息里。";
    }

    @Tool("把很长的代码保存为文本文件并上传。用户要求编写大量代码、完整项目文件、完整 HTML/CSS/JS/Java/配置文件时使用；上传后普通台词只说一句，不要再贴完整代码，也不要再加合并转发说明。")
    public String uploadCodeTextFile(@P("文件名，建议带后缀，例如 Main.java 或 dfs_template.txt") String fileName,
                                     @P("完整代码文本，不要省略") String content) {
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "文件内容为空。";
        }
        String safeName = safeCodeFileName(fileName);
        try {
            Path dir = Path.of("cache", "aichat-code");
            Files.createDirectories(dir);
            Path file = dir.resolve(System.currentTimeMillis() + "-" + safeName);
            Files.writeString(file, content, StandardCharsets.UTF_8);
            String absolutePath = file.toAbsolutePath().toString();
            if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
                ctx.bot().uploadGroupFile(ctx.scopeGroupId(), absolutePath, safeName, "");
            } else if (ctx.senderId() != null && ctx.senderId() > 0) {
                ctx.bot().uploadPrivateFile(ctx.senderId(), absolutePath, safeName);
            }
            LOGGER.debug("AI uploaded code text file name={} chars={} path={}", safeName, content.length(), absolutePath);
            return "代码文件已上传：" + safeName;
        } catch (Exception e) {
            LOGGER.warn("AI upload code text file failed name={}", safeName, e);
            return "代码文件上传失败";
        }
    }

    @Tool("搜索 MC百科/MCMod 并直接发送插件渲染的结果图片。type 可用 mod/modpack/author/user/item/tutorial；keyword 为关键词或 ID。用户要查 MC百科、MCMod、模组、作者、资料、教程时优先用这个。")
    public String searchMcmod(@P("类型：mod/modpack/author/user/item/tutorial") String type, @P("关键词或 ID") String keyword) {
        Map<String, String> args = Map.of(
                "type", StringUtils.isNotNullOrEmpty(type) ? type : "",
                "keyword", StringUtils.isNotNullOrEmpty(keyword) ? keyword : ""
        );
        return executeWithPolicy("search_mcmod", args, false);
    }

    @Tool("给 MC百科/MCMod 模组或整合包点红票/黑票，并直接发送结果。type 为 mod/modpack，keywordOrId 为目标名称或 ID，vote 为 red/black。只在用户当前消息明确要求投票时使用。")
    public String voteMcmod(@P("类型：mod/modpack") String type,
                            @P("模组/整合包名称或 ID") String keywordOrId,
                            @P("red/black 或 红票/黑票") String vote) {
        Map<String, String> args = Map.of(
                "type", StringUtils.isNotNullOrEmpty(type) ? type : "",
                "keywordOrId", StringUtils.isNotNullOrEmpty(keywordOrId) ? keywordOrId : "",
                "vote", StringUtils.isNotNullOrEmpty(vote) ? vote : ""
        );
        return executeWithPolicy("vote_mcmod", args, false);
    }

    @Tool("给 MC百科/MCMod 模组或整合包点推荐，并直接发送结果。type 为 mod/modpack，keywordOrId 为目标名称或 ID。只在用户当前消息明确要求点推荐时使用。")
    public String pushMcmod(@P("类型：mod/modpack") String type,
                            @P("模组/整合包名称或 ID") String keywordOrId) {
        Map<String, String> args = Map.of(
                "type", StringUtils.isNotNullOrEmpty(type) ? type : "",
                "keywordOrId", StringUtils.isNotNullOrEmpty(keywordOrId) ? keywordOrId : ""
        );
        return executeWithPolicy("push_mcmod", args, false);
    }

    @Tool("在 MC百科/MCMod 模组、整合包、作者或用户中心下方发布评论/留言，并直接发送结果。type 为 mod/modpack/author/user。只在用户当前消息明确给出留言内容时使用。")
    public String commentMcmod(@P("类型：mod/modpack/author/user") String type,
                               @P("目标名称或 ID") String keywordOrId,
                               @P("留言/评论内容") String content) {
        Map<String, String> args = Map.of(
                "type", StringUtils.isNotNullOrEmpty(type) ? type : "",
                "keywordOrId", StringUtils.isNotNullOrEmpty(keywordOrId) ? keywordOrId : "",
                "content", StringUtils.isNotNullOrEmpty(content) ? content : ""
        );
        return executeWithPolicy("comment_mcmod", args, false);
    }

    @Tool("回复 MC百科/MCMod 评论，并直接发送结果。type 为 mod/modpack/author/user，containerId 为容器 ID，commentId 为评论 ID。只在当前消息明确要求回复评论时使用。")
    public String replyMcmodComment(@P("类型：mod/modpack/author/user") String type,
                                    @P("容器 ID") String containerId,
                                    @P("评论 ID") String commentId,
                                    @P("回复内容") String content) {
        Map<String, String> args = Map.of(
                "type", StringUtils.isNotNullOrEmpty(type) ? type : "",
                "containerId", StringUtils.isNotNullOrEmpty(containerId) ? containerId : "",
                "commentId", StringUtils.isNotNullOrEmpty(commentId) ? commentId : "",
                "content", StringUtils.isNotNullOrEmpty(content) ? content : ""
        );
        return executeWithPolicy("reply_mcmod_comment", args, false);
    }

    @Tool("删除 MC百科/MCMod 评论，并直接发送结果。commentId 为评论 ID。只在当前消息明确要求删除指定评论时使用。")
    public String deleteMcmodComment(@P("评论 ID") String commentId) {
        Map<String, String> args = Map.of(
                "commentId", StringUtils.isNotNullOrEmpty(commentId) ? commentId : ""
        );
        return executeWithPolicy("delete_mcmod_comment", args, false);
    }

    @Tool("撤回我刚刚在当前群或私聊里发送的上一轮回复。用户说撤回你刚发的、撤回上一条时使用。")
    public String recallLastAiReply(@P("可选数量；为空表示撤回上一轮全部分片") String count) {
        Map<String, String> args = StringUtils.isNullOrEmptyEx(count)
                ? Map.of()
                : Map.of("count", integerText(count, "1"));
        if (StringUtils.isNullOrEmptyEx(count)) {
            return executeWithPolicy("recall_last_ai_reply", Map.of(), false);
        }
        return executeWithPolicy("recall_last_ai_reply", args, false);
    }

    @Tool("禁言当前发消息的人自己。用户表达想让自己被禁言、静音、关小黑屋、冷静一下等自我群管请求时优先使用；不要填其他人的 QQ。")
    public String muteSelf(@P("禁言时长，支持分钟数或口语时长；为空默认 10 分钟") String minutes) {
        if (ctx.senderId() == null || ctx.senderId() <= 0 || ctx.scopeGroupId() <= 0) {
            return "当前不是可执行自我禁言的群聊上下文。";
        }
        Map<String, String> params = Map.of(
                "action", "mute",
                "args", ctx.senderId() + " " + muteDurationText(minutes),
                "confirm", "true",
                "selfTarget", "true"
        );
        return executeWithPolicy("execute_kanri", params, false);
    }

    @Tool("开启当前群全员禁言。高影响操作，首次调用 confirm=false 保存待确认；用户确认后再次调用并传 confirm=true。")
    public String muteAllGroup(@P("是否已得到当前用户明确确认，true/false") String confirm) {
        Map<String, String> params = Map.of(
                "action", "mute",
                "args", "all",
                "confirm", wholeGroupConfirmValue(confirm)
        );
        return executeWithPolicy("execute_kanri", params, false);
    }

    @Tool("关闭当前群全员禁言。高影响操作，首次调用 confirm=false 保存待确认；用户确认后再次调用并传 confirm=true。")
    public String unmuteAllGroup(@P("是否已得到当前用户明确确认，true/false") String confirm) {
        Map<String, String> params = Map.of(
                "action", "loud",
                "args", "all",
                "confirm", wholeGroupConfirmValue(confirm)
        );
        return executeWithPolicy("execute_kanri", params, false);
    }

    @Tool("禁言当前群成员。target 为 QQ 号或 @ 码，minutes 为分钟数。只有用户明确要求禁言别人时使用；用户想禁自己时改用 muteSelf。")
    public String muteGroupMember(@P("目标 QQ 号或 [CQ:at,qq=...]") String target, @P("禁言分钟数") String minutes) {
        return muteGroupMembersInternal(List.of(target), minutes, true);
    }

    @Tool("批量禁言多个群成员。用户说「都禁了」「那几个」时使用；targets 为逗号分隔的 QQ 号，minutes 为分钟数。")
    public String muteGroupMembers(@P("逗号分隔的 QQ 号") String targets, @P("禁言分钟数") String minutes) {
        List<String> parsed = new ArrayList<>();
        if (StringUtils.isNotNullOrEmpty(targets)) {
            for (String part : targets.split("[,，\\s]+")) {
                if (StringUtils.isNotNullOrEmpty(part)) {
                    parsed.add(part.trim());
                }
            }
        }
        return muteGroupMembersInternal(parsed, minutes, true);
    }

    @Tool("按群昵称关键词查找成员 QQ，禁言前先用来确认目标。")
    public String searchGroupMember(@P("群昵称关键词") String keyword) {
        return executeWithPolicy("search_group_member", Map.of(
                "keyword", StringUtils.isNotNullOrEmpty(keyword) ? keyword : ""
        ), false);
    }

    @Nonnull
    private String muteGroupMembersInternal(@Nonnull List<String> targets, @Nullable String minutes, boolean confirmed) {
        if (ctx.kanriMuteSucceeded()) {
            return "禁言本回合已由系统执行完成，请直接向用户口语说明【本回合群管已执行】中的结果，勿重复调用禁言工具。";
        }
        List<String> qqList = new ArrayList<>();
        for (String target : targets) {
            String normalized = normalizeTarget(target);
            if (StringUtils.isNotNullOrEmpty(normalized)) {
                qqList.add(normalized);
            }
        }
        if (qqList.isEmpty()) {
            return "缺少要禁言的目标。";
        }
        String duration = muteDurationText(minutes);
        String argsLine = String.join(" ", qqList) + " " + duration;
        Map<String, String> params = Map.of(
                "action", "mute",
                "args", argsLine,
                "confirm", confirmed ? "true" : "false"
        );
        return executeWithPolicy("execute_kanri", params, false);
    }

    @Tool("解除当前群成员禁言。只在用户明确要求解禁某个可识别目标时使用；target 为 QQ 号或 @ 码。不要给当前发言者提供自助解禁。")
    public String unmuteGroupMember(@P("目标 QQ 号或 [CQ:at,qq=...]") String target) {
        String normalizedTarget = normalizeTarget(target);
        if (StringUtils.isNullOrEmptyEx(normalizedTarget)) {
            return "缺少要解禁的目标。";
        }
        Map<String, String> args = Map.of(
                "action", "loud",
                "args", normalizedTarget,
                "confirm", "true"
        );
        return executeWithPolicy("execute_kanri", args, false);
    }

    @Tool("修改当前群成员群名片。target 必须是 QQ 号或结构化 @；card 是新群名片。用户说“改正常一点/你随便想一个”时，可以直接拟一个简洁正常的群名片后执行。@你只是唤醒，不是目标；“我的”指当前发言者，“你自己/你的”指机器人自己。")
    public String setGroupCard(@P("目标 QQ 号或 [CQ:at,qq=...]；不要把唤醒你的 @ 当目标；我的=当前发言者，你自己/你的=机器人自己") String target,
                               @P("新群名片，简短正常，不要包含换行") String card) {
        String normalizedCard = normalizeCard(card);
        String normalizedTarget = resolveGroupCardTarget(target, normalizedCard);
        if (StringUtils.isNullOrEmptyEx(normalizedTarget)) {
            return "缺少要修改群名片的目标。";
        }
        if (StringUtils.isNullOrEmptyEx(normalizedCard)) {
            return "缺少新的群名片。";
        }
        Map<String, String> args = Map.of(
                "action", "card",
                "args", normalizedTarget + " " + normalizedCard,
                "confirm", "true"
        );
        return executeWithPolicy("execute_kanri", args, false);
    }

    @Tool("列出 AI 可调用的群管动作。用户询问能做哪些群管操作时使用。")
    public String listKanriActions() {
        return executeWithPolicy("list_kanri_actions", Map.of(), false);
    }

    @Tool("执行允许的群管动作。action 为动作别名，args 为空格分隔参数。只在用户当前消息明确要求该群管动作时使用；敏感动作必须确认。")
    public String executeKanriAction(@P("动作别名，例如 mute/loud/card/tag/essence/groupName") String action,
                                     @P("参数字符串，空格分隔") String args,
                                     @P("是否已明确确认，true/false") String confirm) {
        String normalizedAction = StringUtils.isNotNullOrEmpty(action) ? action.trim() : "";
        String argLine = StringUtils.isNotNullOrEmpty(args) ? args.trim() : "";
        if ("card".equalsIgnoreCase(normalizedAction)) {
            argLine = normalizeGroupCardActionArgs(argLine);
            if (StringUtils.isNullOrEmptyEx(argLine)) {
                return "群名片目标不明确。";
            }
        }
        String confirmed = StringUtils.isNotNullOrEmpty(confirm) ? confirm : "false";
        Map<String, String> params = Map.of(
                "action", normalizedAction,
                "args", argLine,
                "confirm", confirmed
        );
        return executeWithPolicy("execute_kanri", params, false);
    }

    @Tool("查询 Minecraft 服务器在线状态。host 为地址，port 默认 25565，name 可选。")
    public String queryMcServer(@P("服务器地址") String host,
                                @P("查询端口，可空") String port,
                                @P("显示名称，可空") String name) {
        return executeWithPolicy("query_mc_server", Map.of(
                "host", StringUtils.isNotNullOrEmpty(host) ? host : "",
                "port", StringUtils.isNotNullOrEmpty(port) ? integerText(port, "25565") : "25565",
                "name", StringUtils.isNotNullOrEmpty(name) ? name : ""
        ), false);
    }

    @Tool("按已保存名称查询 Minecraft 服务器在线状态。")
    public String queryMcServerByName(@P("服务器名称") String name) {
        return executeWithPolicy("query_mc_server_by_name", Map.of(
                "name", StringUtils.isNotNullOrEmpty(name) ? name : ""
        ), false);
    }

    @Tool("列出当前群已保存的 Minecraft 服务器。")
    public String listMcServers() {
        return executeWithPolicy("list_mc_servers", Map.of(), false);
    }

    @Tool("列出当前用户有权查看的 RCON 服务器。")
    public String listRconServers() {
        return executeWithPolicy("list_rcon_servers", Map.of(), false);
    }

    @Tool("向已授权的 MC 服务器发送 RCON 命令。confirm=false 会返回待确认提示；只有用户当前消息明确确认后才可传 true。")
    public String executeRcon(@P("服务器编号或名称") String server,
                              @P("要执行的 RCON 命令") String command,
                              @P("是否已明确确认，true/false") String confirm) {
        Map<String, String> params = Map.of(
                "server", StringUtils.isNotNullOrEmpty(server) ? server : "",
                "command", StringUtils.isNotNullOrEmpty(command) ? command : "",
                "confirm", StringUtils.isNotNullOrEmpty(confirm) ? confirm : "false"
        );
        return executeWithPolicy("execute_rcon", params, false);
    }

    @Tool("解析当前消息或给定文本中的 B 站/抖音等社交媒体链接。")
    public String parseSocialMedia(@P("包含链接的文本，可空") String message) {
        String text = StringUtils.isNotNullOrEmpty(message) ? message : ctx.userMessage();
        return executeWithPolicy("parse_social_media", Map.of(
                "message", StringUtils.isNotNullOrEmpty(text) ? text : ""
        ), false);
    }

    @Tool("查询机器人与宿主机运行状态摘要。")
    public String getBotStatus() {
        return executeWithPolicy("get_bot_status", Map.of(), false);
    }

    @Tool("搜索机器人功能帮助。path 为功能路径或关键词，page 默认 1。")
    public String searchHelp(@P("功能路径或关键词，可空") String path, @P("页码，可空") String page) {
        return executeWithPolicy("search_help", Map.of(
                "path", StringUtils.isNotNullOrEmpty(path) ? path : "",
                "page", StringUtils.isNotNullOrEmpty(page) ? integerText(page, "1") : "1"
        ), false);
    }

    @Tool("列出当前群可用的顶层功能主题名称与别名。")
    public String listHelpTopics() {
        return executeWithPolicy("list_help_topics", Map.of(), false);
    }

    @Tool("列出当前群的定时任务。keyword 可空，page 默认 1。")
    public String listTimers(@P("关键词，可空") String keyword, @P("页码，可空") String page) {
        return executeWithPolicy("list_timers", Map.of(
                "keyword", StringUtils.isNotNullOrEmpty(keyword) ? keyword : "",
                "page", StringUtils.isNotNullOrEmpty(page) ? integerText(page, "1") : "1"
        ), false);
    }

    @Tool("创建定时提醒。cron 是 Quartz cron 表达式；message 是提醒内容；target=private 私聊提醒当前用户，target=group 发到当前群。用户说几分钟后提醒我时优先用 private。")
    public String createTimer(@P("Quartz cron 表达式") String cron,
                              @P("提醒内容") String message,
                              @P("private 或 group") String target) {
        Map<String, String> args = Map.of(
                "cron", StringUtils.isNotNullOrEmpty(cron) ? cron : "",
                "message", StringUtils.isNotNullOrEmpty(message) ? message : "",
                "target", StringUtils.isNotNullOrEmpty(target) ? target : "private"
        );
        return executeWithPolicy("create_timer", args, false);
    }

    @Tool("创建群内 @ 指定用户的一次性提醒。用户说提醒 @某人、几分钟后提醒某个被@的人时使用；targetUser 填 QQ 号。")
    public String createTimerForUser(@P("Quartz cron 表达式") String cron,
                                     @P("提醒内容") String message,
                                     @P("要 @ 提醒的 QQ 号") String targetUser) {
        Map<String, String> args = Map.of(
                "cron", StringUtils.isNotNullOrEmpty(cron) ? cron : "",
                "message", StringUtils.isNotNullOrEmpty(message) ? message : "",
                "target", "group",
                "targetUser", StringUtils.isNotNullOrEmpty(targetUser) ? targetUser : ""
        );
        return executeWithPolicy("create_timer", args, false);
    }

    @Tool("查询当前用户今天在群内抽到的老婆。")
    public String getTodayWife() {
        return executeWithPolicy("get_today_wife", Map.of(), false);
    }

    @Tool("为当前用户主动抽取今天的老婆并直接发送结果。用户说抽老婆、帮我抽个老婆、使用你的能力抽老婆时使用。")
    public String drawTodayWife(@P("昵称，可空，默认老婆") String nick) {
        Map<String, String> args = Map.of(
                "nick", StringUtils.isNotNullOrEmpty(nick) ? nick : "老婆"
        );
        return executeWithPolicy("draw_today_wife", args, false);
    }

    @Tool("花言草语编码或解码。")
    public String plantCodec(@P("要处理的文本") String text) {
        return executeWithPolicy("plant_codec", Map.of(
                "text", StringUtils.isNotNullOrEmpty(text) ? text : ""
        ), false);
    }

    @Tool("对指定用户发送戳一戳。target 为 QQ 号或 [CQ:at,qq=...]；用户戳你且你决定回应时，通常用它戳回发送者。")
    public String pokeUser(@P("目标 QQ 号或 [CQ:at,qq=...]；为空时默认戳当前发言者") String target,
                           @P("次数，默认 1，最多 3") String count) {
        if (ctx.bot() == null) {
            return "当前不能发送戳一戳。";
        }
        long targetId = StringUtils.toLong(normalizeTarget(target), 0L);
        if (targetId <= 0 && ctx.senderId() != null) {
            targetId = ctx.senderId();
        }
        if (targetId <= 0) {
            return "戳一戳目标不明确。";
        }
        int times = Math.max(1, Math.min(StringUtils.toInt(integerText(count, "1"), 1), 3));
        for (int i = 0; i < times; i++) {
            if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
                ctx.bot().sendGroupPoke(ctx.scopeGroupId(), targetId);
            } else if (ctx.senderId() != null && ctx.senderId() > 0) {
                ctx.bot().sendFriendPoke(ctx.senderId(), targetId);
            } else {
                return "当前会话不能发送戳一戳。";
            }
        }
        LOGGER.debug("AI sent poke group={} sender={} target={} count={}",
                ctx.scopeGroupId(), ctx.senderId(), targetId, times);
        return AiDirectResult.sent("已发送戳一戳。");
    }

    @Tool("发送一个已自动收集的表情包。用户让你发图、发表情包、拿个表情包回应时使用；keyword 可空。")
    public String sendSticker(@P("表情包关键词，可空") String keyword) {
        return stickerService().sendSticker(ctx, keyword);
    }

    @Tool("检索已收集表情包的描述和使用场景。你想主动用表情包但不确定有没有合适收藏，或用户要求分享收藏前，先用它筛选。")
    public String searchStickers(@P("表情包关键词或当前语境，可空") String keyword,
                                 @P("返回数量，默认 6，最多 12") String count) {
        return stickerService().describeStickers(ctx, keyword, StringUtils.toInt(integerText(count, "6"), 6));
    }

    @Tool("分享多个已收集表情包。需要分享多个收藏时使用；会放进合并转发，一条转发节点一个表情包，不要反复调用 sendSticker 刷屏。")
    public String shareStickerPack(@P("表情包关键词，可空") String keyword,
                                   @P("数量，默认 3，最多 8") String count) {
        List<String> stickers = stickerService().stickerForwardMessages(ctx, keyword, StringUtils.toInt(integerText(count, "3"), 3));
        if (stickers.isEmpty()) {
            return "没有找到合适的表情包。";
        }
        toolReferences.addAll(stickers);
        LOGGER.debug("AI added sticker pack references group={} user={} count={} keyword={}",
                ctx.scopeGroupId(), ctx.senderId(), stickers.size(), keyword);
        return "已加入 " + stickers.size() + " 个表情包到合并转发。最终回复只用一句短话说明即可。";
    }

    @Tool("按主人要求丢弃已自动收集的表情包。keyword 可填关键词；填 全部/all/* 表示清理当前群可见的所有已收集表情包。")
    public String discardSticker(@P("要丢弃的表情包关键词，或 全部/all/*") String keyword) {
        return stickerService().discardStickers(ctx, keyword);
    }

    @Tool("按主人反馈修正已收集表情包的描述或使用场景。比如主人说“这个表情不是嘲讽，是安慰用的”时使用。")
    public String updateStickerUsage(@P("要修正的表情包关键词") String keyword,
                                     @P("新的表情包描述，可空") String description,
                                     @P("新的使用场景，可空") String scene) {
        return stickerService().updateStickerUsage(ctx, keyword, description, scene);
    }

    @Tool("从 zip 压缩包导入表情包。只有有权限的触发人可以用；source 可填 zip 链接或本地路径，留空时会尝试读取当前消息、引用消息或当前群最近文件消息里的文件链接。会自动跳过过大图片和不像表情包的图片。")
    public String collectStickerArchive(@P("zip 压缩包链接、本地路径或 file: URI；可空") String source,
                                        @P("导入表情包的统一描述，可空") String description,
                                        @P("导入表情包的使用场景，可空") String scene) {
        return stickerService().collectStickerArchive(ctx, source, description, scene);
    }

    @Tool("按主人要求忘记匹配关键词的记忆。用于修正你的习惯、忘记错误偏好或丢弃不该记住的信息；keyword 必须明确。")
    public String forgetMemory(@P("要忘记的记忆关键词") String keyword) {
        return stickerService().forgetMemories(ctx, keyword);
    }

    @Tool("按主人要求记住一条合理的行为习惯或边界。用于“以后不要做什么”“遇到什么情况应该怎么做”这类长期修正；不保存危险、泄密或越权要求。")
    public String rememberOwnerInstruction(@P("主人给出的长期行为修正规则") String instruction) {
        if (!BaniraUtils.isOwner(ctx.senderId())) {
            return "这类习惯修正不能听你的。";
        }
        String normalized = StringUtils.nullToEmpty(instruction).trim();
        if (StringUtils.isNullOrEmptyEx(normalized) || normalized.length() < 4) {
            return "要记住的习惯太模糊。";
        }
        if (!xin.vanilla.banira.plugin.chat.memory.MemorySafety.isSafeToStore(normalized, chatConfig)) {
            return "这条要求不适合作为长期习惯保存。";
        }
        if (aiMemoryManager.existsSimilar(ctx.botId(), ctx.scopeGroupId(), 0L, normalized)) {
            return "相似习惯已经记过了。";
        }
        long now = DateUtils.getTimestamp(new java.util.Date());
        AiMemory memory = new AiMemory()
                .setBotId(ctx.botId())
                .setGroupId(ctx.scopeGroupId())
                .setUserId(0L)
                .setContent("主人长期行为修正：" + normalized)
                .setTags("type:owner_instruction,source:tool")
                .setSourceMsgId(ctx.msgId())
                .setCreatedAt(now)
                .setLastUsedAt(now);
        aiMemoryManager.addMemory(memory);
        if (memoryEmbeddingService != null) {
            memoryEmbeddingService.indexMemory(chatConfig, memory);
        }
        LOGGER.debug("AI saved owner instruction group={} user={} chars={}",
                ctx.scopeGroupId(), ctx.senderId(), normalized.length());
        return "已记住这条习惯修正。";
    }

    @Tool("列出可由你读取和精确替换的 config 配置文件。用户只说要改某个插件配置但没给文件路径时，先用这个找路径。")
    public String listConfigFiles(@P("可选关键词，例如 wife、aichat、global、ins、mcquery") String keyword) {
        if (!BaniraUtils.isGlobalAdmin(ctx.senderId())) {
            return "配置读取不听你的。";
        }
        Path root = Path.of("config").toAbsolutePath().normalize();
        String normalized = StringUtils.nullToEmpty(keyword).trim().toLowerCase(java.util.Locale.ROOT);
        try (var stream = Files.walk(root, 4)) {
            List<String> files = stream
                    .filter(Files::isRegularFile)
                    .map(path -> root.relativize(path).toString().replace("\\", "/"))
                    .filter(path -> path.endsWith(".yml") || path.endsWith(".yaml") || path.endsWith(".json")
                            || path.endsWith(".properties") || path.endsWith(".txt"))
                    .filter(path -> StringUtils.isNullOrEmptyEx(normalized)
                            || path.toLowerCase(java.util.Locale.ROOT).contains(normalized))
                    .sorted()
                    .limit(80)
                    .toList();
            if (files.isEmpty()) {
                return "没有找到匹配的配置文件。";
            }
            return String.join("\n", files);
        } catch (Exception e) {
            LOGGER.warn("AI list config files failed keyword={}", keyword, e);
            return "配置文件列表读取失败。";
        }
    }

    @Tool("读取 config 目录下的非敏感配置文件片段。用于用户要求你帮忙修改配置前先查看当前内容；会自动遮蔽 key/token/password/cookie 等敏感行。")
    public String readConfigFile(@P("config 下的相对路径，例如 group/wife-group-config.yml 或 global-config.yml") String path,
                                 @P("可选关键词；不为空时只返回附近片段") String keyword) {
        if (!BaniraUtils.isGlobalAdmin(ctx.senderId())) {
            return "配置读取不听你的。";
        }
        Path target = resolveConfigPath(path);
        if (target == null || !Files.isRegularFile(target)) {
            return "没有找到这个配置文件。";
        }
        try {
            String content = Files.readString(target, StandardCharsets.UTF_8);
            content = redactSensitiveConfigLines(content);
            String normalizedKeyword = StringUtils.nullToEmpty(keyword).trim();
            if (StringUtils.isNotNullOrEmpty(normalizedKeyword)) {
                content = configSnippet(content, normalizedKeyword, 8);
            }
            return AiTextLimits.truncate(content, 6000);
        } catch (Exception e) {
            LOGGER.warn("AI read config failed path={}", path, e);
            return "配置读取失败。";
        }
    }

    @Tool("精确替换 config 目录下配置文件的一段文本并保存。必须先读取配置，oldText 必须与文件中现有片段完全一致；会自动备份。禁止修改 key/token/password/cookie 等敏感行。")
    public String replaceConfigText(@P("config 下的相对路径，例如 group/wife-group-config.yml") String path,
                                    @P("要被替换的原始文本，必须完整来自 readConfigFile 返回内容") String oldText,
                                    @P("新的配置文本") String newText,
                                    @P("是否已由有权限的用户明确确认，true/false") String confirm) {
        if (!BaniraUtils.isGlobalAdmin(ctx.senderId())) {
            return "配置修改不听你的。";
        }
        if (!"true".equalsIgnoreCase(StringUtils.nullToEmpty(confirm).trim())) {
            return "配置修改需要先向有权限的人确认，确认后再带 confirm=true 调用。";
        }
        Path target = resolveConfigPath(path);
        if (target == null || !Files.isRegularFile(target)) {
            return "没有找到这个配置文件。";
        }
        String before = StringUtils.nullToEmpty(oldText);
        String after = StringUtils.nullToEmpty(newText);
        if (before.isBlank() || after.isBlank()) {
            return "配置替换内容不能为空。";
        }
        if (touchesSensitiveConfig(before) || touchesSensitiveConfig(after)) {
            return "这段配置涉及敏感字段，不能由聊天工具修改。";
        }
        try {
            String content = Files.readString(target, StandardCharsets.UTF_8);
            if (!content.contains(before)) {
                return "没有找到完全匹配的原始配置片段，先重新读取配置再改。";
            }
            String updated = content.replace(before, after);
            validateConfigText(target, updated);
            Path backup = target.resolveSibling(target.getFileName() + ".ai-bak-" + System.currentTimeMillis());
            Files.copy(target, backup, StandardCopyOption.COPY_ATTRIBUTES);
            Files.writeString(target, updated, StandardCharsets.UTF_8);
            LOGGER.info("AI replaced config text path={} user={} backup={}", target, ctx.senderId(), backup);
            return "配置已保存，备份：" + backup.getFileName();
        } catch (Exception e) {
            LOGGER.warn("AI replace config failed path={}", path, e);
            return "配置保存失败：" + e.getMessage();
        }
    }

    @Tool("搜索记忆，keyword 为关键词；包括重要长期记忆和轻量会话记忆。用户问你之前说过什么时优先使用。")
    public String searchMemory(@P("搜索关键词") String keyword) {
        LOGGER.debug("AI search memory group={} user={} keywordChars={}",
                ctx.scopeGroupId(), ctx.senderId(), keyword != null ? keyword.length() : 0);
        return memoryRetriever.search(ctx, keyword, memoryRetrieveLimit);
    }

    @Tool("按当前群隔离搜索聊天记录。用户问刚刚谁说过、上次聊到什么、之前你说过什么、某人前面说了什么时使用；sender 优先填 QQ 号，limit 默认 10。")
    public String searchChatHistory(@P("搜索关键词") String keyword,
                                    @P("发送者 QQ，可空；不要填群名片，除非用户只给了名字") String sender,
                                    @P("返回条数，默认 10，最大 30") String limit) {
        if (messageRecordManager == null || ctx.bot() == null) {
            return "消息记录暂不可用";
        }
        int count = clampHistoryLimit(limit);
        MessageRecordQueryParam param = baseHistoryParam(count);
        String normalizedKeyword = StringUtils.nullToEmpty(keyword).trim();
        String normalizedSender = StringUtils.nullToEmpty(sender).trim();
        if (StringUtils.isNotNullOrEmpty(normalizedKeyword)) {
            param.addKeyWord(normalizedKeyword);
        }
        long senderId = StringUtils.toLong(normalizedSender, 0L);
        if (senderId > 0) {
            param.setSenderId(senderId);
        } else if (StringUtils.isNotNullOrEmpty(normalizedSender)) {
            param.addKeyWord(normalizedSender);
        }
        List<MessageRecord> records = messageRecordManager.getMessageRecordList(param);
        return formatHistoryRecords(records, count);
    }

    @Tool("读取当前群最近聊天记录摘要。只有当当前 prompt 历史不够、用户问刚刚上下文、或你需要确认前文时使用；count 默认 10，最大 30。")
    public String getRecentChatHistory(@P("返回条数，默认 10，最大 30") String count) {
        if (messageRecordManager == null || ctx.bot() == null) {
            return "消息记录暂不可用";
        }
        int limit = clampHistoryLimit(count);
        List<MessageRecord> records = messageRecordManager.getMessageRecordList(baseHistoryParam(limit));
        return formatHistoryRecords(records, limit);
    }

    @Tool("按消息 ID 获取当前群内某条消息的发送者与内容。用户引用消息、问某条消息是谁说的、或需要核对原消息时使用。")
    public String getMessageById(@P("消息 ID") String msgId) {
        if (messageRecordManager == null || ctx.bot() == null) {
            return "消息记录暂不可用";
        }
        int id = StringUtils.toInt(msgId, 0);
        if (id <= 0) {
            return "消息 ID 无效";
        }
        MessageRecord record;
        if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
            record = messageRecordManager.getGroupMessageRecord(ctx.scopeGroupId(), id);
        } else {
            record = messageRecordManager.getPrivateMessageRecord(ctx.senderId() != null ? ctx.senderId() : 0L, id);
        }
        if (record == null) {
            return "未找到这条消息";
        }
        if (record.recalled()) {
            return "message recalled";
        }
        return formatHistoryRecord(record);
    }

    @Tool("按消息 ID 加载当前群内某条历史消息里的图片，让你在本轮后续可以实际查看图片内容。只有用户要求你看之前/上面/引用的图片，且当前 prompt 没有直接给出图片内容时使用；一次只加载一条消息。")
    public String loadMessageImages(@P("消息 ID") String msgId) {
        if (!chatConfig.model().imageInputEnabled()) {
            return "当前配置未启用图片输入，不能加载图片内容。";
        }
        if (messageRecordManager == null || ctx.bot() == null) {
            return "消息记录暂不可用";
        }
        int id = StringUtils.toInt(msgId, 0);
        if (id <= 0) {
            return "消息 ID 无效，不能加载图片。";
        }
        MessageRecord record;
        if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
            record = messageRecordManager.getGroupMessageRecord(ctx.scopeGroupId(), id);
        } else {
            record = messageRecordManager.getPrivateMessageRecord(ctx.senderId() != null ? ctx.senderId() : 0L, id);
        }
        if (record == null || record.recalled()) {
            return "未找到可读取的图片消息，可能已撤回或不在当前会话。";
        }
        ChatMessage mediaMessage = buildMediaMessage(record);
        if (mediaMessage == null) {
            return "这条消息没有可加载的图片内容，或图片下载失败。";
        }
        pendingMediaMessages.add(mediaMessage);
        LOGGER.debug("AI loaded message images group={} user={} msgId={}", ctx.scopeGroupId(), ctx.senderId(), msgId);
        return "图片内容已加载到本轮上下文。下一步请直接观察图片并回答，不要再说只能看到链接。";
    }

    @Nonnull
    public List<ChatMessage> drainPendingMediaMessages() {
        if (pendingMediaMessages.isEmpty()) {
            return List.of();
        }
        List<ChatMessage> drained = new ArrayList<>(pendingMediaMessages);
        pendingMediaMessages.clear();
        return drained;
    }

    @Tool("保存值得长期记住的信息。content 为记忆内容，tags 为可选标签（逗号分隔）")
    public String saveMemory(@P("记忆内容") String content, @P("标签，逗号分隔，可空") String tags) {
        if (!chatConfig.memory().allowToolSave()) {
            return "当前配置不允许 AI 主动写入记忆。";
        }
        if (StringUtils.isNullOrEmptyEx(content)) {
            return "记忆内容不能为空";
        }
        if (!xin.vanilla.banira.plugin.chat.memory.MemorySafety.isSafeToStore(content, chatConfig)) {
            return "这条内容不适合写入长期记忆。";
        }
        String trimmed = xin.vanilla.banira.plugin.chat.memory.MemorySafety.normalize(content, chatConfig);
        if (xin.vanilla.banira.plugin.chat.memory.MemoryScopePolicy.isUnsafeOwnerTitleClaim(ctx.senderId(), trimmed)) {
            return "这条称呼记忆容易串人，先不存。";
        }
        if (aiMemoryManager.existsSimilar(ctx.botId(), ctx.scopeGroupId(), ctx.senderId(), trimmed)) {
            return "相似记忆已存在";
        }
        long now = DateUtils.getTimestamp(new java.util.Date());
        AiMemory memory = new AiMemory()
                .setBotId(ctx.botId())
                .setGroupId(ctx.scopeGroupId())
                .setUserId(ctx.senderId())
                .setContent(trimmed)
                .setTags(normalizeTags(tags, "tool"))
                .setSourceMsgId(ctx.msgId())
                .setCreatedAt(now)
                .setLastUsedAt(now);
        aiMemoryManager.addMemory(memory);
        if (memoryEmbeddingService != null) {
            memoryEmbeddingService.indexMemory(chatConfig, memory);
        }
        LOGGER.debug("AI saved memory group={} user={} chars={} tags={}",
                ctx.scopeGroupId(), ctx.senderId(), trimmed.length(), normalizeTags(tags, "tool"));
        return "已保存记忆";
    }

    @Tool("列出已配置的 LLM 接口名称、地址与模型（仅主人可查，不含完整密钥）")
    public String listModelEndpoints() {
        if (!BaniraUtils.isOwner(ctx.senderId())) {
            return "仅主人可查询 LLM 接口配置。";
        }
        return chatQuotaService.describeEndpoints(chatConfig);
    }

    @Tool("查询 LLM 接口配额或余额。name 为接口名称，留空则查询全部（仅主人）")
    public String checkModelEndpointQuota(@P("接口名称，可空") String name) {
        if (!BaniraUtils.isOwner(ctx.senderId())) {
            return "仅主人可查询 LLM 接口配额。";
        }
        return chatQuotaService.checkQuota(chatConfig, name);
    }

    @Nonnull
    private String executeWithPolicy(@Nonnull String name, @Nonnull Map<String, String> args, boolean resolveBeforePolicy) {
        if (PendingAiActionStore.isKanriProceedIntent(ctx.userMessage())) {
            PendingAiActionStore.PendingAction pending = PendingAiActionStore.consumeMatching(ctx, name);
            if (pending != null) {
                Map<String, String> confirmedArgs = new java.util.LinkedHashMap<>(pending.args());
                confirmedArgs.put("confirm", "true");
                LOGGER.debug("AI confirmed pending capability name={} group={} user={}",
                        name, ctx.scopeGroupId(), ctx.senderId());
                return capabilityRegistry.execute(ctx, chatConfig, name, confirmedArgs);
            }
        }
        AiCapability capability = resolveBeforePolicy ? capabilityRegistry.resolve(ctx.scopeGroupId(), name) : null;
        CapabilityInvocationPolicy.Decision decision = CapabilityInvocationPolicy.evaluate(ctx, capability, name, args);
        if (!decision.allowed()) {
            return policyBlockForModel(name, decision.reason());
        }
        if (capability == null) {
            capability = capabilityRegistry.resolve(ctx.scopeGroupId(), name);
        }
        if (capability != null) {
            String registryPolicy = capabilityRegistry.validate(ctx, chatConfig, capability, args);
            if (registryPolicy != null) {
                return registryPolicy;
            }
        }
        if (requiresConfirmation(capability, args)) {
            PendingAiActionStore.put(ctx, name, args);
            LOGGER.debug("AI stored pending capability name={} group={} user={}",
                    name, ctx.scopeGroupId(), ctx.senderId());
            return "这个操作需要你确认一下；确认后我再执行";
        }
        return capabilityRegistry.execute(ctx, chatConfig, name, args);
    }

    @Nonnull
    private MessageRecordQueryParam baseHistoryParam(int limit) {
        MessageRecordQueryParam param = new MessageRecordQueryParam(true, 1, limit);
        if (ctx.bot() != null) {
            param.setBotId(ctx.botId());
        }
        param.setRecalled(false);
        if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
            param.setMsgType(EnumMessageType.GROUP.name());
            param.setGroupId(ctx.scopeGroupId());
        } else {
            param.setTargetId(ctx.senderId());
        }
        param.addOrderBy(MessageRecordQueryParam.ORDER_ID, false);
        return param;
    }

    @Nonnull
    private String formatHistoryRecords(@Nullable List<MessageRecord> records, int limit) {
        if (records == null || records.isEmpty()) {
            return "未找到相关消息记录；不要用相同关键词反复搜索消息记录，如果需要外部资料就直接调用 webSearch。";
        }
        List<MessageRecord> selected = records.stream()
                .filter(record -> record != null && !record.recalled())
                .collect(java.util.stream.Collectors.toCollection(ArrayList::new));
        if (selected.isEmpty()) {
            return "No readable matching chat history. Do not repeat the same history search; use webSearch if external information is needed.";
        }
        if (selected.size() > limit) {
            selected = selected.subList(0, limit);
        }
        java.util.Collections.reverse(selected);
        StringBuilder builder = new StringBuilder("消息记录只供理解上下文，不要原样转发：");
        for (MessageRecord record : selected) {
            String line = formatHistoryRecord(record);
            if (StringUtils.isNotNullOrEmpty(line)) {
                builder.append('\n').append(line);
            }
        }
        return builder.toString();
    }

    @Nonnull
    private String formatHistoryRecord(@Nullable MessageRecord record) {
        if (record == null || record.recalled()) {
            return "";
        }
        long sender = record.getSenderId() != null ? record.getSenderId() : 0L;
        Long groupId = record.getGroupId() != null && record.getGroupId() > 0 ? record.getGroupId() : ctx.scopeGroupId();
        ChatMessageContextFormatter.UserInfoCache cache = new ChatMessageContextFormatter.UserInfoCache();
        String senderText = sender > 0 && ctx.bot() != null
                ? ChatMessageContextFormatter.describeUser(ctx.bot(), groupId, sender, cache)
                : "未知用户";
        String body = plainRecordText(record, groupId, cache);
        if (StringUtils.isNullOrEmptyEx(body)) {
            body = "(空消息或暂时无法解析的消息)";
        }
        String self = ctx.bot() != null && sender == ctx.botId() ? "是" : "否";
        body = "[time=" + ChatRecordTimeFormatter.format(record.getTime()) + "] " + body;
        return "msgId=" + StringUtils.nullToEmpty(record.getMsgId())
                + "；发送者=" + senderText
                + "；是否你自己发送=" + self
                + "；内容=" + body;
    }

    @Nonnull
    private String plainRecordText(@Nonnull MessageRecord record
            , @Nullable Long groupId
            , @Nonnull ChatMessageContextFormatter.UserInfoCache cache
    ) {
        String source = recordTextSource(record);
        if (StringUtils.isNullOrEmptyEx(source)) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        for (ArrayMsg arrayMsg : MessageConverser.stringToArray(source)) {
            String text = MessageConvert.toPlainText(ctx.bot(), groupId, arrayMsg, cache);
            if (StringUtils.isNotNullOrEmpty(text)) {
                builder.append(text);
            }
        }
        String result = builder.toString().replaceAll("\\s+", " ").trim();
        if (StringUtils.isNullOrEmptyEx(result) && !source.trim().startsWith("[")) {
            result = source.trim();
        }
        return AiTextLimits.truncate(result, 500);
    }

    @Nullable
    private ChatMessage buildMediaMessage(@Nonnull MessageRecord record) {
        String source = recordTextSource(record);
        if (StringUtils.isNullOrEmptyEx(source) || ctx.bot() == null) {
            return null;
        }
        Long groupId = record.getGroupId() != null && record.getGroupId() > 0 ? record.getGroupId() : ctx.scopeGroupId();
        ChatMessageContextFormatter.UserInfoCache cache = new ChatMessageContextFormatter.UserInfoCache();
        List<Content> contents = new ArrayList<>();
        contents.add(new TextContent("按工具请求加载的历史图片："
                + ChatMessageContextFormatter.describeUser(ctx.bot(), groupId, record.getSenderId(), cache)
                + "，消息ID=" + StringUtils.nullToEmpty(record.getMsgId())
                + "。这是刚加载的真实图片内容。\n"));
        boolean hasImage = false;
        for (ArrayMsg arrayMsg : MessageConverser.stringToArray(source)) {
            List<Content> converted = MessageConvert.toContents(ctx.bot(), groupId, arrayMsg, true, cache);
            if (containsImageContent(converted)) {
                hasImage = true;
            }
            contents.addAll(converted);
        }
        if (!hasImage) {
            return null;
        }
        long sender = record.getSenderId() != null ? record.getSenderId() : 0L;
        return UserMessage.userMessage("qq_" + sender, contents);
    }

    private static boolean containsImageContent(@Nonnull List<Content> contents) {
        for (Content content : contents) {
            if (content instanceof ImageContent) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String recordTextSource(@Nonnull MessageRecord record) {
        String recode = StringUtils.nullToEmpty(record.getMsgRecode()).trim();
        if (StringUtils.isNotNullOrEmpty(recode)) {
            return recode;
        }
        String raw = StringUtils.nullToEmpty(record.getMsgRaw()).trim();
        if (StringUtils.isNullOrEmptyEx(raw)) {
            return "";
        }
        return raw;
    }

    private static int clampHistoryLimit(@Nullable String value) {
        int parsed = StringUtils.toInt(value, 10);
        return Math.max(1, Math.min(parsed, 30));
    }

    @Nonnull
    private String normalizeWebSearchQuery(@Nullable String query) {
        String normalized = StringUtils.nullToEmpty(query).trim();
        if (!looksLikeBadFollowUpSearchQuery(normalized)) {
            return normalized;
        }
        String resolved = resolveFollowUpSearchTargetInternal(normalized);
        if (StringUtils.isNotNullOrEmpty(resolved)) {
            LOGGER.debug("AI resolved follow-up web search query group={} user={} query='{}' resolved='{}'",
                    ctx.scopeGroupId(), ctx.senderId(), normalized, resolved);
            return resolved;
        }
        return normalized;
    }

    @Nonnull
    private String resolveFollowUpSearchTargetInternal(@Nullable String hint) {
        String normalizedHint = StringUtils.nullToEmpty(hint).trim();
        String fromHint = extractSearchCandidate(normalizedHint);
        if (isPlausibleSearchTarget(fromHint) && !looksLikeBadFollowUpSearchQuery(fromHint)) {
            return fromHint;
        }
        if (messageRecordManager == null || ctx.bot() == null) {
            return "";
        }
        MessageRecord anchor = findQuotedRecord();
        MessageRecordQueryParam param = baseHistoryParam(80);
        if (anchor != null && anchor.getId() != null && anchor.getId() > 0) {
            param.setIdByLt(anchor.getId());
        } else if (anchor != null && anchor.getTime() != null && anchor.getTime() > 0) {
            param.setTimeByLt(anchor.getTime());
        }
        List<MessageRecord> records = messageRecordManager.getMessageRecordList(param);
        String best = "";
        int bestScore = Integer.MIN_VALUE;
        for (MessageRecord record : records) {
            if (record == null || record.recalled() || record.getSenderId() != null && record.getSenderId() == ctx.botId()) {
                continue;
            }
            String candidate = extractSearchCandidate(plainRecordText(
                    record,
                    record.getGroupId() != null && record.getGroupId() > 0 ? record.getGroupId() : ctx.scopeGroupId(),
                    new ChatMessageContextFormatter.UserInfoCache()
            ));
            if (!isPlausibleSearchTarget(candidate)) {
                continue;
            }
            int score = searchTargetScore(candidate);
            if (score > bestScore) {
                best = candidate;
                bestScore = score;
            }
        }
        return best;
    }

    @Nullable
    private MessageRecord findQuotedRecord() {
        long replyId = 0;
        if (ctx.messageContext() != null && ctx.messageContext().originalMsg() != null) {
            for (ArrayMsg msg : ctx.messageContext().originalMsg()) {
                if (msg != null && msg.getType() == com.mikuac.shiro.enums.MsgTypeEnum.reply) {
                    replyId = msg.getLongData("id");
                    break;
                }
            }
        }
        if (replyId <= 0) {
            Matcher matcher = CQ_REPLY_ID.matcher(StringUtils.nullToEmpty(ctx.userMessage()));
            if (matcher.find()) {
                replyId = StringUtils.toLong(matcher.group(1), 0L);
            }
        }
        if (replyId <= 0 || replyId > Integer.MAX_VALUE) {
            return null;
        }
        MessageRecord record;
        if (ctx.msgType() == EnumMessageType.GROUP && ctx.scopeGroupId() > 0) {
            record = messageRecordManager.getGroupMessageRecord(ctx.scopeGroupId(), (int) replyId);
        } else {
            record = messageRecordManager.getPrivateMessageRecord(ctx.senderId() != null ? ctx.senderId() : 0L, (int) replyId);
        }
        return record != null && !record.recalled() ? record : null;
    }

    @Nonnull
    private static String extractSearchCandidate(@Nullable String text) {
        String candidate = StringUtils.nullToEmpty(text)
                .replaceAll("(?s)消息记录只供理解上下文.*", " ")
                .replaceAll("(?s)完整内容[:：].*", " ")
                .replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("引用消息\\[msgId=\\d+[^]]*]\\s*", " ")
                .replaceAll("\\[被@用户:[^]]+]", " ")
                .replaceAll("\\[图片链接]\\s*\\S+", " ")
                .replaceAll("https?://\\S+", " ")
                .replaceAll("@你", " ")
                .replaceAll("[\"“”'‘’`]", " ")
                .trim();
        candidate = candidate.replaceAll("(香草白茶|白茶酱|白茶|你再搜搜看|你再搜搜|再搜搜看|再搜搜|你搜搜|搜搜看|搜搜|再查查|查查|来看看这个|看看这个|看一下这个|好好分析|分析一下|这是什么|这个|确实不知道|我搜了也没搜到|没搜到|不知道|没找到)", " ");
        candidate = candidate.replaceAll("[，。！？!?,；;：:\\s]+", " ").trim();
        if (candidate.contains(" ")) {
            String best = "";
            for (String part : candidate.split("\\s+")) {
                if (part.length() > best.length() && isPlausibleSearchTarget(part)) {
                    best = part;
                }
            }
            if (StringUtils.isNotNullOrEmpty(best)) {
                candidate = best;
            }
        }
        return AiTextLimits.truncate(candidate.trim(), 80);
    }

    private static boolean looksLikeBadFollowUpSearchQuery(@Nullable String query) {
        String normalized = StringUtils.nullToEmpty(query).trim();
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return true;
        }
        String compact = normalized.replaceAll("\\s+", "");
        return compact.length() <= 1
                || compact.matches("(你)?再?(搜|搜索|搜搜|查|查查)(看|一下)?")
                || compact.contains("没搜到")
                || compact.contains("不知道")
                || compact.contains("没找到")
                || compact.contains("确实不知道");
    }

    private static boolean isPlausibleSearchTarget(@Nullable String value) {
        String text = StringUtils.nullToEmpty(value).trim();
        if (text.length() < 2 || text.length() > 60) {
            return false;
        }
        if (looksLikeBadFollowUpSearchQuery(text)) {
            return false;
        }
        if (text.contains("图片链接") || text.contains("消息记录") || text.contains("显示名可能被修改")) {
            return false;
        }
        return Pattern.compile("[\\p{IsHan}A-Za-z0-9]").matcher(text).find();
    }

    private static int searchTargetScore(@Nonnull String candidate) {
        int score = 0;
        int length = candidate.length();
        if (length <= 12) {
            score += 25;
        } else if (length <= 24) {
            score += 10;
        }
        if (candidate.contains("定律") || candidate.contains("是谁") || candidate.contains("什么")) {
            score += 40;
        }
        if (candidate.matches(".*[\\p{IsHan}A-Za-z].*")) {
            score += 10;
        }
        return score;
    }

    private static boolean requiresConfirmation(AiCapability capability, @Nonnull Map<String, String> args) {
        if (capability == null || "true".equalsIgnoreCase(args.getOrDefault("confirm", ""))) {
            return false;
        }
        return capability.requireConfirmation()
                || capability.confirmationPolicy() == AiConfirmationPolicy.ALWAYS
                || capability.confirmationPolicy() == AiConfirmationPolicy.REQUIRED_FOR_SENSITIVE && capability.sensitive();
    }

    @Nonnull
    private static String policyBlockForModel(@Nonnull String capabilityName, @Nonnull String reason) {
        String normalized = capabilityName.trim().toLowerCase(java.util.Locale.ROOT);
        if (reason.contains("权限不足")) {
            return reason + " 请直接告诉当前发起者没有权限，不要列工具名或让用户复述指令。";
        }
        if (reason.contains("全员禁言") || reason.contains("全员解禁")) {
            return "全员禁言还没有执行成功。请根据当前上下文重试对应工具，或简短说明还缺少明确确认；禁止声称接口不支持全员禁言。";
        }
        if (reason.contains("需要当前消息明确确认")) {
            return reason + " 请先用一句话向用户确认，不要执行。";
        }
        if (normalized.contains("kanri") || reason.contains("群管")) {
            return "未能执行禁言：请结合上文已确认的对象调用 searchGroupMember 和 muteGroupMembers，不要要求用户复述固定指令。";
        }
        if (reason.contains("已阻止") || reason.contains("没有明确")) {
            return "未能执行：请根据当前消息重新选择工具参数，不要向用户解释内部规则或工具名称。";
        }
        return reason;
    }

    @Nonnull
    private static String normalizeTags(@Nullable String tags, @Nonnull String source) {
        String tagText = tags != null ? tags.trim() : "";
        if (StringUtils.isNullOrEmptyEx(tagText)) {
            return "source:" + source;
        }
        if (tagText.contains("source:")) {
            return tagText;
        }
        return tagText + ",source:" + source;
    }

    @Nonnull
    private static String normalizeTarget(@Nullable String target) {
        if (target == null) {
            return "";
        }
        String trimmed = target.trim();
        Matcher cqAt = Pattern.compile("\\[CQ:at,qq=(\\d+)]").matcher(trimmed);
        if (cqAt.find()) {
            return cqAt.group(1);
        }
        Matcher number = Pattern.compile("\\d+").matcher(trimmed);
        return number.find() ? number.group() : "";
    }

    @Nonnull
    private String resolveGroupCardTarget(@Nullable String target, @Nullable String card) {
        String current = StringUtils.nullToEmpty(ctx.userMessage());
        String normalized = normalizeTarget(target);
        CardTargetHint hint = inferGroupCardTargetFromCard(current, card);
        if (hint == CardTargetHint.SENDER && ctx.senderId() != null && ctx.senderId() > 0) {
            return String.valueOf(ctx.senderId());
        }
        if (hint == CardTargetHint.BOT && ctx.bot() != null) {
            return String.valueOf(ctx.botId());
        }
        if (isSelfGroupCardRequest(current) && ctx.senderId() != null && ctx.senderId() > 0) {
            return String.valueOf(ctx.senderId());
        }
        if (StringUtils.isNullOrEmptyEx(normalized)) {
            return "";
        }
        if (ctx.bot() != null && StringUtils.toLong(normalized, 0L) == ctx.botId() && !isBotGroupCardRequest(current)) {
            LOGGER.debug("AI blocked bot-card target from likely wake mention group={} sender={} message={}",
                    ctx.scopeGroupId(), ctx.senderId(), current);
            return "";
        }
        return normalized;
    }

    @Nonnull
    private String normalizeGroupCardActionArgs(@Nullable String args) {
        String argLine = StringUtils.nullToEmpty(args).trim();
        if (StringUtils.isNullOrEmptyEx(argLine)) {
            return "";
        }
        String current = StringUtils.nullToEmpty(ctx.userMessage());
        String target = normalizeTarget(argLine);
        if (StringUtils.isNullOrEmptyEx(target)) {
            return "";
        }
        String card = normalizeCard(removeLeadingTarget(argLine));
        CardTargetHint hint = inferGroupCardTargetFromCard(current, card);
        if (hint == CardTargetHint.SENDER && ctx.senderId() != null && ctx.senderId() > 0) {
            return StringUtils.isNotNullOrEmpty(card) ? ctx.senderId() + " " + card : "";
        }
        if (hint == CardTargetHint.BOT && ctx.bot() != null) {
            return StringUtils.isNotNullOrEmpty(card) ? ctx.botId() + " " + card : "";
        }
        if (isSelfGroupCardRequest(current) && ctx.senderId() != null && ctx.senderId() > 0) {
            return StringUtils.isNotNullOrEmpty(card) ? ctx.senderId() + " " + card : "";
        }
        if (ctx.bot() != null && StringUtils.toLong(target, 0L) == ctx.botId() && !isBotGroupCardRequest(current)) {
            LOGGER.debug("AI blocked bot-card action target from likely wake mention group={} sender={} args={} message={}",
                    ctx.scopeGroupId(), ctx.senderId(), argLine, current);
            return "";
        }
        return StringUtils.isNotNullOrEmpty(card) ? target + " " + card : "";
    }

    @Nonnull
    private static String removeLeadingTarget(@Nonnull String args) {
        String trimmed = args.trim();
        Matcher cqAt = Pattern.compile("^\\[CQ:at,qq=\\d+][\\s,，:：]*").matcher(trimmed);
        if (cqAt.find()) {
            return trimmed.substring(cqAt.end()).trim();
        }
        Matcher number = Pattern.compile("^\\d{5,}[\\s,，:：]*").matcher(trimmed);
        if (number.find()) {
            return trimmed.substring(number.end()).trim();
        }
        return trimmed;
    }

    private static boolean isSelfGroupCardRequest(@Nullable String current) {
        String text = stripCq(current);
        String compact = text.replaceAll("\\s+", "");
        return containsCardWord(compact)
                && (compact.contains("我的")
                || compact.contains("把我")
                || compact.contains("给我")
                || compact.contains("帮我把我"));
    }

    private static boolean isBotGroupCardRequest(@Nullable String current) {
        String compact = stripCq(current).replaceAll("\\s+", "");
        return containsCardWord(compact)
                && containsAnyStatic(compact, "你的", "你自己", "把你", "给你", "将你", "替你");
    }

    private enum CardTargetHint {
        BOT,
        SENDER,
        UNKNOWN
    }

    @Nonnull
    private CardTargetHint inferGroupCardTargetFromCard(@Nullable String current, @Nullable String card) {
        String normalizedCard = normalizeCard(card);
        if (StringUtils.isNullOrEmptyEx(normalizedCard)) {
            return CardTargetHint.UNKNOWN;
        }
        String compact = stripCq(current).replaceAll("\\s+", "");
        int cardIndex = compact.indexOf(normalizedCard);
        if (cardIndex < 0) {
            return CardTargetHint.UNKNOWN;
        }
        String segment = sentenceSegmentAround(compact, cardIndex);
        if (!containsCardWord(segment)) {
            return CardTargetHint.UNKNOWN;
        }
        if (containsAnyStatic(segment, "我的", "把我", "给我", "将我", "替我", "帮我把我")) {
            return CardTargetHint.SENDER;
        }
        if (containsAnyStatic(segment, "你的", "你自己", "把你", "给你", "将你", "替你", "帮你把你")) {
            return CardTargetHint.BOT;
        }
        return CardTargetHint.UNKNOWN;
    }

    @Nonnull
    private static String sentenceSegmentAround(@Nonnull String text, int index) {
        int start = 0;
        int end = text.length();
        for (int i = index - 1; i >= 0; i--) {
            if (isSegmentSeparator(text.charAt(i))) {
                start = i + 1;
                break;
            }
        }
        for (int i = index; i < text.length(); i++) {
            if (isSegmentSeparator(text.charAt(i))) {
                end = i;
                break;
            }
        }
        return text.substring(start, end);
    }

    private static boolean isSegmentSeparator(char c) {
        return c == '，' || c == ',' || c == '。' || c == '；' || c == ';'
                || c == '！' || c == '!' || c == '？' || c == '?';
    }

    private static boolean containsCardWord(@Nonnull String text) {
        return containsAnyStatic(text, "群名片", "名片", "群昵称", "群昵称");
    }

    @Nonnull
    private static String stripCq(@Nullable String text) {
        return StringUtils.nullToEmpty(text).replaceAll("\\[CQ:[^]]+]", " ");
    }

    private static boolean containsAnyStatic(@Nonnull String text, String... needles) {
        for (String needle : needles) {
            if (StringUtils.isNotNullOrEmpty(needle) && text.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    @Nonnull
    private static String normalizeCard(@Nullable String card) {
        if (StringUtils.isNullOrEmptyEx(card)) {
            return "";
        }
        String normalized = card.replaceAll("[\\r\\n\\t]+", " ")
                .replaceAll("\\s+", "")
                .trim();
        if (normalized.length() > 24) {
            normalized = normalized.substring(0, 24);
        }
        return normalized;
    }

    @Nonnull
    private String wholeGroupConfirmValue(@Nullable String confirm) {
        if (PendingAiActionStore.isKanriProceedIntent(ctx.userMessage()) && recentlyDiscussedWholeGroupKanri()) {
            return "true";
        }
        if (!"true".equalsIgnoreCase(StringUtils.nullToEmpty(confirm).trim())) {
            return "false";
        }
        return PendingAiActionStore.isConfirmationText(ctx.userMessage()) ? "true" : "false";
    }

    private boolean recentlyDiscussedWholeGroupKanri() {
        if (messageRecordManager == null || ctx.bot() == null || ctx.scopeGroupId() <= 0) {
            return false;
        }
        try {
            List<MessageRecord> records = messageRecordManager.getMessageRecordList(baseHistoryParam(16));
            for (MessageRecord record : records) {
                if (record == null) {
                    continue;
                }
                String text = StringUtils.nullToEmpty(record.getMsgRecode());
                if (StringUtils.isNullOrEmptyEx(text)) {
                    continue;
                }
                if (mentionsWholeGroup(text) && containsKanriMuteVerb(text)) {
                    return true;
                }
                if (record.getSenderId() != null && record.getSenderId() == ctx.botId()
                        && mentionsWholeGroup(text)
                        && (text.contains("确定") || text.contains("确认") || text.contains("要开") || text.contains("要关"))) {
                    return true;
                }
            }
        } catch (Exception e) {
            LOGGER.debug("AI whole-group kanri history lookup failed group={} user={}",
                    ctx.scopeGroupId(), ctx.senderId(), e);
        }
        return false;
    }

    private static boolean mentionsWholeGroup(@Nonnull String text) {
        String lower = text.toLowerCase(java.util.Locale.ROOT);
        return text.contains("全员")
                || text.contains("全体")
                || text.contains("@全员")
                || text.contains("@全体")
                || lower.contains("@all");
    }

    private static boolean containsKanriMuteVerb(@Nonnull String text) {
        String lower = text.toLowerCase(java.util.Locale.ROOT);
        return text.contains("禁言")
                || text.contains("解禁")
                || text.contains("关闭")
                || text.contains("开启")
                || text.contains("打开")
                || lower.contains("mute")
                || lower.contains("loud");
    }

    @Nullable
    private static Path resolveConfigPath(@Nullable String path) {
        String raw = StringUtils.nullToEmpty(path).trim().replace("\\", "/");
        if (StringUtils.isNullOrEmptyEx(raw)) {
            return null;
        }
        while (raw.startsWith("/")) {
            raw = raw.substring(1);
        }
        if (raw.startsWith("config/")) {
            raw = raw.substring("config/".length());
        }
        Path root = Path.of("config").toAbsolutePath().normalize();
        Path target = root.resolve(raw).normalize();
        if (!target.startsWith(root)) {
            return null;
        }
        String lower = target.getFileName() != null
                ? target.getFileName().toString().toLowerCase(java.util.Locale.ROOT)
                : "";
        if (!(lower.endsWith(".yml") || lower.endsWith(".yaml") || lower.endsWith(".json")
                || lower.endsWith(".properties") || lower.endsWith(".txt"))) {
            return null;
        }
        return target;
    }

    @Nonnull
    private static String redactSensitiveConfigLines(@Nonnull String content) {
        StringBuilder out = new StringBuilder();
        String[] lines = content.split("\\R", -1);
        for (String line : lines) {
            if (isSensitiveConfigLine(line)) {
                int idx = line.indexOf(':');
                out.append(idx >= 0 ? line.substring(0, idx + 1) + " \"***\"" : "***");
            } else {
                out.append(line);
            }
            out.append('\n');
        }
        return out.toString();
    }

    private static boolean touchesSensitiveConfig(@Nonnull String text) {
        return Arrays.stream(text.split("\\R"))
                .anyMatch(AiToolBridge::isSensitiveConfigLine);
    }

    private static boolean isSensitiveConfigLine(@Nonnull String line) {
        String lower = line.toLowerCase(java.util.Locale.ROOT);
        return lower.matches(".*(^|[\\s_-])(api[-_ ]?key|apikey|key|token|secret|password|passwd|cookie|authorization|access[-_ ]?token)([\\s_-]|:).*");
    }

    @Nonnull
    private static String configSnippet(@Nonnull String content, @Nonnull String keyword, int radius) {
        String[] lines = content.split("\\R", -1);
        List<Integer> hits = new ArrayList<>();
        for (int i = 0; i < lines.length; i++) {
            if (lines[i].contains(keyword)) {
                hits.add(i);
            }
        }
        if (hits.isEmpty()) {
            return "未找到关键词：" + keyword;
        }
        StringBuilder out = new StringBuilder();
        int lastEnd = -1;
        for (Integer hit : hits) {
            int start = Math.max(0, hit - radius);
            int end = Math.min(lines.length - 1, hit + radius);
            if (start <= lastEnd) {
                start = lastEnd + 1;
            }
            if (!out.isEmpty()) {
                out.append("\n...\n");
            }
            for (int i = start; i <= end; i++) {
                out.append(lines[i]).append('\n');
            }
            lastEnd = end;
            if (out.length() > 5000) {
                break;
            }
        }
        return out.toString();
    }

    private static void validateConfigText(@Nonnull Path target, @Nonnull String text) throws Exception {
        String lower = target.getFileName().toString().toLowerCase(java.util.Locale.ROOT);
        if (lower.endsWith(".yml") || lower.endsWith(".yaml")) {
            new com.fasterxml.jackson.databind.ObjectMapper(new com.fasterxml.jackson.dataformat.yaml.YAMLFactory())
                    .readTree(text);
        } else if (lower.endsWith(".json")) {
            new com.fasterxml.jackson.databind.ObjectMapper().readTree(text);
        }
    }

    @Nonnull
    private static ChatStickerService stickerService() {
        return SpringContextHolder.getBean(ChatStickerService.class);
    }

    @Nonnull
    private static String safeCodeFileName(@Nullable String fileName) {
        String name = StringUtils.isNotNullOrEmpty(fileName) ? fileName.trim() : "code.txt";
        name = name.replace("\\", "/");
        int slash = name.lastIndexOf('/');
        if (slash >= 0) {
            name = name.substring(slash + 1);
        }
        name = name.replaceAll("[/:*?\"<>|\\r\\n\\t]", "_").trim();
        if (name.isBlank()) {
            name = "code.txt";
        }
        if (!name.contains(".")) {
            name = name + ".txt";
        }
        return name.length() > 80 ? name.substring(name.length() - 80) : name;
    }

    private static boolean shouldUploadAsCodeFile(@Nullable String title, @Nonnull String content) {
        String trimmed = content.trim();
        if (trimmed.length() < 1200) {
            return false;
        }
        String lower = trimmed.toLowerCase(java.util.Locale.ROOT);
        String titleLower = StringUtils.nullToEmpty(title).toLowerCase(java.util.Locale.ROOT);
        if (titleLower.contains("代码") || titleLower.contains("html") || titleLower.contains("css")
                || titleLower.contains("javascript") || titleLower.contains("java") || titleLower.contains("gradle")) {
            return true;
        }
        return lower.contains("<!doctype html")
                || lower.contains("<html")
                || lower.contains("<style")
                || lower.contains("<script")
                || lower.contains("package ")
                || lower.contains("public class ")
                || lower.contains("import java.")
                || lower.contains("plugins {")
                || lower.contains("dependencies {")
                || lower.contains("function ")
                || lower.contains("const ")
                || lower.contains("let ")
                || lower.contains("class ");
    }

    @Nonnull
    private static String suggestCodeFileName(@Nullable String title, @Nonnull String content) {
        String existing = extractExplicitFileName(title);
        if (StringUtils.isNotNullOrEmpty(existing)) {
            return existing;
        }
        String lowerTitle = StringUtils.nullToEmpty(title).toLowerCase(java.util.Locale.ROOT);
        String lowerContent = content.trim().toLowerCase(java.util.Locale.ROOT);
        String base = StringUtils.nullToEmpty(title)
                .replaceAll("[^\\p{IsAlphabetic}\\p{IsDigit}._-]+", "_")
                .replaceAll("_+", "_")
                .replaceAll("^_+|_+$", "");
        if (StringUtils.isNullOrEmptyEx(base) || base.length() > 40) {
            base = "code";
        }
        if (lowerContent.contains("<!doctype html") || lowerContent.contains("<html") || lowerTitle.contains("html")) {
            return base + ".html";
        }
        if (lowerContent.contains("plugins {") || lowerContent.contains("dependencies {") || lowerTitle.contains("gradle")) {
            return "build.gradle";
        }
        if (lowerContent.contains("public class ") || lowerContent.contains("package ") || lowerTitle.contains("java")) {
            String className = extractJavaClassName(content);
            return StringUtils.isNotNullOrEmpty(className) ? className + ".java" : base + ".java";
        }
        if (lowerContent.contains("function ") || lowerContent.contains("const ") || lowerContent.contains("let ")
                || lowerTitle.contains("javascript") || lowerTitle.contains("js")) {
            return base + ".js";
        }
        if (lowerContent.contains("{") && lowerContent.contains("}") && lowerTitle.contains("css")) {
            return base + ".css";
        }
        return base + ".txt";
    }

    @Nullable
    private static String extractExplicitFileName(@Nullable String title) {
        if (StringUtils.isNullOrEmptyEx(title)) {
            return null;
        }
        Matcher matcher = Pattern.compile("([A-Za-z0-9_.-]+\\.(?:java|kt|html|css|js|ts|json|xml|toml|gradle|mcmeta|txt|md|yml|yaml))",
                Pattern.CASE_INSENSITIVE).matcher(title);
        return matcher.find() ? matcher.group(1) : null;
    }

    @Nullable
    private static String extractJavaClassName(@Nonnull String content) {
        Matcher matcher = Pattern.compile("\\bpublic\\s+class\\s+([A-Za-z_$][A-Za-z0-9_$]*)").matcher(content);
        if (matcher.find()) {
            return matcher.group(1);
        }
        matcher = Pattern.compile("\\bclass\\s+([A-Za-z_$][A-Za-z0-9_$]*)").matcher(content);
        return matcher.find() ? matcher.group(1) : null;
    }

    @Nonnull
    private static String numberText(@Nullable String value, @Nonnull String defaultValue) {
        if (value == null) {
            return defaultValue;
        }
        Matcher matcher = FIRST_NUMBER.matcher(value);
        return matcher.find() ? matcher.group() : defaultValue;
    }

    @Nonnull
    private String muteDurationText(@Nullable String value) {
        int parsed = KanriMuteIntent.extractMinutes(value, 0);
        if (parsed <= 0) {
            parsed = KanriMuteIntent.extractMinutes(ctx.userMessage(), 0);
        }
        return parsed > 0 ? String.valueOf(parsed) : numberText(value, "10");
    }

    @Nonnull
    private static String integerText(@Nullable String value, @Nonnull String defaultValue) {
        String number = numberText(value, defaultValue);
        int dot = number.indexOf('.');
        return dot >= 0 ? number.substring(0, dot) : number;
    }

}
