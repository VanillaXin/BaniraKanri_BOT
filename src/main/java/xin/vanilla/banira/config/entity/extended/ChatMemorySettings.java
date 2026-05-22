package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class ChatMemorySettings {
    /**
     * 每次对话自动检索并注入的长期记忆条数
     */
    private int retrieveLimit = 5;
    /**
     * 是否在对话结束后异步提取长期记忆
     */
    private boolean autoExtract = true;
    /**
     * 单轮最多自动写入几条长期记忆
     */
    private int maxExtractedPerTurn = 3;
    /**
     * 是否保存低重要度会话记忆，用于记住自己最近说过的普通判断或承诺
     */
    private boolean lowImportanceMemoryEnabled = true;
    /**
     * 每次自动注入的低重要度会话记忆条数
     */
    private int lowImportanceRetrieveLimit = 3;
    /**
     * 低重要度会话记忆最大字符数
     */
    private int maxLowImportanceMemoryChars = 220;
    /**
     * 单条长期记忆最大字符数
     */
    private int maxMemoryChars = 160;
    /**
     * 小于该长度的内容不写入长期记忆
     */
    private int minMemoryChars = 4;
    /**
     * 是否允许 Agent 主动调用 saveMemory 写入记忆
     */
    private boolean allowToolSave = true;
    /**
     * 是否启用语义记忆检索。未配置可用 embeddings 接口时会自动回退关键词检索。
     */
    private boolean semanticRetrieveEnabled = true;
    /**
     * Embeddings 接口名称；为空时使用 model.defaultEndpoint，仍为空则使用第一个可用 LLM endpoint。
     */
    private String embeddingEndpoint = "";
    /**
     * OpenAI 兼容 embeddings 模型名。
     */
    private String embeddingModelName = "text-embedding-3-small";
    /**
     * 语义检索最多取几条。
     */
    private int semanticTopK = 5;
    /**
     * 语义记忆最低余弦相似度。
     */
    private double semanticMinScore = 0.62;
    /**
     * 语义检索不可用或结果不足时是否回退关键词检索。
     */
    private boolean keywordFallbackEnabled = true;
    /**
     * 自动记忆抽取队列上限。
     */
    private int memoryExtractionQueueSize = 100;
}
