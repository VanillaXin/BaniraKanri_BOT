package xin.vanilla.banira.plugin.mcmod;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.mcmod.McModContent;
import xin.vanilla.banira.util.mcmod.McModSearchResult;

import java.util.ArrayList;
import java.util.List;

/**
 * MCMod 搜索列表选择会话
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class McModSearchListSession {

    /**
     * 列表消息 ID（用户需回复此消息）
     */
    private Long messageId;

    /**
     * 发起搜索的用户
     */
    private Long userId;

    /**
     * 群号，私聊为 0
     */
    private Long groupId;

    /**
     * 搜索类型名称（模组、整合包、用户等）
     */
    private String typeName;

    /**
     * 列表数据来源
     */
    private McModSearchListSource source;

    /**
     * 搜索页结果
     */
    private List<McModSearchResult> searchResults;

    /**
     * CommonSelect 结果
     */
    private List<McModContent> contents;

    /**
     * 过期时间戳（毫秒）
     */
    private long expireAt;

    {
        this.messageId = 0L;
        this.userId = 0L;
        this.groupId = 0L;
        this.typeName = "";
        this.source = McModSearchListSource.SEARCH;
        this.searchResults = new ArrayList<>();
        this.contents = new ArrayList<>();
        this.expireAt = 0L;
    }

    public boolean expired() {
        return expireAt > 0 && System.currentTimeMillis() > expireAt;
    }

    public int itemCount() {
        return source == McModSearchListSource.CONTENT ? contents.size() : searchResults.size();
    }

}
