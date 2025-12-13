package xin.vanilla.banira.util.mcmod;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.mikuac.shiro.common.utils.MsgUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * MCMod 评论行
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
public class McModCommentRow {
    /**
     * 评论类型
     */
    private EnumContentType commentType;

    /**
     * 容器ID
     */
    private String containerId;
    /**
     * 评论ID
     */
    private String id;
    /**
     * 楼层
     */
    private String floor;
    /**
     * 用户信息
     */
    private McModCommentUser user;
    /**
     * 时间信息
     */
    private McModCommentTime time;
    /**
     * 态度/表情信息
     */
    private McModCommentAttitude attitude;
    /**
     * 回复数量
     */
    @JsonProperty("reply_count")
    private String replyCount;
    /**
     * 评论内容（HTML格式）
     */
    private String content;

    /**
     * 被回复的用户信息
     */
    @JsonProperty("reply_user")
    private McModCommentUser replyUser;

    /**
     * 父评论ID
     */
    private String parentId;

    /**
     * 回复列表
     */
    transient private List<McModCommentRow> replies;
    /**
     * 父评论信息
     */
    transient private McModCommentRow parentComment;

    public boolean isReply() {
        return StringUtils.isNotNullOrEmpty(this.parentId);
    }

    @JsonIgnore
    public String getFormattedContent() {
        String result = "";
        if (StringUtils.isNotNullOrEmpty(this.content)) {
            Document document = Jsoup.parse(this.content);
            AtomicInteger index = new AtomicInteger(0);
            List<String> imgUrls = new ArrayList<>();
            document.select("img").replaceAll((e) -> {
                imgUrls.add(e.attr("src"));
                return new Element("div").text("[bkode:img:" + index.incrementAndGet() + "]");
            });
            result = document.text();
            for (int i = 0; i < imgUrls.size(); i++) {
                result = result.replace("[bkode:img:" + (i + 1) + "]", MsgUtils.builder().img(imgUrls.get(i)).build());
            }
        }
        return result;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof McModCommentRow that)) return false;
        return getCommentType() == that.getCommentType()
                && Objects.equals(getContainerId(), that.getContainerId())
                && Objects.equals(getId(), that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getCommentType(), getContainerId(), getId());
    }

}
