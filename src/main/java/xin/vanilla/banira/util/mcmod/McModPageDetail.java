package xin.vanilla.banira.util.mcmod;

import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * MCMod 详情页解析结果
 */
@Data
@Accessors(chain = true, fluent = true)
public class McModPageDetail {
    private EnumContentType contentType;
    private String typeName;
    private String id;
    private String link;
    private String title;
    private String subTitle;
    private String shortName;
    private String coverUrl;
    private String iconUrl;
    private String description;
    private String score;
    private String scoreComment;
    private String yIndex;
    private String yIndexAvg;
    private String viewNum;
    private String fillRate;
    private String pushNum;
    private String favNum;
    private String subscribeNum;
    private String redVote;
    private String blackVote;
    private String bio;
    private List<McModPageTag> tags = new ArrayList<>();
    private List<McModPageProp> metaItems = new ArrayList<>();
    private List<McModPageProp> props = new ArrayList<>();
    private List<String> categories = new ArrayList<>();
    private List<McModPageAuthor> authorDetails = new ArrayList<>();
    private List<String> authors = new ArrayList<>();
    private List<McModPageVersion> versions = new ArrayList<>();
    private List<String> relatedMods = new ArrayList<>();
    private List<String> links = new ArrayList<>();
    private boolean pageFetched;
    private McModUserInteractionState userInteraction;

    public boolean isRich() {
        if (contentType == EnumContentType.AUTHOR) {
            return StringUtils.isNotNullOrEmpty(bio)
                    || !metaItems.isEmpty()
                    || StringUtils.isNotNullOrEmpty(viewNum);
        }
        return StringUtils.isNotNullOrEmpty(description)
                || !metaItems.isEmpty()
                || !props.isEmpty()
                || !tags.isEmpty()
                || StringUtils.isNotNullOrEmpty(score)
                || !authorDetails.isEmpty()
                || !versions.isEmpty()
                || !relatedMods.isEmpty();
    }

    public String getDisplayCover() {
        if (StringUtils.isNotNullOrEmpty(coverUrl)) {
            return coverUrl;
        }
        return iconUrl;
    }

    public String getDisplayIcon() {
        if (StringUtils.isNotNullOrEmpty(iconUrl)) {
            return iconUrl;
        }
        return coverUrl;
    }
}
