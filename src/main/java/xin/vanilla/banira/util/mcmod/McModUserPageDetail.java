package xin.vanilla.banira.util.mcmod;

import lombok.Data;
import lombok.experimental.Accessors;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * MCMod 用户中心详情
 */
@Data
@Accessors(chain = true, fluent = true)
public class McModUserPageDetail {
    private String userId;
    private String link;
    private String typeName;
    private String username;
    private String avatarUrl;
    private String sign;
    private Integer rank;
    private Integer online;
    private String expTotal;
    private String expYet;
    private Integer expRate;
    private String userGroup;
    private String registerTime;
    private List<McModPageProp> stats = new ArrayList<>();
    private List<McModUserCardTracker> trackers = new ArrayList<>();
    private List<McModUserCardBadge> badges = new ArrayList<>();
    private List<String> developerMods = new ArrayList<>();

    public boolean isRenderable() {
        return StringUtils.isNotNullOrEmpty(username) || StringUtils.isNotNullOrEmpty(userId);
    }
}
