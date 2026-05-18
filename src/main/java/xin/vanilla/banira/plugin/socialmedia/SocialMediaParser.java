package xin.vanilla.banira.plugin.socialmedia;

import java.util.List;

/**
 * 社交媒体解析器
 */
public interface SocialMediaParser {

    String type();

    boolean hasSocialMedia(String msg);

    List<String> extractTargets(String msg);

    void build(SocialMediaContent content);

}
