package xin.vanilla.banira.plugin.socialmedia;

import java.util.List;

/**
 * 社交媒体解析器
 */
public interface SocialMediaParser {

    boolean hasSocialMedia(String msg);

    List<SocialMediaContent> parse(String msg);

    void build(SocialMediaContent content);

}
