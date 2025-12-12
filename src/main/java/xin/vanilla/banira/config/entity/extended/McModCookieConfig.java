package xin.vanilla.banira.config.entity.extended;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * MCMod Cookie 配置
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class McModCookieConfig {

    /**
     * 用户名
     */
    private String username;

    /**
     * 密码
     */
    private String password;

    /**
     * Cookie 字符串
     */
    private String cookie;

    /**
     * Cookie 过期时间（时间戳，毫秒）
     */
    private Long expireTime;

    /**
     * 当前登录用户ID
     */
    private String userId;

    /**
     * 默认过期时间：25天
     */
    private static final long DEFAULT_EXPIRE_DURATION = 25L * 24 * 60 * 60 * 1000;

    /**
     * 检查 Cookie 是否过期
     */
    public boolean isExpired() {
        if (expireTime == null) {
            return true;
        }
        return System.currentTimeMillis() > expireTime;
    }

    /**
     * 设置 Cookie 并更新过期时间（默认25天）
     */
    public McModCookieConfig setCookieWithExpire(String cookie) {
        this.cookie = cookie;
        this.expireTime = System.currentTimeMillis() + DEFAULT_EXPIRE_DURATION;
        return this;
    }

    /**
     * 设置 Cookie 并指定过期时间
     *
     * @param cookie     Cookie 字符串
     * @param expireTime 过期时间（时间戳，毫秒）
     */
    public McModCookieConfig setCookieWithExpire(String cookie, Long expireTime) {
        this.cookie = cookie;
        this.expireTime = expireTime;
        return this;
    }
}
