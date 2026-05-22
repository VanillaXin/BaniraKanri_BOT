package xin.vanilla.banira.plugin.chat.memory;

import xin.vanilla.banira.util.BaniraUtils;
import xin.vanilla.banira.util.StringUtils;

public final class MemoryScopePolicy {

    private MemoryScopePolicy() {
    }

    public static boolean isUnsafeOwnerTitleClaim(long senderId, String content) {
        if (BaniraUtils.isOwner(senderId) || StringUtils.isNullOrEmptyEx(content)) {
            return false;
        }
        String normalized = content.replaceAll("\\[CQ:[^]]+]", " ")
                .replaceAll("\\s+", "")
                .trim();
        if (!normalized.contains("主人")) {
            return false;
        }
        return normalized.contains("叫我主人")
                || normalized.contains("喊我主人")
                || normalized.contains("称呼我为主人")
                || normalized.contains("称呼当前用户为主人")
                || normalized.contains("当前用户希望被叫主人")
                || normalized.contains("当前用户希望被称呼为主人")
                || normalized.contains("用户希望被叫主人")
                || normalized.contains("用户希望被称呼为主人")
                || normalized.matches(".*(称呼|叫|喊|称为|叫作).{0,12}主人.*");
    }
}
