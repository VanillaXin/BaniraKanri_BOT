package xin.vanilla.banira.coder.common;

import com.google.gson.JsonObject;
import com.mikuac.shiro.common.utils.ShiroUtils;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.NonNull;
import xin.vanilla.banira.util.CollectionUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class BaniraCodeUtils {

    private BaniraCodeUtils() {
    }

    @Nonnull
    public static List<BaniraCode> getAllBaniraCode(@NonNull String msg) {
        if (StringUtils.isNullOrEmpty(msg)) {
            return new ArrayList<>();
        }

        List<BaniraCode> chain = new ArrayList<>();
        chain.add(new BaniraCode().setType("text").setRaw(msg).setData(new JsonObject()));
        int len = msg.length();
        int i = 0;

        while (i < len) {
            boolean isBkode = true;
            for (int j = 0; j < MessageCoder.CODE_START.length(); j++) {
                if (i + j >= len || msg.charAt(i + j) != MessageCoder.CODE_START.charAt(j)) {
                    isBkode = false;
                    break;
                }
            }

            if (isBkode) {
                // 找到 bk 码开始位置
                int start = i;
                i += MessageCoder.CODE_START.length(); // 跳过 "[bkode:"

                // 解析 bk 码类型
                StringBuilder typeBuilder = new StringBuilder();
                while (i < len && msg.charAt(i) != MessageCoder.ARG_SEPARATOR && msg.charAt(i) != MessageCoder.VAL_SEPARATOR
                        && !msg.startsWith(MessageCoder.CODE_END, i)
                ) {
                    typeBuilder.append(msg.charAt(i));
                    i++;
                }

                if (i >= len) {
                    // 格式错误，当作普通文本处理
                    addTextMsg(chain, msg.substring(start, len));
                    break;
                }

                String type = typeBuilder.toString();
                Map<String, String> data = new HashMap<>();

                // 解析参数
                if (msg.charAt(i) == MessageCoder.ARG_SEPARATOR || (msg.charAt(i) == MessageCoder.VAL_SEPARATOR)) {
                    i++; // 跳过 ARG_SEPARATOR || VAL_SEPARATOR
                    while (i < len
                            && !msg.startsWith(MessageCoder.CODE_END, i)
                    ) {
                        // 解析键
                        StringBuilder keyBuilder = new StringBuilder();
                        while (i < len && msg.charAt(i) != MessageCoder.VAL_SEPARATOR
                                && !msg.startsWith(MessageCoder.CODE_END, i)
                        ) {
                            keyBuilder.append(msg.charAt(i));
                            i++;
                        }

                        // 解析值
                        StringBuilder valueBuilder = new StringBuilder();
                        if (msg.charAt(i) == MessageCoder.VAL_SEPARATOR) {
                            i++; // 跳过 VAL_SEPARATOR
                            while (i < len && msg.charAt(i) != MessageCoder.ARG_SEPARATOR
                                    && !msg.startsWith(MessageCoder.CODE_END, i)
                            ) {
                                valueBuilder.append(msg.charAt(i));
                                i++;
                            }
                        } else {
                            valueBuilder.append(keyBuilder);
                            keyBuilder = new StringBuilder("value");
                        }

                        String key = keyBuilder.toString();
                        String value = ShiroUtils.unescape(valueBuilder.toString());
                        if (!key.isEmpty()) {
                            data.put(key, value);
                        }

                        if (i < len && msg.charAt(i) == MessageCoder.ARG_SEPARATOR) {
                            i++; // 跳过 ARG_SEPARATOR 继续解析下一个参数
                        }
                    }
                }

                if (msg.startsWith(MessageCoder.CODE_END, i)) {
                    i++; // 跳过 CODE_END
                    // 创建 BaniraCode
                    BaniraCode item = new BaniraCode();
                    item.setType(type);
                    item.setRaw(msg.substring(start, i));
                    item.setData(JsonUtils.parseJsonObject(data));
                    chain.add(item);
                    addTextMsg(chain, placeholder(chain.size() - 1));
                } else {
                    // 格式错误，当作普通文本处理
                    addTextMsg(chain, msg.substring(start, i));
                }
            } else {
                // 普通文本
                StringBuilder textBuilder = new StringBuilder();
                while (i < len && !(msg.charAt(i) == MessageCoder.CODE_START.charAt(0)
                        && i + MessageCoder.CODE_START.length() < len && msg.startsWith(MessageCoder.CODE_START, i))
                ) {
                    textBuilder.append(msg.charAt(i));
                    i++;
                }
                String text = textBuilder.toString();
                if (!text.isEmpty()) {
                    addTextMsg(chain, text);
                }
            }
        }
        return chain;
    }

    private static void addTextMsg(List<BaniraCode> chain, String text) {
        if (StringUtils.isNullOrEmpty(text)) {
            return;
        }
        // 合并普通文本
        if (!chain.isEmpty()) {
            BaniraCode textCode = chain.stream().filter(m -> m.getType().equalsIgnoreCase("text")).findFirst().orElse(null);
            if (textCode != null) {
                JsonObject obj = textCode.getData();
                if (obj.has("text")) text = obj.get("text").getAsString() + text;
                obj.addProperty("text", text);
                return;
            }
        }
        // 创建新的文本消息
        BaniraCode item = new BaniraCode();
        item.setType("text");
        item.setData(new JsonObject());
        item.getData().addProperty("text", text);
        chain.add(item);
    }

    public static String placeholder(int i) {
        return MessageCoder.CODE_START + i + MessageCoder.CODE_END;
    }

    @Nullable
    public static BaniraCode getTextBaniraCode(List<BaniraCode> codeList) {
        if (CollectionUtils.isNullOrEmpty(codeList)) return null;
        return codeList.stream().filter(c -> "text".equals(c.getType())).findFirst().orElse(null);
    }

    public static void main(String[] args) {
        String s = "abcds[bkode:tg:123456789] wdwe [bkode:tf,value:123456789,data:012456]ds2ad\n" +
                "[bkode:img,value:https://www.baidu.com/img/bd_logo1.png]\n" +
                "mute[bkode:mute:60-18000]";
        List<BaniraCode> codeList = getAllBaniraCode(s);
        System.out.println(codeList);
    }
}
